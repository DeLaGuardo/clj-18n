(ns clj-i18n.macros
  (:require [clojure.java.io :refer [resource reader writer file make-parents]]
            [clj-yaml.core :refer [parse-string YAMLCodec]]
            [clojure.string :refer [join split] :as s]
            [clojure.pprint :refer [write]]
            [clj-i18n.plural-rules :as plr])
  (:import java.io.PushbackReader
           java.util.ArrayList))

;; Support
(defn deep-merge
  ([a] a)
  ([a b]
   (merge-with (fn [x y]
                 (cond
                   (and (not (map? x)) (map y)) y
                   (map? y) (deep-merge x y)
                   :else y))
               a b))
  ([a b & more]
   (apply deep-merge
          (deep-merge a b)
          more)))

(defn cartesian-product
  ([xs] xs)
  ([xs ys]
   (mapcat (fn [x]
             (map (fn [y]
                    (list x y))
                  ys))
           xs))
  ([xs ys & more]
   (mapcat (fn [x]
             (map (fn [z]
                    (cons x z))
                  (apply cartesian-product (cons ys more))))
           xs)))

;; Tools
(extend-protocol YAMLCodec
  ArrayList
  (decode [data]
    (vec (remove nil? data))))

(defn- write-dataset-edn! [out-file raw-dataset-map]
  (with-open [w (writer out-file)]
    (binding [*out* w]
      (write raw-dataset-map))))

;; Example: (yaml-to-edn "./rails-i18n/rails/locale/" "./resources/base/locale/")
(defn yaml-to-edn [from to]
  (let [yml-files (filter #(.endsWith (str %) ".yml")
                          (file-seq (file from)))]
    (map (fn [file]
           (let [data (first (vals (parse-string (slurp file))))
                 base-dir (s/replace (str file) (re-pattern (str from "|.yml"))
                                     {from to
                                      ".yml" ""})]
             (map (fn [key]
                    (let [key-file (str base-dir "/" (name key) ".edn")
                          data (key data)]
                      (make-parents key-file)
                      (write-dataset-edn! key-file data)))
                  (keys data))))
         yml-files)))

;; Default config
(def ^:dynamic *config* (atom {:current-locale "en"
                               :fallback "en"
                               :base-modules #{:date :datetime :errors :helpers :number :support :time}
                               :modules #{}
                               :locale-resources nil
                               :translations nil}))

(def ^:private base-path "base/locale")

(defn- read-file [file-name]
  (if-let [f (resource file-name)]
    (with-open [r (PushbackReader. (reader f))]
      (binding [*read-eval* false]
        (read r)))
    {}))

(defn- load-locale-module [module locale path-to-data]
  (let [file-name (join "/" [path-to-data locale (str (name module) ".edn")])]
    (read-file file-name)))

(defn- load-translations [locale]
  (let [modules (into #{}
                      (apply concat (-> @*config*
                                        (select-keys [:base-modules :modules])
                                        vals)))
        [lang _] (split locale #"-")
        locales (remove nil? (into (sorted-set (:fallback @*config*)) [lang locale]))
        paths (remove nil? [base-path (:locale-resources @*config*)])]
    (into {} (map (fn [module]
                    {module (apply deep-merge (map #(apply load-locale-module module %) (cartesian-product locales paths)))})
                  modules))))

(defmacro generate-translations! [locale & opts]
  (let [opts (apply sorted-map opts)]
    (swap! *config* merge opts)
    (swap! *config* assoc :translations (load-translations locale))
    (swap! *config* assoc :current-locale locale)
    `~(not (or (nil? (:translations @*config*)) (empty? (:translations @*config*))))))

(defmacro translate [& tr-keys]
  (let [translations-dict (let [tr-dict (last tr-keys)]
                            (if (map? tr-dict)
                              tr-dict
                              (:translations @*config*)))
        tr-keys (vec (remove map? tr-keys))]
    `~(get-in translations-dict tr-keys)))
