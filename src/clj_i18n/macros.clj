(ns clj-i18n.macros
  (:require [clojure.java.io :refer [file resource make-parents]]
            [clj-yaml.core :as yaml]
            [clojure.string :refer [split] :as s]))

(extend-protocol yaml/YAMLCodec
  java.util.ArrayList
  (decode [data]
    (vec (remove nil? data))))

(defn- deep-merge
  ([a b]
   (merge-with (fn [x y]
                 (cond (map? y) (deep-merge x y)
                       :else y))
               a b))
  ([a b & other]
   (apply deep-merge
          (deep-merge a b)
          (first other)
          (rest other))))

(defn- read-file [res]
  (let [res (resource res)]
    (first
     (vals
      (yaml/parse-string
       (slurp (if (.startsWith (str res) "f")
                (file res)
                res)))))))

(defn- read-translations [translation-key path-to-locale-data]
  (let [en-data (read-file "base/locale/en.yml")
        [locale _] (split translation-key #"-")
        locale-data (if (not= locale translation-key)
                      (deep-merge (read-file (str "base/locale/" locale ".yml"))
                                  (read-file (str "base/locale/" translation-key ".yml"))
                                  (if path-to-locale-data
                                    (read-file (str path-to-locale-data "/" locale ".yml"))
                                    {}))
                      (read-file (str "base/locale/" translation-key ".yml")))]
    (deep-merge en-data
                locale-data
                (if path-to-locale-data
                  (read-file (str path-to-locale-data "/" translation-key ".yml"))
                  {}))))

(def memoized-read-translations (memoize read-translations))

(defmacro generate-translations [locale & path-to-locale-data]
  (let [translations (read-translations locale (first path-to-locale-data))]
    `(do ~translations)))

(defmacro translate [locale translation-keys & path-to-locale-data]
  (let [translations (read-translations locale (first path-to-locale-data))
        translation (get-in translations translation-keys)]
    `(do ~translation)))

