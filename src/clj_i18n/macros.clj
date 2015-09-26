(ns clj-i18n.macros
  (:require [clj-i18n.tools :refer [cart deep-merge]]
            [clj-yaml.core :as yaml]
            [clojure.string :refer [split join] :as s]))

(def base-path "base/locale")
(def base-modules '(:date :datetime :errors :helpers :number :support :time))

(defn- read-file [file-name]
  (let [f (clojure.java.io/file (clojure.java.io/resource file-name))]
    (if (.exists f)
      (with-open [r (java.io.PushbackReader. (clojure.java.io/reader f))]
        (binding [*read-eval* false]
          (read r)))
      {})))

(defn- load-locale-module [module locale path-to-data]
  (let [file-name (join "/" [path-to-data locale (str (name module) ".edn")])]
    (read-file file-name)))

(defn- load-translations [locale {:keys [locale-resources modules] :or {modules base-modules}}]
  (let [[lang _] (split locale #"-")
        locales (remove nil? (into (sorted-set "en") [lang locale]))
        paths (remove nil? [base-path locale-resources])]
    (into {} (map (fn [module]
                    {module (apply deep-merge (map #(apply load-locale-module module %) (cart locales paths)))})
                  modules))))

(def memoized-read-translations (memoize load-translations))

(defmacro generate-translations [locale opts]
  (let [translations (load-translations locale opts)]
    `(do ~translations)))

(defmacro translate [locale translation-keys opts]
  (let [translations (load-translations locale opts)
        translation (get-in translations translation-keys)]
    `(do ~translation)))

