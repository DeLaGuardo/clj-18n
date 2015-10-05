(ns clj-i18n.tools.converters
  (:require [clojure.java.io :refer [file resource make-parents]]
            [clj-yaml.core :as yaml]
            [clojure.string :refer [split] :as s]))

(extend-protocol yaml/YAMLCodec
  java.util.ArrayList
  (decode [data]
    (vec (remove nil? data))))

(defn write-dataset-edn! [out-file raw-dataset-map]
  (with-open [w (clojure.java.io/writer out-file)]
    (binding [*out* w]
      (clojure.pprint/write raw-dataset-map))))

(defn yaml-to-edn []
  (let [yml-files (filter #(.endsWith (str %) ".yml")
                          (file-seq (file "./rails-i18n/rails/locale/")))]
    (map (fn [file]
           (let [data (first (vals (yaml/parse-string (slurp file))))
                 base-dir (s/replace (str file) #"rails-i18n/rails|.yml"
                                     {"rails-i18n/rails" "resources/base"
                                      ".yml" ""})]
             (map (fn [key]
                    (let [key-file (str base-dir "/" (name key) ".edn")
                          data (key data)]
                      (make-parents key-file)
                      (write-dataset-edn! key-file data)))
                  (keys data))))
         yml-files)))
