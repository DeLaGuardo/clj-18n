(ns clj-i18n.macros
  (:require [clojure.java.io :as io]
            [clj-yaml.core :as yaml]
            [clojure.string :as str]))

(extend-protocol yaml/YAMLCodec
  java.util.ArrayList
  (decode [data]
    (vec data)))

(defn deep-merge [a b]
  (merge-with (fn [x y]
                (cond (map? y) (deep-merge x y)
                      :else y))
              a b))

(defn read-translations [key]
  (let [en-fallback (yaml/parse-string (slurp "resources/i18n/en.yml"))
        [locale _] (str/split key #"-")
        main-translations (if (not= locale key)
                            (deep-merge (yaml/parse-string (slurp (str "resources/i18n/" locale ".yml")))
                                        (yaml/parse-string (slurp (str "resources/i18n/" key ".yml"))))
                            (yaml/parse-string (slurp (str "resources/i18n/" key ".yml"))))]
    (deep-merge en-fallback main-translations)))

(defmacro translations [key]
  (let [translations (read-translations key)]
    `(do ~translations)))


