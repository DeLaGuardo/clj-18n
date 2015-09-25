(set-env!
 :resource-paths #{"src" "resources"}
 :dependencies '[[org.clojure/clojure       "1.7.0"  :scope "provided"]
                 [org.clojure/clojurescript "1.7.48" :scope "test"]
                 [clj-yaml                  "0.4.0"  :scope "provided"]
                 [adzerk/bootlaces          "0.1.12" :scope "test"]])

(require '[adzerk.bootlaces :refer :all])

(def +version+ "0.1.0-SNAPSHOT")

(bootlaces! +version+)

(task-options!
 pom {:project     'delaguardo/i18n-cljs
      :version     +version+
      :description "ClojureScript RoR-style i18n library."
      :url         "https://github.com/DeLaGuardo/i18n-cljs"
      :scm         {:url "https://github.com/DeLaGuardo/i18n-cljs"}
      :license     {"EPL" "http://www.eclipse.org/legal/epl-v10.html"}})
