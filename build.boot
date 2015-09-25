(set-env!
 :source-paths #{"src"}
 :resource-paths #{"resources"}
 :dependencies '[[org.clojure/clojure       "1.7.0"  :scope "provided"]
                 [org.clojure/clojurescript "1.7.48" :scope "provided"]
                 [clj-yaml                  "0.4.0"  :scope "provided"]
                 [adzerk/bootlaces          "0.1.12" :scope "test"]])

(require '[adzerk.bootlaces :refer :all])

(def +version+ "0.1.0")
(bootlaces! +version+)

(task-options!
 pom {:project     'delaguardo/clj-i18n
      :version     +version+
      :description "Clojure(Script) RoR-style i18n library."
      :url         "https://github.com/DeLaGuardo/i18n-cljs"
      :scm         {:url "https://github.com/DeLaGuardo/i18n-cljs"}
      :license     {"EPL" "http://www.eclipse.org/legal/epl-v10.html"}}
 jar {:manifest {"Foo" "bar"}})
