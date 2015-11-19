(set-env!
 :source-paths #{"src"}
 :resource-paths #{"html" "build" "resources"}
 :dependencies '[[org.clojure/clojure       "1.7.0"   :scope "provided"]
                 [org.clojure/clojurescript "1.7.145" :scope "provided"]
                 [clj-yaml                  "0.4.0"]
                 [adzerk/bootlaces          "0.1.12"  :scope "test"]])

(require '[adzerk.bootlaces :refer :all])

(def +version+ "0.1.0-SNAPSHOT")
(bootlaces! +version+)

(task-options!
 pom {:project     'delaguardo/clj-i18n
      :version     +version+
      :description "Clojure(Script) RoR-style i18n library."
      :url         "https://github.com/DeLaGuardo/clj-i18n"
      :scm         {:url "https://github.com/DeLaGuardo/clj-i18n"}
      :license     {"EPL" "http://www.eclipse.org/legal/epl-v10.html"}}
 jar {:manifest {"Foo" "bar"}})

(deftask dev []
  (set-env! :dependencies #(conj %
                                 '[adzerk/boot-cljs            "1.7.48-6"       :scope "test"]
                                 '[adzerk/boot-cljs-repl       "0.2.0"          :scope "test"]
                                 '[adzerk/boot-reload          "0.4.1"          :scope "test"]
                                 '[pandeiro/boot-http          "0.7.0-SNAPSHOT" :scope "test"]
                                 '[org.clojure/tools.nrepl     "0.2.11"]))
  (require 'adzerk.boot-cljs)
  (require 'adzerk.boot-cljs-repl)
  (require 'adzerk.boot-reload)
  (require 'pandeiro.boot-http)
  (let [cljs (resolve 'adzerk.boot-cljs/cljs)
        cljs-repl (resolve 'adzerk.boot-cljs-repl/cljs-repl)
        start-repl (resolve 'adzerk.boot-cljs-repl/start-repl)
        reload (resolve 'adzerk.boot-reload/reload)
        serve (resolve 'pandeiro.boot-http/serve)]
    (comp (serve :port 3000)
       (watch)
       (speak)
       (reload :on-jsload 'clj-i18n.dev/refresh)
       (cljs-repl)
       (cljs :source-map true
             :optimizations :none))))
