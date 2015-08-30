(set-env!
 :source-paths #{"src" "test" "dev"}
 :resource-paths #{"resources"}
 :target-path "target"
 :dependencies '[ ;; boot
                 [adzerk/boot-cljs "1.7.48-3" :scope "test"]
                 [adzerk/boot-test "1.0.4" :scope "test"]
                 [cider/cider-nrepl "0.9.1" :scope "test"]
                 ;;[adzerk/boot-reload "0.2.4" :scope "test"]

                 ;; Clojure
                 [org.clojure/clojure "1.7.0"]
                 [org.clojure/tools.namespace "0.2.11"]
                 ;; [instaparse "1.4.0"]
                 ;; [joda-time "2.8"]
                 [incanter "1.5.6"]

                 ;; ClojureScript
                 [org.clojure/clojurescript "1.7.48"]
                 [hiccups "0.3.0"]
                 ;;[com.lucasbradstreet/instaparse-cljs "1.4.0.0-SNAPSHOT"]
                 ])

(require
 '[adzerk.boot-cljs :refer [cljs]]
 ;;'[adzerk.boot-reload :refer [reload]]
 '[clojure.tools.namespace.repl :as repl :refer [refresh]]
 '[org.craigandera.popup-computer :as pc]
 '[clojure.string :as str]
 '[adzerk.boot-test :refer :all]
 '[user :as user])

(swap! boot.repl/*default-middleware*
       conj 'cider.nrepl/cider-middleware)

(apply repl/set-refresh-dirs (get-env :directories))

(task-options!
  pom {:project 'org.craigandera/popup-computer
       :version "0.1.0"}
  jar {:manifest {"author" "Craig Andera"}}
  cljs {:main 'org.craigandera.popup-computer.main
        :asset-path "target/out"
        :optimizations :simple
        :source-map true})

