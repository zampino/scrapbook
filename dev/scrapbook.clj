(ns scrapbook
  (:require [nextjournal.clerk :as clerk]
            [shadow.cljs.devtools.api :as shadow.api]
            [shadow.cljs.devtools.server :as shadow.server]))

(defn dev [_]
  (set! *print-namespace-maps* false)
  (shadow.server/start!)
  (shadow.api/watch :sci)
  (clerk/serve! {:browse? false}))

(comment
  ;; boot cljs repl
  (shadow.api/repl :sci)
  )
