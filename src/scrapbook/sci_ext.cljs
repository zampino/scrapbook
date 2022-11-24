(ns scrapbook.sci-ext
  (:require [nextjournal.clerk.sci-env :as se]
            [sci.core :as sci]))

(swap! se/!sci-ctx sci/merge-opts {:namespaces {'scrapbook {'hello "scrap-me"}}})

(js/console.log :ehllo )

(comment
 (js/console.log :ehllo )
 (do @se/!sci-ctx)
 )
