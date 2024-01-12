(ns scrapbook.sci-ext
  (:require [nextjournal.clerk.sci-env]
            [sci.ctx-store]
            ["ts-geometric-algebra" :as ga]
            [applied-science.js-interop :as j]
            [sci.core :as sci]))

(sci.ctx-store/swap-ctx! sci/merge-opts {:classes {'ga ga}})
