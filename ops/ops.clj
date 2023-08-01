(ns ops
  (:require [clojure.string :as str]
            [nextjournal.clerk :as clerk]
            [nextjournal.clerk.viewer :as viewer]
            [clojure.java.shell :refer [sh]]
            [nextjournal.clerk.webserver :as webserver]
            [nrepl.server]))

(defonce !nrepl (atom nil))

(def sha (-> (sh "git" "rev-parse" "--short" "HEAD" :out :string) :out str/trim))

(defn header [{:as opts :keys [nav-path]}]
  (clerk/html [:div.viewer.w-full.max-w-prose.px-8.not-prose.mt-3
               [:div.mb-8.text-xs.sans-serif.text-slate-400
                [:a.font-medium.border-b.border-dotted.border-slate-300.hover:text-indigo-500.hover:border-indigo-500.dark:border-slate-500.dark:hover:text-white.dark:hover:border-white.transition
                 {:href (viewer/doc-url "")} "Index"]
                [:span.mx-2 "•"]
                [:span "Generated with "
                 [:a.font-medium.border-b.border-dotted.border-slate-300.hover:text-indigo-500.hover:border-indigo-500.dark:border-slate-500.dark:hover:text-white.dark:hover:border-white.transition
                  {:href "https://clerk.vision"} "Clerk"]
                 " from "
                 [:a.font-medium.border-b.border-dotted.border-slate-300.hover:text-indigo-500.hover:border-indigo-500.dark:border-slate-500.dark:hover:text-white.dark:hover:border-white.transition
                  {:href (str "github.com/zampino/scrapbook/blob/" sha "/" nav-path)
                   :ignore-anchor-click true}
                  nav-path
                  (when sha [:<> "@" [:span.tabular-nums (subs sha 0 7)]])]]]]))

(clerk/reset-viewers! :default
                      (clerk/add-viewers [(assoc viewer/header-viewer
                                                 :transform-fn (comp viewer/mark-presented (viewer/update-val header)))]))

(reset! webserver/!doc {:nav-path "index"})

(defn boot [opts]
  (reset! !nrepl (nrepl.server/start-server :bind "0.0.0.0" :port 6666))

  ;; cannot create concurrent connections, e.g. caused by old container during re-deploy of a new version
  ;;
  ;; Execution error (ExceptionInfo) at nextjournal.clerk.eval/eval+cache! (eval.clj:155).
  ;; Execution error (LmdbNativeException$ConstantDerivedException) at org.lmdbjava.ResultCodeMapper/checkRc (ResultCodeMapper.java:114).
  ;; Platform constant error code: EAGAIN Resource temporarily unavailable (11)
  ;;
  ;; see Caveats section at http://www.lmdb.tech/doc/

  #_
  (clerk/show! "notebooks/todo.clj")

  ;; hack to get have / go to notebook


  ;; disable header

  ;; boot clerk
  (clerk/serve! opts))

(defn stop []
  (nrepl.server/stop-server @!nrepl)
  (clerk/halt!))
