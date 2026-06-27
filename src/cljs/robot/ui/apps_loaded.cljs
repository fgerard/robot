(ns robot.ui.apps-loaded
  (:require-macros [robot.ui.macros :refer [sfn]]
                   [cljs.core.async.macros :refer [go]])
  (:require [re-frame.core :as re-frame]
            [re-com.core :as re-com]
            [robot.ui.db :as db]
            [cljs.pprint :refer [pprint]]
            [cljs.core.async :refer [<! ]]
            [cljs-http.client :as http]))

;;;;;;;;;;;;;;;;;;;; SUBSCRIPTIONS

(re-frame/reg-sub
 [:applications :loaded]
 (fn [db [path]]
   (get-in db path)))

(re-frame/reg-sub
 [:applications :ctrl :loaded :selected]
 (fn [db [path]]
   (get-in db path)))

;;;;;;;;;;;;;;;;;;;; HANDLERS

(re-frame/reg-event-db
 [:applications :ctrl :loaded :selected]
 (fn [{:keys [url-base] :as db} [path selection]]
   (when (seq selection)
     (go
      (.log js/console "Loading application:" selection)
      (let [{:keys [status body]} (<! (http/post
                                       (str url-base "instantiate/" (first selection))
                                       {;query-params {}
                                        :headers {"Accept" "application/edn"}}))]
        (.log js/console "load-application status:" status)
        ;(pprint body)
        (if (#{204} status)
          (re-frame/dispatch [[:applications :editable] (first selection) body])
          (re-frame/dispatch [:rest-error status body])))))
   (assoc-in db path selection)))

;;;;;;;;;;;;;;;;;;;; COMPONENTS

(defn apps-loaded-com []
  (let [apps-loaded (re-frame/subscribe [[:applications :loaded]])
        apps-loaded-selected (re-frame/subscribe [[:applications :ctrl :loaded :selected]])]
    (fn []
      (let [loaded @apps-loaded
            choices (reduce
                     (fn [result app]
                       (conj result {:label app}))
                     []
                     (sort loaded))]
        (.log js/console (str ":apps-loaded-selection " @apps-loaded-selected " " (into #{} @apps-loaded-selected)))
        [re-com/v-box
         :height "100%"
         :width "100%"
         :children [[re-com/title :label "Loaded" :level :level2]
                    [re-com/box
                     ;:width "15%"
                     :height "100%"
                     :child [re-com/selection-list
                             :choices choices
                             :model (or @apps-loaded-selected #{})
                             :id-fn :label
                             :height "150px"
                             :multi-select? false
                             :on-change (fn [selection]
                                          (.log js/console (str "loaded-selection:" selection))
                                          (re-frame/dispatch [[:applications :ctrl :loaded :selected] selection]))]]]]))))
