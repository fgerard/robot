(ns robot.ui.apps-stored
  (:require-macros [robot.ui.macros :refer [sfn]]
                   [cljs.core.async.macros :refer [go]])
  (:require [re-frame.core :as re-frame]
            [re-com.core :as re-com]
            [robot.ui.db :as db]
            [robot.ui.events :as events]
            [cljs.pprint :refer [pprint]]
            [cljs.core.async :refer [<! ]]
            [cljs-http.client :as http]))

;;;;;;;;;;;;;;;;;;;; SUBSCRIPTIONS

(re-frame/reg-sub
 [:applications :stored]
 (fn [db [path]]
   (get-in db path))) ;[:applications :stored])))

(re-frame/reg-sub
 [:applications :ctrl :stored :selected] ;:apps-stored-selected
 (fn [db [path]]
   (get-in db path)))

;;;;;;;;;;;;;;;;;;;; HANDLERS






(re-frame/reg-event-db
 [:applications :ctrl :stored :selected] ;:apps-stored-selected
 (fn [{:keys [url-base] :as db} [path selection]]
   (when (seq selection)
     (go
      (.log js/console (str "Loading application:" (pr-str selection)))
      (let [{:keys [status body]} (<! (http/post
                                       (str url-base "load/" (first selection))
                                       {;query-params {}
                                        :headers {"Accept" "application/edn"}}))]
        (.log js/console "load-application status:" status)
        ;(pprint body)
        (if (#{200} status)
          (re-frame/dispatch [[:applications :editable] (first selection) body])
          (re-frame/dispatch [:log (events/create-log :error status body)])))))
   (assoc-in db path selection)))



;;;;;;;;;;;;;;;;;;;; COMPONENTS

(defn apps-stored-com []
  (let [apps-stored (re-frame/subscribe [[:applications :stored]]) ;:apps-stored
        apps-stored-selected (re-frame/subscribe [[:applications :ctrl :stored :selected]])] ;[:apps-stored-selected]
    (fn []
      (let [stored @apps-stored
            choices (reduce
                     (fn [result app]
                       (conj result {:label app}))
                     []
                     (sort stored))]
        (.log js/console (str ":apps-stored-selection ***** " @apps-stored-selected))
        [re-com/v-box
         ;:height "100%"
         :width "100%"
         :children [[re-com/title :label "Stored" :level :level2]
                    [re-com/box
                     ;:width "15%"
                     :height "100%"
                     :child [re-com/selection-list
                             :choices choices
                             :model (or @apps-stored-selected #{})
                             :id-fn :label
                             :height "150px"
                             :multi-select? false
                             :on-change (fn [selection]
                                          (.log js/console (str "stored-selection:" selection))
                                          (re-frame/dispatch [[:applications :ctrl :stored :selected] selection]))]]]])))) ;:apps-stored-selected
