(ns robot.ui.apps-ready
  (:require-macros [robot.ui.macros :refer [sfn]]
                   [cljs.core.async.macros :refer [go]])
  (:require [re-frame.core :as re-frame]
            [re-com.core :as re-com]
            [robot.ui.db :as db]
            [clojure.set :as S]
            [cljs.pprint :refer [pprint]]
            [cljs.core.async :refer [<! ]]
            [cljs-http.client :as http]
            [taoensso.sente  :as sente :refer (cb-success?)]))

;;;;;;;;;;;;;;;;;;;; SUBSCRIPTIONS



(re-frame/reg-sub
 [:applications :ctrl :ready :selected]
 (fn [db [path]]
   (get-in db path)))

(re-frame/reg-sub
 [:applications :ctrl :ready :instances]
 (fn [db [path]]
   (get-in db path)))

;;;;;;;;;;;;;;;;;;;; HANDLERS

(re-frame/reg-event-db
 [:applications :ctrl :ready :selected]
 (fn [db [path selection]]
   (re-frame/dispatch [:load-applications])
   (let [old (get-in db path)]
     (.log js/console "old: " old " - " selection)
     (cond-> (assoc-in db path selection)
             (not= old selection) (update-in (butlast path) dissoc :instances)
             ))))

(re-frame/reg-event-db
 [:applications :ctrl :ready :instances]
 (fn [{:keys [url-base] :as db} [path app instances]]
   (let [old (get-in db path #{})
         turn-on (S/difference instances old)
         turn-off (S/difference old instances)]
     (.log js/console (str "app: " app " --- " instances " ---ON>" turn-on " OFF>" turn-off))
     (go
      (doseq [on turn-on]
        (let [{:keys [status body]} (<! (http/post
                                         (str url-base "start/" app "/" on)))]
          (.log js/console (str "starting: " app "/" on " status:" status))))
      (doseq [off turn-off]
        (let [{:keys [status body]} (<! (http/post
                                         (str url-base "stop/" app "/" off)))]
          (.log js/console (str "stopping: " app "/" off " status:" status))))
      (re-frame/dispatch [:load-applications]))
     (assoc-in db path instances))))

;;;;;;;;;;;;;;;;;;;; COMPONENTS
{"uno" {:status :stopped, :mood :good, :current :prueba}}

(defn apps-ready-status []
  (let [apps-ready (re-frame/subscribe [[:applications :ready]])
        apps-ready-selected (re-frame/subscribe [[:applications :ctrl :ready :selected]])
        ready-selected-app-instances (re-frame/subscribe [[:applications :ctrl :ready :instances]]);OJO
        ]
    (fn []
      (.log js/console "Entrando a apps-ready-status")
      (.log js/console (str "1:" @apps-ready))
      (.log js/console (str "2:" @apps-ready-selected))
      (.log js/console (str "3:" @ready-selected-app-instances))

      (if (seq @apps-ready-selected)
        (let [selected (first @apps-ready-selected)
              instances (get @apps-ready selected)
              _ (.log js/console "instances: " instances)
              [choices instances] (reduce-kv (fn [[result instances] instance-name {status :robot/status
                                                                                    mood :robot/mood
                                                                                    current :robot/current
                                                                                    :as inst-status}]
                                               [(conj result {:id instance-name
                                                              :label (str instance-name ": " (or status "status?") ", " (or mood "mood?") ", " (or current "current?"))})
                                                (if (= status :running) (conj instances instance-name) instances)])
                                             [[] #{}]
                                             instances)
              _ (.log js/console (str "INSTANCES: " instances))
              ;_ (pprint choices)
              ]
          [re-com/v-box
           :width "100%"
           :children [[re-com/title :label "Instances" :level :level2]
                      [re-com/selection-list
                       :choices choices
                       :model (or @ready-selected-app-instances instances)
                       ;:id-fn :label
                       :height "150px"
                       :multi-select? true
                       :on-change (fn [selection]
                                    (.log js/console "instance selection:" selection)
                                    (re-frame/dispatch [[:applications :ctrl :ready :instances] selected selection])
                                    )]]])))))

(defn apps-ready-com []
  (let [apps-ready (re-frame/subscribe [[:applications :ready]])
        apps-ready-selected (re-frame/subscribe [[:applications :ctrl :ready :selected]])]
    (fn []
      (let [ready @apps-ready
            choices (reduce
                     (fn [result app]
                       (conj result {:label app}))
                     []
                     (sort (keys ready)))]
        (.log js/console (str ":apps-ready-selection " @apps-ready-selected))
        [re-com/v-box
         ;:height "100%"
         :width "100%"
         :children [[re-com/title :label "Ready" :level :level2]
                    [re-com/box
                     ;:width "15%"
                     :height "100%"
                     :child [re-com/selection-list
                             :choices choices
                             :model (or @apps-ready-selected #{})
                             :id-fn :label
                             :height "150px"
                             :multi-select? false
                             :on-change (fn [selection]
                                          (.log js/console (str "loaded-selection:" selection))
                                          (re-frame/dispatch [[:applications :ctrl :ready :selected] selection]))]]]]))))
