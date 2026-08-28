(ns robot2.apps.ready
  "A diferencia de loaded/stored, seleccionar una app aqui no descarga su
   configuracion -- solo filtra que instancias se muestran. Seleccionar
   instancias si dispara red: prende las que se agregaron y apaga las que se
   quitaron (comparando contra la seleccion anterior)."
  (:require [clojure.set :as set]
            [re-frame.core :as re-frame]
            [re-com.core :as re-com]
            [robot2.apps.shared :as shared]))

(re-frame/reg-sub :apps/ready (fn [db _] (get-in db [:applications :ready])))
(re-frame/reg-sub :apps/ready-selected (fn [db _] (get-in db [:applications :ctrl :ready :selected])))
(re-frame/reg-sub :apps/ready-instances-selected (fn [db _] (get-in db [:applications :ctrl :ready :instances])))

(re-frame/reg-event-fx
  :apps/select-ready!
  (fn [{:keys [db]} [_ selection]]
    (let [old (get-in db [:applications :ctrl :ready :selected])]
      {:db (cond-> (assoc-in db [:applications :ctrl :ready :selected] selection)
             (not= old selection) (update-in [:applications :ctrl :ready] dissoc :instances))
       :dispatch [:api/load-applications]})))

(defn- running-instances [db app]
  (->> (get-in db [:applications :ready app])
       (keep (fn [[inst-name {:robot/keys [status]}]]
               (when (= status :running) inst-name)))
       set))

(re-frame/reg-event-fx
  :apps/set-ready-instances!
  (fn [{:keys [db]} [_ app instances]]
    ;; El default de old es lo que de veras esta corriendo, no #{}: al entrar a una
    ;; app :instances viene vacio, y con #{} la primera deseleccion no apagaba nada.
    (let [old (get-in db [:applications :ctrl :ready :instances] (running-instances db app))
          turn-on (set/difference instances old)
          turn-off (set/difference old instances)]
      {:db (assoc-in db [:applications :ctrl :ready :instances] instances)
       :dispatch-n (concat (map (fn [inst] [:api/start app inst]) turn-on)
                            (map (fn [inst] [:api/stop app inst]) turn-off))})))

(defn apps-ready-com []
  (let [items (re-frame/subscribe [:apps/ready])
        selected (re-frame/subscribe [:apps/ready-selected])]
    (fn []
      [shared/single-select-list
       {:title "Ready"
        :items (keys @items)
        :selected @selected
        :on-change-event [:apps/select-ready!]}])))

(defn apps-ready-status []
  (let [ready (re-frame/subscribe [:apps/ready])
        selected (re-frame/subscribe [:apps/ready-selected])
        instances-selected (re-frame/subscribe [:apps/ready-instances-selected])]
    (fn []
      (when (seq @selected)
        (let [app (first @selected)
              instances (get @ready app)
              [choices running] (reduce-kv
                                  (fn [[choices running] inst-name {:robot/keys [status mood current]}]
                                    [(conj choices {:id inst-name
                                                     :label (str inst-name ": " (or status "status?") ", "
                                                                  (or mood "mood?") ", " (or current "current?"))})
                                     (if (= status :running) (conj running inst-name) running)])
                                  [[] #{}]
                                  instances)]
          (let [model (or @instances-selected running)
                all (set (keys instances))]
            [re-com/v-box
             :width "100%"
             :gap "6px"
             :children
             [[re-com/title :label "Instances" :level :level2]
              [re-com/selection-list
               :choices choices
               :model model
               :height "150px"
               :multi-select? true
               :on-change (fn [selection] (re-frame/dispatch [:apps/set-ready-instances! app selection]))]
              [re-com/h-box
               :gap "8px"
               :children
               [[re-com/button
                 :label "Start all"
                 :disabled? (= model all)
                 :on-click #(re-frame/dispatch [:apps/set-ready-instances! app all])]
                [re-com/button
                 :label "Stop all"
                 :disabled? (empty? model)
                 :on-click #(re-frame/dispatch [:apps/set-ready-instances! app #{}])]]]]]))))))
