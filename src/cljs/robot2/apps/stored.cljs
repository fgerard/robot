(ns robot2.apps.stored
  (:require [re-frame.core :as re-frame]
            [re-com.core :as re-com]))

(re-frame/reg-sub :apps/stored (fn [db _] (get-in db [:applications :stored])))
(re-frame/reg-sub :apps/stored-selected (fn [db _] (get-in db [:applications :ctrl :stored :selected])))

;; Cargar un app desde "Stored" tambien lo vuelve el app activo del designer
;; (:designer/ctrl :app) -- antes esto pedia un segundo paso (elegirlo de
;; nuevo en el selector de arriba del canvas) para que se viera algo.
(re-frame/reg-event-fx
  :apps/select-stored!
  (fn [{:keys [db]} [_ id]]
    (cond-> {:db (assoc-in db [:applications :ctrl :stored :selected] id)}
      id (assoc :dispatch-n [[:api/load-stored-app id]
                              [:reset! [:designer/ctrl :app] id]]))))

(defn apps-stored-com []
  (let [items (re-frame/subscribe [:apps/stored])
        selected (re-frame/subscribe [:apps/stored-selected])]
    (fn []
      [re-com/v-box
       :width "100%"
       :children
       [[re-com/title :label "Stored" :level :level2]
        [re-com/single-dropdown
         :class "stored-selector" :choices (mapv (fn [a] {:id a :label a}) (sort @items))
         :model @selected :width "100%" :placeholder "Select a stored app"
         :on-change (fn [id] (re-frame/dispatch [:apps/select-stored! id]))]]])))
