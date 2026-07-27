(ns robot2.apps.loaded
  (:require [re-frame.core :as re-frame]
            [re-com.core :as re-com]))

(re-frame/reg-sub :apps/loaded (fn [db _] (get-in db [:applications :loaded])))
(re-frame/reg-sub :apps/loaded-selected (fn [db _] (get-in db [:applications :ctrl :loaded :selected])))

(re-frame/reg-event-fx
  :apps/select-loaded!
  (fn [{:keys [db]} [_ id]]
    (cond-> {:db (assoc-in db [:applications :ctrl :loaded :selected] id)}
      id (assoc :dispatch [:api/instantiate id]))))

(defn apps-loaded-com []
  (let [items (re-frame/subscribe [:apps/loaded])
        selected (re-frame/subscribe [:apps/loaded-selected])]
    (fn []
      [re-com/v-box
       :width "100%"
       :children
       [[re-com/title :label "Loaded" :level :level2]
        [re-com/single-dropdown
         :class "loaded-selector" :choices (mapv (fn [a] {:id a :label a}) (sort @items))
         :model @selected :width "100%" :placeholder "Select a loaded app"
         :on-change (fn [id] (re-frame/dispatch [:apps/select-loaded! id]))]]])))
