(ns robot2.apps.loaded
  (:require [re-frame.core :as re-frame]
            [robot2.apps.shared :as shared]))

(re-frame/reg-sub :apps/loaded (fn [db _] (get-in db [:applications :loaded])))
(re-frame/reg-sub :apps/loaded-selected (fn [db _] (get-in db [:applications :ctrl :loaded :selected])))

(re-frame/reg-event-fx
  :apps/select-loaded!
  (fn [{:keys [db]} [_ selection]]
    (cond-> {:db (assoc-in db [:applications :ctrl :loaded :selected] selection)}
      (seq selection) (assoc :dispatch [:api/instantiate (first selection)]))))

(defn apps-loaded-com []
  (let [items (re-frame/subscribe [:apps/loaded])
        selected (re-frame/subscribe [:apps/loaded-selected])]
    (fn []
      [shared/single-select-list
       {:title "Loaded"
        :items @items
        :selected @selected
        :on-change-event [:apps/select-loaded!]}])))
