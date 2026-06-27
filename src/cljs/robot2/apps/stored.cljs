(ns robot2.apps.stored
  (:require [re-frame.core :as re-frame]
            [robot2.apps.shared :as shared]))

(re-frame/reg-sub :apps/stored (fn [db _] (get-in db [:applications :stored])))
(re-frame/reg-sub :apps/stored-selected (fn [db _] (get-in db [:applications :ctrl :stored :selected])))

(re-frame/reg-event-fx
  :apps/select-stored!
  (fn [{:keys [db]} [_ selection]]
    (cond-> {:db (assoc-in db [:applications :ctrl :stored :selected] selection)}
      (seq selection) (assoc :dispatch [:api/load-stored-app (first selection)]))))

(defn apps-stored-com []
  (let [items (re-frame/subscribe [:apps/stored])
        selected (re-frame/subscribe [:apps/stored-selected])]
    (fn []
      [shared/single-select-list
       {:title "Stored"
        :items @items
        :selected @selected
        :on-change-event [:apps/select-stored!]}])))
