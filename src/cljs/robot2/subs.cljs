(ns robot2.subs
  "Subscripciones genericas, sin dueño de feature especifico. El resto de las
   subs viven junto a los eventos de su propio dominio (apps/*, users/views,
   designer/shell, operations/registry...)."
  (:require [re-frame.core :as re-frame]))

(re-frame/reg-sub :control/uid (fn [db _] (get-in db [:control :uid])))
(re-frame/reg-sub :control/admin (fn [db _] (get-in db [:control :admin])))
(re-frame/reg-sub :log (fn [db _] (:log db)))
(re-frame/reg-sub :ui/main-tab (fn [db _] (:ui/main-tab db)))

(re-frame/reg-event-db
  :ui/select-tab!
  (fn [db [_ tab]]
    (assoc db :ui/main-tab tab)))
