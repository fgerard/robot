(ns robot.ui.subs
    (:require-macros [reagent.ratom :refer [reaction]])
    (:require [re-frame.core :as re-frame]))

(re-frame/reg-sub
 :name
 (fn [db [_]]
   (:name db)))

(re-frame/reg-sub
 [:control :uid]
 (fn [db [path]]
   (get-in db path)))

(re-frame/reg-sub
 [:control :admin]
 (fn [db [path]]
   (get-in db path)))

(re-frame/reg-sub
 [:log]
 (fn [db [path]]
   (get-in db path)))

(re-frame/reg-sub
 [:control :log]
 (fn [db [path]]
   (get-in db path)))

(re-frame/reg-sub
  :canvas1
  (fn [db [_]]
    (:canvas1 db)))

(re-frame/reg-sub
  :control
  (fn [db [_]]
    (:control db)))

(re-frame/reg-sub
 [:applications :ready]
 (fn [db [path]]
   (get-in db path)))

; [:desginer :ctrl] es donde se guardan los elementos de control del tab de designer
; :designer {:ctrl {:app (reagent/atom nil)
;                   :new-app (reagent/atom nil)
;                   :open-instances (reagent/atom #{})
;                   :watch-instance (reagent/atom nil)
;                   :new-inst (reagent/atom nil)
;}}
;
(re-frame/reg-sub
 [:designer :ctrl]
 (fn [db [path]]
   (get-in db path)))

(re-frame/reg-sub
 :users
 (fn [db _]
   (get-in db [:users])))
