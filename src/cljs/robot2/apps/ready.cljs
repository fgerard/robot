(ns robot2.apps.ready
  (:require [re-frame.core :as re-frame]))

(re-frame/reg-sub :apps/ready (fn [db _] (get-in db [:applications :ready])))
