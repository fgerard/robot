(ns robot.ui.core
    (:require [reagent.core :as reagent]
              [re-frame.core :as re-frame]
              [robot.ui.events]
              [robot.ui.subs]
              [robot.ui.views :as views]
              [robot.ui.config :as config]))


(defn dev-setup []
  (enable-console-print!)
  (when config/debug?
    (enable-console-print!)
    (println "dev mode")))

(defn mount-root []
  (reagent/render [views/main-panel]
                  (.getElementById js/document "app")))

(defn ^:export init []
  (let [url-base (str (-> js/window .-location .-origin) "/")]
    (re-frame/dispatch-sync [:initialize-db url-base])
    ;(.log js/console "Mandando start-server-comm!")
    ;(re-frame/dispatch-sync [:start-server-comm])
    ;(re-frame/dispatch-sync [:login {:uid "felipe" :pass "123456"}])
    ;(.log js/console "Mandando load-applications!")
    ;(re-frame/dispatch-sync [:load-applications])
    (dev-setup)
    (mount-root)))
