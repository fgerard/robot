(ns robot2.core
  (:require [reagent.dom :as reagent]
            [re-frame.core :as re-frame]
            [robot2.config :as config]
            [robot2.subs]
            [robot2.events.core]
            [robot2.events.api]
            [robot2.operations.registry]
            [robot2.apps.loaded]
            [robot2.apps.stored]
            [robot2.apps.ready]
            [robot2.users.views]
            [robot2.views.shell :as views]))

(def BUILD 1)

(defn dev-setup []
  (when config/debug?
    (enable-console-print!)
    (println "robot2 dev mode")))

(defn mount-root []
  (reagent/render [views/main-panel] (.getElementById js/document "app")))

(defn ^:export init []
  (let [url-base (str (-> js/window .-location .-origin) "/")]
    (.log js/console (str "robot2 build: " BUILD))
    (re-frame/dispatch-sync [:initialize-db url-base])
    (re-frame/dispatch [:operations/load-schema])
    (dev-setup)
    (mount-root)))
