(ns robot2.events.core
  "Eventos genericos, sin dependencia de ningun feature en particular."
  (:require [re-frame.core :as re-frame]
            [robot2.db :as db]
            [robot2.undo :as undo]
            [robot2.interop :as interop]))

(defn now []
  (js/goog.date.DateTime.))

(defn create-log [level status msg]
  (let [ts (now)]
    [:log [(.toString ts) level status msg (.getTime ts)]]))

(re-frame/reg-event-db
  :initialize-db
  (fn [_ [_ url-base]]
    (-> db/default-db
        (assoc :url-base url-base :ui/theme (interop/current-theme))
        undo/init-history)))

(re-frame/reg-sub
  :ui/theme
  (fn [db _] (:ui/theme db)))

(re-frame/reg-event-db
  :ui/set-theme
  (fn [db [_ theme]]
    (interop/apply-theme! theme)
    (assoc db :ui/theme theme)))

(re-frame/reg-event-db
  :reset!
  [undo/track]
  (fn [db [_ path val]]
    (assoc-in db path val)))

(re-frame/reg-event-db
  :flip-set-elem!
  (fn [db [_ path elem]]
    (update-in db path (fn [d-set]
                          (let [d-set (or d-set #{})]
                            (if (d-set elem)
                              (disj d-set elem)
                              (conj d-set elem)))))))

(re-frame/reg-event-db
  :log
  (fn [db [_ log-entry]]
    (update db :log (fn [log] (take 1000 (conj log log-entry))))))

(re-frame/reg-sub
  :ui/error-dialog
  (fn [db _] (:ui/error-dialog db)))

(re-frame/reg-event-db
  :ui/error-dialog!
  (fn [db [_ status body]]
    (assoc db :ui/error-dialog {:status status :body (str body)})))

(re-frame/reg-event-db
  :pprint
  (fn [db _]
    (interop/log (pr-str db))
    db))
