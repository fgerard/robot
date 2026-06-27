(ns robot2.undo
  "Subsistema de undo/redo, aislado y explicito.

   La version 1 envolvia eventos con la macro `sfn`, que SIEMPRE llamaba a
   snapshot-event despues de correr el handler, y esa misma funcion contenia
   tambien la logica de :undo/:redo escondida en un condp por k. Aqui se
   separan las dos cosas:
     - `track` es un interceptor que se agrega EXPLICITAMENTE solo a los
       eventos que de verdad mutan estado que debe ser undo-able (no a todos).
     - :undo/:redo son handlers normales que llaman `time-travel`."
  (:require [re-frame.core :as re-frame]))

(def history-limit 100)

(def ephemeral-paths
  "Paths de UI efimera (dialogos abiertos, etc.) que no se preservan en el
   historial -> evita que abrir/cerrar un dialogo cuente como un cambio."
  [[:designer/ctrl :new-app]
   [:designer/ctrl :import]])

(defn- strip-ephemeral [db]
  (reduce (fn [db path]
            (update-in db (butlast path) dissoc (last path)))
          db
          ephemeral-paths))

(defn push-snapshot
  "Agrega `db` (sin :history y sin paths efimeros) al historial de `db`."
  [db]
  (let [{:keys [curr-idx snapshots]} (:history db {:curr-idx 0 :snapshots (list (strip-ephemeral db))})
        kept (strip-ephemeral (dissoc db :history))]
    (assoc db :history {:curr-idx 0
                         :snapshots (take history-limit (conj (nthrest snapshots curr-idx) kept))})))

(defn time-travel
  "direction es :undo o :redo. Mueve curr-idx y restaura ese snapshot."
  [db direction]
  (let [{:keys [curr-idx snapshots]} (:history db {:curr-idx 0 :snapshots (list (dissoc db :history))})
        last-idx (dec (count snapshots))
        new-idx (case direction
                  :undo (min last-idx (inc curr-idx))
                  :redo (max 0 (dec curr-idx)))
        snapshot (nth snapshots new-idx db)]
    (-> db
        (merge snapshot)
        (assoc-in [:history :curr-idx] new-idx))))

(defn init-history [db]
  (assoc db :history {:curr-idx 0 :snapshots (list (dissoc db :history))}))

(def track
  "Interceptor: despues de que el handler corre, empuja un snapshot al
   historial. Se agrega explicitamente a los eventos que mutan estado
   editable: (re-frame/reg-event-db :evt [undo/track] handler-fn)."
  (re-frame/->interceptor
    :id :undo/track
    :after (fn [context]
             (let [new-db (get-in context [:effects :db])]
               (if (some? new-db)
                 (assoc-in context [:effects :db] (push-snapshot new-db))
                 context)))))

(re-frame/reg-event-db :undo (fn [db _] (time-travel db :undo)))
(re-frame/reg-event-db :redo (fn [db _] (time-travel db :redo)))
