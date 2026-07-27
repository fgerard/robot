(ns robot2.operations.registry
  "Catalogo de tipos de operacion (titulo + campos de su formulario de
   configuracion). Antes (robot.ui.state-editor/opr-diag-confs) era un
   literal de 500+ lineas compilado dentro del bundle de JS; ahora se carga
   UNA vez en runtime desde /edn/operations-schema.edn con el mismo efecto
   :http que usa el resto de las llamadas al servidor (ver
   robot2.events.api), y se guarda en :operations/schema."
  (:require [re-frame.core :as re-frame]
            [cljs.reader :as reader]))

(re-frame/reg-event-fx
  :operations/load-schema
  (fn [_ _]
    {:http {:method :get
            :url "edn/operations-schema.edn"
            :headers {"Accept" "application/edn"}
            :on-success [:operations/schema-loaded]
            :on-failure [:api/log-error]}}))

;; cljs.reader/read-string (el reader liviano que se usa en runtime, distinto
;; del que usa el compilador sobre el codigo fuente) NO soporta el literal de
;; regex #"..." -- el .edn guarda cada :re como string plano y aqui se
;; convierte a RegExp real con re-pattern, una sola vez al cargar.
(defn compile-schema [raw]
  (letfn [(compile-fld [fld]
            (cond-> fld
              (string? (:re fld)) (update :re re-pattern)))
          (compile-entry [entry]
            (update entry :flds (fn [flds] (mapv compile-fld flds))))]
    (into {} (map (fn [[k v]] [k (compile-entry v)])) raw)))

(re-frame/reg-event-db
  :operations/schema-loaded
  (fn [db [_ _status body]]
    (assoc db :operations/schema (compile-schema (reader/read-string body)))))

(re-frame/reg-sub
  :operations/schema
  (fn [db _] (:operations/schema db)))

;; :operations (crudo, del backend via /operations) trae, por tipo de
;; operacion, el icono a mostrar en el selector -- es informacion distinta a
;; :operations/schema (que trae el formulario de configuracion). El backend
;; ya conoce los tipos de operacion soportados; este catalogo de iconos no se
;; duplica aqui.
(re-frame/reg-sub
  :operations/raw
  (fn [db _] (:operations db)))

(defn schema-for
  "Entrada del catalogo para el tipo de operacion `operation` (un keyword),
   o nil si el catalogo no ha terminado de cargar todavia."
  [schema operation]
  (get schema operation))

(defn choices
  "Opciones {:id :label :image} para el selector de tipo de operacion, a
   partir del catalogo crudo del backend (:operations/raw)."
  [raw-operations]
  (reduce (fn [result [opr {:keys [image]}]]
            (conj result {:id (name opr) :label (name opr) :image image}))
          []
          (sort raw-operations)))
