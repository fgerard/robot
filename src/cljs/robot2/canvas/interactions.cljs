(ns robot2.canvas.interactions
  "Maquina de estados de interaccion del canvas: arrastrar un estado, arrastrar
   una conexion nueva, reconectar/desconectar una flecha existente, hacer pan
   del lienzo, y que dialogo (si alguno) esta abierto sobre el canvas.

   Diferencia de fondo con la v1 (robot.ui.svg): TODO esto vivia en el app-db
   (:svg-ctrl :target/:mouse-down/:connecting/:hovered), y CADA pointermove
   disparaba un evento re-frame que mutaba el db completo -> reagent volvia a
   correr todas las subscripciones (incluida la consola) en cada pixel de
   arrastre. Aqui el estado de la interaccion EN CURSO vive en un atomo local
   (creado una vez por `new-state`, ver canvas/render.cljs), y solo se
   dispatcha UN evento a re-frame al soltar el puntero, con el resultado
   final. Eso es lo unico que entra al historial de undo.

   Tambien se usan Pointer Events + setPointerCapture en vez de Mouse Events:
   el navegador garantiza que pointermove/pointerup sigan llegando aunque el
   cursor salga del SVG durante el arrastre, así que ya no se necesitan los
   handlers de respaldo mouseout/mouseleave que tenia la v1."
  (:require [reagent.core :as reagent]
            [re-frame.core :as re-frame]
            [robot2.interop :as interop]
            [robot2.undo :as undo]
            [robot2.canvas.flow :as flow]))

;; --- estado local de interaccion ---------------------------------------------
;; {:drag    nil | {:type :state|:pan|:connector|:arrow
;;                  :id ...            ; state-id que origina el drag
;;                  :origin ...        ; corner [x y] (state) | zoom [x y] (pan)
;;                                     ; | old-dest-state-id (arrow) | nil (connector)
;;                  :start [x y]       ; punto svg al iniciar el drag
;;                  :pointer [x y]}    ; punto svg actual
;;  :hovered nil | [:state state-id]
;;  :dialog  nil | {:type :menu|:flow-re :state-id ... :other-state ...}}

(defn new-state []
  (reagent/atom {:drag nil :hovered nil :dialog nil}))

;; --- hover --------------------------------------------------------------------
(defn set-hovered! [state type id]
  (swap! state assoc :hovered [type id]))

(defn clear-hovered! [state]
  (swap! state assoc :hovered nil))

(defn hovered? [state type id]
  (= (:hovered @state) [type id]))

;; --- dialogo (menu de operacion / editor de regex de transicion) ------------
(defn open-dialog! [state dialog]
  (swap! state assoc :dialog dialog))

(defn close-dialog! [state]
  (swap! state assoc :dialog nil))

;; --- coordenadas ---------------------------------------------------------------
(defn svg-point [svg-el e]
  (interop/client-point->svg-point svg-el (.-clientX e) (.-clientY e)))

;; --- inicio/actualizacion de un arrastre --------------------------------------
(defn begin-drag!
  "type: :state | :pan | :connector | :arrow
   id: state-id que origina el drag (irrelevante para :pan)
   origin: ver comentario de forma del atomo, arriba"
  [state svg-el e type id origin]
  (.stopPropagation e) (.preventDefault e)
  (interop/capture-pointer! svg-el (.-pointerId e))
  (let [p (svg-point svg-el e)]
    (swap! state assoc :drag {:type type :id id :origin origin
                               :start p :pointer p})))

(defn update-drag! [state svg-el e]
  (when (:drag @state)
    (.preventDefault e) (.stopPropagation e)
    (swap! state update :drag assoc :pointer (svg-point svg-el e))))

(defn dragging-corner
  "Esquina en vivo de un estado mientras se arrastra (:type :state). nil si no
   aplica -- se usa para renderizar la caja en su posicion actual sin tocar el
   app-db hasta soltar el puntero."
  [state]
  (let [{:keys [type origin start pointer]} (:drag @state)]
    (when (= type :state)
      (let [[ox oy] origin [sx sy] start [px py] pointer]
        [(+ ox (- px sx)) (+ oy (- py sy))]))))

(defn dragging-pan
  "Desplazamiento [x y] en vivo del viewBox mientras se hace pan (:type :pan)."
  [state]
  (let [{:keys [type origin start pointer]} (:drag @state)]
    (when (= type :pan)
      (let [[ox oy] origin [sx sy] start [px py] pointer]
        [(+ ox (- sx px)) (+ oy (- sy py))]))))

(defn connecting-point
  "Punto suelto donde esta el cursor mientras se arrastra una conexion nueva o
   se reconecta una existente (:type :connector o :arrow)."
  [state]
  (let [{:keys [type pointer]} (:drag @state)]
    (when (#{:connector :arrow} type)
      pointer)))

(defn dragging? [state type]
  (= type (get-in @state [:drag :type])))

;; --- resolucion al soltar el puntero sobre una conexion en curso -------------
(defn- resolve-connector-release [{:keys [origin]} hovered]
  (let [[h-type h-id] hovered]
    (cond
      (and h-id (= h-type :state) (nil? origin)) :new-state-connection
      (and h-id (= h-type :state) origin (not= origin h-id)) :disconnect-and-connect
      (and h-id (= h-type :state) origin (= origin h-id)) :do-nothing
      (and (nil? h-id) origin) :disconnect
      :else :do-nothing)))

(defn end-drag! [state svg-el e app-id]
  (let [{:keys [type id origin] :as drag} (:drag @state)
        hovered (:hovered @state)]
    (interop/release-pointer! svg-el (.-pointerId e))
    (case type
      :state (re-frame/dispatch [:canvas/move-state! app-id id (dragging-corner state)])
      :pan (re-frame/dispatch [:canvas/pan! app-id (dragging-pan state)])
      (:connector :arrow)
      (let [[_ h-id] hovered]
        (case (resolve-connector-release drag hovered)
          :new-state-connection (re-frame/dispatch [:canvas/connect! app-id id h-id])
          :disconnect-and-connect (re-frame/dispatch [:canvas/reconnect! app-id id origin h-id])
          :disconnect (re-frame/dispatch [:canvas/disconnect! app-id id origin])
          :do-nothing nil))
      nil)
    (swap! state assoc :drag nil)))

;; --- zoom con la rueda del mouse ------------------------------------------------
;; Es discreto (un evento por "tick" de rueda, no un stream continuo de
;; pixeles), asi que no sufre el mismo problema de fluidez que el arrastre; se
;; puede seguir dispatchando directo a re-frame sin pasar por el atomo local.
(def zoom-bounds {:min-w 200 :max-w 1500 :min-h 150 :max-h 1500})

(defn- clamp [v lo hi default]
  (if (and (< v hi) (> v lo)) v default))

(defn wheel->zoom [{:keys [x y w h] :or {x 0 y 0 w 1342 h 600}} wheel-delta]
  (let [{:keys [min-w max-w min-h max-h]} zoom-bounds
        new-w (- w (/ wheel-delta 12))
        new-h (- h (/ wheel-delta 12))]
    {:x x :y y
     :w (clamp new-w min-w max-w w)
     :h (clamp new-h min-h max-h h)}))

;; --- eventos re-frame: el unico punto donde el resultado de una interaccion
;; entra al app-db (y, salvo pan/zoom, al historial de undo) ------------------
(re-frame/reg-event-db
  :canvas/move-state!
  [undo/track]
  (fn [db [_ app-id state-id corner]]
    (assoc-in db [:applications :editable app-id :states state-id :diagram :corner] corner)))

(re-frame/reg-event-db
  :canvas/pan!
  (fn [db [_ app-id [x y]]]
    (update-in db [:applications :editable app-id :svg-ctrl :zoom] merge {:x x :y y})))

(re-frame/reg-event-db
  :canvas/zoom!
  (fn [db [_ app-id wheel-delta]]
    (update-in db [:applications :editable app-id :svg-ctrl :zoom]
               (fn [zoom] (wheel->zoom (or zoom {}) wheel-delta)))))

(re-frame/reg-event-db
  :canvas/connect!
  [undo/track]
  (fn [db [_ app-id from-id to-id]]
    (update-in db [:applications :editable app-id :states from-id :flow] flow/add-connection to-id)))

(re-frame/reg-event-db
  :canvas/reconnect!
  [undo/track]
  (fn [db [_ app-id from-id old-to-id new-to-id]]
    (update-in db [:applications :editable app-id :states from-id :flow] flow/reconnect-flow old-to-id new-to-id)))

(re-frame/reg-event-db
  :canvas/disconnect!
  [undo/track]
  (fn [db [_ app-id from-id to-id]]
    (update-in db [:applications :editable app-id :states from-id :flow] flow/remove-flow-to to-id)))

(re-frame/reg-event-db
  :canvas/delete-state!
  [undo/track]
  (fn [db [_ app-id state-id]]
    (update-in db [:applications :editable app-id :states] flow/delete-state state-id)))

(re-frame/reg-event-db
  :canvas/update-flow-re!
  [undo/track]
  (fn [db [_ app-id orig-state other-state new-re-txt]]
    ;; solo las transiciones que YA tenian una regex se pueden editar aqui --
    ;; la entrada default (sin regex, siempre la ultima) no dispara este
    ;; dialogo desde el render (ver canvas/render.cljs), igual que en la v1.
    (update-in db [:applications :editable app-id :states orig-state :flow]
               (fn [flow]
                 (mapv (fn [[other re]]
                         (if (and re (= other other-state))
                           [other new-re-txt]
                           (if re [other re] [other])))
                       flow)))))

;; --- animacion del robotito ----------------------------------------------------
(defn animate-robot! [dom-id [x0 y0] [x1 y1]]
  (interop/animate-translate! dom-id x0 y0 x1 y1 500))
