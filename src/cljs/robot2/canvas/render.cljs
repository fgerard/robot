(ns robot2.canvas.render
  "Render del diagrama SVG: cajas de estado, flechas de transicion, marcador
   del robot en ejecucion. La interaccion (drag/pan/connect) vive en
   robot2.canvas.interactions; aqui solo se leen sus valores `dragging-*` /
   `connecting-point` para dibujar la posicion EN VIVO mientras se arrastra,
   sin que eso pase por re-frame en cada frame."
  (:require [clojure.string :as string]
            [reagent.core :as reagent]
            [re-frame.core :as re-frame]
            [re-com.core :as re-com]
            [goog.string :as gstring]
            [goog.string.format]
            [robot2.canvas.geometry :as geom]
            [robot2.canvas.interactions :as inter]
            [robot2.interop :as interop]
            [robot2.operations.dialog :as opr-dialog]))

(def CLAVIJA "M 0 0 l0,10 l-7,0 q 7 -20 14 0 l-4,0 l0,4 l0,-4 l-6,0 l0,4 l0,-4 l-4,0")
(def CLAVIJA-ICON "M56.1515696,29 C56.1129034,29 56.0763576,29 56.0460483,28.9966947 C56.0434913,28.9963828 56.040872,28.9961334 56.038315,28.9960086 C53.0345731,28.7569641 50.2462395,27.4052054 48.1870137,25.1898179 C46.1198675,22.9665101 44.981459,20.0639864 44.981459,17.0168385 C44.981459,12.9712914 46.9989628,9.2226634 50.378266,6.98925245 C52.3456283,5.68788458 54.6314881,5 56.9889428,5 C58.7336002,5 60.4793178,5.36651838 62.1775758,6.08938966 C64.7158886,7.17110843 66.7211688,9.12599784 67.8240293,11.5939007 C69.0042222,14.2321221 69.317481,17.0185224 68.7299415,19.6521287 C68.1796338,22.106935 66.8904271,24.2970647 65.0017692,25.9855313 C64.8862695,26.0901796 64.7385271,26.1471187 64.5846105,26.1471187 C64.4059352,26.1471187 64.2353049,26.0706594 64.1164998,25.9373233 C64.0031828,25.8123441 63.9464931,25.6527524 63.9553489,25.4863005 C63.9642671,25.3180401 64.0384813,25.1634999 64.1642713,25.0512431 C67.7658429,21.8387036 68.7517068,16.7596458 66.6757672,12.1119034 C65.6977612,9.92426825 63.9254138,8.19719774 61.6850804,7.24869034 C60.1493453,6.59597954 58.5709526,6.26500915 56.9941191,6.26500915 C54.8757714,6.26500915 52.8282701,6.87892899 51.0730107,8.04053717 C48.0451336,10.0402669 46.2373006,13.3977424 46.2373006,17.0217653 C46.2373006,22.5683727 50.560309,27.2778563 56.0789769,27.7433477 C56.0865854,27.743909 56.0941939,27.7442209 56.1017401,27.7442209 C56.1691565,27.7442209 56.2344526,27.7190254 56.2844692,27.6730002 C56.3400987,27.6217986 56.3718424,27.5497048 56.3718424,27.4741186 L56.3718424,24.2360095 C56.3718424,24.102611 56.2743661,23.9891069 56.1424644,23.9690254 C54.8207032,23.7672751 53.6067085,23.0957924 52.7240584,22.0783719 C51.832303,21.0504116 51.3399947,19.7346375 51.3377496,18.3732746 L51.3377496,14.7517462 C51.3377496,14.4053717 51.6193894,14.1236695 51.9655768,14.1236695 L53.9213393,14.1236695 C54.0705161,14.1236695 54.1914416,14.0026817 54.1914416,13.8535049 L54.1914416,10.9100075 C54.1914416,10.5637577 54.4730814,10.2821179 54.8192688,10.2821179 C55.1655809,10.2821179 55.4473455,10.5637577 55.4473455,10.9100075 L55.4473455,13.8535049 C55.4473455,14.0026817 55.5682709,14.1236695 55.7174477,14.1236695 L58.2719754,14.1236695 C58.4212146,14.1236695 58.5420777,14.0026817 58.5420777,13.8535049 L58.5420777,10.9100075 C58.5420777,10.5637577 58.8237798,10.2821179 59.1699673,10.2821179 C59.5161547,10.2821179 59.7977945,10.5637577 59.7977945,10.9100075 L59.7977945,13.8535049 C59.7977945,14.0026817 59.9186576,14.1236695 60.0678968,14.1236695 L62.0239087,14.1236695 C62.3700962,14.1236695 62.6517359,14.4053717 62.6517359,14.7517462 L62.6517359,18.3737111 C62.6517359,19.945306 61.9922897,21.4569682 60.8425933,22.5211001 C60.7264699,22.6276193 60.5751727,22.6868036 60.4173895,22.6868036 C60.2411464,22.6868036 60.0774386,22.6152087 59.9564507,22.4852403 C59.7246405,22.2326626 59.7402317,21.8348994 59.9924976,21.6000956 C60.8874336,20.769146 61.4008836,19.5930692 61.4008836,18.3737111 L61.4008836,15.6497381 C61.4008836,15.5005613 61.2799581,15.3795734 61.1307813,15.3795734 L52.8686826,15.3795734 C52.7195058,15.3795734 52.5985179,15.5005613 52.5985179,15.6497381 L52.5985179,18.3737111 C52.5985179,20.8006403 54.5728652,22.7751123 56.9996072,22.7751123 C57.3459194,22.7751123 57.6276839,23.056752 57.6276839,23.4029395 C57.6276839,23.4400466 57.6241291,23.4777773 57.6178926,23.5092716 C57.610783,23.544383 57.610783,23.5804923 57.6178302,23.6155413 C57.6242538,23.6477216 57.6276839,23.6846416 57.6276839,23.72231 L57.6276839,27.5090429 C57.6276839,27.9222102 57.4516902,28.3229669 57.1449174,28.608598 C56.879243,28.8575586 56.517402,29 56.1515696,29")
(def ARROW "0,0 5,-15 -5,-15")
(def STATE-W-MIN 130)
(def STATE-H 60)
(def HOVER-COLOR "#F7941D")
(def SHADOW-STROKE "#B07FC8")
(def STROKE "#7B3FA0")

(defn- state-width [state-id]
  (max STATE-W-MIN (* 15 (count (name state-id)))))

(defn svg-id [app-id] (str "svg-diagram-" app-id))

(defn- find-svg [svg-el-id]
  (.getElementById js/document svg-el-id))

(defn- state-box [state svg-el-id app-id hovered? state-id {:keys [diagram operation flow]}]
  (let [img-path (str "images/icons/operations/" (string/lower-case (name operation)) "-opr.png")
        being-dragged? (= state-id (get-in @state [:drag :id]))
        [x y] (if being-dragged?
                (or (inter/dragging-corner state) (:corner diagram))
                (:corner diagram))
        w (state-width state-id)
        h STATE-H
        shadow-color (if hovered? HOVER-COLOR SHADOW-STROKE)
        shadow-blur (if hovered? 6 2)
        [x-center y-center] [(+ x (/ w 2)) (+ y (/ h 2))]
        pin-down (fn [e]
                   (inter/begin-drag! state (find-svg svg-el-id) e :connector state-id nil))]
    ^{:key (str state-id)}
    [:g {:onPointerOver (fn [e] (.stopPropagation e) (inter/set-hovered! state :state state-id))
         :onPointerOut (fn [e] (.stopPropagation e) (inter/clear-hovered! state))
         :onPointerDown (fn [e]
                          (.stopPropagation e) (.preventDefault e)
                          (inter/begin-drag! state (find-svg svg-el-id) e :state state-id (:corner diagram)))}
     [:defs [:filter {:id (str "shadow-" state-id)}
             [:feGaussianBlur {:in "SourceGraphic" :stdDeviation shadow-blur}]]]
     [:rect {:x x :y y :width w :height h :rx 5 :ry 5
             :filter (str "url(#shadow-" state-id ")")
             :fill shadow-color}]
     [:rect {:x x :y y :width w :height h :rx 5 :ry 5
             :style {:fill "white" :stroke STROKE :stroke-width 3 :cursor "grab"}}]
     [:image {:href img-path :x (+ x 4) :y (+ y h -57) :height 32 :width 32
              :style {:cursor "pointer"}
              :onPointerDown (fn [e]
                               (.stopPropagation e) (.preventDefault e)
                               (inter/open-dialog! state {:type :menu :state-id state-id}))}]
     [:g {:transform (str "translate(" (+ x (- w 9)) "," (+ y 9) ")")
          :fill "#F66C4A"
          :style {:cursor "pointer"}
          :onPointerDown (fn [e] (.stopPropagation e) (.preventDefault e))
          :onPointerUp (fn [e]
                         (.stopPropagation e) (.preventDefault e)
                         (re-frame/dispatch [:canvas/delete-state! app-id state-id]))}
      [:circle {:r 9 :cx 0 :cy 0 :fill "transparent" :stroke "transparent"}]
      [:text {:x -5 :y 4 :style {:font-size "14px"}} "x"]]
     [:text {:x (+ x 36) :y (+ y h -8) :fill STROKE
             :style {:font-family "'Helvetica Neue', Helvetica, Arial, sans-serif" :font-size "20px" :font-weight 500 :cursor "grab"}}
      (str state-id)]
     [:g {:transform (str "translate(" x-center "," y-center ")")}
      [:g {:transform "translate(-55.000000, -27.500000)"
           :style {:cursor "cell"}
           :onPointerDown (fn [e] (.stopPropagation e) (.preventDefault e) (pin-down e))}
       [:rect {:x 44 :y 5 :width 25 :height 25 :style {:fill "transparent" :stroke "transparent" :stroke-width 1}}]
       [:path {:d CLAVIJA-ICON
               :stroke (if hovered? HOVER-COLOR "transparent")
               :stroke-width 1
               :fill (if hovered? HOVER-COLOR "transparent")}]]]]))

(defn- live-corner
  "Esquina en vivo de state-id si es justo el que se esta arrastrando
   (:type :state), si no la esquina por defecto (la persistida). Permite que
   las flechas sigan al cuadrito mientras se arrastra, en vez de solo
   repintarse cuando se suelta."
  [state state-id default]
  (let [drag (:drag @state)]
    (if (and (= (:type drag) :state) (= (:id drag) state-id))
      (or (inter/dragging-corner state) default)
      default)))

(defn- transition-arrows [state svg-el-id app-id state-id {:keys [diagram flow]} states hovered? ommit-arrow2]
  (let [[x1 y1] (live-corner state state-id (:corner diagram))
        w (state-width state-id)
        [x-center y-center] [(+ x1 (/ w 2)) (+ y1 (/ STATE-H 2))]]
    (doall
      (for [[idx [other-state re]] (map-indexed vector flow)
            :when (not= ommit-arrow2 other-state)]
        (let [w-other (state-width other-state)
              [ox oy] (live-corner state other-state (get-in states [other-state :diagram :corner] [0 0]))
              [[ax1 ay1] [ax2 ay2]] (geom/compute-entry&exit {:x x1 :y y1 :h STATE-H :w w}
                                                              {:x ox :y oy :h STATE-H :w w-other})
              re-txt (if re (str (inc idx) ") " re) (str (inc idx) ") default"))
              x-t (- (/ (+ ax1 ax2) 2) 25)
              y-t (- (/ (+ ay1 ay2) 2) 15)
              path (gstring/format "M%d,%dL%d,%d" ax1 ay1 ax2 ay2)
              stroke (if hovered? HOVER-COLOR SHADOW-STROKE)]
          ^{:key (str state-id "->" other-state)}
          [:g {:onPointerOver (fn [e] (.stopPropagation e) (inter/set-hovered! state :state state-id))
               :onPointerOut (fn [e] (.stopPropagation e) (inter/clear-hovered! state))}
           [:path {:d path :stroke stroke :stroke-width 2 :fill "transparent"}]
           [:polygon {:points ARROW
                      :transform (geom/position&rotate ax1 ay1 ax2 ay2)
                      :style {:stroke STROKE :fill STROKE}
                      :onPointerDown (fn [e]
                                       (.stopPropagation e) (.preventDefault e)
                                       (inter/begin-drag! state (find-svg svg-el-id) e :arrow state-id other-state))}]
           [:rect {:x x-t :y y-t :width (* (count re-txt) 9) :height "2em" :rx 3 :ry 3
                   :style {:fill (if hovered? "#FFFFFF" "transparent") :stroke (if hovered? "#000000" "transparent")}}]
           [:text {:x (+ x-t 3) :y (+ y-t 20) :fill (if hovered? "#000000" "transparent")
                   :style {:font-family "'Helvetica Neue', Helvetica, Arial, sans-serif" :font-size "16px" :font-weight 500}
                   :onPointerDown (fn [e]
                                    (.stopPropagation e) (.preventDefault e)
                                    (when re
                                      (inter/open-dialog! state {:type :flow-re :state-id state-id :other-state other-state})))}
            re-txt]])))))

(defn- robot-marker
  "Anima imperativamente con la Web Animations API en vez de dispatchar un
   evento re-frame durante el render (que era lo que hacia la v1)."
  []
  ;; atom plano (NO reagent/atom): si fuera reactivo, leerlo dentro de
  ;; reagent-render lo suscribiria, y el reset! en component-did-update
  ;; dispararia un segundo render que pisa la posicion base mientras la
  ;; animacion CSS sigue corriendo, duplicando el desplazamiento.
  (let [last-current (atom nil)]
    (reagent/create-class
      {:display-name "robot-marker"
       :component-did-update
       (fn [this]
         (let [[_ states current status] (reagent/argv this)]
           ;; component-did-update no corre en el mount inicial, asi que la
           ;; PRIMERA vez que algo dispara una actualizacion (p.ej. status
           ;; transitioning->running aunque current no haya cambiado todavia)
           ;; last-current sigue en nil. Sin el chequeo de @last-current aqui,
           ;; eso se contaba como "cambio" y animaba desde [0 0] (la esquina
           ;; del SVG) hacia la posicion actual, aunque la base declarativa ya
           ;; estuviera puesta ahi -- el robot se iba "hacia afuera" en vez de
           ;; quedarse quieto.
           (when (and current @last-current (not= current @last-current))
             (let [[px py] (get-in states [@last-current :diagram :corner] [0 0])
                   [cx cy] (get-in states [current :diagram :corner] [0 0])]
               (inter/animate-robot! "robotito" [(+ px 30) py] [(+ cx 30) cy])))
           (reset! last-current current)))
       :reagent-render
       (fn [states current status]
         ;; Usa la posicion ANTERIOR (no la actual) como base declarativa: la
         ;; animacion CSS de component-did-update desplaza desde ahi hasta la
         ;; posicion actual. Si se usara la posicion actual aqui, la animacion
         ;; se sumaria encima y el robotito terminaria sobrepasando el destino.
         (let [base (or @last-current current)
               [x y] (get-in states [base :diagram :corner] [0 0])
               img (if (= :running status) "images/robot-start.gif" "images/robot-waiting.gif")]
           [:image {:id "robotito" :href (if current img "images/robot-stop.gif")
                    :x (+ x 30) :y y :height "57" :width "57"}]))})))

(defn svg-comp [editables ready width height app-id watch-instance]
  (let [state (inter/new-state)
        ;; Definidos UNA SOLA VEZ (no en cada render) para que la identidad de
        ;; la funcion sea estable y React no desmonte/remonte el listener via
        ;; :ref en cada re-render. app-id puede cambiar entre renders (el
        ;; usuario elige otra app), asi que el handler lo lee de @state en vez
        ;; de capturarlo por closure.
        wheel-handler (fn [e]
                        (let [app-id (:wheel/app-id @state)]
                          (if (.-ctrlKey e)
                            (do (.preventDefault e)
                                (let [anchor (inter/svg-point (.-currentTarget e) e)]
                                  (re-frame/dispatch [:canvas/zoom! app-id (.-deltaY e) anchor])))
                            (re-frame/dispatch [:canvas/pan-by! app-id [(.-deltaX e) (.-deltaY e)]]))))
        svg-ref (fn [el]
                  (when el (interop/add-wheel-listener! el wheel-handler)))]
    (fn [editables ready width height app-id watch-instance]
      (swap! state assoc :wheel/app-id app-id)
      (let [states (get-in editables [app-id :states])
            zoom (get-in editables [app-id :svg-ctrl :zoom] {:x 0 :y 0 :w 1342 :h 600})
            [pan-x pan-y] (or (inter/dragging-pan state) [(:x zoom) (:y zoom)])
            connecting (inter/connecting-point state)
            dialog (:dialog @state)
            svg-el-id (svg-id app-id)
            on-pointer-down (fn [e]
                              (when (= (.-target e) (.-currentTarget e))
                                (.stopPropagation e) (.preventDefault e)
                                (inter/begin-drag! state (.-currentTarget e) e :pan app-id [(:x zoom) (:y zoom)])))
            on-pointer-move (fn [e]
                              (inter/update-drag! state (.-currentTarget e) e)
                              (inter/update-connector-hover!
                                state states
                                (fn [sid]
                                  (let [[bx by] (get-in states [sid :diagram :corner] [0 0])]
                                    [bx by (state-width sid) STATE-H]))))
            on-pointer-up (fn [e]
                            (when (:drag @state)
                              (inter/end-drag! state (.-currentTarget e) e app-id)))]
        [:div
         [:svg {:id svg-el-id :width width :height height
                :viewBox (str pan-x " " pan-y " " (:w zoom) " " (:h zoom))
                :style {:touch-action "none" :cursor (if (inter/dragging? state :pan) "grabbing" "default")}
                ;; :ref en vez de :onWheel: el listener sintetico de wheel de
                ;; React es passive, asi que preventDefault ahi se ignora (el
                ;; pellizco zoomearia tambien la pagina completa, no solo el
                ;; canvas). Ver wheel-handler/svg-ref arriba e
                ;; interop/add-wheel-listener!.
                :ref svg-ref
                :onPointerDown on-pointer-down
                :onPointerMove on-pointer-move
                :onPointerUp on-pointer-up}
         (when connecting
           (let [[mx my] connecting
                 {:keys [id origin]} (:drag @state)
                 [x y] (get-in states [id :diagram :corner] [0 0])
                 [cx cy] (geom/compute-exit2point {:x x :y y :h STATE-H :w (state-width id)} [mx my])
                 path (gstring/format "M%d,%dL%d,%d" cx cy mx my)]
             [:g
              [:path {:d path :stroke HOVER-COLOR :stroke-width 2 :fill "transparent"}]
              [:path {:d CLAVIJA :stroke HOVER-COLOR :stroke-width 1 :fill HOVER-COLOR
                      :transform (geom/position&rotate cx cy mx my)}]]))
         (doall
           (for [[state-id state-data] states]
             (let [hovered? (inter/hovered? state :state state-id)
                   ommit (when (and (= state-id (get-in @state [:drag :id])) (= (get-in @state [:drag :type]) :arrow))
                           (get-in @state [:drag :origin]))]
               ^{:key (str "wrap-" state-id)}
               [:g
                [state-box state svg-el-id app-id hovered? state-id state-data]
                (transition-arrows state svg-el-id app-id state-id state-data states hovered? ommit)])))
         (when (seq watch-instance)
           (let [{current :robot/current status :robot/status} (get-in ready [app-id watch-instance])]
             [robot-marker states current status]))]
        (when (= (:type dialog) :flow-re)
          (let [{:keys [state-id other-state]} dialog
                current-re (->> (get-in states [state-id :flow])
                                 (filter (fn [[other]] (= other other-state)))
                                 first second)]
            [opr-dialog/flow-re-modal app-id state-id other-state current-re #(inter/close-dialog! state)]))
        (when (= (:type dialog) :menu)
          [opr-dialog/operation-modal app-id (:state-id dialog)
           (get-in editables [app-id :init-state])
           (get-in states [(:state-id dialog)])
           #(inter/close-dialog! state)])]))))
