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
            [robot2.operations.dialog :as opr-dialog]))

(def CLAVIJA "M 0 0 l0,10 l-7,0 q 7 -20 14 0 l-4,0 l0,4 l0,-4 l-6,0 l0,4 l0,-4 l-4,0")
(def ARROW "0,0 5,-15 -5,-15")
(def STATE-W-MIN 130)
(def STATE-H 60)
(def HOVER-COLOR "#F66C4A")
(def SHADOW-STROKE "#97d0e5")
(def STROKE "#4ea9c3")

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
      [:path {:d CLAVIJA
              :stroke (if hovered? HOVER-COLOR "transparent")
              :fill (if hovered? HOVER-COLOR "transparent")
              :onPointerDown (fn [e] (.stopPropagation e) (.preventDefault e) (pin-down e))}]]]))

(defn- transition-arrows [state svg-el-id app-id state-id {:keys [diagram flow]} states hovered? ommit-arrow2]
  (let [[x1 y1] (:corner diagram)
        w (state-width state-id)
        [x-center y-center] [(+ x1 (/ w 2)) (+ y1 (/ STATE-H 2))]]
    (doall
      (for [[idx [other-state re]] (map-indexed vector flow)
            :when (not= ommit-arrow2 other-state)]
        (let [w-other (state-width other-state)
              [[ax1 ay1] [ax2 ay2]] (geom/compute-entry&exit {:x x1 :y y1 :h STATE-H :w w}
                                                              {:x (first (get-in states [other-state :diagram :corner] [0 0]))
                                                               :y (second (get-in states [other-state :diagram :corner] [0 0]))
                                                               :h STATE-H :w w-other})
              re-txt (if re (str (inc idx) ") " re) (str (inc idx) ") default"))
              x-t (- (/ (+ ax1 ax2) 2) 25)
              y-t (- (/ (+ ay1 ay2) 2) 15)
              path (gstring/format "M%d,%dL%d,%d" ax1 ay1 ax2 ay2)
              stroke (if hovered? HOVER-COLOR SHADOW-STROKE)]
          ^{:key (str state-id "->" other-state)}
          [:g
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
  (let [last-current (reagent/atom nil)]
    (reagent/create-class
      {:display-name "robot-marker"
       :component-did-update
       (fn [this]
         (let [[_ states current status] (reagent/argv this)]
           (when (and current (not= current @last-current))
             (let [[px py] (get-in states [@last-current :diagram :corner] [0 0])
                   [cx cy] (get-in states [current :diagram :corner] [0 0])]
               (inter/animate-robot! "robotito" [(+ px 30) py] [(+ cx 30) cy])))
           (reset! last-current current)))
       :reagent-render
       (fn [states current status]
         (let [[x y] (get-in states [current :diagram :corner] [0 0])
               img (if (= :running status) "images/robot-start.gif" "images/robot-waiting.gif")]
           [:image {:id "robotito" :href (if current img "images/robot-stop.gif")
                    :x (+ x 30) :y y :height "57" :width "57"}]))})))

(defn svg-comp [editables ready width height app-id watch-instance]
  (let [state (inter/new-state)]
    (fn [editables ready width height app-id watch-instance]
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
                              (inter/update-drag! state (.-currentTarget e) e))
            on-pointer-up (fn [e]
                            (when (:drag @state)
                              (inter/end-drag! state (.-currentTarget e) e app-id)))]
        [:div
         [:svg {:id svg-el-id :width width :height height
                :viewBox (str pan-x " " pan-y " " (:w zoom) " " (:h zoom))
                :style {:touch-action "none" :cursor (if (inter/dragging? state :pan) "grabbing" "default")}
                :onPointerDown on-pointer-down
                :onPointerMove on-pointer-move
                :onPointerUp on-pointer-up
                :onWheel (fn [e]
                           (re-frame/dispatch [:canvas/zoom! app-id (.-deltaY (.-nativeEvent e))]))}
         (when connecting
           (let [[mx my] connecting
                 {:keys [id origin]} (:drag @state)
                 [x y] (get-in states [id :diagram :corner] [0 0])
                 [cx cy] (geom/compute-exit2point {:x x :y y :h STATE-H :w (state-width id)} [mx my])]
             [:path {:d (gstring/format "M%d,%dL%d,%d" cx cy mx my)
                     :stroke HOVER-COLOR :stroke-width 2 :fill "transparent"}]))
         (doall
           (for [[state-id state-data] states]
             (let [hovered? (inter/hovered? state :state state-id)
                   ommit (when (and (= state-id (get-in @state [:drag :id])) (= (get-in @state [:drag :type]) :arrow))
                           (get-in @state [:drag :origin]))]
               ^{:key (str "wrap-" state-id)}
               [:g
                [state-box state svg-el-id app-id hovered? state-id state-data]
                [transition-arrows state svg-el-id app-id state-id state-data states hovered? ommit]])))
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
