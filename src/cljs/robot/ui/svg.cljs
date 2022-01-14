(ns robot.ui.svg
  (:require
    [clojure.string :as string]
    [reagent.core :as reagent :refer [atom create-class]]
    [re-frame.core :as re-frame]
    [re-com.core :as re-com]
    [robot.ui.db :refer [DEF-ATTR]]
    [cljs.pprint :refer [pprint]]
    [goog.string :as gstring]
    [goog.string.format]
    [fipp.edn :refer (pprint) :rename {pprint fipp}]
    [robot.ui.state-editor :as state-editor]
    [robot.ui.events :refer [snapshot-event]]
    ;[cljsjs.snapsvg]
    [cljsjs.svgjs]))

;;;;;;;;;;;;;;;; SUBSCRIPTIONS


;;;;;;;;;;;;;; EVENTS

(re-frame/reg-event-db
  :wheel
  (fn [db [_ selected-app wheelDelta wheelDeltaX wheelDeltaY :as evt]]
    (js/registerListener)
    (-> db
        (update-in [:applications :editable selected-app :svg-ctrl :zoom]
                   (fn [{:keys [x y h w] :or {x 0 y 0 w 1342 h 600}}]
                     (let [new-w (- w (/ wheelDelta 12))
                           new-h (- h (/ wheelDelta 12))
                           new-x x                          ; (/ pageX 2)
                           new-y y                          ; (/ pageY 2)
                           new-state {:x new-x
                                      :y new-y
                                      :w (if (and (< new-w 1500) (> new-w 200))
                                           new-w
                                           w)
                                      :h (if (and (< new-h 1500) (> new-h 150))
                                           new-h
                                           h)}]
                       new-state))))))

(re-frame/reg-event-db
  :target
  (fn [db [_ app-id type id coords]]
    (js/registerListener)
    (-> db
        (assoc-in [:applications :editable app-id :svg-ctrl :target] [type id coords])
        (assoc-in [:applications :editable app-id :svg-ctrl :mouse-down] true))))

(defn fix-flow [flow id]
  (let [new-flow (vec (filter (fn [[other-id re]]
                                (not= other-id id)) flow))]
    (if (seq new-flow)
      (if (= 2 (count (last new-flow)))
        (conj (butlast new-flow) [(first (last new-flow))])
        new-flow)
      new-flow)))

(defn delete-state [states id]
  (js/registerListener)
  (reduce
    (fn [new-states [state-id {:keys [flow] :as state-conf}]]
      (if (not= state-id id)
        (assoc new-states state-id (assoc state-conf :flow (fix-flow flow id)))
        new-states))
    {}
    states))

(re-frame/reg-event-db
  :delete-state
  (fn [db [_ app state-id]]
    (js/registerListener)
    (let [new-db (-> db
                     (update-in [:applications :editable app :states] delete-state state-id)
                     (update-in [:applications :editable app :svg-ctrl] dissoc :target)
                     (update-in [:applications :editable app :svg-ctrl] dissoc :mouse-down)
                     (update-in [:applications :editable app :svg-ctrl] dissoc :hovered))]
      new-db)))

(re-frame/reg-event-db
  :hovered
  (fn [db [_ app-id type id]]
    (assoc-in db [:applications :editable app-id :svg-ctrl :hovered] [type id])))

(defmulti process-move (fn [_ [type & _ :as param]] 
                         #_(.log js/console (pr-str param)) 
                         type) :default :default)

(defmethod process-move :default [db params]
  (.error js/console (str "No process-move for :" params))
  db)

(defmethod process-move :svg [db [type app-id id [orig-x orig-y] [new-x new-y]]]
  (js/registerListener)
  (if (get-in db [:applications :editable app-id :svg-ctrl :mouse-down])
    (update-in db [:applications :editable app-id :svg-ctrl :zoom]
               (fn [{:keys [x y w h] :or {x 0 y 0 w 1342 h 600} :as zoom}]
                 {:x (+ x (- orig-x new-x)) :y (+ y (- orig-y new-y)) :w w :h h}))
    db))

(defmethod process-move :state [db [type app-id id [delta-x delta-y] [x y]]]
  (js/registerListener)
  (if (get-in db [:applications :editable app-id :svg-ctrl :mouse-down])
    (assoc-in db [:applications :editable app-id :states id :diagram :corner] [(+ x delta-x) (+ y delta-y)])
    db))

(defmethod process-move :connector [db [type app-id id coords [x y]]]
  (js/registerListener)
  (if (get-in db [:applications :editable app-id :svg-ctrl :mouse-down])
    (assoc-in db [:applications :editable app-id :svg-ctrl :connecting] {:state-id id
                                                                         :mouse    [x y]
                                                                         :coords   coords}) ;guardamos en connecting el id del state coords del mouse
    db))

(defmethod process-move :arrow [db [type app-id [state-orig state-dest] coords [x y]]]
  (js/registerListener)
  (if (get-in db [:applications :editable app-id :svg-ctrl :mouse-down])
    (assoc-in db [:applications :editable app-id :svg-ctrl :connecting] {:state-id  state-orig
                                                                         :mouse     [x y]
                                                                         :coords    coords
                                                                         :old-state state-dest})
    db)) ;correccion

(re-frame/reg-event-db
  :mouse-move
  (fn [db [_ app-id [x y] :as e]]
    (let [target (get-in db [:applications :editable app-id :svg-ctrl :target])]
      (if target
        (let [[type id coords] target]
          (-> db
              (process-move [type app-id id coords [x y]])
              ))
        db))))

(defn mouseup-selector2 [db app-id svg-ctrl]
  (let [{:keys [state-id coords old-state]} (:connecting svg-ctrl)
        [type id] (:hovered svg-ctrl)
        selection (cond
                    (and state-id id (= type :state) (not= state-id id) (not old-state))
                    :new-state-connection

                    (and state-id id (= type :state) (not= state-id id) old-state (not= old-state id))
                    :disconnect-and-connect

                    (and state-id id (= type :state) (not= state-id id) old-state (= old-state id))
                    :do-nothing

                    (not (nil? (and state-id (nil? id) old-state)))
                    :disconnect

                    :OTHERWIZE
                    :do-nothing
                    )]
    selection))

(defmulti process-mouse-up mouseup-selector2 :default :do-nothing)

(defmethod process-mouse-up :do-nothing
  [db app-id svg-ctrl]
  db)

(defn remove-flow-to [flow id]
  (let [flow (remove (fn [[state re]] (= state id)) flow)
        [last-id re] (last flow)
        flow (if re
               (conj (butlast flow) [last-id])
               flow)]
    flow))

(defmethod process-mouse-up :disconnect
  [db app-id svg-ctrl]
  (js/registerListener)
  (let [{:keys [state-id coords old-state]} (:connecting svg-ctrl)
        [_ state-old] (second (:target svg-ctrl))]
    (update-in db [:applications :editable app-id :states state-id :flow]
               remove-flow-to state-old)))

(defmethod process-mouse-up :new-state-connection
  [db app-id svg-ctrl]
  (js/registerListener)
  (let [{:keys [state-id coords old-state]} (:connecting svg-ctrl)
        [type id] (:hovered svg-ctrl)]
    (update-in db [:applications :editable app-id :states state-id :flow]
               (fn [flow]
                 (conj (mapv (fn [[s-id re]]
                               (if re
                                 [s-id re]
                                 [s-id "undefined"])) flow) [id])))))

(defn reconnect-flow [flow old-state new-state]
  (let [old-connections (into #{} (map first flow))]
    (if (old-connections new-state)
      (do
        (.error js/console "Can't connecto 2 arrows to same state!")
        flow)
      (let [new-flow (mapv (fn [[state re]]
                             (let [flow-elem (if (= state old-state)
                                               [new-state]
                                               [state])
                                   flow-elem (if re (conj flow-elem re) flow-elem)]
                               flow-elem)) flow)]
        new-flow))))

(defmethod process-mouse-up :disconnect-and-connect
  [db app-id svg-ctrl]
  (js/registerListener)
  (let [{:keys [state-id coords old-state]} (:connecting svg-ctrl)
        ;[_ state-old] (second (:target svg-ctrl))
        [type id] (:hovered svg-ctrl)]
    (-> db
        (update-in [:applications :editable app-id :states state-id :flow]
                   reconnect-flow old-state id))))

(re-frame/reg-event-db
  :mouseup
  (fn [db [_ app-id :as e]]
    (js/registerListener)
    (let [svg-ctrl (get-in db [:applications :editable app-id :svg-ctrl])
          {:keys [state-id coords old-state]} (:connecting svg-ctrl)
          [type id] (:hovered svg-ctrl)
          db (process-mouse-up db app-id svg-ctrl)]
      (-> db
          (update-in [:applications :editable app-id :svg-ctrl] dissoc :target)
          (update-in [:applications :editable app-id :svg-ctrl] dissoc :mouse-down)
          (update-in [:applications :editable app-id :svg-ctrl] dissoc :connecting)
          (snapshot-event e)))))

(re-frame/reg-event-db
  :mouseout
  (fn [db [_ app-id]]
    #_(.log js/console (pr-str [:mouseout app-id]))
    (update-in db [:applications :editable app-id :svg-ctrl] dissoc :hovered)))

(re-frame/reg-event-db
  :mouseout-svg
  (fn [db [_ app-id]]
    #_(.log js/console (pr-str [:mouseout-svg app-id]))
    db))

(re-frame/reg-event-db
 :mouseleave-svg
 (fn [db [_ app-id]]
   #_(.log js/console (pr-str [:mouseout-svg app-id]))
   (-> db
       (update-in [:applications :editable app-id :svg-ctrl] dissoc :target)
       (update-in [:applications :editable app-id :svg-ctrl] dissoc :mouse-down)
       (update-in [:applications :editable app-id :svg-ctrl] dissoc :connecting))))

(re-frame/reg-event-db
  :update-re
  (fn [db [_ app orig-state other-state new-re-txt]]
    (js/registerListener)
    (-> db
        (update-in [:applications :editable app :states orig-state :flow]
                   (fn [flow]
                     (mapv (fn [[other re]]
                             (if (and re (= other-state other))
                               [other new-re-txt]
                               (if re
                                 [other re]
                                 [other])))
                           flow)))
        (update-in [:applications :editable app :svg-ctrl] dissoc :target)
        (update-in [:applications :editable app :svg-ctrl] dissoc :mouse-down)
        (update-in [:applications :editable app :svg-ctrl] dissoc :connecting))))

(re-frame/reg-event-db
  :cancel-update
  (fn [db [_ app-id]]
    (-> db
        (update-in [:applications :editable app-id :svg-ctrl] dissoc :target)
        (update-in [:applications :editable app-id :svg-ctrl] dissoc :mouse-down)
        (update-in [:applications :editable app-id :svg-ctrl] dissoc :connecting))))

(re-frame/reg-event-db
  :add-state
  (fn [db [_ app-id state-id opr-id]]
    (js/registerListener)
    (.log js/console (str "ADD-STATE: " [(get-in db [:applications :editable app-id :init-state])
                                         (nil? (get-in db [:applications :editable app-id :init-state]))
                                         state-id]))
    (cond-> db
            true
            (update-in [:applications :editable app-id :states] assoc state-id {:operation opr-id,
                                                                                :conf      {},
                                                                                :diagram   {:corner [100 100]},
                                                                                :flow      []})

            (nil? (get-in db [:applications :editable app-id :init-state]))
            (assoc-in [:applications :editable app-id :init-state] state-id))))

(re-frame/reg-event-db
  :update-conf
  (fn [db [_ app-id state-id conf]]
    (js/registerListener)
    (println "CONF:" (pr-str conf))
    (-> db
        (assoc-in [:applications :editable app-id :states state-id :conf] conf)
        (update-in [:applications :editable app-id :svg-ctrl] dissoc :target)
        (update-in [:applications :editable app-id :svg-ctrl] dissoc :mouse-down)
        (update-in [:applications :editable app-id :svg-ctrl] dissoc :connecting))))

(re-frame/reg-event-db
  :init-state
  (fn [db [_ selected-app state-id]]
    (assoc-in db [:applications :editable selected-app :init-state] state-id)))

;;;;;;;;;;;;;;;;;;;;;  1/2 = .50 <---width   1/1.6666 = .60 <---height

(defn compute-entry&exit [{x1 :x y1 :y h1 :h w1 :w}
                          {x2 :x y2 :y h2 :h w2 :w}]
  (let [alfa1 (/ h1 w1)
        alfa2 (/ h2 w2)
        x1 (+ x1 (/ w1 2))
        y1 (+ y1 (/ h1 1.6666))
        x2 (+ x2 (/ w2 2))
        y2 (+ y2 (/ h2 1.6666))
        m (if (not= x2 x1)
            (/ (- y2 y1) (- x2 x1)))
        w1' (/ w1 2)
        h1' (/ h1 1.6666)
        w2' (/ w2 2)
        h2' (/ h2 1.6666)

        [x1' y1'] (cond
                    (> x2 x1)
                    (if (<= m 0)
                      (if (<= (Math/abs m) alfa1)
                        [(+ x1 w1') (+ y1 (* w1' m))]
                        [(- x1 (/ h1' m)) (- y1 h1')])
                      (if (<= (Math/abs m) alfa1)
                        [(+ x1 w1') (+ y1 (* w1' m))]
                        [(+ x1 (/ h1' m)) (+ y1 h1')]))
                    (< x2 x1)
                    (if (<= m 0)
                      (if (<= (Math/abs m) alfa1)
                        [(- x1 w1') (- y1 (* w1' m))]
                        [(+ x1 (/ h1' m)) (+ y1 h1')])
                      (if (<= (Math/abs m) alfa1)
                        [(- x1 w1') (- y1 (* w1' m))]
                        [(- x1 (/ h1' m)) (- y1 h1')]))
                    (= x2 x1)
                    (if (>= y1 y2)
                      [x1 (- y1 h1')]
                      [x1 (+ y1 h1')]))
        [x2' y2'] (cond
                    (> x1 x2)
                    (if (<= m 0)
                      (if (<= (Math/abs m) alfa2)
                        [(+ x2 w2') (+ y2 (* w2' m))]
                        [(- x2 (/ h2' m)) (- y2 h2')])
                      (if (<= (Math/abs m) alfa2)
                        [(+ x2 w2') (+ y2 (* w2' m))]
                        [(+ x2 (/ h2' m)) (+ y2 h2')]))
                    (< x1 x2)
                    (if (<= m 0)
                      (if (<= (Math/abs m) alfa2)
                        [(- x2 w2') (- y2 (* w2' m))]
                        [(+ x2 (/ h2' m)) (+ y2 h2')])
                      (if (<= (Math/abs m) alfa2)
                        [(- x2 w2') (- y2 (* w2' m))]
                        [(- x2 (/ h2' m)) (- y2 h2')]))
                    (= x1 x2)
                    (if (>= y2 y1)
                      [x2 (- y2 h2')]
                      [x2 (+ y2 h2')]))

        ]
    [[x1' y1'] [x2' y2']]))

(defn compute-exit2point [{x1 :x y1 :y h1 :h w1 :w}
                          [x2 y2]]
  (let [alfa1 (/ h1 w1)
        x1 (+ x1 (/ w1 2))
        y1 (+ y1 (/ h1 2))
        m (if (not= x2 x1)
            (/ (- y2 y1) (- x2 x1)))
        w1' (/ w1 2)
        h1' (/ h1 2)
        [x1' y1'] (cond
                    (> x2 x1)
                    (if (<= m 0)
                      (if (<= (Math/abs m) alfa1)
                        [(+ x1 w1') (+ y1 (* w1' m))]
                        [(- x1 (/ h1' m)) (- y1 h1')])
                      (if (<= (Math/abs m) alfa1)
                        [(+ x1 w1') (+ y1 (* w1' m))]
                        [(+ x1 (/ h1' m)) (+ y1 h1')]))
                    (< x2 x1)
                    (if (<= m 0)
                      (if (<= (Math/abs m) alfa1)
                        [(- x1 w1') (- y1 (* w1' m))]
                        [(+ x1 (/ h1' m)) (+ y1 h1')])
                      (if (<= (Math/abs m) alfa1)
                        [(- x1 w1') (- y1 (* w1' m))]
                        [(- x1 (/ h1' m)) (- y1 h1')]))
                    (= x2 x1)
                    (if (>= y1 y2)
                      [x1 (- y1 h1')]
                      [x1 (+ y1 h1')]))]
    [x1' y1']))

(defn connect2points [[x1 y1] [x2 y2]]
  (let [deltaX (- x2 x1)
        deltaY (- y2 y1)
        byX? (> (Math/abs deltaX) (Math/abs deltaY))
        deltas (if byX?
                 [[(/ deltaX 2) 0] [0 deltaY] [(/ deltaX 2) 0]]
                 [[0 (/ deltaY 2)] [deltaX 0] [0 (/ deltaY 2)]])]
    (second
      (reduce
        (fn [[[x y] ps] [dx dy]]
          (let [x1 (+ x dx)
                y1 (+ y dy)]
            [[x1 y1] (conj ps [x1 y1])]))
        [[x1 y1] []]
        deltas))))

(defn connecting-path [[x1 y1] [x2 y2]]
  (let [[[dx1 dy1] [dx2 dy2] [dx3 dy3] :as pts] (connect2points [x1 y1] [x2 y2])
        pts (flatten (concat [x1 y1] pts))]
    (apply gstring/format "M%d,%dL%d,%dL%d,%dL%d,%d" pts)))

(defn- get-svg-node [native]                                ;native (.-nativeEvent e)
  (let []
    (reduce
      (fn [result path-elem]
        (if (= "svg" (.-nodeName path-elem))
          (reduced path-elem)))
      nil
      (.-path native))))

;(defn- compute-adjusted-old [native]                        ;native (.-nativeEvent e)
;  (let [svg-elem (get-svg-node native)
;        ;svg-elem (.getElementById js/document "svg_diagram")
;        ;svg-elem (js/SVG "svg_diagram")
;        svg-p (.createSVGPoint svg-elem)]
;    (set! (.-x svg-p) (.-clientX native))
;    (set! (.-y svg-p) (.-clientY native))
;    (let [mtx (.matrixTransform
;                svg-p
;                (.inverse (.getScreenCTM svg-elem)))
;          x1 (.-x mtx)
;          y1 (.-y mtx)]
;      [x1 y1])))

(defn- compute-adjusted [native]                            ;native (.-nativeEvent e)
  (let [svg-elem (.getElementById js/document "svg_diagram")
        punto (js/SVGWrap svg-elem (.-clientX native) (.-clientY native))
        x1 (.-x punto)
        y1 (.-y punto)]
    [x1 y1]))

(def CLAVIJA "M 0 0 l0,10 l-7,0 q 7 -20 14 0 l-4,0 l0,4 l0,-4 l-6,0 l0,4 l0,-4 l-4,0")
(def CLAVIJA-ICON "M56.1515696,29 C56.1129034,29 56.0763576,29 56.0460483,28.9966947 C56.0434913,28.9963828 56.040872,28.9961334 56.038315,28.9960086 C53.0345731,28.7569641 50.2462395,27.4052054 48.1870137,25.1898179 C46.1198675,22.9665101 44.981459,20.0639864 44.981459,17.0168385 C44.981459,12.9712914 46.9989628,9.2226634 50.378266,6.98925245 C52.3456283,5.68788458 54.6314881,5 56.9889428,5 C58.7336002,5 60.4793178,5.36651838 62.1775758,6.08938966 C64.7158886,7.17110843 66.7211688,9.12599784 67.8240293,11.5939007 C69.0042222,14.2321221 69.317481,17.0185224 68.7299415,19.6521287 C68.1796338,22.106935 66.8904271,24.2970647 65.0017692,25.9855313 C64.8862695,26.0901796 64.7385271,26.1471187 64.5846105,26.1471187 C64.4059352,26.1471187 64.2353049,26.0706594 64.1164998,25.9373233 C64.0031828,25.8123441 63.9464931,25.6527524 63.9553489,25.4863005 C63.9642671,25.3180401 64.0384813,25.1634999 64.1642713,25.0512431 C67.7658429,21.8387036 68.7517068,16.7596458 66.6757672,12.1119034 C65.6977612,9.92426825 63.9254138,8.19719774 61.6850804,7.24869034 C60.1493453,6.59597954 58.5709526,6.26500915 56.9941191,6.26500915 C54.8757714,6.26500915 52.8282701,6.87892899 51.0730107,8.04053717 C48.0451336,10.0402669 46.2373006,13.3977424 46.2373006,17.0217653 C46.2373006,22.5683727 50.560309,27.2778563 56.0789769,27.7433477 C56.0865854,27.743909 56.0941939,27.7442209 56.1017401,27.7442209 C56.1691565,27.7442209 56.2344526,27.7190254 56.2844692,27.6730002 C56.3400987,27.6217986 56.3718424,27.5497048 56.3718424,27.4741186 L56.3718424,24.2360095 C56.3718424,24.102611 56.2743661,23.9891069 56.1424644,23.9690254 C54.8207032,23.7672751 53.6067085,23.0957924 52.7240584,22.0783719 C51.832303,21.0504116 51.3399947,19.7346375 51.3377496,18.3732746 L51.3377496,14.7517462 C51.3377496,14.4053717 51.6193894,14.1236695 51.9655768,14.1236695 L53.9213393,14.1236695 C54.0705161,14.1236695 54.1914416,14.0026817 54.1914416,13.8535049 L54.1914416,10.9100075 C54.1914416,10.5637577 54.4730814,10.2821179 54.8192688,10.2821179 C55.1655809,10.2821179 55.4473455,10.5637577 55.4473455,10.9100075 L55.4473455,13.8535049 C55.4473455,14.0026817 55.5682709,14.1236695 55.7174477,14.1236695 L58.2719754,14.1236695 C58.4212146,14.1236695 58.5420777,14.0026817 58.5420777,13.8535049 L58.5420777,10.9100075 C58.5420777,10.5637577 58.8237798,10.2821179 59.1699673,10.2821179 C59.5161547,10.2821179 59.7977945,10.5637577 59.7977945,10.9100075 L59.7977945,13.8535049 C59.7977945,14.0026817 59.9186576,14.1236695 60.0678968,14.1236695 L62.0239087,14.1236695 C62.3700962,14.1236695 62.6517359,14.4053717 62.6517359,14.7517462 L62.6517359,18.3737111 C62.6517359,19.945306 61.9922897,21.4569682 60.8425933,22.5211001 C60.7264699,22.6276193 60.5751727,22.6868036 60.4173895,22.6868036 C60.2411464,22.6868036 60.0774386,22.6152087 59.9564507,22.4852403 C59.7246405,22.2326626 59.7402317,21.8348994 59.9924976,21.6000956 C60.8874336,20.769146 61.4008836,19.5930692 61.4008836,18.3737111 L61.4008836,15.6497381 C61.4008836,15.5005613 61.2799581,15.3795734 61.1307813,15.3795734 L52.8686826,15.3795734 C52.7195058,15.3795734 52.5985179,15.5005613 52.5985179,15.6497381 L52.5985179,18.3737111 C52.5985179,20.8006403 54.5728652,22.7751123 56.9996072,22.7751123 C57.3459194,22.7751123 57.6276839,23.056752 57.6276839,23.4029395 C57.6276839,23.4400466 57.6241291,23.4777773 57.6178926,23.5092716 C57.610783,23.544383 57.610783,23.5804923 57.6178302,23.6155413 C57.6242538,23.6477216 57.6276839,23.6846416 57.6276839,23.72231 L57.6276839,27.5090429 C57.6276839,27.9222102 57.4516902,28.3229669 57.1449174,28.608598 C56.879243,28.8575586 56.517402,29 56.1515696,29")
(def ARROW "0,0 5,-15 -5,-15")

(defn position&rotate [x1 y1 x2 y2]
  (let [m (/ (- y2 y1) (- x2 x1))
        transform-fmt "translate(%d %d) rotate(%d 0 0)"
        rot-init (if (> x1 x2) 270 90)
        rotation (- (* (.atan js/Math m) (/ 180 3.1416)) rot-init)
        transform (gstring/format transform-fmt x2 y2 rotation)]
    transform))

(defn svg4state [states app hovered? ommit-arrow2 state-id {:keys [diagram operation flow]}]
  (let [img-path (str "images/icons/operations/" (clojure.string/lower-case (name operation)) "-opr.png")
        [x y] (:corner diagram)
        w (max 130 (* 15 (count (name state-id))))
        h 60
        rx 5
        ry 5
        hover-color "#F66C4A"
        shadow-stroke "#97d0e5"
        shadow-color  (if hovered? hover-color shadow-stroke)
        shadow-blur   (if hovered? 6 2)
        shadow-offset (if hovered? 2 0)

        stroke "#4ea9c3"
        stroke-width (if hovered? 3 3)

        menu-x (+ x 14)
        menu-y (+ y 9)

        close-x (+ x (- w 9))
        close-y (+ y 9)

        clav-x (+ x (- (/ w 2)))
        clav-y (+ y  9)

        menu-cmd (gstring/format "M%d,%dl25,0 m0,4 l-25,0 m0,4 l25,0 " menu-x menu-y)
        [x-center y-center] [(+ x (/ w 2)) (+ y (/ h 2))]
        down-fn (fn [e]
                  (.stopPropagation e) (.preventDefault e)
                  (re-frame/dispatch [:target app :connector state-id [x-center y-center]])) ;(compute-adjusted e)
        clavija [:path {:d            CLAVIJA
                        :stroke (if hovered? hover-color "transparent")
                        :stroke-width (if hovered? hover-color "transparent")
                        :fill         (if hovered? hover-color "transparent")
                        :onMouseDown  down-fn
                                        ;:transform    (gstring/format "translate(%d %d) rotate(180)" x-center y-center)
                        }]
        clavija-icon[:g {:transform (str "translate(" x-center "," y-center ")")}
                     [:g {:transform "translate(-55.000000, -27.500000)"
                          :style {:cursor "cell"}
                          :onMouseDown  down-fn}
                      [:rect {:x 44 :y 5 :width 25 :height 25 :style {:fill "transparent" :stroke "transparent" :stroke-width 1}}]
                      [:path {:d            CLAVIJA-ICON
                              :stroke (if hovered? hover-color "transparent")
                              :stroke-width (if hovered? hover-color "transparent")
                              :fill         (if hovered? hover-color "transparent")
                              }]]]
        state-with-out-connections ^{:key (str state-id)} [:g {:onMouseOver (fn [e]
                                                                              (let [native (.-nativeEvent e)]
                                                                                (.stopPropagation e) (.preventDefault e)
                                                                                (re-frame/dispatch [:hovered app :state state-id])))
                                                               :onMouseDown (fn [e]
                                                                              (let [native (.-nativeEvent e)]
                                                                                (.stopPropagation e) (.preventDefault e)
                                                                                (let [[adjusted-x adjusted-y] (compute-adjusted native)
                                                                                      delta-x (- x adjusted-x)
                                                                                      delta-y (- y adjusted-y)]
                                                                                  (re-frame/dispatch [:target app :state state-id [delta-x delta-y]]))))
                                                               :onMouseOut  (fn [e]
                                                                              (let [native (.-nativeEvent e)]
                                                                                (.stopPropagation e) (.preventDefault e)
                                                                                (re-frame/dispatch [:mouseout app :state state-id])))}
                                                           ;; Shadow
                                                           [:defs
                                                            [:filter {:id "shadow"}
                                                             [:feGaussianBlur {:in "SourceGraphic" :stdDeviation shadow-blur}]]]
                                                           [:rect {:x (- x shadow-offset) :y (- y shadow-offset)
                                                                   :width (+ w (* 2 shadow-offset)) :height (+ h (* 2 shadow-offset)) :rx rx :ry ry
                                                                   :shape-rendering "optimizeQuality"
                                                                   :filter "url(#shadow)"
                                                                   :fill shadow-color}]

                                                           ;; Main Rect
                                                           [:rect {:x x :y y :width w :height h :rx rx :ry ry
                                                                   :shape-rendering "optimizeQuality"
                                                                   :style {:fill "white" :stroke stroke
                                                                           :stroke-width stroke-width
                                                                           :cursor "move"}}]
                                                           ;; Icon
                                                           [:image {:href img-path :x (+ x 4) :y (+ y h -57) :height 32 :width 32
                                                                    :style {:cursor "pointer"}
                                                                    :onMouseDown (fn [e]
                                                                               (let [native (.-nativeEvent e)]
                                                                                 (.stopPropagation e) (.preventDefault e)
                                                                                 (re-frame/dispatch [:target app :menu state-id])))}]

                                                           ;; Config
                                                           #_[:g {:transform (str "translate(" menu-x "," menu-y ")")
                                                                :fill "#49D0A4"

                                                                }

                                                              [:g {:transform "translate(-14.200000, -9.000000)"}
                                                             [:rect {:width 18 :height 18 :style {:fill "transparent" :stroke "transparent" :stroke-width 1}}]
                                                             [:path {:d "M11.5000127,12.7147313 C10.2787974,12.7147313 9.2853111,11.7212177 9.2853111,10.5000254 C9.2853111,9.27880773 10.2788482,8.28529408 11.5000127,8.28529408 C12.7212026,8.28529408 13.7147143,9.27880773 13.7147143,10.5000254 C13.7147143,11.7212177 12.721228,12.7147313 11.5000127,12.7147313 L11.5000127,12.7147313 Z M11.5000127,7.67825265 C9.94407216,7.67825265 8.67824547,8.94408181 8.67824547,10.5000254 C8.67824547,12.055969 9.94407216,13.3217727 11.5000127,13.3217727 C13.0559532,13.3217727 14.3217799,12.055969 14.3217799,10.5000254 C14.3217799,8.94408181 13.0559532,7.67825265 11.5000127,7.67825265 L11.5000127,7.67825265 Z M17.3929598,11.0811429 C17.3929598,11.1179594 17.366198,11.1497232 17.3306764,11.1550553 C17.0899221,11.1910847 16.8427692,11.2256161 16.5960988,11.2578369 C16.3229459,11.2934347 16.0994572,11.4869628 16.0266876,11.7508994 C15.9243885,12.122061 15.7759292,12.4803241 15.5854992,12.8155833 C15.4500907,13.0539768 15.4711396,13.3490932 15.6391497,13.5674534 C15.790859,13.7646885 15.9412733,13.9639803 16.0861017,14.1597936 C16.1074807,14.1886628 16.103926,14.2300751 16.0779259,14.256126 L15.2560806,15.0779222 C15.2300551,15.1039731 15.1886176,15.1074516 15.1597737,15.0861488 C14.9643417,14.9415486 14.7650249,14.7911593 14.5674093,14.6391451 C14.3491003,14.47116 14.0540099,14.4500857 13.8156168,14.5854945 C13.480333,14.7759249 13.1220959,14.9243845 12.7508843,15.0266837 C12.4869482,15.0994535 12.2934205,15.3229427 12.257772,15.5961215 C12.225602,15.8429701 12.1910199,16.090098 12.1550159,16.3307005 C12.1497093,16.3662221 12.1179202,16.392984 12.0811037,16.392984 L10.9189217,16.392984 C10.8821052,16.392984 10.8503161,16.3662221 10.8450095,16.3306751 C10.8090563,16.0903519 10.7744742,15.843224 10.7422534,15.5961215 C10.7066303,15.3229427 10.5130772,15.0994535 10.2491411,15.0266837 C9.87790406,14.9243845 9.51969242,14.7759249 9.18440856,14.5854691 C9.07987516,14.5261056 8.96442376,14.4968301 8.84935322,14.4968301 C8.70198575,14.4968301 8.55520226,14.5448185 8.43264147,14.6391705 C8.23500046,14.7911847 8.03570905,14.941574 7.84027703,15.0861742 C7.81138244,15.1075024 7.77002104,15.1039985 7.74399559,15.0779476 L6.92217563,14.2561514 C6.8961248,14.2301259 6.8925701,14.1887136 6.91394905,14.1598444 C7.05870129,13.9641581 7.20906486,13.7648663 7.36092649,13.5674788 C7.5288858,13.3491439 7.54996006,13.0540783 7.41457698,12.8156848 C7.22409614,12.4802734 7.07566226,12.1220864 6.97336323,11.7509502 C6.90061895,11.4870136 6.67713023,11.2934601 6.40400274,11.2578623 C6.15740851,11.2256923 5.9102049,11.1910847 5.66932357,11.1550807 C5.63382741,11.1497486 5.60704025,11.1179848 5.60704025,11.0811683 L5.60704025,9.91893328 C5.60704025,9.88211673 5.63382741,9.85037832 5.66932357,9.84504627 C5.9102049,9.80901683 6.15738312,9.77443467 6.40392657,9.74226462 C6.67710484,9.70666682 6.90059356,9.51308794 6.97333784,9.24917675 C7.07561148,8.87806593 7.22407075,8.5198282 7.41455159,8.18444213 C7.54993467,7.94604862 7.52886041,7.65095762 7.3608757,7.43264817 C7.20942033,7.23571772 7.05903137,7.03645131 6.91392366,6.84028258 C6.8925701,6.81141333 6.8961248,6.76997566 6.92215024,6.74397556 L7.74389403,5.9221286 C7.76991947,5.89610311 7.81130627,5.89259919 7.84017547,5.91390201 C8.03563288,6.05847679 8.23492429,6.20886605 8.43253991,6.36090571 C8.65089971,6.52886535 8.94596474,6.54993965 9.18438317,6.41455631 C9.51961625,6.22410048 9.87785328,6.07566631 10.2490649,5.97334169 C10.513001,5.90057188 10.7065541,5.67710811 10.7422026,5.40392931 C10.7744234,5.15685218 10.8090055,4.90969887 10.8449587,4.66929949 C10.8502907,4.63380326 10.8820545,4.60704143 10.9188709,4.60704143 L12.0810783,4.60704143 C12.1178948,4.60704143 12.1496839,4.63380326 12.1549905,4.66932488 C12.1909691,4.90967348 12.2255258,5.15680139 12.257772,5.40392931 C12.2933951,5.67710811 12.4869482,5.90057188 12.7508843,5.97334169 C13.1220959,6.07566631 13.4803076,6.22410048 13.8156168,6.41455631 C14.0539591,6.54988887 14.3490495,6.52886535 14.5673839,6.36088032 C14.7649995,6.20886605 14.9643163,6.05847679 15.159723,5.91390201 C15.1885922,5.89254841 15.2300044,5.89605233 15.2560552,5.9221286 L16.0778498,6.74392478 C16.1039006,6.76997566 16.1074299,6.81141333 16.0860763,6.8402318 C15.9412479,7.03609584 15.7908336,7.23536225 15.6390989,7.43259739 C15.4711396,7.65098301 15.4500653,7.94607401 15.5854738,8.18446752 C15.7759292,8.51975203 15.9243631,8.87796437 16.0266876,9.24917675 C16.0994318,9.51308794 16.3229459,9.70661604 16.5960226,9.74223923 C16.8427438,9.77443467 17.0898967,9.80901683 17.3306764,9.84499549 C17.366198,9.85035293 17.3929598,9.88209134 17.3929598,9.91890789 L17.3929598,11.0811429 Z M17.4204832,9.24460642 C17.1759964,9.207993 16.9250096,9.17287763 16.6745306,9.14022516 C16.6441634,9.1362642 16.6195598,9.11569772 16.6118918,9.08779331 C16.4962627,8.66826238 16.3285065,8.26345806 16.1133205,7.88455228 C16.0988225,7.85911078 16.1015647,7.8270169 16.1202268,7.80271798 C16.274272,7.60251212 16.4269715,7.40012266 16.5741105,7.20123711 C16.7727925,6.93262864 16.7439487,6.55153926 16.5071045,6.31464381 L15.6853099,5.49284763 C15.4484403,5.25600295 15.0673517,5.22718448 14.7987184,5.4258669 C14.6002395,5.57267607 14.3978758,5.72540127 14.1972387,5.87972609 C14.173016,5.89843906 14.140846,5.90113047 14.1154045,5.88668315 C13.7365503,5.67147137 13.3317214,5.50371486 12.9121658,5.3880855 C12.8842869,5.38044289 12.8636697,5.35581389 12.8597087,5.3253958 C12.8270055,5.07453545 12.7919156,4.82357353 12.7554038,4.57951789 C12.7059428,4.24913379 12.4160828,4 12.0811037,4 L10.9189217,4 C10.5839172,4 10.2940318,4.24913379 10.2446977,4.57951789 C10.2081352,4.82359892 10.1730453,5.07456084 10.1403421,5.32547197 C10.1363557,5.35583928 10.1157893,5.38044289 10.0878595,5.38811089 C9.66830404,5.50379103 9.26344973,5.67152215 8.88467165,5.88670854 C8.8592048,5.90115586 8.82706021,5.89836288 8.80278672,5.87975148 C8.602175,5.72542666 8.39978594,5.57272685 8.20128164,5.4258669 C7.93264831,5.22718448 7.55153428,5.25607913 7.31471546,5.49289841 L6.49289549,6.31469459 C6.25602589,6.55159005 6.22720748,6.93273021 6.42591489,7.2012879 C6.57325698,7.40050352 6.72598189,7.6028422 6.8797732,7.80274337 C6.89843535,7.8270169 6.90117754,7.85911078 6.88673025,7.88457767 C6.67146811,8.26353423 6.50373731,8.66838933 6.38810818,9.0878187 C6.38046559,9.11569772 6.35586202,9.1362642 6.32546939,9.14025055 C6.07511733,9.17292841 5.82415591,9.20801839 5.57954215,9.24463181 C5.2491333,9.29401677 5,9.58390267 5,9.9188825 L5,11.0811175 C5,11.4160973 5.2491333,11.7059832 5.57951676,11.7553936 C5.82413052,11.792007 6.07511733,11.827097 6.32546939,11.8597495 C6.35586202,11.8637358 6.3804402,11.8843023 6.38810818,11.9122067 C6.50373731,12.3316868 6.6714935,12.7365165 6.88673025,13.1154477 C6.90117754,13.1409146 6.89843535,13.1730339 6.8797732,13.197282 C6.72560103,13.3976656 6.57290151,13.6000297 6.42591489,13.7987375 C6.22720748,14.067346 6.25602589,14.4484861 6.49289549,14.6853562 L7.31469007,15.5071524 C7.55158506,15.7440224 7.93272448,15.7728663 8.20128164,15.5741331 C8.39976055,15.4273239 8.60212422,15.2746241 8.80276133,15.1202739 C8.82700943,15.1015863 8.85915402,15.0988949 8.88459548,15.1133168 C9.26342434,15.3285286 9.66822787,15.4962598 10.0878342,15.6119145 C10.1157131,15.6196079 10.1363303,15.6441861 10.1402659,15.6745788 C10.1729945,15.9254392 10.2081098,16.1764265 10.2446216,16.4204567 C10.2940318,16.7508662 10.5839172,17 10.9189217,17 L12.0811037,17 C12.4160828,17 12.7059936,16.7508662 12.7553784,16.4204313 C12.7919156,16.1760964 12.8270055,15.9251345 12.8596833,15.6745534 C12.8636697,15.6441607 12.8842361,15.6195317 12.9121405,15.6118637 C13.331696,15.4961836 13.7365503,15.3284525 14.1153537,15.1132661 C14.1408206,15.0988695 14.1729144,15.1016117 14.1972133,15.1202231 C14.3977996,15.2745479 14.6001887,15.4272478 14.7986676,15.5740823 C15.0672755,15.7727647 15.4483642,15.7439717 15.6852845,15.5070762 L16.5070791,14.68528 C16.7439233,14.44841 16.7727417,14.0673206 16.5740851,13.7986867 C16.4269461,13.599852 16.2742466,13.3974625 16.1202014,13.1972058 C16.1015393,13.1729577 16.0987971,13.1408638 16.1132698,13.1153969 C16.3285065,12.7364658 16.4962373,12.3316107 16.611841,11.9121813 C16.6195598,11.8842515 16.644138,11.863685 16.6745306,11.8596987 C16.9249588,11.8270462 17.1759457,11.7919308 17.4204325,11.7553428 C17.7508667,11.7059578 18,11.4160719 18,11.0810921 L18,9.9188571 C18,9.58387728 17.7508667,9.29399138 17.4204832,9.24460642 L17.4204832,9.24460642 Z"}]]]

                                                           ;; Close
                                                           [:g {:transform (str "translate(" close-x "," close-y ")")
                                                                :fill "#F66C4A"
                                                                :style {:cursor "pointer"}
                                                                :onMouseUp   (fn [e]
                                                                               (let [native (.-nativeEvent e)]
                                                                                 (.stopPropagation e) (.preventDefault e)
                                                                                 (re-frame/dispatch [:delete-state app state-id])))
                                                                :onMouseDown (fn [e]
                                                                                (let [native (.-nativeEvent e)]
                                                                                  (.stopPropagation e) (.preventDefault e)))}
                                                            [:g {:transform "translate(-105.000000, -9.000000)"}
                                                             [:path {:d "M105.293103,12.2604711 L104.258147,13.2954271 C104.258147,13.2954271 102.621742,11.5364087 102.499128,11.5364087 C102.378839,11.5364087 100.739529,13.2954271 100.739529,13.2954271 L99.7045729,12.2604711 C99.7045729,12.2604711 101.463591,10.6467301 101.463591,10.5031961 C101.463591,10.3561754 99.7045729,8.74185329 99.7045729,8.74185329 L100.739529,7.70689732 C100.739529,7.70689732 102.392204,9.46359126 102.499128,9.46359126 C102.605471,9.46359126 104.258147,7.70689732 104.258147,7.70689732 L105.293103,8.74185329 C105.293103,8.74185329 103.534084,10.380582 103.534084,10.5031961 C103.534084,10.619418 105.293103,12.2604711 105.293103,12.2604711 M102.499128,4 C98.9101962,4 96,6.91019624 96,10.5014528 C96,14.0898038 98.9101962,17 102.499128,17 C106.089804,17 109,14.0898038 109,10.5014528 C109,6.91019624 106.089804,4 102.499128,4"}]]]

                                                           ;; State Name
                                                           [:text {:x (+ x 36) :y (+ y h -8) :fill stroke :shape-rendering "optimizeQuality"

                                                                   :style {:font-family "'Helvetica Neue', Helvetica, Arial, sans-serif" :font-size "20px" :font-weight 500 :cursor "move"}}
                                                            (str state-id)]

                                                           clavija-icon
                                                           ]]
    (reduce
      (fn [result [idx [other-state re]]]
        (let [[x1 y1] (:corner diagram)
              [x2 y2] (get-in states [other-state :diagram :corner])
              w-other (max 130 (* 15 (count (name other-state))))
              h 50
              [[x1 y1] [x2 y2] :as e&e-points] (compute-entry&exit {:x x1 :y y1 :h h :w w}
                                                                   {:x x2 :y y2 :h h :w w-other})
              idx (inc idx)
              re-txt (if re (str idx ") " re) (str idx ") default"))
              re-width (count re-txt)
              re-w (* re-width 9)
              x-t (- (/ (+ x1 x2) 2) 25)
              y-t (- (/ (+ y1 y2) 2) 15)
              path-points (flatten e&e-points)
              path (apply gstring/format "M%d,%dL%d,%d" path-points)
              arrow {:points      ARROW
                     :transform   (position&rotate x1 y1 x2 y2)
                     :style       {:stroke      stroke
                                   :fill        stroke
                                   :stroke-with 1}
                     :onMouseDown (fn [e]
                                    (let [native (.-nativeEvent e)]
                                      (.stopPropagation e) (.preventDefault e)
                                      (re-frame/dispatch [:target app :arrow [state-id other-state] [x-center y-center]])))}
              ]
          (if (= ommit-arrow2 other-state)
            result
            (conj
              result
              [:g
               [:defs
                [:filter {:id "arrow-shadow"}
                 [:feGaussianBlur {:in "SourceGraphic" :stdDeviation shadow-blur}]
                 ]]
               (let [stroke (if hovered? hover-color shadow-stroke)
                     stroke-width (if hovered? 2 1)]
                 [:path {:d path :stroke stroke :stroke-width stroke-width :fill "transparent" :filter "url(#arrow-shadow)"}])
               [:path {:d path :stroke stroke :stroke-width 2 :fill "transparent"}]
               [:polygon arrow]
               [:rect {:x x-t :y y-t :width re-w :height "2em" :rx 3 :ry 3 :style {:fill         (if hovered? "#FFFFFF" "transparent")
                                                                                   :stroke       (if hovered? "#000000" "transparent")
                                                                                   :stroke-width "1"}}]
               [:text {:x           (+ x-t 3) :y (+ y-t 20) :fill (if hovered? "#000000" "transparent")
                       :style       {:font-family "'Helvetica Neue', Helvetica, Arial, sans-serif" :font-size "16px" :font-weight 500}
                       :onMouseDown (fn [e]
                                      (let [native (.-nativeEvent e)]
                                        (.stopPropagation e) (.preventDefault e)
                                        (if re
                                          (re-frame/dispatch [:target app :flow-re [state-id other-state] [x-center y-center]]))))} re-txt]]))))
      state-with-out-connections
      (map-indexed (fn [idx flow-pair]
                     [idx flow-pair]) flow))))

(let [last-current-atm (atom [nil nil])]
  (defn swap-current [current]
    (let [[new old] (swap! last-current-atm (fn [[new old]]
                                              [current new]))]
      old)))

(defn show-robotito [robot-img-path animate-it x0 y0 x1 y1]
  (let []
    (when animate-it (re-frame/dispatch [:move-robot "robotito" x0 y0 x1 y1 500]))
    [:image {:id "robotito" :href robot-img-path  :x x0 :y y0
             :height "57" :width "57"}]))

(defn svgOnMouseDown [selected-app e]
  (let [native (.-nativeEvent e)]
    (.stopPropagation e) (.preventDefault e)
    (re-frame/dispatch [:target selected-app :svg :svg (compute-adjusted native)])))
(defn svgOnWheel [selected-app e]
  (let [native (.-nativeEvent e)]
    (re-frame/dispatch [:wheel selected-app (.-wheelDelta native) (.-wheelDeltaX native) (.-wheelDeltaY native)])))
(defn svgOnMouseMove[selected-app e]
  (let [native (.-nativeEvent e)]
    (.preventDefault e) (.stopPropagation e)
    (re-frame/dispatch [:mouse-move selected-app (compute-adjusted native)])))
(defn svgOnMouseUp[selected-app e]
  (let [native (.-nativeEvent e)]
    (.stopPropagation e) (.preventDefault e)
    (re-frame/dispatch [:mouseup selected-app])))
(defn svgOnMouseOut [selected-app e]
  (let [native (.-nativeEvent e)]
    (.stopPropagation e) (.preventDefault e)
    (re-frame/dispatch [:mouseout-svg selected-app])))
(defn svgOnMouseLeave [selected-app e]
  (let [native (.-nativeEvent e)]
    (.stopPropagation e) (.preventDefault e)
    #_(.log js/console (pr-str [:onMouseLeave]))
    (re-frame/dispatch [:mouseleave-svg selected-app])
    ))

(defn svg-comp [editables ready width height selected-app watch-instance]
  (let [states (get-in editables [selected-app :states])
        svg-ctrl (get-in editables [selected-app :svg-ctrl])
        instances (get-in editables [selected-app :instances])
        {:keys [x y w h] :or {x 0 y 0 w 1342 h 600}} (get-in svg-ctrl [:zoom] {:x 0 :y 0 :w 1342 :h 600})
        {:keys [hovered plain]} (group-by (fn [[state-id _]]
                                            (if (= (:hovered svg-ctrl) [:state state-id])
                                              :hovered
                                              :plain)) states)
        {connecting-state-id :state-id connecting-old-state :old-state} (:connecting svg-ctrl)
        connector-color "#F66C4A"
        shadow-blur     6
        ;svgOnMouseDown (fn [e]
        ;                 (let [native (.-nativeEvent e)]
        ;                   (.stopPropagation e) (.preventDefault e)
        ;                   (re-frame/dispatch [:target selected-app :svg :svg (compute-adjusted native)])))
        ;svgOnWheel (fn [e]
        ;             (let [native (.-nativeEvent e)]
        ;               (re-frame/dispatch [:wheel selected-app (.-wheelDelta native) (.-wheelDeltaX native) (.-wheelDeltaY native)])))
        ;svgOnMouseMove (fn [e]
        ;                 (let [native (.-nativeEvent e)]
        ;                   (.preventDefault e) (.stopPropagation e)
        ;                   (re-frame/dispatch [:mouse-move selected-app (compute-adjusted native)])))
        ;svgOnMouseUp (fn [e]
        ;               (let [native (.-nativeEvent e)]
        ;                 (.stopPropagation e) (.preventDefault e)
        ;                 (re-frame/dispatch [:mouseup selected-app])))
        ;svgOnMouseOut (fn [e]
        ;                (let [native (.-nativeEvent e)]
        ;                  (.stopPropagation e) (.preventDefault e)
        ;                  (re-frame/dispatch [:mouseout-svg selected-app])))
        ]
    ^{:key (str "d-svg")}
    [:svg {:id          (str "svg_diagram") :width width :height height :viewBox (str x " " y " " w " " h)
           :onMouseDown (partial svgOnMouseDown selected-app)
           :onWheel     (partial svgOnWheel selected-app)
           :onMouseMove (partial svgOnMouseMove selected-app)
           :onMouseUp   (partial svgOnMouseUp selected-app)
           :onMouseOut  (partial svgOnMouseOut selected-app)
           :onMouseLeave (partial svgOnMouseLeave selected-app)
           ;:onMouseDown svgOnMouseDown
           ;:onWheel     svgOnWheel
           ;:onMouseMove svgOnMouseMove
           ;:onMouseUp   svgOnMouseUp
           ;:onMouseOut  svgOnMouseOut
           }
     (when-let [{:keys [state-id mouse coords]} (:connecting svg-ctrl)]
       (let [[x-m y-m] mouse
             [x-c y-c] coords
             [x y] (get-in states [state-id :diagram :corner])
             w 130
             h 50
             [x1 y1] (compute-exit2point {:x x :y y :h h :w w} coords)
             path (apply gstring/format "M%d,%dL%d,%d" (flatten (concat [x-c y-c] [(- x-m 0) (- y-m 0)])))]
         [:g
          [:defs
           [:filter {:id "connector-shadow"}
            [:feGaussianBlur {:in "SourceGraphic" :stdDeviation shadow-blur}]
            ]]
          [:path {:d path :stroke connector-color :stroke-width 2 :fill "transparent" :filter "url(#connector-shadow)"}]
          [:path {:d CLAVIJA :stroke connector-color :stroke-width 1 :fill connector-color :transform (position&rotate x-c y-c (+ x-m 0) (+ y-m 0))}]
          [:path {:d path :stroke connector-color :stroke-width 2 :fill "trasparent"}]]))
     (doall
       (for [[state-id state-data] plain]
         (svg4state states selected-app false (and (= connecting-state-id state-id) connecting-old-state) state-id state-data)))
     (doall
       (for [[state-id state-data] hovered]
         (svg4state states selected-app true (and (= connecting-state-id state-id) connecting-old-state) state-id state-data)))
     ;(let [robot-img-path "images/robot-start.gif"]
     ;[:image {:id "robotito" :href robot-img-path :x 0 :y 0 :height 25 :width 25 }])
     (if watch-instance
       (let [{current  :robot/current
              status   :robot/status
              mood     :robot/mood
              previous :robot/previous} (get-in ready [selected-app watch-instance])
             last-current (swap-current current)
             robot-img-path (if (= :running status) "images/robot-start.gif" "images/robot-waiting.gif")]
         (when (and current)
           (let [[x-p y-p] (get-in states [previous :diagram :corner])
                 x-p (+ 30 x-p)
                 [x y] (get-in states [current :diagram :corner])
                 x (+ 30 x)]
             ;(if (not= current last-current) (re-frame/dispatch [:move-robot "robotito" x-p y-p x y 500]))
             (show-robotito robot-img-path (not= current last-current) x-p y-p x y)
             ;[:image {:id "robotito" :href robot-img-path  :x x-p-str :y y-p-str
              ;        :height "25" :width "25"}]
             )))
       (show-robotito "images/robot-stop.gif" false 0 0 0 0)
       ;[:image {:id "robotito" :href "images/robot-waiting.gif" :x 0 :y 0 :height "25" :width "25"}]
       )]))

(comment
  [:g
   [:path {:style {:fill-rule "evenodd" :clip-rule "evenodd" :transform "scale(0.1)"}
           :d     "M208.632,133.579c10.345,0.472,19.121-7.677,19.574-18.203   c0.453-10.526-6.821-19.989-17.174-20.444l-68.73-8.63c0,0,14.323-23.958,14.323-59.455C156.625,3.184,139.72,0,129.778,0   c-7.821-0.003-9.927,15.151-9.927,15.151h-0.016c-1.771,9.717-4.077,18.203-12.09,33.827C98.775,66.49,86.559,64.847,72.297,80.445   c-2.517,2.747-5.899,7.281-9.195,12.86c-0.269,0.295-0.52,0.708-0.763,1.289c-0.294,0.692-0.646,1.172-0.956,1.812   c-0.546,1.003-1.083,2.006-1.611,3.059c-8.827,8.827-22.579,7.925-28.435,7.925c-11.746,0-17.898,6.825-17.898,17.898   l-0.004,81.828c0,12.423,5.083,16.613,17.903,16.613h17.898c9.011,0,16.067,5.166,26.848,8.949   c14.767,5.116,36.821,8.956,74.811,8.956c6.644,0,27.251,0.025,27.251,0.025c6.309,0,11.377-2.882,15.034-6.362   c1.392-1.323,2.844-3.245,3.465-6.995c0.101-0.581,0.209-3.017,0.193-3.346c0.477-10.728-6.008-14.612-9.682-15.835   c0.1-0.034,0.034-0.126,0.234-0.118l11.663,0.522c10.353,0.472,20.572-6.986,20.572-19.669c0-10.517-8.525-17.934-18.844-18.439   l6.184,0.287c10.352,0.455,19.103-7.694,19.582-18.22C226.998,142.959,218.977,134.052,208.632,133.579z"
           :fill  "#91DC5A"}]
   [:path {:style {:fill-rule "evenodd" :clip-rule "evenodd" :transform "scale(0.1)"}
           :d     "M208.632,133.579c10.345,0.472,19.121-7.677,19.574-18.203   c0.453-10.526-6.821-19.989-17.174-20.444l-68.73-8.63c0,0,14.323-23.958,14.323-59.455C156.625,3.184,139.72,0,129.778,0   c-7.821-0.003-9.927,15.151-9.927,15.151h-0.016c-1.771,9.717-4.077,18.203-12.09,33.827C98.775,66.49,86.559,64.847,72.297,80.445   c-2.517,2.747-5.899,7.281-9.195,12.86c-0.269,0.295-0.52,0.708-0.763,1.289c-0.294,0.692-0.646,1.172-0.956,1.812   c-0.546,1.003-1.083,2.006-1.611,3.059c-8.827,8.827-22.579,7.925-28.435,7.925c-11.746,0-17.898,6.825-17.898,17.898   l-0.004,81.828c0,12.423,5.083,16.613,17.903,16.613h17.898c9.011,0,16.067,5.166,26.848,8.949   c14.767,5.116,36.821,8.956,74.811,8.956c6.644,0,27.251,0.025,27.251,0.025c6.309,0,11.377-2.882,15.034-6.362   c1.392-1.323,2.844-3.245,3.465-6.995c0.101-0.581,0.209-3.017,0.193-3.346c0.477-10.728-6.008-14.612-9.682-15.835   c0.1-0.034,0.034-0.126,0.234-0.118l11.663,0.522c10.353,0.472,20.572-6.986,20.572-19.669c0-10.517-8.525-17.934-18.844-18.439   l6.184,0.287c10.352,0.455,19.103-7.694,19.582-18.22C226.998,142.959,218.977,134.052,208.632,133.579z"
           :fill  "red"}]
   ]

  )

(defn select-flow-re [flow other-state]
  (->> flow
       (filter (fn [[other re]]
                 (= other-state other)))
       first
       second))

(defn dialog-flow-re [[app orig-state other-state re]]
  (let [new-re-txt (reagent/atom re)]
    [re-com/border
     :border "5px solid #559"                               ;
     :radius "25px"
     ;:width "15em"
     ;:height "10em"
     :child [re-com/v-box
             :padding "10px"
             :style {:background-color "#d4e3f7" :border-radius "19px"}
             :children [[re-com/title :label "Transition" :level :level2]
                        [re-com/title :label (str "Rule for transition from " orig-state " to " other-state) :level :level3]
                        [re-com/v-box
                         :class "form-group"
                         :children [[re-com/h-box
                                     :children [[re-com/title :label (str "Regex:") :level :level3]
                                                [re-com/info-button
                                                 :info (str "Regular expression that result must match in order to follow the path")]]]
                                    [re-com/input-text
                                     :model new-re-txt
                                     :width "100%"
                                     :placeholder "Enter regex"
                                     :class "form-control"
                                     :style {:border-radius "5px"}
                                     :on-change (fn [re-txt]
                                                  (reset! new-re-txt re-txt))]
                                    ]]

                        [re-com/line :color "#ddd" :style {:margin "10px 0 10px"}]
                        [re-com/h-box
                         :gap "30px"
                         :justify :center
                         :children [[re-com/button
                                     :label "Cancel"
                                     :on-click (fn []
                                                 (re-frame/dispatch [:cancel-update app]))]
                                    [re-com/button
                                     :label "Ok"
                                     :class "btn-primary"
                                     :on-click (fn []
                                                 (re-frame/dispatch [:update-re app orig-state other-state @new-re-txt]))]]]]]]))

(defn svg
  [selected-app watch-instance]
  (let [editables (re-frame/subscribe [[:applications :editable]])
        ready (re-frame/subscribe [[:applications :ready]])
        operations (re-frame/subscribe [[:operations]])
        opr-choices (reduce (fn [result [opr {:keys [image fields]}]]
                              (conj result {:id    (name opr)
                                            :label (name opr)
                                            :image image}))
                            []
                            (sort @operations))]
    (fn [selected-app watch-instance]
      (if (seq selected-app)
        (let [[type id coords] (get-in @editables [selected-app :svg-ctrl :target])]
          [re-com/h-box
           :width "100%"
           :height "100%"
           :children [[svg-comp @editables @ready "100%" "100%" selected-app watch-instance]
                      (cond
                        (= type :flow-re)
                        (let [[state-id other-state] id]
                          [re-com/modal-panel
                           :backdrop-color "grey"
                           :backdrop-opacity 0.4
                           :wrap-nicely? false
                           :backdrop-on-click (fn []
                                                (re-frame/dispatch [:cancel-update selected-app]))
                           :style {:font-family "'Helvetica Neue', Helvetica, Arial, sans-serif" :font-size "15px"}
                           :child [dialog-flow-re
                                   [selected-app state-id other-state
                                    (select-flow-re
                                      (get-in @editables [selected-app :states state-id :flow])
                                      other-state)]]])

                        (= type :menu)
                        (do
                          [re-com/modal-panel
                           :backdrop-color "grey"
                           :backdrop-opacity 0.4
                           :wrap-nicely? false
                           :style {:font-family "'Helvetica Neue', Helvetica, Arial, sans-serif" :font-size "15px"}
                           :child [state-editor/show-dialog selected-app id
                                   (get-in @editables [selected-app :init-state])
                                   (get-in @editables [selected-app :states id])
                                   opr-choices]]))]])
        [re-com/title
         :label "Please select an app."
         ]))))
