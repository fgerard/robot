(ns robot2.interop
  "Unica frontera hacia JS global: navegador y resources/public/js/lib/externs.js.
   Nada fuera de este namespace debe llamar js/* directamente.")

;; --- Transformacion de coordenadas SVG -------------------------------------
;; Reemplaza el SVGWrap de externs.js (que hacia exactamente esto mismo desde
;; JS a mano) por la misma Web API estandar invocada directo desde cljs:
;; createSVGPoint + getScreenCTM + matrixTransform. Un solo lugar de verdad,
;; sin pasar por una funcion global adicional.
(defn client-point->svg-point
  "Convierte coordenadas de cliente (event.clientX/Y) a coordenadas internas
   del SVG identificado por svg-el, respetando el viewBox/zoom actual."
  [svg-el client-x client-y]
  (let [pt (.createSVGPoint svg-el)]
    (set! (.-x pt) client-x)
    (set! (.-y pt) client-y)
    (let [m (.. svg-el (getScreenCTM) (inverse))
          transformed (.matrixTransform pt m)]
      [(.-x transformed) (.-y transformed)])))

;; --- Pointer capture --------------------------------------------------------
;; setPointerCapture asegura que pointermove/pointerup sigan llegando al mismo
;; elemento aunque el cursor salga del SVG durante un arrastre, eliminando la
;; necesidad de los handlers mouseout/mouseleave de respaldo de la version 1.
(defn capture-pointer! [dom-el pointer-id]
  (.setPointerCapture dom-el pointer-id))

(defn release-pointer! [dom-el pointer-id]
  (when (.hasPointerCapture dom-el pointer-id)
    (.releasePointerCapture dom-el pointer-id)))

;; --- Animacion del robotito --------------------------------------------------
(defn animate-translate!
  "Anima el elemento dom-id de (x0,y0) a (x1,y1) en duration-ms usando la
   misma Web Animations API que SVGAnimate de externs.js."
  [dom-id x0 y0 x1 y1 duration-ms]
  (js/SVGAnimate dom-id x0 y0 x1 y1 duration-ms))

;; --- Google sign-in -----------------------------------------------------------
(defn google-login! [on-success on-error]
  (js/GOOGlogin on-success on-error))

(defn google-logout! []
  (js/GOOGlogout))

;; --- Aviso de cambios sin guardar --------------------------------------------
(defn register-unsaved-changes-listener! []
  (js/registerListener))

(defn remove-unsaved-changes-listener! []
  (js/removeListener))

;; --- console -------------------------------------------------------------------
(defn log [& args]
  (apply js/console.log args))

(defn error [& args]
  (apply js/console.error args))
