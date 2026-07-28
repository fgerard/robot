(ns robot2.interop
  "Unica frontera hacia JS global: navegador y resources/public/js/lib/externs.js.
   Nada fuera de este namespace debe llamar js/* directamente.")

;; --- Transformacion de coordenadas SVG -------------------------------------
;; Usa SVGWrap de externs.js (createSVGPoint + getScreenCTM + matrixTransform)
;; en vez de llamar esos metodos directo desde cljs: externs.js se compila
;; aparte del bundle cljs, asi que el Closure Compiler de los builds
;; :advanced (robot2-min) no le renombra las propiedades -- llamar los
;; metodos SVG crudos desde cljs si se mangleaba, dejando drag/resize/conectar
;; rotos en produccion aunque funcionaran en dev (:none/figwheel).
(defn client-point->svg-point
  "Convierte coordenadas de cliente (event.clientX/Y) a coordenadas internas
   del SVG identificado por svg-el, respetando el viewBox/zoom actual."
  [svg-el client-x client-y]
  (let [p (js/SVGWrap svg-el client-x client-y)]
    [(.-x p) (.-y p)]))

;; --- Pointer capture --------------------------------------------------------
;; setPointerCapture asegura que pointermove/pointerup sigan llegando al mismo
;; elemento aunque el cursor salga del SVG durante un arrastre, eliminando la
;; necesidad de los handlers mouseout/mouseleave de respaldo de la version 1.
;; hasPointerCapture pasa por SVGHasPointerCapture de externs.js por la misma
;; razon que client-point->svg-point (setPointerCapture/releasePointerCapture
;; si sobreviven el build :advanced actual, por eso se quedan como llamada directa).
(defn capture-pointer! [dom-el pointer-id]
  (.setPointerCapture dom-el pointer-id))

(defn release-pointer! [dom-el pointer-id]
  (when (js/SVGHasPointerCapture dom-el pointer-id)
    (.releasePointerCapture dom-el pointer-id)))

;; --- Wheel no-pasivo ----------------------------------------------------------
;; React adjunta su listener sintetico de "wheel" como passive, asi que
;; preventDefault no funciona ahi (el navegador solo lo ignora con un warning
;; en consola) -- para evitar que un Ctrl+wheel (pellizco de trackpad) tambien
;; zoomee la pagina completa ademas del canvas, hace falta un listener nativo
;; explicitamente no-pasivo.
(defn add-wheel-listener! [dom-el handler]
  (.addEventListener dom-el "wheel" handler #js {:passive false}))

(defn remove-wheel-listener! [dom-el handler]
  (.removeEventListener dom-el "wheel" handler))

;; --- Animacion del robotito --------------------------------------------------
(defn animate-translate!
  "Anima el elemento dom-id de (x0,y0) a (x1,y1) en duration-ms usando la
   misma Web Animations API que SVGAnimate de externs.js."
  [dom-id x0 y0 x1 y1 duration-ms]
  (js/SVGAnimate dom-id x0 y0 x1 y1 duration-ms))

;; --- Google sign-in -----------------------------------------------------------
(defn google-render-button! [dom-el on-success on-error]
  (js/GOOGrenderButton dom-el on-success on-error))

(defn google-logout! []
  (js/GOOGlogout))

;; --- Aviso de cambios sin guardar --------------------------------------------
(defn register-unsaved-changes-listener! []
  (js/registerListener))

(defn remove-unsaved-changes-listener! []
  (js/removeListener))

;; --- Tema claro/oscuro --------------------------------------------------------
;; El <html data-theme="..."> ya se fija sincronamente en un <script> inline en
;; el <head> del HTML (leyendo localStorage, antes de que cargue el CSS, para
;; evitar un parpadeo del tema equivocado) -- current-theme solo lee ese
;; estado inicial para que el re-frame db arranque en sync con el DOM real.
(defn current-theme []
  (or (.getAttribute (.-documentElement js/document) "data-theme") "dark"))

(defn apply-theme! [theme]
  (.setAttribute (.-documentElement js/document) "data-theme" theme)
  (try
    (.setItem js/localStorage "robot-theme" theme)
    (catch :default _)))

;; --- console -------------------------------------------------------------------
(defn log [& args]
  (apply js/console.log args))

(defn error [& args]
  (apply js/console.error args))
