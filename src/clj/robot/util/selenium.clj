(ns robot.util.selenium
  (:require
    [clojure.xml :as X]
    [clojure.walk :as w]
    [clojure.pprint :as pp]
    [integrant.core :as ig]
    [robot.core.util :as U])
  (:import
   (org.openqa.selenium By NoSuchElementException WebDriver WebElement JavascriptExecutor)
   (org.openqa.selenium.firefox FirefoxDriver)
   (org.openqa.selenium.support.ui Select)))


(def max-retries 60)
(def delay-time 2000)



;Funciones

(defn exit [driver]
  "Función que cierra el navegador **driver**
  driver: {Firefox|Chrome} Selenium Driver"
  (.quit driver))

(defn assertText [element text]
  "Función que verifica que el texto del elemento **element** contenga **text**
  element: Elemento de selenium
  text: Texto de comparación"
  (.contains (.getText element) text))


(defn setTimeout [driver seconds]
  "Función que configura el tiempo de espera en segundos **seconds** para la carga de paginas en **driver**
   driver: {Firefox|Chrome} Selenium Driver.
   seconds: tiempo de espera en segundos para la carga de paginas"
  (-> driver
      (.manage)
      (.timeouts)
      (.implicitlyWait seconds java.util.concurrent.TimeUnit/SECONDS)))

(defn getHandlerTitle [driver handler]
  "Función que obtiene el titulo de un tab **handler** del navegador **driver**
  driver: driver: {Firefox|Chrome} Selenium Driver
  handler: tab o instancia del navegador. "
  (-> driver
      (.switchTo)
      (.window handler)
      (.getTitle)))

(defn switchTo [driver title]
  "Función que cambia al tab con el titulo **title** en el **driver**
  driver: driver: {Firefox|Chrome} Selenium Driver
  title: titulo del tab a cambiar"
  (let [handlers (seq (.getWindowHandles driver))]
    (loop [i 0]
      (let [handler (nth handlers i)]
        (if-not (= (getHandlerTitle driver handler) title)
          (recur (inc i)))))))

(defn wait-for-page [driver]
  "Función que duerme la ejecución del test case hasta que el navegador {driver} se encuentre en el estado de carga completada
  driver: {Firefox|ChromeDriver} Selenium Driver"
  (println "######### Esperando")
  (let [js (cast org.openqa.selenium.JavascriptExecutor driver)
        oa (object-array 0)
        script (.executeScript driver "return document.readyState" oa)]
    (println "######### js " js)
    (println "######### cargando?: " script)
    (while (not= (.toString (.executeScript js "return document.readyState" oa)) "complete")
      (Thread/sleep 1000))))

(defn find-element [driver query-string]
  "Función que busca y devuelve un elemento {query-string en el DOM de {driver} a través de selector CSS, Name, Texto de link e id
  driver: {Firefox|ChromeDriver} Selenium Driver
  query-string: name=userID"
  (try
    (if (or (.startsWith query-string "//"))
      (.findElement driver (By/xpath query-string))
      (let [type (subs query-string 0 (clojure.string/index-of query-string "="))
            value (subs query-string (+ (clojure.string/index-of query-string "=") 1))]
        (println "Buscando: " type ": " value)
        (cond
          (= type "xpath")
          (.findElement driver (By/xpath value))
          (= type "css")
          (.findElement driver (By/cssSelector value))
          (= type "name")
          (.findElement driver (By/name value))
          (= type "link")
          (.findElement driver (By/partialLinkText value))
          (= type "id")
          (.findElement driver (By/id value)))))
    (catch Exception ex
      (println "Exception: " (.getMessage ex)))))


(defn javascript-operation [driver script & parameters]
  "Función que ejecuta el código javascript {script} en el navegador {driver} enviandole los argumentos {parameters}
    driver: {Firefox|Chrome} Selenium Driver.
    script: Código javascript a ejecutar en el navegador.
    parameters: Código javascript a ejecutar en el navegador."
  (println "Ejecutando javascript")
  (let [oa (if parameters (object-array parameters) (object-array 0))
        js-element (cast org.openqa.selenium.JavascriptExecutor driver)]
    (.executeScript js-element script oa)
    (println "Javascript Ejecutado")))

(defn select-element-by [driver find-element-query change-element-by-query]
  "Función que selecciona {change-element-by-query} de una caja de selección {find-element-query} del navegador {driver}
    driver: {Firefox|Chrome} Selenium Driver.
    find-element-query: Consulta para encontrar un elemento en el árbol DOM del navegador (name=username).
    find-element-query: Consulta para seleccionar una opcion del elemento {find-element-query} (label=Spanish)."
  (let [type (subs change-element-by-query 0 (clojure.string/index-of change-element-by-query "="))
        value (subs change-element-by-query (+ (clojure.string/index-of change-element-by-query "=") 1))]
    (println "Seleccionando: " type ": " value)
    (cond
      (= type "label")
      (javascript-operation driver
                            (str "var select = arguments[0];"
                                 "for (var i = 0; i < select.options.length; i++) {"
                                 "if (select.options[i].text == arguments[1]) {"
                                 "select.options[i].selected = true;"
                                 "}"
                                 "}"
                                 "select.dispatchEvent(new Event('change', {}))")
                            (find-element driver find-element-query)
                            value)
      (= type "index")
      (javascript-operation driver
                            (str "var select = arguments[0];"
                                 "select.options[arguments[1]].selected = true;"
                                 "select.dispatchEvent(new Event('change', {}))")
                            (find-element driver find-element-query)
                            value)
      (= type "value")
      (javascript-operation driver
                            (str "var select = arguments[0];"
                                 "for (var i = 0; i < select.options.length; i++) {"
                                 "if (select.options[i].value == arguments[1]) {"
                                 "select.options[i].selected = true;"
                                 "}"
                                 "}"
                                 "select.dispatchEvent(new Event('change', {}))"
                                 "select.dispatchEvent(new Event('click', {}))")
                            (find-element driver find-element-query)
                            value))))

;Esta funcion deberia funcionar mejor en versiones posteriores, actualmente no funciona por un bug en firefox,
; la dejo aquí para que cuándo la versión se estabilice, se utilice ésta en lugar de la que se encuentra arriba
(comment defn select-element-by [selector query-string]
         (let [type (subs query-string 0 (clojure.string/index-of query-string "="))
               value (subs query-string (+ (clojure.string/index-of query-string "=") 1))]
           (println "Selector: " selector)
           (println "Seleccionando: " type ": " value)
           (cond
             (= type "label")
             (.selectByVisibleText selector value)
             (= type "index")
             (.selectByIndex selector value)
             (= type "value")
             (.selectByValue selector value))))


(defn write-element [driver find-by text]
  "Función que escribe el texto {text} en una caja de texto seleccionada por {find-by} dentro del navegador {driver}.
  driver: {Firefox|Chrome} Selenium Driver.
  find-by: Consulta para encontrar un elemento en el árbol DOM del navegador (name=username).
  text: Texto a escribir en el elemento encontrado en {find-by}"
  (let [query (find-element driver find-by)]
    (.sendKeys query (into-array [text]))))


(defn click-element [driver find-by]
  "Función da click izquierdo sobre el elemento encontrado con {find-by} en el navegador {driver}.
  driver: {Firefox|Chrome} Selenium Driver.
  find-by: Consulta para encontrar un elemento en el árbol DOM del navegador (name=username)."
  (let [query (find-element driver find-by)]
    (.click query)))

(defn wait-for-element-present [driver find-by max-retries]
  "Función que duerme la ejecución del test case hasta que se encuentre el elemento {find-by} en el navegador {driver} {max-retries} veces.
  driver: {Firefox|Chrome} Selenium Driver.
  find-by: Consulta para encontrar un elemento en el árbol DOM del navegador (name=username).
  max-retries: Número máximo de reintentos para búscar el elemento del árbol DOM."
  (loop [retries-count 0]
    (if (and (not (find-element driver find-by)) (< retries-count max-retries))
      (do
        (Thread/sleep 1000)
        (recur (+ retries-count 1))))))


(defmulti exec-cmd (fn [_ {:keys [cmd]}] cmd))

(defmethod exec-cmd "to" [driver {:keys [cmd val]}]
  (let [start-time (System/currentTimeMillis)]
    (println "Abriendo página inicial: " (first val))
    (-> driver
        (.navigate)
        (.to (first val)))
    (- (System/currentTimeMillis) start-time)))


(defmethod exec-cmd "open" [driver {:keys [cmd val]}]
  (let [start-time (System/currentTimeMillis)]
    (println "############## Abriendo " (second (re-matches #"((https|http)://[A-Za-z.\-0-9]*)(/)?.*" (.getCurrentUrl driver))))
    (-> driver
        (.navigate)
        (.to (str
               (second (re-matches #"((https|http)://[A-Za-z.\-0-9]*)(/)?.*" (.getCurrentUrl driver)))
               (first val))))
    (- (System/currentTimeMillis) start-time)))


(defmethod exec-cmd "type" [driver {:keys [cmd val]}]
  (let [start-time (System/currentTimeMillis)]
    (println "############## type" val)
    (-> driver
        (write-element (first val) (second val)))
    (- (System/currentTimeMillis) start-time)))

(defmethod exec-cmd "click" [driver {:keys [cmd val]}]
  (let [start-time (System/currentTimeMillis)]
    (println "############## click")
    (-> driver
        (click-element (first val)))
    (- (System/currentTimeMillis) start-time)))

(defmethod exec-cmd "clickAndWait" [driver {:keys [cmd val]}]
  (let [start-time (System/currentTimeMillis)]
    (println "############## clickAndWait")
    (-> driver
        (click-element (first val)))
    (wait-for-page driver)
    (- (System/currentTimeMillis) start-time)))


(defmethod exec-cmd "select" [driver {:keys [cmd val]}]
  (let [start-time (System/currentTimeMillis)]
    (println "############## select")
    (-> driver
        (select-element-by (first val) (second val)))
    (- (System/currentTimeMillis) start-time)))

(defmethod exec-cmd "waitForElementPresent" [driver {:keys [cmd val]}]
  (let [start-time (System/currentTimeMillis)]
    (println "############## waitForElementPresent")
    (-> driver
        (wait-for-element-present (first val) max-retries))
    (- (System/currentTimeMillis) start-time)))


(defmethod exec-cmd "selectWindow" [driver {:keys [cmd val]}]
  (let [start-time (System/currentTimeMillis)]
    (println "############## selectWindow")
    (-> driver
        (switchTo (first val)))
    (- (System/currentTimeMillis) start-time)))

(defmethod exec-cmd "waitForPopUp" [driver {:keys [cmd val]}]
  (let [start-time (System/currentTimeMillis)]
    (println "############## waitForPopUp")
    (-> driver
        (setTimeout (Long/parseLong (first val))))
    (- (System/currentTimeMillis) start-time)))

(defmethod exec-cmd "assertText" [driver {:keys [cmd val]}]
  (let [start-time (System/currentTimeMillis)
        element (find-element driver (first val))]
    (println "############## assertText")
    (assertText element (second val))
    (- (System/currentTimeMillis) start-time)))


(defmethod exec-cmd "execJS" [driver {:keys [cmd val]}]
  (let [start-time (System/currentTimeMillis)]
    (println "############## execJS")
    (javascript-operation driver (first val))
    ;(.executeScript driver (first val) (into-array []))
    (- (System/currentTimeMillis) start-time)))


(defn create-nav [xml]
  "Función que crea la estructura de datos requerida por el reader a partir de un xml parseado mediante (clojure.xml/parse).
  nav-xml: xml parseado mediante (clojure.xml/parse).   "
  (let [xml-file (slurp xml)
        xml-text (clojure.string/replace xml-file #"<!DOCTYPE.*\.dtd\">" "")
        nav-xml (clojure.xml/parse (java.io.ByteArrayInputStream. (.getBytes xml-text)))
        head-tag (first (filter (fn [content]
                                  (= (:tag content) :head))
                                (:content nav-xml)))
        link-tag (first (filter (fn [content]
                                  (= (:tag content) :link))
                                (:content head-tag)))
        selenium-base (get-in link-tag [:attrs :href])

        body-tag (first (filter (fn [content]
                                  (= (:tag content) :body))
                                (:content nav-xml)))

        table-tag (first (filter (fn [content]
                                   (= (:tag content) :table))
                                 (:content body-tag)))

        tbody-tag (first (filter (fn [content]
                                   (= (:tag content) :tbody))
                                 (:content table-tag)))

        nav (map (fn [tr]
                   (let [tds (:content tr)
                         cmd (first (:content (first tds)))
                         target (:content (second tds))
                         value (:content (last tds))
                         parameters (into [] (into target value))]
                     {:cmd cmd :val parameters}))
                 (:content tbody-tag))
        nav (into [{:cmd "to" :val [selenium-base]}] nav)]
    (println "NAV: " (pr-str nav))
    nav))

(defn create-firefox "Función que crea una nueva instancia de FireFox."
  []
  (println "Creando firefox")
  (org.openqa.selenium.firefox.FirefoxDriver.))

(defn create-chrome []
  "Función que crea una nueva instancia de Chrome."
  (org.openqa.selenium.chrome.ChromeDriver.))


(defn nav-reducer [[driver result] [step command]]
  "Función que reduce la estructura de datos generada por **create-nav** para ejecutar cada instrucción mediante **exec-cmd**
   driver: {Firefox|Chrome} Selenium Driver.
   result: Vector dónde se almacenará los resultados de cada operación (tiempo de ejecución).
   step: indice de paso de ejecución.
   command: comando a ejecutar"
  (Thread/sleep delay-time)
  (println "Comando: " command)
  (println "Step: " step)
  (try
    (let [r (exec-cmd driver command)]
      [driver (conj result [step r (.getPageSource driver)])])
    (catch Exception e
      (println "Exception: " (.getMessage e)))))


(defn calculate-deltas [driver steps delta-steps]
  (if (and delta-steps (> (count delta-steps) 0))
    (do
      (let [steps (rest steps)
            delta-steps (into #{} (first delta-steps))
            filtered-steps (filter (fn [el]
                                     (delta-steps (first el)))
                                   (first steps))
            deltas (map second filtered-steps)
            sum (apply + deltas)]
        {:driver driver :total-time sum :steps (first steps)}))
    (do
      (let [steps (first (rest steps))
            deltas (map second steps)
            sum (apply + deltas)]
        {:driver driver :total-time sum :steps (rest steps)}))))

(defn play-navigation [driver nav & delta-steps]
  "Función que reduce la estructura de datos generada por **create-nav** para ejecutar cada instrucción mediante **exec-cmd**
   driver: {Firefox|Chrome} Selenium Driver.
   nav: Estructura de datos generada por {create-nav} a partir del XML de entrada, define los pasos de la prueba.
   delta-steps: Set de indices de pasos de los que se calculara el tiempo total de ejecución."
  (try
    (let [steps (reduce nav-reducer
                        [driver []]
                        (map-indexed (fn [step comando]
                                       [step comando]) nav))]
      ;(println "Exiting browser")
      ;(exit driver)
      (println "Deltas: " steps)
      (calculate-deltas driver steps delta-steps))
  (catch Exception e
    (.printStackTrace e))
  (finally
    (println "Exiting browser")
    ;(.quit driver)
    (doseq [win-handle (.getWindowHandles driver)]
      (-> driver .switchTo (.window win-handle) .close))
    )))


(defn -main [& args]
  (println "Argumentos: " args)
  (let [file (first args)
        _ (if file
            (println "archivo: " file)
            (do
              (println "Parámetros inválidos, archivo no especificado")
              (println "uso: java -jar reader {archivo (requerido)} {indices delta (opcional)}")
              (System/exit 0)))
        delta-steps (vec (rest args))
        delta-steps (map read-string delta-steps)
        delta-steps (into #{} delta-steps)
        _ (println (count delta-steps))
        _ (println "delta-steps: " delta-steps)
        _ (println "tipo: " (class (first delta-steps)))
        result (play-navigation (create-firefox)
                                file
                                delta-steps)]
    (println result)))
