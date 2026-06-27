(ns robot.util.selenium-direct
  (:gen-class)
  (:require
    [clojure.core.async :refer [chan <! <!! >!! >! go alts! alts!!]]
    [clojure.xml :as X]
    [clojure.walk :as w]
    [clojure.java.io :as io]
    [clojure.java.shell :as shell]
    [clojure.tools.logging :as log]
    [clojure.pprint :as pp]
    [integrant.core :as ig]
    [pl.danieljanus.tagsoup :as soup]
    [robot.core.util :as U]
    [robot.util.selenium-common :as scommon])
  (:import
    (org.openqa.selenium By NoSuchElementException WebDriver WebElement JavascriptExecutor Dimension)
    (org.openqa.selenium.interactions Actions)
    (org.openqa.selenium.firefox FirefoxDriver FirefoxBinary FirefoxOptions)
    (org.openqa.selenium.firefox.internal ProfilesIni)
    (org.openqa.selenium.chrome ChromeDriver ChromeOptions)
    (org.openqa.selenium.support.ui Select ExpectedConditions WebDriverWait)))

(defn sleep [wait]
  (let [delta (Integer/parseInt (str wait))]
    (Thread/sleep delta)))

(defn fn-with-retry [timeout fun]
  (let [timeout (Integer/parseInt (str timeout))]
    (fn [driver]
      (let [retry-delay (min 1000 timeout)
            retry-cnt (inc (int (/ timeout retry-delay)))]
        (reduce
          (fn [result retry-idx]
            (let [value (try
                          (fun driver)
                          (catch Exception e
                            e))]
              ;(when value (log/debug "resultado intermedio: [" value "]"))
              (cond
                (or (nil? value) (>= retry-idx retry-cnt))
                (reduced value)

                (< retry-idx retry-cnt)
                (sleep retry-delay)

                :OTHERWIZE
                value)))
          nil
          (range 1 (inc retry-cnt)))))))

(defn get-url [url delta]
  (fn [driver]
    (let [ftr (future
                (try
                  (and url (.get driver url))
                  (catch Exception e
                    e))
                nil)]
      (deref ftr delta (str "Getting " url " took more than " delta "ms")))))

(defn click-elem [elem label]
  (if elem
    (do
      (try
        (.click elem)
        (Thread/sleep 300)
        (catch Throwable e
          (.printStackTrace e)
          ))
      ;(.click elem)
      nil)
    (str "No element " label " available for click")))

(defn type-elem [elem text label]
  (if elem
    (do
      (-> elem
          ;(.clear)
          (.sendKeys (into-array java.lang.CharSequence [text])))
      nil)
    (str "No element " label " available for typing")))

(defn clear-elem [elem text label]
  (if elem

    (do
      (-> elem
          (.clear))
      nil)
    (str "No element " label " available for clearing")))

(defn find-elem
  ([driver dom-id]
   (find-elem driver dom-id 10))
  ([driver dom-id max-wait]
   (let [waiter (WebDriverWait. driver max-wait)
         expectation (ExpectedConditions/visibilityOfElementLocated dom-id)]
     (.until waiter expectation))))

;(try;
;  (.findElement driver dom-id)
;(catch NoSuchElementException e
;  nil))

(defmulti by (fn [selector value] selector))
(defmethod by :id [_ value] (By/id value))
(defmethod by :css [_ value] (By/cssSelector value))
(defmethod by :xpath [_ value] (By/xpath value))
(defmethod by :name [_ value] (By/name value))
(defmethod by :link [_ value] (By/partialLinkText value))

(defmulti exec-step (fn [timeout {:keys [opr]}] opr))

(defmethod exec-step :wait [timeout {:keys [opr params]}]
  (let [[delta] params]
    (fn [driver]
      (log/debug "Sleep: " delta)
      (Thread/sleep delta)
      nil)))

(defmethod exec-step :print [timeout {:keys [opr params msg]}]
  (let [[delta] params]
    (fn [driver]
      (println params)
      nil)))


(defmethod exec-step :get [timeout {:keys [opr params msg]}]
  (let [getter (get-url (first params) (second params))]
    getter))

(defmethod exec-step :click [timeout {:keys [opr selector params msg]}]
  (fn [driver]
    (try
      (let [timeout (int (/ timeout 1000))
            waiter (WebDriverWait. driver timeout)
            sel (by selector (first params))
            expect (ExpectedConditions/elementToBeClickable sel)
            elem (.until waiter expect)]
        (.click elem)
        nil)
      (catch Exception e
        (or msg (str "No element available for clicking: " (first params)))))))

(defmethod exec-step :action-click [timeout {:keys [opr selector params msg]}]
  (fn [driver]
    (try
      (let [timeout (int (/ timeout 1000))
            waiter (WebDriverWait. driver timeout)
            sel (by selector (first params))
            expect (ExpectedConditions/visibilityOfElementLocated sel)
            elem (.until waiter expect)                     ;(.findElement driver sel)
            action (-> (Actions. driver)
                       (.moveToElement elem)
                       (.moveToElement elem)
                       .click
                       .build)]
        (.perform action)
        nil)
      (catch Exception e
        (or msg (str "No element available for clicking: " (first params)))))))

(defmethod exec-step :click-nowait [timeout {:keys [opr selector params msg]}]
  (fn [driver]
    (try
      (let [sel (by selector (first params))
            elem (.findElement driver sel)]
        (.click elem)
        nil)
      (catch Exception e
        (or msg (str "No element available for clicking: " (first params)))))))

(defmethod exec-step :type [timeout {:keys [opr selector params msg]}]
  (fn [driver]
    (try
      (let [timeout (int (/ timeout 1000))
            waiter (WebDriverWait. driver timeout)
            sel (by selector (first params))
            expect (ExpectedConditions/visibilityOfElementLocated sel)
            elem (.until waiter expect)]
        (.sendKeys elem (into-array java.lang.CharSequence [(second params)]))
        nil)
      (catch Exception e
        (or msg (str "No element available for typing: " (first params)))))))

(defmethod exec-step :clear [timeout {:keys [opr selector params msg]}]
  (fn [driver]
    (try
      (let [timeout (int (/ timeout 1000))
            waiter (WebDriverWait. driver timeout)
            sel (by selector (first params))
            expect (ExpectedConditions/visibilityOfElementLocated sel)
            elem (.until waiter expect)]
        (.clear elem)
        nil)
      (catch Exception e
        (or msg (str "No element available for clearing: " (first params)))))))

(defmethod exec-step :set-window-size [timeout {:keys [opr params msg]}]
  (let [[width height] params]
    (fn [driver]
      (try
        (-> driver .manage .window (.setSize (Dimension. width height)))
        nil
        (catch Exception e
          (or msg (str "Can't set window size: " (.getMessage e))))))))

(defmethod exec-step :switch2window2 [timeout {:keys [opr params msg]}]
  (let [title (first params)
        title (if (string? title) (re-pattern title) title)]
    (fn-with-retry
      timeout
      (fn [driver]
        (let [current (.getWindowHandle driver)]
          (if-let [found (some (fn [win-handle]
                                 (let [w-title (-> driver .switchTo (.window win-handle) .getTitle)]
                                   (log/debug "Switching to window: " w-title " looking for: " title)
                                   (re-find title w-title)))
                               (.getWindowHandles driver))]
            nil
            (do
              (-> driver .switchTo (.window current))
              (or msg (str "No window with title [" title "]")))))))))

(defmethod exec-step :switch2window [timeout {:keys [opr params msg]}]
  (let [title (first params)
        title (if (string? title) (re-pattern title) title)]
    (fn-with-retry
      timeout
      (fn [driver]
        (let [current (.getWindowHandle driver)
              handles (into [] (.getWindowHandles driver))
              n (count handles)]
          (loop [idx 0]
            (if (>= idx n)
              (do
                (-> driver .switchTo (.window current))
                (or msg (str "No window with title [" title "]")))
              (let [handle (handles idx)
                    w-title (-> driver .switchTo (.window handle) .getTitle)]
                (log/debug "Switching to window: " w-title " looking for: " title)
                (if-not (re-find title w-title)
                  (recur (inc idx))
                  nil)))))))))

(defmethod exec-step :switch2last-window [timeout {:keys [opr msg]}]
  ;(fn-with-retry
  ; timeout)
  (fn [driver]
    (let [handles (into [] (.getWindowHandles driver))]
      (doseq [handle handles]
        (-> driver .switchTo (.window handle) .getTitle println))
      nil)))

(defmethod exec-step :screenshot [timeout {:keys [opr params]}]
  (fn [driver]
    (io/copy
      (.getScreenshotAs driver org.openqa.selenium.OutputType/FILE)
      (io/file (first params)))
    nil))

(defmethod exec-step :wait4element-visible [timeout {:keys [opr selector params msg]}]
  (let [[elem-id timeout] params]
    (fn [driver]
      (try
        (let [timeout (int (/ timeout 1000))
              waiter (WebDriverWait. driver timeout)]
          (.until waiter (ExpectedConditions/visibilityOfElementLocated (by selector elem-id))))
        nil
        (catch Exception e
          (log/warn e)
          (or msg (str "No visible element " elem-id)))))))

(defmethod exec-step :wait4element-clickable [timeout {:keys [opr selector params msg]}]
  (let [[elem-id timeout] params]
    (fn [driver]
      (try
        (let [timeout (int (/ timeout 1000))
              waiter (WebDriverWait. driver timeout)]
          (.until waiter (ExpectedConditions/elementToBeClickable (by selector elem-id)))
          nil)
        (catch Exception e
          (or msg (str "No clickable element " elem-id)))))))

(defmethod exec-step :wait4element-selectable [timeout {:keys [opr selector params msg]}]
  (let [[elem-id timeout] params]
    (fn [driver]
      (try
        (let [timeout (int (/ timeout 1000))
              waiter (WebDriverWait. driver timeout)]
          (.until waiter (ExpectedConditions/elementToBeSelected (by selector elem-id))))
        nil
        (catch Exception e
          (log/warn e)
          (or msg (str "No selectable element " elem-id)))))))

(defmethod exec-step :wait4element-present [timeout {:keys [opr selector params msg]}]
  (let [[elem-id timeout] params]
    (fn [driver]
      (try
        (let [timeout (int (/ timeout 1000))
              waiter (WebDriverWait. driver timeout)]
          (.until waiter (ExpectedConditions/presenceOfElementLocated (by selector elem-id))))
        nil
        (catch Exception e
          (log/warn e)
          (or msg (str "No element present" elem-id)))))))


(defmethod exec-step :wait4frame2switch [timeout {:keys [opr selector params msg]}]
  (let [[frame timeout2] params]
    (fn [driver]
      (try
        (let [timeout (if timeout2 timeout2 (int (/ timeout 1000)))
              waiter (WebDriverWait. driver timeout)]
          (.until waiter (ExpectedConditions/frameToBeAvailableAndSwitchToIt frame))
          nil)
        (catch Exception e
          (or msg (str "No frame " frame " available to switch to")))))))

(defmethod exec-step :switch2frame [timeout {:keys [opr params msg]}]
  (let [title (first params)]
    (fn-with-retry
      timeout
      (fn [driver]
        (try
          (-> driver
              (.switchTo)
              (.frame (first params)))
          nil
          (catch Exception e
            (throw (Exception. (or msg (str "No frame " (first params) " present"))))))))))

(defmethod exec-step :switch2frame-params [timeout {:keys [opr selector params msg]}]
  (fn [driver]
    (try
      (let [sel (by selector (first params))
            elem (.findElement driver sel)]
        (-> driver
            (.switchTo)
            (.frame elem)))
      nil
      (catch Exception e
        (throw (Exception. (or msg (str "No frame " (first params) " present"))))))))

(defmethod exec-step :close-window [timeout {:keys [msg]}]
  (let []
    (fn [driver]
      (try
        (.close driver)
        nil
        (catch Exception e
          (throw (Exception. (or msg (str "Can't close current window")))))))))

(defmethod exec-step :accept-alert [timeout {:keys [msg]}]
  (fn [driver]
    (try
      (-> driver
          .switchTo
          .alert
          .accept)
      nil
      (catch Exception e
        (log/warn e)
        nil))))

(defmethod exec-step :switch2default [timeout {:keys [opr msg]}]
  (let []
    (fn-with-retry
      timeout
      (fn [driver]
        (try
          (-> driver
              (.switchTo)
              (.defaultContent))
          nil
          (catch Exception e
            (throw (Exception. (or msg (str "No default content to switch"))))))))))

(defmethod exec-step :exec-js [timeout {:keys [opr params msg]}]
  (fn [^JavascriptExecutor driver]
    (try
      (sleep 500)
      (let [[js-code & js-params] params
            js-params (if (seq js-params) (object-array js-params) (object-array 0))]
        (.executeScript driver js-code js-params))
      nil
      (catch Exception e
        (or msg (str e ": " (.getMessage e)))))))

(defmethod exec-step :select-one [timeout {:keys [opr selector params msg]}]
  (fn [driver]
    (try
      (let [timeout (int (/ timeout 1000))
            waiter (WebDriverWait. driver timeout)
            elem-selector (by (first selector) (first params))
            elem (.until waiter (ExpectedConditions/presenceOfElementLocated elem-selector))]
        (condp = (second selector)
          :index (do (doto (Select. elem) .deselectAll (.selectByIndex (second params))) nil)
          :value (do (doto (Select. elem) .deselectAll (.selectByValue (second params))) nil)
          :text (do (doto (Select. elem) .deselectAll (.selectByVisibleText (second params))) nil)
          (str "Value " (second selector) " invalid, valid options: " [:index :value :text])))
      (catch Exception e
        (or msg (str "No element " (first params) " available for selecting"))))))

(defmethod exec-step :select [timeout {:keys [opr selector params msg]}]
  (fn [driver]
    (try
      (let [timeout (int (/ timeout 1000))
            waiter (WebDriverWait. driver timeout)
            elem-selector (by (first selector) (first params))
            elem (.until waiter (ExpectedConditions/presenceOfElementLocated elem-selector))]
        (condp = (second selector)
          :index (do (doto (Select. elem) (.selectByIndex (second params))) nil)
          :value (do (doto (Select. elem) (.selectByValue (second params))) nil)
          :text (do (doto (Select. elem) (.selectByVisibleText (second params))) nil)
          (str "Value " (second selector) " invalid, valid options: " [:index :value :text])))
      (catch Exception e
        (or msg (str "No element " (first params) " available for selecting"))))))

(defmethod exec-step :assert-text [timeout {:keys [opr selector params msg]}]
  (fn [driver]
    (try
      (let [timeout (int (/ timeout 1000))
            waiter (WebDriverWait. driver timeout)
            text (second params)
            regex (if (string? text) (re-pattern text) text)
            elem (.until waiter (ExpectedConditions/presenceOfElementLocated (by selector (first params))))]
        (if (and (.isDisplayed elem)
                 (.getText elem)
                 (re-find regex (.getText elem)))
          nil
          (or msg (str "Text " regex " not found in element: " (first params)))))
      (catch Exception e
        (or msg (str "No element " (first params) " available"))))))

(defmethod exec-step :element-present [timeout {:keys [opr selector params msg]}]
  (fn [driver]
    (try
      (let [timeout (int (/ timeout 1000))
            waiter (WebDriverWait. driver timeout)
            elem (.until waiter (ExpectedConditions/presenceOfElementLocated (by selector (first params))))]
        nil)
      (catch Exception e
        (or msg (str "No element " (first params) " present"))))))

(defmethod exec-step :assert-one [timeout {:keys [opr params msg]}]
  (fn [driver]
    (if-let [[_ error] (reduce
                         (fn [[driver err] step-fn]
                           (if-let [error (step-fn driver)]
                             [driver (conj err error)]
                             (reduced nil)))
                         [driver nil]
                         (map (fn [{:keys [timeout] :or {timeout timeout} :as step-conf}]
                                (exec-step timeout step-conf))
                              params))]
      (or msg (pr-str error)))))

(defn extract-inner-html [{:keys [driver] :as result} {:keys [timeout selector params] :or {timeout 3000} :as step}]
  (try
    (let [[id output-as] params
          timeout (int (/ timeout 1000))
          waiter (WebDriverWait. driver timeout)
          elem (.until waiter (ExpectedConditions/presenceOfElementLocated (by selector (first params))))]
      (let [innerHTML (.getAttribute elem "innerHTML")
            content (condp = output-as
                      :edn (soup/parse-string innerHTML)
                      :xml innerHTML
                      innerHTML)]
        (println "content:\n" content)
        (assoc result :content content)))
    (catch Exception e
      (log/warn "No innerHTML for: " step)
      result)))

(defn flow-reducer [default-timeout beat-delay {:keys [driver running-delta deltas] :as result} {:keys [opr tag] :as step}]
  (sleep beat-delay)
  (log/debug "flow-reducer " opr tag (pr-str step))
  (condp = opr
    :end (reduced (-> result
                      (assoc :out "ok")))

    :timer (assoc result
             :running-delta 0
             :deltas (assoc deltas tag running-delta))

    :inner-html (extract-inner-html result step)

    (let [{:keys [timeout] :or {timeout default-timeout}} step
          _ (log/debug :FLOW-REDUCER timeout (pr-str step))
          step-fn (exec-step timeout step)
          t0 (System/currentTimeMillis)
          error (step-fn driver)
          delta (if (= opr :wait)
                  0
                  (- (System/currentTimeMillis) t0))
          running-delta (+ running-delta delta)]
      (log/debug (pr-str (dissoc result :driver)) :running-delta running-delta :error error)
      (if error
        (reduced (assoc result
                   :out error
                   :err-flow-k (:error? step)))
        (assoc result :running-delta running-delta)))))

(defn contextualize-flow [ctx flow]
  (w/postwalk (fn [elem]
                (if (string? elem)
                  (U/contextualize ctx elem)
                  elem)) flow))

;(def D (FirefoxDriver.))

(defn close-all [driver]
  (try
    (try
      (doseq [win-handle (.getWindowHandles driver)]
        (-> driver .switchTo (.window win-handle) .close))
      (catch Exception e
        (.printStackTrace e)
        (log/warn "Problens closing browser: " (.getMessage e)))
      (finally
        (try
          (.quit driver)
          (catch Exception e
            ;(.printStackTrace e)
            (log/debug "Can't quit driver, this Exception is not important! " e)))))
    (finally
      (try
        (let [port (-> driver
                       .getCommandExecutor
                       .getAddressOfRemoteServer
                       .getPort)]
          (scommon/kill-port-pid port))
        (catch Exception e
          (.printStackTrace e)
          (log/warn "Can't clean old child process "))))))

(defn toDefault [driver]
  (try
    (-> driver
        (.switchTo)
        (.defaultContent))
    (catch Exception e
      (log/warn "No default content to switch"))))

(defn get-new-driver [app-inst-id driver-type profile]
  (loop [i 1]
    (log/info "get-new-driver-i: " i " " app-inst-id)
    (let [mk1 (System/currentTimeMillis)]
      (if-let [driver (try
                        (if (= driver-type "chrome")
                          (if profile
                            (-> (doto (ChromeOptions.)
                                  (.addArguments (into-array String (into (read-string (System/getProperty "robot.chrome.args"))
                                                                          [(str "user-data-dir=" profile)]))))
                                (ChromeDriver.))
                            (-> (doto (ChromeOptions.)
                                  (.addArguments (into-array String (read-string (System/getProperty "robot.chrome.args")))))
                                (ChromeDriver.)))
                          (if profile
                            (-> (doto (FirefoxOptions.)
                                  (.addArguments (into-array String (into (read-string (System/getProperty "robot.gecko.args"))
                                                                          ["-profile" profile]))))
                                (FirefoxDriver.))
                            (-> (doto (FirefoxOptions.)
                                  (.addArguments (into-array String (read-string (System/getProperty "robot.gecko.args")))))
                                (FirefoxDriver.))))
                        (catch Throwable e
                          (log/warn (str "Can't create driver retry:" i " >" (-> e class .getName) (.getMessage e)))
                          (Thread/sleep 1000)
                          nil))]
        (do (let [mk2 (- (System/currentTimeMillis) mk1)
                  _ (log/warn app-inst-id "Driver time ok: " mk2 " ms")
                  port (-> driver
                           .getCommandExecutor
                           .getAddressOfRemoteServer
                           .getPort)]
              (log/debug "initial driver port: " port))
            driver)
        (let [mk2 (- (System/currentTimeMillis) mk1)
              _ (log/warn app-inst-id "Driver time error: " mk2 " ms")]
          (if (< i 6)
            (recur (inc i))
            (throw (Exception. (str "Can't create driver after 5 retry")))))))))

(defn add-screen-shot [{:keys [driver] :as result}]
  (try
    (assoc result :png (.getScreenshotAs driver org.openqa.selenium.OutputType/BASE64))
    (catch Exception e
      (assoc result :txt (str (-> e class .getName) (.getMessage e))))))

(defn reduce-flow [page-load driver reducer d-flow]
  (try
    (reduce
      reducer                                               ;(partial deltas-decorator reducer)
      {:driver driver :running-delta page-load :deltas {}}
      (concat d-flow [{:opr :end}]))
    (catch Exception e
      (log/error "ENTRO A UN LUGAR QUE SE SUPONE QUE NO!!!")
      (log/error e)
      {:out (str "ERROR IMPOSIBLE:" e " : " (.getMessage e))}
      )))

(defn create-full-response [{:keys [driver out] :as result}]
  (if (= out "ok")
    result
    (add-screen-shot result)))

(defn execute-flow [driver reducer flow sub-flow page-load]
  (let [{:keys [err-flow-k] :as result} (reduce-flow page-load driver reducer flow) ;[msg sub-flow-k deltas]
        full-response (create-full-response result)]        ;driver msg deltas
    (when-let [d-sub-flow (get sub-flow err-flow-k)]
      (reduce-flow 0 driver reducer d-sub-flow))
    full-response))

(defn exec-driver [app-inst-id driver-type profile new-browser? close-it? {:keys [url beat-delay default-timeout flow sub-flow] :as conf} ctx response]
  (fn [driver]
    (log/info "exec-driver " app-inst-id)
    (letfn [(close-driver [d]
              (when (and d close-it?)
                (close-all d)))]
      (try
        (let [driver (cond (nil? driver) (get-new-driver app-inst-id driver-type profile)
                           (and driver new-browser?) (do (close-all driver)
                                                         (get-new-driver app-inst-id driver-type profile))
                           :else driver)
              url (U/contextualize ctx url)
              opener (get-url url default-timeout)
              t0 (System/currentTimeMillis)
              _ (opener driver)
              p-load (- (System/currentTimeMillis) t0)
              reducer (partial flow-reducer default-timeout beat-delay)
              flow (contextualize-flow ctx flow)
              status (execute-flow driver reducer flow sub-flow p-load)]
          (close-driver driver)
          (>!! response status)
          (if close-it? nil driver))
        (catch Exception e
          (log/error e)
          (>!! response {:out "Problems creating driver" :app-inst-id app-inst-id :txt (str (-> e class .getName) (.getMessage e))}))
        (finally
          (close-driver driver))))))

(defn exec-conf [driver-type profile browsers-atm new-browser? close-it? {:keys [url beat-delay default-timeout flow sub-flow] :as conf} ctx]
  (let [response (chan)
        app-inst-id ((juxt :robot/app :robot/instance) ctx)]
    (go
      (send-off browsers-atm update app-inst-id (exec-driver app-inst-id driver-type profile new-browser? close-it? conf ctx response)))
    (log/info "Waiting for response " app-inst-id)
    (<!! response)))

(defn exec-conf-2 [driver-type profile browsers-atm new-browser? close-it? {:keys [url beat-delay default-timeout flow sub-flow] :as conf} ctx]
  (try
    (let [app-inst-id ((juxt :robot/app :robot/instance) ctx)
          _ (send browsers-atm update app-inst-id
                  (fn [driver]
                    (try
                      (log/info (pr-str {:app-inst-id app-inst-id :driver driver}))
                      (cond
                        (nil? driver)
                        (get-new-driver driver-type profile)

                        (and driver new-browser?)
                        (do
                          (close-all driver)
                          ;(FirefoxDriver.)
                          (get-new-driver driver-type profile))

                        :OTHERWISE
                        driver)
                      (catch Throwable t
                        (log/error t)))))
          _ (log/debug "app-inst-id: " app-inst-id (nil? (get @browsers-atm app-inst-id)))
          driver (get @browsers-atm app-inst-id)
          _ (log/info "app-inst-id3 " driver)
          url (U/contextualize ctx url)
          _ (log/debug "app-inst-id4 " url)
          t0 (System/currentTimeMillis)
          driver (let [opener (get-url url default-timeout)]
                   (try
                     (opener driver)
                     (log/info "openner! : " driver " url:" url)
                     driver
                     (catch Exception e
                       (log/debug "Warning creating selenium driver: " e " " (.getMessage e)))))
          page-load (- (System/currentTimeMillis) t0)
          _ (log/debug "page-load " page-load)
          reducer (partial flow-reducer default-timeout beat-delay)
          flow (contextualize-flow ctx flow)]
      (if driver
        (try
          (execute-flow driver reducer flow sub-flow page-load)
          (finally
            (when close-it?
              (send browsers-atm dissoc app-inst-id)
              (close-all driver))))
        {:out "Can't create browser driver"}))
    (catch Exception e
      {:out "Problems creating driver" :txt (str (-> e class .getName) (.getMessage e))})))


;;;;;; a pata
(defn switchToWindow [driver title]
  (let [current (.getWindowHandle driver)
        handles (into [] (.getWindowHandles driver))
        n (count handles)]
    (log/debug "Existen " n " ventanas")
    (loop [idx 0]
      (if (>= idx n)
        (do
          (log/debug "idx= " idx)
          (-> driver .switchTo (.window current))
          (log/debug "NO SE ENCONTRO")
          (str "No window with title [" title "]"))
        (let [_ (log/debug "idx= " idx)
              handle (get handles idx)
              w-title (-> driver .switchTo (.window handle) .getTitle)]
          (log/debug "Switching to window: " w-title " looking for: " title " --> " (re-find (re-pattern title) w-title))
          (if-not (re-find (re-pattern title) w-title)
            (recur (inc idx))
            (log/debug "SE ENCONTRO!")))))))


(defn txt [s]
  (into-array java.lang.CharSequence [s]))

