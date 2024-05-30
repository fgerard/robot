(ns robot.core.operations
  (:require
   [clojure.core.async :refer [go go-loop put! >! <! chan timeout alts!] :as async]
   [clojure.pprint :as pp]
   [clojure.tools.logging :as log]
   [clojure.string :as S]
   [clojure.java.shell :as shell]
   [clojure.java.io :as io]
   [clojure.java.jdbc :as sql]
   [clojure.data.json :as json]
   [postal.core :as postal]
   [integrant.core :as ig]
   [twitter.oauth :as tauth]
   [twitter.callbacks]
   [twitter.callbacks.handlers]
   [twitter.api.restful :as tapi]
   [robot.core.util :as U]
   [robot.util.selenium :as selenium-util]
   [robot.util.selenium-direct :as selenium-direct]
   [util.http-client :as http-client]
   [robot.util.j3270 :as j3270]
   [robot.core.util :as util]
   [robot.util.telegram :as telegram]
   [robot.util.basic :as basic]
   [robot.util.google-spreadsheet :as gsheet]
   [clojure.java.jmx :as jmx]
   [clj-time.core :as t]
   [clj-time.format :as tf]
   [clj-time.coerce :as tc])
  (:import
   (org.mozilla.javascript Context)
   (org.mozilla.javascript.json JsonParser)
   (java.time ZonedDateTime ZoneId)
   (java.time.format DateTimeFormatter)))

(def extra-code (atom false))

(defmacro retry-fn [name timeout count delay & [params & fun-body :as body]]
  `(fn ~name [~@params]
     (let [[ctx# you#] [~@params]
           timeout# (U/contextualize-integer ctx# ~timeout 1000)
           count# (U/contextualize-integer ctx# ~count 3)
           delay# (U/contextualize-integer ctx# ~delay 1000)]
       ;(println (pr-str [:timeout timeout# :count count# :delay delay#]))
       (loop [i# 0]
         ;(println "\n\n=============================== i# = " i#)
         (let [futr# (future (try
                               [(do ~@fun-body) nil]
                               (catch Exception e#
                                 ;(.printStackTrace e#)
                                 [nil (assoc ~(first params) ~(second params) (str (class e#) " -> " (.getMessage e#)))])))
               [val# err#] (deref futr# timeout# [nil (assoc ~(first params) ~(second params) :timeout)])]
           ;(println "[val# err#] --> " (pr-str [val# err#]))
           ;(println "NO va a salir:" err# i# count# " -- " (and err# (< i# count#)))
           (if (and err# (< i# count#))
             (do
               (Thread/sleep delay#)
               (recur (inc i#)))
             (or val# err#)))))))

(defmethod ig/init-key :robot.core.operations/sleep-opr-factory
  [_ ui]
  [(fn sleep-opr-factory [{:keys [delay]}]
     (log/debug :sleep-factory delay)
     (fn sleep-opr [context you]
       (let [delta (U/contextualize-integer context delay 1000)]
         (log/debug :you you :sleep [delay] :->> (pr-str [delta]))
         (try
           (Thread/sleep delta)
           (catch Exception e
             (log/error e)))
         (log/info :sleep-ended)
         context)))
   ui])

(defmethod ig/init-key :robot.core.operations/wait-till-opr-factory
  [_ ui]
  [(fn wait-till-opr-factory [{:keys [regex]}]
     (log/debug :wait-till-factory regex)
     (fn wait-till-opr [context you]
       (log/info :you you :wait-till [regex])
       (let [tz (ZoneId/of "America/Mexico_City")
             ct (ZonedDateTime/now tz)
             fmt (DateTimeFormatter/ofPattern "HH:mm:ss")
             
             ;tz (t/time-zone-for-id "America/Mexico_City")
             ;fmt (tf/formatter "HH:mm:ss")
             ;fmt1 (tf/with-zone fmt tz)
             ;now (* (int (/ (System/currentTimeMillis) 1000)) 1000)
             regex (re-pattern (U/contextualize context regex))
             delay (reduce
                    (fn [_ delta]
                      (let [t (.plusSeconds ct delta)
                            d (.format fmt t)
                            ;t (+ now (* delta 1000))
                            ;now (tc/from-long t)
                            ;d (tf/unparse fmt1 now)
                            ]
                        (if (re-matches regex d)
                          (reduced delta)
                          -1)))
                    nil
                    (range 1 (* 3600 24)))]
         (log/info :wait-till-opr :delay delay)
         (if (< delay 0)
           (assoc context you (str regex " is invalid!"))
           (let [sleep (- (* 1000 delay) 500)
                 _ (Thread/sleep sleep)
                 now (->> (ZonedDateTime/now tz)
                          (.format fmt))]
             (assoc context you now))))))
   ui])

(defmethod ig/init-key :robot.core.operations/date-time-opr-factory
  [_ ui]
  [(fn date-time-opr-factory [{:keys [format]}]
     (log/debug :date-time-factory format)
     (fn date-time-opr [context you]
       (log/debug :you you :date-time)
       (let [format (U/contextualize context format)
             formatter (java.text.SimpleDateFormat. format)
             result (.format formatter (java.util.Date.))]
         (assoc context you result))))
   ui])

(defmethod ig/init-key :robot.core.operations/socket-opr-factory
  [_ ui]
  [(fn socket-opr-factory [{:keys [host port timeout retry-count retry-delay]
                            :as   conf}]
     (log/debug :socket-opr-factory host port)
     (retry-fn
       socket-opr timeout retry-count retry-delay [context you]
       (log/debug :socket-opr (pr-str [host port]))
       (with-open [socket (java.net.Socket. (U/contextualize context host)
                                            (U/contextualize-integer context port 22))])
       (assoc context you :ok)))
   ui])

(defmethod ig/init-key :robot.core.operations/http-opr-factory
  [_ ui]
  [(fn http-opr-factory [{:keys [uri method params timeout retry-count retry-delay]
                          :as   conf}]
     (log/debug :post-http-opr-factory uri)
     (retry-fn
       http-opr timeout retry-count retry-delay [context you]
       (let [uri (U/contextualize context uri)]
         (log/debug "Connecting to: " uri)
         (log/debug "Connecting with method: " method)
         (log/debug "Connecting with params: " params)
         (let [params (if (string? params)
                        (read-string params)
                        params)
               {:keys [status body]} (cond
                                       (= method "POST")
                                       @(http-client/post uri {:as   :string
                                                               :body params})
                                       (= method "GET")
                                       (let [url (if (> (count params) 0)
                                                   (if (.contains uri "?")
                                                     (str uri "&" (ring.util.codec/form-encode params))
                                                     (str uri "?" (ring.util.codec/form-encode params)))
                                                   uri)]
                                         @(http-client/get url {:as :string}))
                                       (= method "PUT")
                                       @(http-client/put uri {:as   :string
                                                              :body params})
                                       (= method "DELETE")
                                       @(http-client/delete uri {:as   :string
                                                                 :body params}))]
           (assoc context you (format "%d -> %1.1000s" status body))))))
   ui])



(defmethod ig/init-key :robot.core.operations/switch-good-opr-factory
  [_ ui]
  [(fn switch-good-opr-factory [{:keys [id]}]
     (log/debug :switch-good-opr-factory)
     (fn switch-good-opr [{mood :robot/mood :as context} you]
       (log/debug :you you :switch-good)
       (let [[id-mood] (get mood id)]
         (-> context
             (assoc :robot/mood (assoc mood id [:good (System/currentTimeMillis) 0])
                    you (if (not= id-mood :good) "send" "skip"))))))
   ui])

(defmethod ig/init-key :robot.core.operations/switch-bad-opr-factory
  [_ ui]
  [(fn switch-bad-opr-factory [{:keys [minutes id]}]
     (log/debug :switch-bad-opr-factory minutes)
     (fn switch-bad-opr [{mood         :robot/mood
                          :or          {mood {}}
                          :as          context} you]
       (log/debug :you you :switch-bad)
       (let [now (System/currentTimeMillis)
             [id-mood ts cnt] (get mood id)]
         (log/info "SWITCH-BAD " [id-mood ts cnt])
         (if (not= id-mood :bad)
           (assoc context
                  :robot/mood (assoc mood id [:bad now 0])
                  you "send-0")
           (let [minutes-passed (- (/ now 1000 60) (/ ts 1000 60))
                 cicles (int (/ minutes-passed (U/contextualize-integer context minutes 1)))]
             (if (not= cnt cicles)
               (-> context
                   (assoc :robot/mood (assoc mood id [:bad ts (inc cnt)])
                          you (str "send-" (inc cnt))))
               (assoc context you "skip")))))))
   ui])

(defmethod ig/init-key :robot.core.operations/send-mail-opr-factory
  [_ ui]
  [(fn send-mail-opr-factory [{:keys [from to subject body host port user pass ssl timeout retry-count retry-delay]
                               :as   conf}]
     (log/debug :send-mail-opr-factory conf)
     (retry-fn
       send-mail-opr timeout retry-count retry-delay [context you]
       (log/debug :you you :send-mail)
       (let [body (U/contextualize context body)
             subject (U/contextualize context subject)
             from (U/contextualize context from)
             to (S/split (S/trim (U/contextualize context to)) #",")
             host (U/contextualize context host)
             user (U/contextualize context user)
             pass (U/contextualize context pass)
             ssl (U/contextualize-boolean context ssl true)
             port (U/contextualize-integer context port 25)
             smtp {:host host :port port :user user :pass pass :ssl ssl}
             content {:from from :to to :subject subject :body body}]
         (log/debug "SMTP: ")
         (log/debug smtp)
         (log/debug "Content: ")
         (log/debug content)
         (postal/send-message smtp content)
         (assoc context you "sent"))))
   ui])

(defmethod ig/init-key :robot.core.operations/gsheet-appender-opr-factory
  [_ ui]
  [(fn gsheet-appender-opr-factory[{:keys [client-secret-json oauth-store spreadsheet-id sheet-name as-row? data timeout retry-count retry-delay]
                                    :as   conf}]
     (log/debug :gsheet-appender-opr-factory conf)
     (retry-fn
      gsheet-appender-opr timeout retry-count retry-delay [context you]
      (log/debug :you you :gsheet-appender)
      (let [client-secret-json (U/contextualize context client-secret-json)
            oauth-store        (U/contextualize context oauth-store)
            spreadsheet-id     (U/contextualize context spreadsheet-id)
            sheet-name         (U/contextualize context sheet-name)
            as-row?            (U/contextualize-boolean context as-row? true)
            data               (S/split (S/trim (U/contextualize context data)) #",")]
        (log/debug "GSheetAppender: " {:client-secret-json client-secret-json
                                                  :oauth-store oauth-store
                                                  :spreadsheet-id spreadsheet-id
                                                  :sheet-name sheet-name
                                                  :as-row? as-row?})
        (assoc context you (gsheet/append client-secret-json oauth-store spreadsheet-id sheet-name as-row? data)))))
   ui])

(comment
 (fn [ctx]
   (try
     (let [
           auth (proxy [javax.mail.Authenticator] []
                  (getPasswordAuthentication []
                                             (javax.mail.PasswordAuthentication. (:email ctx) nil)))
           properties (doto
                       (java.util.Properties.)
                       (.setProperty "mail.smtp.host" (:mail-host ctx))
                       ;(.setProperty "mail.smtp.starttls.enable"  "true")
                       ;(.setProperty "mail.smtp.ssl" "true")
                       (.setProperty "mail.smtp.auth" "false")
                       (.setProperty "mail.smtp.port" (:mail-port ctx))
                       (.setProperty "mail.smtp.user" (:user ctx))
                       (.setProperty "mail.smtp.from" (:email ctx)))
           session (javax.mail.Session/getInstance properties auth)
           subject (str "Alerta DB Portal de Agentes")
           recipients (:recipients ctx)
           message (doto
                    (javax.mail.internet.MimeMessage. session)
                    (.addRecipients javax.mail.Message$RecipientType/TO recipients)
                    (.setSubject subject)
                    (.setText (:text ctx)))]
       (javax.mail.Transport/send message)
       "OK")
     (catch Exception e (str "Error: " (.getMessage e))))))

(defn str->fn-old [code]
  (log/debug (str "creating fn for:" code))
  (let [func (load-string (if (nil? code)
                            "(fn [ctx] \"undefined clojure code!\")"
                            (str
                              code)))
        func (eval func)]
    func))

(defn str->fn [code]
  (log/debug (str "creating fn for:" code))
  (let [code (if (nil? code)
               "(fn [ctx] \"undefined clojure code!\")"
               (str
                code))
        CODE (read-string (str "[" code "]"))
        _ (log/debug "CODE: " (pr-str CODE))
        forms (count CODE)]
    (if (and (= forms 1) (= 'fn (ffirst CODE)))
      (eval (if @extra-code
              (do (require '[extra-code :as EC]) (first CODE))
              (first CODE)))
      (fn [ctx]
        (log/warn "You clojure code: \n" code "\n is invalid!")
        "You clojure code MUST be ONE fn with ONE param ctx!"))))

(defn to-int
  ([n]
         (to-int n 0))
  ([n default]
   (try
     (Integer/parseInt
      (clojure.string/trim
       (clojure.string/trim-newline n)))
     (catch Exception e
       (.printStackTrace e)
       default))))

;Operación para código clojure
(defmethod ig/init-key :robot.core.operations/clojure-opr-factory
  [_ ui]
  [
   (fn clojure-opr-factory
     [{:keys [code]
       :as   conf}]
     (let [func (str->fn code)]
       (fn clojure-opr [context you]
         (try
           (let [result (func context)]
             (if (map? result)
               (let [you-val (get result you "ok")]
                 (merge context result {you you-val}))
               (assoc context you (str result))))
           (catch Throwable e
             (log/error e)
             (assoc context you (-> e .getMessage)))))))
   ui])


;Operación para código JS
(defmethod ig/init-key :robot.core.operations/js-opr-factory
  [_ ui]
  [(fn js-opr-factory
     [{:keys [code]
       :as   conf}]
     (let [ctx (Context/enter)
           scope (org.mozilla.javascript.tools.shell.Global. ctx) ;(.initStandardObjects ctx nil)
           func (.compileFunction ctx scope
                                  (if (nil? code)
                                    "function(ctx) {return \"undefined javascript code!\";}"
                                    code)
                                  "src" 1 nil)]
       (Context/exit)
       (fn js-opr [context you]
         (try
           (let [ctx (Context/enter)
                 context-str (json/json-str context)
                 jsonp (JsonParser. ctx scope)
                 jscontext (.parseValue jsonp context-str)
                 jscontext-arr (to-array [jscontext])
                 result (.call func ctx scope scope jscontext-arr)]
             (cond
               (string? result)
               (assoc context you (str result))

               (= (class result) org.mozilla.javascript.NativeObject)
               (let [cresult (into {} (map (fn [k] [(keyword k) (.get result k)]) (.keySet result)))
                     you-val (get cresult you "js-ok")]
                 (merge context cresult {you you-val}))

               :otherwise
               (assoc context you (str result))))
           (catch Throwable t (-> t .printStackTrace) (assoc context you (.getMessage t)))
           (finally (Context/exit))))))
   ui])



;Operación para código browser-opr
(defmethod ig/init-key :robot.core.operations/browser-opr-factory
  [_ ui]
  [(fn browser-opr-factory
     [{:keys [file indexes]
       :as   conf}]
     (fn browser-opr [context you]
       (let [set-indexes (into #{}
                               (map (fn [index]
                                      (Integer/parseInt index))
                                    (clojure.string/split (U/contextualize context indexes) #",")))
             result (try
                      (selenium-util/play-navigation
                        (selenium-util/create-firefox)
                        (selenium-util/create-nav (U/contextualize context file))
                        set-indexes)
                      (catch Exception ex
                        {you (.getCanonicalName (.getClass ex))}))]
         (assoc context you (str result)))))
   ui])


;Operación para código jagacy
(defmethod ig/init-key :robot.core.operations/jagacy3270-opr-factory
  [_ ui]
  [(fn jagacy3270-opr-factory
     [{:keys [file
              address
              port
              ssl?
              protocol]
       :as   conf}]
     (fn jagacy3270-opr [context you]
       (log/debug "Operación: 3270")
       (log/debug "archivo: " file)
       (log/debug "address: " (context address address))
       (log/debug "port: " (context port port))
       (log/debug "ssl: " (= "true" (context ssl? ssl?)))
       (log/debug "protocolo: " (context protocol protocol))
       (let [csv-lst (j3270/create-csv-lst (U/contextualize context file))
             csv-contextualized (doall
                                  (map (fn contextualize-map [csv-line]
                                         (update-in csv-line [2]
                                                    (fn [current]
                                                      (U/contextualize context current))))
                                       csv-lst))
             result (try
                      (merge {you "3270 ok"}
                             (j3270/execute-script csv-contextualized
                                                   (U/contextualize context address)
                                                   (U/contextualize-integer context port 23)
                                                   (U/contextualize-boolean context ssl? true)
                                                   (U/contextualize context protocol)))
                      (catch Exception ex
                        (.printStackTrace ex)
                        {you (.getCanonicalName (.getClass ex))}))]
         (merge context result))))
   ui])


;Operación para Twitter-opr
(defmethod ig/init-key :robot.core.operations/twitter-opr-factory
  [_ ui]
  [(fn twitter-opr-factory
     [{:keys [app-consumer-key app-consumer-secret access-token access-token-secret message]
       :as   conf}]
     (let [x 1]
       (fn twitter-opr [context you]
         (log/debug "Operación Twitter")
         (let [my-creds (tauth/make-oauth-creds
                          (U/contextualize context app-consumer-key)
                          (U/contextualize context app-consumer-secret)
                          (U/contextualize context access-token)
                          (U/contextualize context access-token-secret))
               result (try
                        (tapi/statuses-update :oauth-creds my-creds
                                              :params {:status (U/contextualize context message)})
                        (catch Exception ex
                          (.printStackTrace ex)
                          (.getCanonicalName (.getClass ex))))]
           (assoc context you (str result))))))
   ui])

; Como crear uno de estos:
; En la app de telegram, inicias un chat con @botfather, y de dices /newbot ... y le sigues.
; apuntas el APIToken y es el primer parametro a poner en el dialogo de la telegram-operation
; Liego metes al robot a un grupo y en el grupo le chatesas  '/getUpdates', luego te metes en un browsers
; https://api.telegram.org/bot<APIToken>/getUpdates
; copias le chat id del grupo, este es el segundo parametro del dialogo

; bot-channels asi:
;;{:api-tokens {"123123:asd123a2134" [bot-channel cmd-atom]}
; :cmd-atom {}}

(defmethod ig/init-key :robot.core.operations/telegram-opr-factory
  [_ ui]
  [(fn telegram-opr-factory
     [{:keys [bot-token chat-tokens message path]
       :as   conf}] ;
     (fn telegram-opr [{app :robot/app instance :robot/instance :as context} you]
       (log/debug "Operación Telegram " you)
       (let [bot-token (U/contextualize context bot-token)
             chat-tokens (S/split
                          (S/trim
                           (U/contextualize context chat-tokens)) #",")
             message (U/contextualize context message)
             message (subs message 0 (min 1024 (count message)))
             path (U/contextualize context path)]

         ;(telegram/start-server bot-token)
         (try
           (if-let [response (telegram/send-message bot-token chat-tokens message path)]
             (assoc context you "scheduled for sending")
             (assoc context you "telegram unavailable"))
         (catch Throwable e
           (assoc context you (str e)))))))
   ui])

(defmethod ig/init-key :robot.core.operations/cmd-telegram-opr-factory
  [_ ui]
  [(fn cmd-telegram-opr-factory
     [{:keys [bot-token chat-tokens]
       :as   conf}]
     (retry-fn cmd-telegram-opr 2000 1 100 [{app :robot/app instance :robot/instance :as context} you]
       (log/debug "Operación Telegram")
       (let [bot-token (U/contextualize context bot-token)
             chat-tokens (S/split
                          (S/trim
                           (U/contextualize context chat-tokens)) #",")]
         ;(telegram/start-server bot-token)
         (telegram/register-telegram-bot bot-token)
         (if-let [response (telegram/get-message bot-token chat-tokens app instance)]
           (assoc context you (reduce str (interpose "," response)))
           (assoc context you "no cmd available")))))
   ui])

#_(let [send-chan (chan 10)
      running-bots-atom (atom {})
      bot-state-atoms (atom {})]
  (go-loop []
    (try
      (let [[chat-tokens bot-token context message path app instance] (<! send-chan)
            message (U/contextualize context message)
            message (subs message 0 (min 1024 (count message)))]
        (log/debug "########### chats " chat-tokens)
        (telegram/send-message bot-token
                               chat-tokens
                               message ;(str [app instance] "\n" message)
                               (U/contextualize context path))
        (log/info "telegram sent: " (pr-str [chat-tokens bot-token message path])))
      (catch Exception ex
        (log/warn "telegram problem: " (.getCanonicalName (.getClass ex)))))
    (recur))

  ;Operación para Telegram


)

;Operación para Slack
(defmethod ig/init-key :robot.core.operations/slack-opr-factory
  [_ ui]
  [(fn slack-opr-factory
     [{:keys [webhook message]
       :as   conf}]
     (fn slack-opr [context you]
       (log/debug "WebHook: " webhook)
       (log/debug "MSG: " message)
       (let [params {:body (json/json-str {:text (U/contextualize context message)})}
             uri (U/contextualize context webhook)
             {:keys [status body]} @(http-client/post uri params)]
         (assoc context you (format "%d -> %s" status body)))
       ))
   ui])

;Operación para Skype
(defmethod ig/init-key :robot.core.operations/skype-opr-factory
  [_ ui]
  [(fn twitter-opr
     [{:keys [code]
       :as   conf}]
     (let [x 1]
       (log/debug "Skype OPR")
       ))
   ui])


;Operación para get-mail
(defmacro ctx-all
  ([fn p1 p2 k1]
   `[(~fn (~k1 ~p1) ~p2)])
  ([fn p1 p2 k1 & ks]
   `[(ctx-all ~fn ~p1 ~p2 ~k1)
     (ctx-all ~fn ~p1 ~p2 ~(first ks) ~@(rest ks))]))


(defmethod ig/init-key :robot.core.operations/get-mail-opr-factory
  [_ ui]
  [(fn get-mail-opr-factory
     [{:keys [host port protocol ssl email password subject-re from-re timeout retry-count retry-delay]
       :as   conf}]
     (retry-fn
       mail-opr timeout retry-count retry-delay [context you]
       (log/debug "Operación get-mail")

       (let [send-mail-impl (basic/get-mail-with-subject-and-from-matching
                              {:host       (U/contextualize context host)
                               :port       (U/contextualize-integer context port 996)
                               :protocol   (U/contextualize context protocol)
                               :ssl        (U/contextualize-boolean context ssl true)
                               :email      (U/contextualize context email)
                               :password   (U/contextualize context password)
                               :subject-re (U/contextualize context subject-re)
                               :from-re    (U/contextualize context from-re)})
             _ (log/debug (pr-str :send-mail send-mail-impl))]
         (-> context
             (assoc you (:subject send-mail-impl))
             (assoc (keyword (str (name you) "-from")) (:from send-mail-impl))))
       ))
   ui])

;Operación para OS-CMD
(defmethod ig/init-key :robot.core.operations/os-cmd-opr-factory
  [_ ui]
  [(fn os-cmd-opr-factory
     [{:keys [shell]
       :as   conf}]

     (fn os-cmd-opr [context you]
       (log/debug "Operación os-cmd")
       (log/debug "Ejecutando shell " shell)
       (assoc context you (:out (apply shell/sh (seq (.split (U/contextualize context shell) " ")))))
       ))
   ui])

(defn create-edn-event [context keys]
  (reduce
    (fn [result key]
      (assoc result key (key context (str "Key: [" key "] not found in context!"))))
    {:created (System/currentTimeMillis)}
    keys))

(defmethod ig/init-key :robot.core.operations/caudal-opr-factory
  [_ ui]
  [(fn caudal-opr-factory
     [{:keys [host port] ;message
       :as   conf}]
     (fn caudal-opr [context you]
       (let [host (U/contextualize context host)
             port (U/contextualize-integer context port 0)
             ;edn-event (->
              ;          (create-edn-event context [:robot/app :robot/instance :robot/mood :robot/current :robot/previous])
              ;          (assoc :msg (U/contextualize-edn context message)))
             ]
         (try
           (log/debug (str "SENDING1: " (pr-str context)))
           (with-open [d-socket (java.net.Socket. host port)
                       os (.getOutputStream d-socket)]
             ;(.write os (.getBytes (pr-str edn-event) "UTF-8"))
             (.write os (.getBytes (pr-str context) "UTF-8"))
             (.write os 10))
           (assoc context you "sent")
           (catch Exception e
             (assoc context you (str (class e) " -> " (.getMessage e))))))))
   ui])

(defmethod ig/init-key :robot.core.operations/sql-read-opr-factory
  [_ ui]
  [(fn sql-read-opr-factory
     [{:keys [classname subprotocol subname user password query timeout retry-count retry-delay]
       :as   conf}]
     (retry-fn
       sql-read-opr timeout retry-count retry-delay [context you]
       (sql/with-db-connection
         [con {:classname   (U/contextualize context classname)
               :subprotocol (U/contextualize context subprotocol)
               :subname     (U/contextualize context subname)
               :user        (U/contextualize context user)
               :password    (U/contextualize context password)}]
         (let [result (sql/query
                        con
                        [(U/contextualize context query)])]
           (assoc context you (pr-str result))))))
   ui])



(defmethod ig/init-key :robot.core.operations/play-sound-opr-factory
  [_ ui]
  [(fn play-sound-opr-factory
     [{:keys [path]
       :as   conf}]
     (fn play-sound-opr [context you]
       (let [result (try
                      (basic/play-sound (U/contextualize context path))
                      {you "ok"}
                      (catch Exception ex
                        {you (.getCanonicalName (.getClass ex))}))]
         (assoc context you result))))
   ui])



(defmethod ig/init-key :robot.core.operations/selfie-opr-factory
  [_ ui]
  [(fn selfie-opr-factory
     [{:keys [path]
       :as   conf}]
     (fn selfie-opr [context you]
       (log/debug "Operación Selfie")
       (log/debug "Creando Selfie en: " path)
       (let [robot (java.awt.Robot.)
             screenSize (.getScreenSize (java.awt.Toolkit/getDefaultToolkit))
             rectangle (java.awt.Rectangle. (.width screenSize) (.height screenSize))
             image (. robot createScreenCapture rectangle)
             file (java.io.File. (U/contextualize context path))
             _ (.mkdirs (.getParentFile file))
             output-stream (java.io.FileOutputStream. file)]
         (assoc context you (javax.imageio.ImageIO/write image "png" output-stream)))))
   ui])

(let [browsers-atm (agent {})]
  (defmethod ig/init-key :robot.core.operations/selenium-opr-factory
    [_ ui]
    [(fn selenium-opr-factory
       [{:keys [file new-browser? close-it? profile driver-type]
         :or {driver-type "chrome"}
         :as conf}]
       (log/warn "Calling selenium-opr-factory")
       (fn selenium-opr [context you]
         (if-let [selenium-conf (let [file-conf (U/contextualize context file)]
                                  (try
                                    (read-string (slurp file-conf))
                                    (catch Exception e
                                      (log/error "Error opening EDN browser configuration: "(pr-str {:file file-conf})))))]
           (let [driver-type (U/contextualize context driver-type)
                 profile (U/contextualize context profile)
                 profile (if (= profile "") nil  profile)
                 new-browser? (U/contextualize-boolean context new-browser? true)
                 close-it? (U/contextualize-boolean context close-it? true)
                 {:keys [out deltas png txt content]} (selenium-direct/exec-conf driver-type profile browsers-atm new-browser? close-it? selenium-conf context)
                 out (if (instance? Exception out) (.getMessage out) out)]
             (cond-> (dissoc context :robot/selenium-err :robot/selenium-err-txt :robot/selenium-deltas)
               out (assoc you out)
               png (assoc :robot/selenium-err png)
               txt (assoc :robot/selenium-err-txt txt)
               content (assoc :robot/selenium-content content)
               deltas (assoc :robot/selenium-deltas deltas)))
           (assoc (dissoc context :robot/selenium-err :robot/selenium-err-txt :robot/selenium-deltas) you "Error opening EDN browser configuration"))))
     ui]))

(defmethod ig/init-key :robot.core.operations/ldap-opr-factory
  [_ ui]
  [(fn ldap-opr-factory
     [{:keys [host port bind-dn password]
       :as conf}]
     (fn ldap-opr [context you]
       (let [port (Integer/parseInt (U/contextualize context port))
             host (U/contextualize context host)
             bind-dn (U/contextualize context bind-dn)
             password (U/contextualize context password)
             conn (new com.unboundid.ldap.sdk.LDAPConnection)]
         (try
           (doto conn
                 (.connect host port)
                 (.bind bind-dn password))
           (assoc context you "LDAP ok")
           (catch Exception e
             (assoc context you (str e ": " (.getMessage e))))
           (finally
             (.close conn))))))
   ui])

(defn- toInt [n]
  (try
    (Integer/parseInt n)
  (catch Exception e
    (log/warn "Depth of queue must by an integer! received:" n " using default 100")
    100)))

(defn- contextualize-queues [context queues]
  (let [queues (if (string? queues) (read-string queues) queues)]
    ;(pp/pprint queues)
    (reduce
     (fn [result [k v]]
       (let [k (U/contextualize context k)
             v (toInt (U/contextualize context v))]
         (if (= ":" (subs k 0 1))
           result
           (assoc result k v))))
     {}
     queues)))

(defn create-mq-result [errors]
  (if (seq errors)
    (reduce
     (fn [result [queue current-depth max-depth]]
       (str result (format "%s \t%d \t%d \n" queue current-depth max-depth)))
     "ERROR \nQueue \tdepth \tmax \n"
     (sort errors))
    "All queues within range"))

(fn [{:keys [host port qmanager profundidad] :as ctx} you]
  (let [msg (reduce
             (fn [result [queue current-depth max-depth]]
               (str result (format "%s \t%d \t%d \n" queue current-depth max-depth)))
             "ERROR \nQueue \tdepth \tmax \n"
             (sort profundidad))]
    (assoc ctx
           :error
           (str "Colas fuera de rango en el servidor " host ":" port " qmanager:" qmanager "\n\n" msg))))

(defmethod ig/init-key :robot.core.operations/mq-opr-factory
  [_ ui]
  [(fn mq-opr-factory [{:keys [host port qmanager channel queues] :as conf}]
     (fn mq-opr [context you]
       (try
         (let [openOptions (+ 32 8192 2)
               port (Integer/parseInt (U/contextualize context port))
               host (U/contextualize context host)
               qmanager (U/contextualize context qmanager)
               channel (U/contextualize context channel)
               queues (contextualize-queues context queues)]
           (let [props (doto (java.util.Hashtable.)
                             (.put "hostname" host)
                             (.put "port" port)
                             (.put "channel" channel))
                 d-qmgr (com.ibm.mq.MQQueueManager. qmanager props)]
             (try
               (let [errors (reduce
                             (fn [errors [queue depth]]
                               ;(.accessQueue d-qmgr queue openOptions)
                               (let [current-depth (with-open [MQQueue (.accessQueue d-qmgr queue openOptions)
                                                               ;(com.ibm.mq.MQQueue. d-qmgr queue openOptions qmanager "" "")
                                                               ]
                                                     (.getCurrentDepth MQQueue))]
                                 (if (> current-depth depth)
                                   (conj errors [queue current-depth depth])
                                   errors)))
                             []
                             queues)]
                 (assoc context you (if (seq errors) errors "ok")))
               (catch Exception e
                 (assoc context you (str e " : " (.getMessage e))))
               (finally
                 (.disconnect d-qmgr)))))
       (catch Exception e
         (assoc context you (str e " : " (.getMessage e)))))))
   ui])
