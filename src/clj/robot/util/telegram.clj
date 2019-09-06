(ns robot.util.telegram
  (:require [aleph.http :as http]
            [byte-streams :as bs]
            [clojure.data.json :as json]
            [clojure.java.io :as io]
            [clojure.tools.logging :as log]
            [clojure.string :as S]
            [clojure.core.async :refer [go go-loop put! <!! >!!  >! <! chan timeout alts! close!] :as async]
            [robot.core.state :as state]))

(def base-url "https://api.telegram.org/bot")

(defn send-text
  "Sends message to the chat"
  ([token chat-id text] (send-text token chat-id {} text))
  ([token chat-id options text]
   (try
     (let [url  (str base-url token "/sendMessage")
           body (into {:chat_id chat-id :text text} options)
           resp @(http/request {:request-method "post"
                                :url url
                                :headers {"Content-Type" "application/json"}
                                :query-params body})]
       (-> resp :body))
     (catch Throwable e
       (log/error e)))))

(defn send-file [token chat-id options file method field filename]
  "Helper function to send various kinds of files as multipart-encoded"
  (try
    (let [url          (str base-url token method)
          base-form    [{:part-name "chat_id" :content (str chat-id)}
                        {:part-name field :content file :name filename}]
          options-form (for [[key value] options]
                         {:part-name (name key) :content value})
          form         (into base-form options-form)
          resp         @(http/request {:request-method "post"
                                       :url url
                                       :headers {"Content-Type" "application/json"}
                                       :multipart form})]
      (-> resp :body))
    (catch Throwable e
       (log/error e))))

(defn send-photo [token chat-id options image]
  (send-file token chat-id options image "/sendPhoto" "photo" "photo.png"))

(defn send-message [bot-token chat-ids text path]
  (doall
    (map (fn [chat-id]
           (try
             (log/debug "Enviar desde: " bot-token)
             (log/debug "Enviar a: " chat-id)
             (log/debug "Mensaje: " text)
             (log/debug "img: " path)
             (if (and path (not= path ""))
               (let [imagen (java.io.File. path)]
                 (if (.exists imagen)
                   (send-photo bot-token chat-id
                               {:caption text}
                               imagen)
                   (let [decoded (.decode (java.util.Base64/getDecoder) path)
                         imagen (java.io.File/createTempFile "robot" ".png")
                         _ (log/debug "created:" imagen)]
                     (with-open [out (java.io.FileOutputStream. imagen)]
                       (.write out decoded))
                     (send-photo bot-token chat-id
                                     {:caption text}
                                     imagen))))
               (send-text bot-token chat-id text))
             (catch Throwable e
               (-> e .printStackTrace)
               (log/warn "Problem with chat-id: " chat-id))))
         chat-ids)))

(comment
  {"token" {"chat-id" {:buffer-size 5 :buffer []}}})

(def started-bot (atom {}))
(def received-message-buffer (atom {}))

(defn push-message [token chat-id app instance message]
  (let [robot-info-fn (get state/system [:robot.core.essentials/robot-info :essentials/robot-info])
        robot-info (robot-info-fn)
        stored?    (contains? (into #{} (get robot-info :stored)) app)
        running?   (not (nil? (get-in robot-info [:ready app instance])))]
    (log/info {:app app :instance instance :stored? stored? :running? running?})
    (if (and stored? running?)
      (let [_ (send-text token chat-id (str "Processing " (pr-str message) " to " (pr-str [app instance])))]
        (swap! received-message-buffer
               update-in
               [token chat-id app instance]
               (fn [{:keys [buffer-size buffer] :as old}]
                 (if old
                   (let [length (count buffer)
                         new-buffer (if (< length buffer-size)
                                      (conj buffer message)
                                      (conj (vec (take-last (dec buffer-size) buffer)) message))]
                     {:buffer-size buffer-size :buffer new-buffer})
                   {:buffer-size 5 :buffer [message]}))))
      (if (and stored? (not running?))
        (send-text token chat-id (str "I'm not running: " (pr-str [app instance]) "!"))
        (send-text token chat-id (str "I don't find: " (pr-str [app instance]) "!"))))))

(defn pull-message [token chat-id app instance]
  (let [response (chan)]
    (go
      (swap! received-message-buffer
             update-in
             [token chat-id app instance]
             (fn [{:keys [buffer-size buffer] :as old}]
               (if old
                 (let [return (first buffer)
                       new-buffer (vec (rest buffer))]
                   (>!! response (or return :empty))
                   {:buffer-size buffer-size :buffer new-buffer})
                 (do
                   (>!! response :empty)
                   {:buffer-size 5 :buffer []})))))
    (<!! response)))

(defn get-message [token chat-ids app instance]
  (log/debug (pr-str @received-message-buffer))
  (reduce (fn [_ chat-id]
            (let [response (pull-message token chat-id app instance)]
              (if (not= response :empty)
                (reduced response))))
          nil chat-ids))

(defn poller [url params]
  (try
    (let [result @(http/request {:request-method "get" :url url :request-timeout 3000 :query-params params})
          body-str (slurp (:body result))]
      (log/debug "poler: body: " body-str)
      (json/read-str body-str :key-fn keyword))
    (catch Throwable e
      (log/error e))))

(defn new-offset
  "Returns new offset for Telegram updates"
  [result default]
  (if (and result (< 0 (count result)))
      (-> result last :update_id inc)
      default))

(defn parser-cmd [text]
  (try
    (if text
      (if (S/starts-with? text "/")
        (let [[app instance & params] (S/split text #"\s+")]
          {:app (subs app 1) :instance instance :params params})))
  (catch Exception e
    (log/warn "problema en parser-cmd: " text)
    (log/warn e))))

(defn start-server [token]
  (log/debug "telegram start-server 1) " @started-bot token)
  (log/debug "telegram start-server 2) " (get @started-bot token))
  (locking started-bot
           (let [status (get @started-bot token )]
             (log/debug "telegram status:" status)
             (if (and status (or (> status 0) (> 300000 (+ (System/currentTimeMillis) status))))
               (if (< status 0)
                 (log/info (format "telegram with problems retrying in %d s, token: %s"  (- 300000 (+ (System/currentTimeMillis) status)) token))
                 (log/debug "telegram already running Bot Server, token: " token))
               ;else !!
               (let [_ (swap! started-bot assoc token (System/currentTimeMillis))
                     url (str base-url token "/getUpdates")]
                 (log/info "telegram starting Bot Server, token: " token)
                 (go-loop [offset 0 limit 100]
                          (log/debug "telegram start-server 3.0 ")
                          (let [params {:timeout 1 :offset offset :limit limit}
                                {:keys [ok result] :as data} (try
                                                               (poller url params)
                                                               (catch Exception e
                                                                 (log/error "telegram start-server 3.1")
                                                                 (log/error e))) ]
                            (log/debug "telegram start-server 3.2) " ok result)
                            (if (and ok (try
                                          (dorun (map (fn [message]
                                                        (log/debug "message: " message)
                                                        (let [{:keys [app instance params] :as parsed} (parser-cmd (get-in message [:message :text]))
                                                              chat-id (str (get-in message [:message :chat :id]))]
                                                          (log/debug "incomming message:" (pr-str [token chat-id app instance params]))
                                                          (if parsed
                                                            (push-message token chat-id app instance params)
                                                            (log/warn message))))
                                                      result))
                                          (<! (timeout 1000))
                                          true
                                          (catch Throwable e
                                            (log/error "start-server 4)")
                                            (log/error e)
                                            false)))
                              (recur (new-offset result offset) limit)
                              (do
                                (log/error "start-server:" data)
                                (swap! started-bot assoc token (- (System/currentTimeMillis)))))))
                 (log/debug "telegram start-server 4)"))))))
