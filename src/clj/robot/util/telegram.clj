(ns robot.util.telegram
  (:require [aleph.http :as http]
            [byte-streams :as bs]
            [clojure.pprint :as pp]
            [clojure.data.json :as json]
            [clojure.java.io :as io]
            [clojure.tools.logging :as log]
            [clojure.string :as S]
            [clojure.core.async :refer [go go-loop put! <!! >!!  >! <! chan timeout alts! poll! sliding-buffer close!] :as async]
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
       (log/error e)
       (log/error "Problems in telegram post: " (str base-url token "/sendMessage "  (into {:chat_id chat-id :text text} options)))))))

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


; Este atomo tiene la siguiente estructura ejemplo:
{"token1" {:lease-ts 2331213123 ; este se refresca cada vez que un cmd-telegram-opr es ejecutado si nadie renuava esto en 5 min se apaga la maquinaria de lectura
           "chat-id1" {"app1" {"instance1" {:channel "este es un chan de core.async con sliding buffer"
                                            }
                               }
                       }
           "chat-id2" {}
           }
 "token2" {
           }
 }

(def telegram-bots (atom {}))

(defn get-message [token chat-ids app instance]
  (loop [[chat-id & remaining] chat-ids]
    (when chat-id
      (log/debug "get-message1: " chat-id app instance)
      (let [d-chan  (get-in @telegram-bots [token chat-id app instance :channel])
            _ (log/debug "get-message2 d-chan: " d-chan)
            d-msg (if d-chan (poll! d-chan))]
        (log/debug "get-message3: " d-msg)
        (if (seq d-msg)
          d-msg
          (recur remaining))))))

(defn parser-cmd [text]
  (try
    (if text
      (if (S/starts-with? text "/")
        (let [[app instance & params] (S/split text #"\s+")]
          {:app (subs app 1) :instance instance :params params})))
  (catch Exception e
    (log/warn "problema en parser-cmd: " text)
    (log/warn e))))

(defn telegram-poller [token params]
  (let [URL (str base-url token "/getUpdates")]
    (try
      (let [_ (log/debug {:request-method "get" :url URL :request-timeout 15000 :query-params params})
            result @(http/request {:request-method "get" :url URL :request-timeout 15000 :query-params params})
            body-str (slurp (:body result))]
        (json/read-str body-str :key-fn keyword))
      (catch Throwable e
        (log/error e)
        (log/error "Past error on: " URL params)))))

(defn get-or-create-channel-of [token chat-id app instance]
  (let [bots (swap! telegram-bots
                    update-in
                    [token chat-id app instance :channel]
                    #(or
                      %
                      (chan (sliding-buffer 5))))]
    (get-in bots [token chat-id app instance :channel])))

(defn should-recur? [bot-token]
  (< (- (System/currentTimeMillis) (get-in @telegram-bots [bot-token :lease-ts])) 300000))

(defn remove-bot [bots bot-token]
  (log/info "Removing bot loop: " bot-token)
  (dissoc bots bot-token))

(defn calc-offset [messages]
  (if (seq messages)
    (-> messages last :update_id inc)
    0))

(defn start-bot-poll-server [bot-token]
  (go-loop
   [offset 0 limit 100]
   (let [params {:timeout 10 :offset offset :limit limit}
         {:keys [ok result] :as data} (telegram-poller bot-token params)]
     (when-not ok
       (log/warn "Problems comunicating with telegram, wait 2 min.")
       (<! (timeout 120000)))
     (if ok
       (let [robot-info-fn (get state/system [:robot.core.essentials/robot-info :essentials/robot-info])
             robot-info (robot-info-fn)]
         (doseq [message result]
           (let [{:keys [app instance params] :as parsed} (parser-cmd (get-in message [:message :text]))
                 stored?    (contains? (into #{} (get robot-info :stored)) app)
                 running?   (not (nil? (get-in robot-info [:ready app instance])))
                 chat-id (str (get-in message [:message :chat :id]))]
             (cond
               (and stored? running? parsed)
               (let [d-chan (get-or-create-channel-of bot-token chat-id app instance)]
                 (send-text bot-token chat-id (str "Procesing /" app " " instance " "  params))
                 (>!! d-chan params))

               (and stored? running?)
               (log/warn "Invalid message:" message)

               (and stored? (not running?))
               (send-text bot-token chat-id (str "I'm not running: " (pr-str [app instance]) "!"))

               :OTHERWIZE
               (send-text bot-token chat-id (str "I don't find: " (pr-str [app instance]) "!")))))))

     (if (should-recur? bot-token)
       (recur (calc-offset result) limit)
       (swap! telegram-bots remove-bot bot-token)))))

(defn startORrenew-bot [bots-data bot-token]
  (when-not (get bots-data bot-token)
    (start-bot-poll-server bot-token))
  (assoc-in bots-data [bot-token :lease-ts] (System/currentTimeMillis)))

(defn register-telegram-bot [bot-token]
  (swap! telegram-bots startORrenew-bot bot-token)
  (log/debug "Bot registrado: " bot-token))
