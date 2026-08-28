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
           ;; Sin :parse_mode -- texto plano. Ningun caller de send-text/send-message
           ;; en este repo (el /help del bot, el "Procesing /app instance", ni el
           ;; operador generico de telegram en operations.clj) escapa los
           ;; caracteres reservados de Markdown (. - ! ( ) _ * [ ] ~ ` > # + = | { }),
           ;; asi que :parse_mode "MarkdownV2" tumbaba el envio con "can't parse
           ;; entities" en cuanto el texto traia alguno (ej. nombres de apps/
           ;; instancias con puntos o guiones) -- el error se logueaba pero el
           ;; usuario nunca veia el mensaje en Telegram.
           ;; Si algun dia se quiere Markdown real: pasar {:parse_mode "MarkdownV2"}
           ;; en options (se mezcla encima de este mapa) Y escapar a mano esos
           ;; caracteres en las partes dinamicas del texto antes de mandarlo.
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

;; Topes de la API de Telegram: sendMessage admite 4096 caracteres de texto,
;; pero el caption de sendPhoto solo 1024. De ahi salia el 1024 que antes se
;; aplicaba a todo mensaje, con o sin imagen.
(def ^:const TEXT-LIMIT 4096)
(def ^:const CAPTION-LIMIT 1024)

(def ^:const DEFAULT-MAX-MESSAGES 5)

(defn escape-html [s]
  (-> (str s)
      (S/replace "&" "&amp;")
      (S/replace "<" "&lt;")
      (S/replace ">" "&gt;")))

(defn- safe-cut
  "Punto de corte que no parte una entidad html (&amp; y demas) a la mitad."
  [s n]
  (let [head (subs s 0 n)
        amp  (S/last-index-of head "&")]
    (if (and amp (> amp (- n 8)) (not (S/includes? (subs head amp) ";")))
      amp
      n)))

(defn- hard-split [s n]
  (loop [s s out []]
    (if (<= (count s) n)
      (conj out s)
      (let [cut (max 1 (safe-cut s n))]
        (recur (subs s cut) (conj out (subs s 0 cut)))))))

(defn- pack
  "Empaca lineas completas en trozos de a lo mas max-length. Una linea que por
   si sola no cabe se parte a la fuerza."
  [lines max-length]
  (loop [[line & more] lines
         cur nil
         out []]
    (cond
      (nil? line)
      (if cur (conj out cur) out)

      (> (count line) max-length)
      (let [pieces (hard-split line max-length)]
        (recur more
               (last pieces)
               (into (if cur (conj out cur) out) (butlast pieces))))

      (nil? cur)
      (recur more line out)

      (<= (+ (count cur) 1 (count line)) max-length)
      (recur more (str cur "\n" line) out)

      :else
      (recur more line (conj out cur)))))

(defn- cap-pages
  "Deja a lo mas max-pages trozos y marca en el ultimo cuanto se quedo fuera."
  [pages max-length max-pages total-chars]
  (if (<= (count pages) max-pages)
    pages
    (let [tmpl "\n… (truncado, %s caracteres omitidos)"
          reserve (count (format tmpl (str total-chars)))
          kept (vec (take max-pages pages))
          room (max 0 (- max-length reserve))
          trimmed (let [p (peek kept)]
                    (if (> (count p) room) (subs p 0 room) p))
          kept (conj (pop kept) trimmed)
          omitted (max 0 (- total-chars (count (S/join "\n" kept))))]
      (conj (pop kept) (str trimmed (format tmpl omitted))))))

(defn split-pages
  "Parte text en a lo mas max-pages trozos de max-length caracteres, cortando en
   saltos de linea para no serruchar un renglon a la mitad. Con mas de un trozo
   cada uno se numera [n/N]; el prefijo se descuenta de max-length, calculado
   sobre max-pages porque N nunca lo rebasa."
  [text max-length max-pages]
  (let [lines (S/split-lines (or text ""))]
    (if (<= (count (pack lines max-length)) 1)
      (pack lines max-length)
      (let [prefix-size (count (str "[" max-pages "/" max-pages "]\n"))
            room (max 1 (- max-length prefix-size))
            pages (cap-pages (pack lines room) room max-pages (count text))
            total (count pages)]
        (vec (map-indexed (fn [i p] (str "[" (inc i) "/" total "]\n" p)) pages))))))

(defn parse-mode
  "mono se manda como HTML porque el texto va envuelto en <pre>. Cualquier otro
   valor es texto plano, sin parse_mode, como se mandaba siempre.

   HTML y no MarkdownV2 a proposito: aqui solo hay que escapar & < > en vez de
   los 18 reservados de MarkdownV2, que es justo lo que tumbaba los envios con
   'can't parse entities' (ver el comentario de send-text)."
  [fmt]
  (when (= "mono" (str fmt)) "HTML"))

(defn pages-for
  "Parte el texto en mensajes listos para enviar segun el formato.

   En mono se escapa ANTES de partir, porque escapar despues puede pasarse del
   limite, y cada trozo se envuelve en su propio <pre>: un <pre>...</pre>
   partido a la mitad deja etiquetas rotas en los dos mensajes y Telegram
   rechaza las dos."
  [text fmt max-length max-pages]
  (if (= "mono" (str fmt))
    (let [open "<pre>" close "</pre>"
          room (max 1 (- max-length (count open) (count close)))]
      (mapv #(str open % close)
            (split-pages (escape-html text) room max-pages)))
    (split-pages text max-length max-pages)))

(defn send-message
  ([bot-token chat-ids text path]
   (send-message bot-token chat-ids text path {}))
  ([bot-token chat-ids text path options]
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
                               (assoc options :caption text)
                               imagen)
                   (let [decoded (.decode (java.util.Base64/getDecoder) path)
                         imagen (java.io.File/createTempFile "robot" ".png")
                         _ (log/debug "created:" imagen)]
                     (with-open [out (java.io.FileOutputStream. imagen)]
                       (.write out decoded))
                     (send-photo bot-token chat-id
                                     (assoc options :caption text)
                                     imagen))))
               (send-text bot-token chat-id options text))
             (catch Throwable e
               (-> e .printStackTrace)
               (log/warn "Problem with chat-id: " chat-id))))
         chat-ids))))


; Este atomo tiene la siguiente estructura ejemplo:
{"token1" {:lease-ts 2331213123 ; este se refresca cada vez que un cmd-telegram-opr es ejecutado si nadie renuava esto en 5 min se apaga la maquinaria de lectura
           "chat-id1" {"app1" {"instance1" {:channel "este es un chan de core.async con sliding buffer"}}}
                                            
                               
                       
           "chat-id2" {}}
           
 "token2" {}}
           
 

(def telegram-bots (atom {}))

(def ^:const FILE-MARK "tg-file:")

;; Los nombres los generamos aqui, con esta forma exacta, y la limpieza solo borra
;; lo que casa con ella. El file_path que manda Telegram nunca toca el disco: es
;; texto ajeno, y ademas data/tmp lo comparten otros (get-profile-media escribe sus
;; image-N.jpg ahi), asi que un barrido por edad a secas se llevaria cosas vivas.
(def image-name-rx #"^tg-\d+-[0-9a-f]{8}\.[a-z0-9]{1,5}$")

(defn message-file-id
  "file_id de la foto mas grande, o del documento. nil si el mensaje no trae archivo."
  [msg]
  (or (some->> (:photo msg) (sort-by #(or (:file_size %) 0)) last :file_id)
      (some-> (:document msg) :file_id)))

(defn- file-path-of [token file-id]
  (let [url (str base-url token "/getFile")
        resp @(http/request {:request-method "get" :url url :query-params {:file_id file-id}})]
    (-> resp :body bs/to-string (json/read-str :key-fn keyword) :result :file_path)))

(defn download-file!
  "Baja file-id a dir y devuelve la ruta absoluta, o nil si no se pudo."
  [token file-id dir]
  (try
    (if-let [file-path (file-path-of token file-id)]
      (let [ext  (let [e (S/lower-case (or (last (S/split file-path #"\.")) ""))]
                   (if (re-matches #"[a-z0-9]{1,5}" e) e "jpg"))
            name (format "tg-%d-%08x.%s" (System/currentTimeMillis)
                         (rand-int Integer/MAX_VALUE) ext)
            dest (io/file dir name)
            url  (str "https://api.telegram.org/file/bot" token "/" file-path)
            body (-> @(http/request {:request-method "get" :url url}) :body bs/to-byte-array)]
        (io/make-parents dest)
        (io/copy body dest)
        (log/info :telegram-download (.getPath dest) :bytes (count body))
        (.getPath dest))
      (do (log/warn "getFile no devolvio file_path para" file-id) nil))
    (catch Throwable e
      ;; getFile no pasa de 20 MB; ahi es donde cae lo que manda alguien con un video.
      (log/error e "no se pudo bajar" file-id)
      nil)))

(def ^:const SWEEP-EVERY-MS 3600000)
(defonce ^:private last-sweep (atom {}))

(defn sweep-images!
  "Borra de dir los archivos con la forma tg-... mas viejos que max-age-mins.
   Se rinde solito si ya barrio ese dir en la ultima hora."
  [dir max-age-mins]
  (when-not (S/blank? (str dir))
    (let [now (System/currentTimeMillis)
          [old _] (swap-vals! last-sweep update dir
                              (fn [t] (if (or (nil? t) (> (- now t) SWEEP-EVERY-MS)) now t)))
          prev (get old dir)]
      (when (or (nil? prev) (> (- now prev) SWEEP-EVERY-MS))
        (try
          (let [cutoff (- now (* 60000 (long max-age-mins)))
                borrados (->> (.listFiles (io/file dir))
                              (filter #(and (.isFile %)
                                            (re-matches image-name-rx (.getName %))
                                            (< (.lastModified %) cutoff)))
                              (filter #(.delete %))
                              count)]
            (when (pos? borrados)
              (log/info :telegram-sweep dir :borrados borrados)))
          (catch Throwable e
            (log/warn e "no se pudo limpiar" dir)))))))

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
    (when text
      (if (S/starts-with? text "/")
        (let [[app instance & params] (S/split text #"\s+")]
          {:app (subs app 1) :instance instance :params params})
        (log/info (pr-str [:skip :telegram text]))))
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
  (if-let [lease-ts (get-in @telegram-bots [bot-token :lease-ts])]
    (< (- (System/currentTimeMillis) lease-ts) 300000)
    false))

(defn remove-bot [bots bot-token]
  (log/info "Removing bot loop: " bot-token)
  (dissoc bots bot-token))

(defn calc-offset [messages]
  (if (seq messages)
    (-> messages last :update_id inc)
    0))

(defn inst-running [m]
  (reduce (fn [R [k v]]
            (assoc R k (if v true false)))
          (sorted-map)
          m))

(defn create-apps-instances-menu [{:keys [ready]}]
  (reduce (fn [R [k v]]
            (assoc R k (inst-running v)))
          (sorted-map)
          ready))

(defn create-apps-msg-str [robot-info]
  (let [m (create-apps-instances-menu robot-info)]
    (with-out-str
      (doseq [[app insts] m]
        (println app)
        (doseq [[inst stat] insts]
          (println (if stat "  ! " "  x ") inst ))
        (println)))))

(defn start-bot-poll-server [bot-token]
  (go-loop
   [offset 0 limit 100]
   (let [params {:timeout 10 :offset offset :limit limit}
         {:keys [ok result] :as data} (telegram-poller bot-token params)]
     (log/debug (pr-str data))
     (when-not ok
       (log/warn "Problems comunicating with telegram, wait 2 min.")
       (<! (timeout 120000)))
     (try
       (when ok
         (let [robot-info-fn (get state/system [:robot.core.essentials/robot-info :essentials/robot-info])
               robot-info (robot-info-fn)]
           (doseq [message result]
             (log/debug (pr-str [:start-bot-poll message]))
             (let [msg (:message message)
                   ;; Una foto no trae :text -- el comando viene en :caption.
                   {:keys [app instance params] :as parsed} (parser-cmd (or (:text msg) (:caption msg)))
                   file-id (message-file-id msg)
                   ;; El file_id viaja como un parametro mas y lo baja la operacion,
                   ;; que es la que tiene configurado el directorio. Aqui no: este
                   ;; go-loop corre en el pool fijo de core.async y una descarga
                   ;; lenta detendria el poleo de todos los bots.
                   params (if file-id
                            (concat (or params []) [(str FILE-MARK file-id)])
                            params)
                   stored? (contains? (into #{} (:stored robot-info)) app)
                   running?   (not (nil? (get-in robot-info [:ready app instance])))
                   chat-id (str (:id (:chat msg)))]
               (cond
                 (= app "help")
                 (send-text bot-token chat-id (create-apps-msg-str robot-info))

                 (and stored? running? parsed)
                 (let [d-chan (get-or-create-channel-of bot-token chat-id app instance)]
                   (send-text bot-token chat-id (str "Procesing /" app " " instance " "  params))
                   (if params 
                     (>!! d-chan params)
                     (send-text bot-token chat-id  "Add params to send!")))

                 (and stored? running?)
                 (log/warn "Invalid message:" message)

                 (and stored? (not running?))
                 (send-text bot-token chat-id (str "I'm not running: " (pr-str [app instance]) "!, try /help"))

                 ;; Foto sin pie: no hay a donde mandarla. Antes caia en el
                 ;; "I don't find: [nil nil]", que no dice que hacer.
                 (and file-id (nil? parsed))
                 (send-text bot-token chat-id
                            "Manda el comando en el pie de foto, p.ej. /get profile 522")

                 :OTHERWIZE
                 (send-text bot-token chat-id (str "I don't find: " (pr-str [app instance]) "!, try /help")))))))

       (catch Exception e
         (swap! telegram-bots remove-bot bot-token)
         (log/error e)))

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
