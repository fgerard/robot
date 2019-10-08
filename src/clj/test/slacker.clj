(ns test.slacker
  (:require [clojure.java.io :as io]
            [clojure.string :as S]
            [clojure.tools.logging :as log]
            [clojure.pprint :as pp]
            [cheshire.core :refer [generate-string parse-string]]
            [aleph.http :as http]
            [manifold.deferred :as d]
            [manifold.stream :as stream]
            [byte-streams :as bs]))

(def YMSBOT {:base-url "https://slack.com/api"
             :app-id "AP52AFF4G"
             :client-id "518720992496.787078525152"
             :client-secret "6e054c387384a8f39359dfec9e594639"
             :signing-secret "af3dd77b757858e8ff249deb3aea8bde"
             :verification-token "f3OjBWMVBVIzeZDlvSM2SyV0"
             :webhook-url "https://hooks.slack.com/services/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"
             :OAuth-access-token "xoxp-518720992496-518751457248-786673334900-84e5980159eec9776d7d3ef16a1e3e1d"
             :token "xoxb-518720992496-776481527283-hgujMvfWfhalsMzpUWiLiEyp"})

(def http-pool (http/connection-pool {:connection-options {:insecure? true}}))

(defn post-message [event]
  (try
    (let [http-params (cond->
                       {:pool http-pool
                        :body (generate-string event)
                        :content-type :json
                        :request-timeout 3000
                        :throw-exceptions? false})
          result @(http/post (:webhook-url YMSBOT) http-params)
          result (assoc result :body (slurp (:body result)))]
      result)
  (catch Exception e
    (log/error e))))

#_(post-message {:text "Segundo saludo ahora ya desde clojure!  :)"})

#_(let [conn @(http/websocket-client "ws://localhost:10000/echo")])

(defn get-api-response
  "Takes a full http response map and returns the api response as a map."
  [http-response]
  (let [response-body-bytes (:body http-response)
        response-body-json (bs/to-string response-body-bytes)
        api-response (parse-string response-body-json true)]
    api-response))

(defn call-slack-web-api
  ([method-name]
   (call-slack-web-api method-name {}))
  ([method-name params]
   (let [method-url-base (str (:base-url YMSBOT) "/" method-name)]
     @(http/post method-url-base {:query-params params}))))

(comment
 ;scope: read post
 (require
  '[clojure.pprint :as pp]
  '[cheshire.core :refer [generate-string parse-string]]
  '[aleph.http :as http]
  '[manifold.deferred :as d]
  '[manifold.stream :as stream])

 )

; este sí jaló despues de que pasas por las MUCHAS opciones en la pagina
; https://api.slack.com/apps
;
#_(let [response (->> {:token (:token YMSBOT)
                       :scope "bot channels:read read post"}
                      (call-slack-web-api "rtm.connect")
                      (get-api-response))
        wss-url (:url response)
        conn @(http/websocket-client wss-url)
        ]
    (loop [n 6]
      (let [msg (-> @(stream/take! conn)
                    parse-string)]
        (pp/pprint msg)
        #_(let [m {:type "message"
                 :text "Como estás"
                 :channel "DNT6VHLCS"
                 }
              _ (println "MESSAGE: " (generate-string m))
              r (stream/put! conn (generate-string m))]
          (pp/pprint r))
        (if (> n 0)
          (recur (dec n)))
        )))


#_(->> {:token (:token YMSBOT)
        :scope "bot channels:read"}
       (call-slack-web-api "channels.list")
       (get-api-response))

#_(let [url "https://slack.com/api"
        token ""])

#_(let [url "https://slack.com/oauth/authorize"
        params (str "?client_id=" (:client-id YMSBOT)
                    "&scope=bot")
        response @(http/get (str url params))]
    (get-api-response response))

(comment
 "files:write"
 "chat:write:bot"
 "post"
 "bot"
 ;https://slack.com/oauth/authorize

;The following values should be passed as GET parameters:

;client_id - issued when you created your app (required)
;scope - permissions to request (see below) (required)
;redirect_uri - URL to redirect back to (see below) (optional)
;state - unique string to be passed back upon completion (optional)
;team - Slack team ID of a workspace to attempt to restrict to (optional)

 )
