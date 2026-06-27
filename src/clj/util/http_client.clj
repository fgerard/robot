(ns util.http-client
  (:refer-clojure :exclude [get])
  (:require [clojure.tools.logging :as log]
            [aleph.http :as http]
            [aleph.http.client-middleware :as middleware]
            [byte-streams :as bs]
            [manifold.deferred :as d]))

;(def application-edn "application/edn"); OJO nos sabemos; charset=utf-8")

(defn application-edn? [header]
  (and header (re-matches #"^application/edn.*" header)))

(defn wrap-header [client header value]
  (fn [req]
    (if-let [cur-accept (get-in req [:headers header])]
      (let [_ (log/debug (pr-str {:header-already-defined {header cur-accept}}))]
        (client req))
      (client (assoc-in req [:headers header] value)))))

(defn wrap-body [client]
  (fn [req]
    (if (application-edn? (get-in req [:headers :content-type]))
      (client (assoc req :body (pr-str (:body req))))
      (client req))))

(defn content-type-selector [response]
  (log/debug :response response)
  (and (application-edn? (get-in response [:headers "content-type"]))
       :application/edn))

(defmulti wrap-response content-type-selector)

(defmethod wrap-response :default [{:keys [body] :as resp}]
  (log/debug {:nothing-todo {:status (:status resp)}})
  (let [body (if (instance? java.io.ByteArrayInputStream body)
               (bs/to-string body)
               body)]
    (log/debug "El body es: " body)
    (assoc resp :body body)))

(defmethod wrap-response :application/edn [{:keys [body] :as resp}]
  (if-let [body-str (and body (instance? java.io.InputStream body) (bs/to-string body))]
    (let [body (and (> (.length body-str) 0) (read-string body-str))]
      (assoc resp :body body))
    resp))

(defn wrap-exception [client responsXform]
  (fn [req]
    (-> req
        (d/chain client)
        (d/catch clojure.lang.ExceptionInfo (fn [e]
                                              (println :400?? (.getData e) :req req)
                                              (.getData e)))
        (d/catch Exception (fn [e]
                             {:status 500 :body (str "Can't connect " (.getMessage e))}))
        (d/chain responsXform))))

(defn test-deferred [n]
  (-> n
      (d/chain inc inc #(+ 3 %)
               #(if (> % 6) (throw (java.lang.Exception. "MALO")) %)
               (fn [n]
                 (inc n)))
      (d/catch Exception (fn [e]
                           (.getMessage e)))
      (d/chain #(str % "..."))))

(defn wrap-client-edn [client responsXform]
  (-> client
      (wrap-exception responsXform)
      wrap-body
      ;(wrap-header :accept application-edn)
      ;(wrap-header :content-type application-edn)
      ))

(defn request [{:keys [as middleware url] :as options}]
  (log/debug :as as :url url)
  (let [options (if as options (assoc options :as :clojure))
        options (assoc options :middleware #(wrap-client-edn % wrap-response))]
    (http/request options)))

(defn get [url options] (request (assoc options :url url :request-method "get")))
(defn post [url options] (request (assoc options :url url :request-method "post")))
(defn put [url options] (request (assoc options :url url :request-method "put")))
(defn patch [url options] (request (assoc options :url url :request-method "patch")))
(defn options [url options] (request (assoc options :url url :request-method "options")))
(defn trace [url options] (request (assoc options :url url :request-method "trace")))
(defn head [url options] (request (assoc options :url url :request-method "head")))
(defn delete [url options] (request (assoc options :url url :request-method "delete")))
(defn connect [url options] (request (assoc options :url url :request-method "connect")))
