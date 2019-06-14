(ns util.http
  (:require [clojure.set :refer [subset?]]
            [clojure.string :refer [split]]
            [clojure.tools.logging :as log]
            [bidi.ring :as bidi]
            [robot.main.starter :refer [name&version]]))

(defmulti validate (fn [[k v]] k))

(defmethod validate :ssl-client-cert [[k v]]
  true)

(def valid-request-keys #{:ssl-client-cert
                          :protocol
                          :remote-addr
                          :params
                          :route-params
                          :headers
                          :server-port
                          :content-length
                          :form-params
                          :query-params
                          :content-type
                          :character-encoding
                          :uri
                          :server-name
                          :query-string
                          :body
                          :scheme
                          :request-method})

(def valid-headers #{"user-agent"
                     "host"
                     "accept"
                     "content-length"
                     "expect"
                     "content-type"})

(defn valid-post-request? [request]
  (log/debug :class (class request))
  (log/debug (keys request))
  (log/debug (keys (:headers request)))

  (and
   (subset? (into #{} (keys request)) valid-request-keys)
   (subset? (into #{} (keys (:headers request))) valid-headers)
   (re-matches #"^[a-zA-Z0-9/-_]*$" (:uri request ""))
   (not (re-find #"\([ ]*\)" (get-in request [:headers "user-agent"])))))

(defn http-code-of [{:keys [result-code] :as context}]
  (condp = result-code
    :success 200
    :fatal 400
    :error 500
     400))

(defn not-found [request]
  {:status 404 :body (str (:uri request) " not found!")})

(def now (System/currentTimeMillis))

(def _ah-cmds (atom {}))

(defn register_ah-fn [cmd f]
  (log/info "Register fn for: " (pr-str cmd))
  (swap! _ah-cmds assoc cmd f))

(let [[name version] (name&version)]

  (defn redirect2project [request]
     {:status 301 :headers {"location" (str "/" name)}})
  
  (defn index [routes]
    (let [nss (reduce (fn [result [path _]]
                        (conj result path))
                      []
                      routes)]
      (fn [request]
        (if-let [cmd-fn (->>
                         (get-in request [:route-params :cmd])
                         (get @_ah-cmds))]
          (cmd-fn)
          {:status 200
           :body {:name name
                  :version version
                  :uptime (- (System/currentTimeMillis) now)
                  :ns nss}}))))

  (defn make-handler [routes]
                                        ;(clojure.pprint/pprint (concat routes [[true not-found]]))
    (bidi/make-handler
     ["/"
      (-> routes
          (concat [["" redirect2project]])
          (concat [[name (index routes)]])
          (concat [[["_ah/" :cmd] {{:request-method :get} (index routes)}]])
          (concat [[true not-found]]))]))

  (defn make-handler-ah [routes ah-handler]
                                        ;(clojure.pprint/pprint (concat routes [[true not-found]]))
    (bidi/make-handler
     ["/"
      (-> routes
          (concat [["" redirect2project]])
          (concat [[name (index routes)]])
          (concat [[["_ah/" :cmd] {{:request-method :get} ah-handler}]])
          (concat [[true not-found]]))])))

(defn get-params [query-params]
  (if (and query-params (seq query-params))
    (into {} (map #(split % #"=") (split query-params #"&")))
    nil))

(defn middleware-query-params [f]
  (fn MW-query-params [request]
    (let [query-params (get-params (:query-string request))]
      (f (assoc request :query-params query-params)))))

(defn smart-params [request]
  (let [params (:params request)
        multipart-params (:multipart-params request)
        params (merge params
                      (reduce (fn [result [p-name val]]
                                (if (map? val)
                                  (assoc result (keyword p-name) (String. (:bytes val) "UTF-8"))
                                  (assoc result (keyword p-name) val)))
                              {}
                              multipart-params))]
    params))

(defn result-code [http-code]
  (cond
    (and (>= http-code 200) (< http-code 300))
    :success

    (and (>= http-code 400) (< http-code 500))
    :fatal

    :OTHERWIZE
    :error))

(defn create-tx []
  (str (java.util.UUID/randomUUID)))

(defmacro defhandler [name [req-sym tx-sym] & body]
  `(fn ~name [~req-sym]
     (let [params# (:query-params ~req-sym)
           tx# (get params# "tx" (create-tx))
           _# (log/info (pr-str {:status :start :uri (:uri ~req-sym) :tx tx#}))
           result# (let [~tx-sym tx#] ~@body)]
       (log/info (pr-str {:status :end :result-code (result-code (:status result#)) :uri (:uri ~req-sym) :tx tx#}))
       result#)))
