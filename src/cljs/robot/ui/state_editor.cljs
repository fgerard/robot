(ns robot.ui.state-editor
  (:require [goog.string :as gstring]
            [goog.string.format]
            [fipp.edn :refer (pprint) :rename {pprint fipp}]
            [reagent.core :as reagent]
            [re-frame.core :as re-frame]
            [re-com.core :as re-com]
            [robot.ui.events :as events]
            [cljsjs.codemirror]
            [cljsjs.codemirror.mode.javascript]
            [cljsjs.codemirror.mode.htmlmixed]
            [cljsjs.codemirror.mode.clojure]
            [cljsjs.codemirror.keymap.sublime]
            [cljsjs.codemirror.addon.hint.javascript-hint]
            cljsjs.codemirror.addon.edit.closebrackets
            cljsjs.codemirror.addon.edit.matchbrackets
            [cljsjs.codemirror.addon.display.rulers]
            [cljs.reader :as reader]))


(defn validate-int-or-keyword [int-val-or-keyword]
  (re-matches #"^:[a-zA-Z]+[a-zA-Z_\-0-9]*$|^[0-9]+$" (str int-val-or-keyword)))

(defn validate-bool-or-keyword [int-val-or-keyword]
  (re-matches #"^:[a-zA-Z]+[a-zA-Z_\-0-9]*$|^(true|false)$" (str int-val-or-keyword)))

(defn validate-value-type [int-flds bool-flds m]
  (reduce
    (fn [result [k v]]
      (cond
        (int-flds k)
        (or (validate-int-or-keyword v) (reduced false))

        (bool-flds k)
        (or (validate-bool-or-keyword v) (reduced false))

        :OTHERWISE
        true))
    true
    m))

(defmulti dialog-state (fn [selected-app state-id init-state {:keys [operation]}]
                         operation) :default :default)

(def KEYWORD-RE #":[a-zA-ZñÑ][a-zA-ZñÑ0-9\-_]*")

(defn valid-conf? [validations conf]
  (every? (fn [{:keys [kwd re type]}]
            (if-not (= type :params)
              (or (re-matches re (get conf kwd ""))
                  (re-matches KEYWORD-RE (get conf kwd "")))
              true))
          validations))

(defn edit-params-comp [kwd  conf-atm] ;params-atm
  (println "KWD 0: " (pr-str kwd))
  (let [entry-k-v (reagent/atom {:k "" :v ""})
        k-re #"^[a-zA-Z0-9\-_&]*$"]
    (fn [kwd conf-atm] ;params-atm
      (swap! conf-atm update kwd (fn [params]
                                   (if params
                                     (if (string? params) (cljs.reader/read-string params) params)
                                     {})))
      (let [{:keys [k v]} @entry-k-v
            params (get @conf-atm kwd {});@params-atm
            ;params (if (string? params) (cljs.reader/read-string params) params)
            ]
        (println "ESTE: "
                 (or (re-matches #"[a-zA-Z][A-Za-z-_0-9]*" k)
                     (re-matches KEYWORD-RE k))
                 k
                 (re-matches #"[a-zA-Z][A-Za-z-_0-9]*" k)
                 (re-matches KEYWORD-RE k))
        [re-com/border
         :border "1px"
         :radius "5px"
         :width "100%"
         :height "10em"
         :style {:border "1px solid lightgrey" :margin-top "0.5em"}
         :child [re-com/v-box
                 :width "100%"
                 :children [[re-com/h-box
                             :width "100%"
                             :children [[re-com/input-text
                                         :placeholder "Param name"
                                         :width "30%"
                                         :model k
                                         :change-on-blur? false
                                         :status-icon? false
                                         :change-on-blur? false
                                         :status (if (= k "")
                                                   nil
                                                   (if (or (re-matches #"[a-zA-Z][A-Za-z-_0-9\.]*" k)
                                                           (re-matches KEYWORD-RE k))
                                                     :success
                                                     :error))
                                         :attr {:max-length "30"}
                                         :style {:padding-right "5px" :border-radius "0px 0px 0px 0px"
                                                 :border-right  "0px"
                                                 :border-left   "0px"
                                                 :border-top    "0px"
                                                 }
                                         :on-change (fn [txt]
                                                      (swap! entry-k-v assoc :k txt))]
                                        [re-com/input-text
                                         :placeholder "Value"
                                         :change-on-blur? false
                                         :width "57%"
                                         :model v
                                         :style {:padding-right "5px" :border-radius "0px 4px 4px 0px"
                                                 :border-top    "0px"}
                                         :on-change (fn [txt]
                                                      (swap! entry-k-v assoc :v txt))]

                                        [re-com/md-icon-button
                                         :md-icon-name "zmdi-plus-circle-o"
                                         :size :regular
                                         :tooltip "Add parameter"
                                         :style {:margin-top "0.2em"}
                                         :disabled? (or
                                                      (not (or (re-matches #"[a-zA-Z][A-Za-z-_0-9\.]*" k)
                                                               (re-matches KEYWORD-RE k)))
                                                      (nil? (seq k)) (nil? (seq v)))
                                         :on-click (fn []
                                                     (reset! entry-k-v {:k "" :v ""})
                                                     ;(swap! params-atm assoc k v)
                                                     ;(swap! conf-atm assoc kwd @params-atm)
                                                     (swap! conf-atm assoc-in [kwd k] v)
                                                     )
                                         ]
                                        [re-com/md-icon-button
                                         :md-icon-name "zmdi-close-circle-o"
                                         :size :regular
                                         :tooltip "Clear boxes"
                                         :style {:margin-top "0.2em"}
                                         :on-click (fn []
                                                     (reset! entry-k-v {:k "" :v ""}))
                                         ]
                                        ]]
                            [re-com/scroller
                             :h-scroll :off
                             :v-scroll :on
                             :height "7em"
                             :child [re-com/v-box
                                     :children [(doall
                                                  (for [[k v] (sort (into [] params))]
                                                    ^{:key (str k)}
                                                    [re-com/h-box
                                                     :children [[re-com/label
                                                                 :label k
                                                                 :width "30%"
                                                                 :style {:overflow "hidden"
                                                                         :flex     "initial"
                                                                         }]
                                                                [re-com/gap :size "0.2em"]
                                                                [re-com/label
                                                                 :label v
                                                                 :width "60%"
                                                                 :style {:overflow "hidden"
                                                                         :flex     "initial"
                                                                         }]
                                                                [re-com/md-circle-icon-button
                                                                 :md-icon-name "zmdi-delete"
                                                                 :size :smaller
                                                                 :style {:margin-left "0.2em"
                                                                         :overflow    "hidden"
                                                                         :flex        "initial"}
                                                                 :on-click (fn []
                                                                             ;(swap! params-atm dissoc k)
                                                                             ;(swap! conf-atm assoc kwd @params-atm)
                                                                             (swap! conf-atm update kwd dissoc k)
                                                                             )]]]))]]]]]]))))

(defn code-editor
  [{:keys [value mode onchange onblur]}]
  (let [x 1]
    (reagent/create-class
      {:component-did-mount
                     (fn []
                       (let [codemirror (js/CodeMirror (.getElementById js/document "editor")
                                                       (clj->js {:value             value
                                                                 :mode              mode
                                                                 :tabMode           "indent",
                                                                 :autoCloseBrackets true
                                                                 :matchBrackets     true
                                                                 :lineNumbers       true}))]

                         ;(.setSize codemirror "100%" 400)
                         (.setSize codemirror 700 300)
                         (if onchange
                           (.on codemirror "change"
                                onchange))
                         (if onblur
                           (.on codemirror "blur"
                                onblur))))
       :display-name "code-editor"
       :reagent-render
                     (fn []
                       [:div
                        {:id "editor"}
                        ])})))

(defn dialog-body [{:keys [title flds]} conf-atm selected-app state-id init-state
                   {:keys [operation conf diagram flow]} opr-choices
                   ;params-atm
                   ]
  (let [init-state? (= state-id init-state)
        status (valid-conf? flds @conf-atm)]
    [re-com/v-box
     :width "100%"
     :children [[re-com/title :label (clojure.string/capitalize (subs (str operation " settings") 1)) :level :level3]
                [re-com/v-box
                 :class "form-group"
                 :style {:margin-bottom "1em"}
                 :children [(doall
                              (for [{:keys [kwd re label info placeholder type mode]} flds]
                                ^{:key (str kwd)}
                                [:div
                                 [re-com/h-box
                                  :children [
                                             [:label label]
                                             [re-com/gap
                                              :size "0.5em"]
                                             [re-com/info-button
                                              :info info]]]
                                 (condp = type
                                   :code-editor [code-editor
                                                 {:mode     mode
                                                  :value    (kwd @conf-atm (if (= mode "javascript")
                                                                             "function(ctx) {\n return true;\n}"
                                                                             "(fn [ctx]\n true)"))
                                                  :onchange (fn [e]
                                                              (swap! conf-atm assoc kwd (.getValue e)))}]
                                   :params [edit-params-comp
                                            kwd
                                            ;params-atm
                                            conf-atm]

                                   [re-com/input-text
                                    :model (kwd @conf-atm "")
                                    :width "100%"
                                    :placeholder placeholder
                                    :status (if (or
                                                  (re-matches re (kwd @conf-atm ""))
                                                  (re-matches KEYWORD-RE (kwd @conf-atm ""))) :success :error)
                                    :class "form-control"
                                    :style {:border-radius "5px"}
                                    :change-on-blur? false
                                    :on-change (fn [txt]
                                                 (swap! conf-atm assoc kwd txt))])
                                 [re-com/gap :size "0.5em"]]
                                ))

                            [re-com/checkbox
                             :label "Set as initial:"
                             :model init-state?
                             :on-change (fn [is-init?]
                                          (if is-init?
                                            (re-frame/dispatch [:init-state selected-app state-id])))]]]

                [re-com/h-box
                 :gap "30px"
                 :justify :center
                 :children [[re-com/button
                             :label "Cancel"
                             :on-click (fn []
                                         (re-frame/dispatch [:cancel-update selected-app]))]
                            [re-com/button
                             :label "Ok"
                             :class "btn-primary"
                             :disabled? (not status)
                             :on-click (fn []
                                         (println "A enviar ")
                                         (println @conf-atm)

                                         (re-frame/dispatch [:update-conf selected-app state-id @conf-atm]))]]]]]))

(def opr-diag-confs {:sleep      {:flds  [{:kwd         :delay
                                           :re          #"[0-9]+"
                                           :label       "Delta"
                                           :info        "Specifies the time in millis to sleep or a ctx key that cointains it, E.g. 1000, :delta"
                                           :placeholder "Enter delta"}]
                                  :title "Delta to wait"}
                     :js         {:flds  [{:kwd   :code
                                           :re    #"(.|\n)*"
                                           :type  :code-editor
                                           :mode  "javascript"
                                           :label "Code"
                                           :info  "Specifies a javascript function to execute, the function must return an object with a result key in order to save the result in context E.g. function (ctx) {\n  return true;\n}"}]
                                  :title "Javascript code"}
                     :clojure    {:flds  [{:kwd   :code
                                           :re    #"(.|\n)*"
                                           :type  :code-editor
                                           :mode  "clojure"
                                           :label "Code"
                                           :info  "Specifies a clojure function to execute, the function must return an object with a result key in order to save the result in context E.g. (fn [ctx]\n \"ok\"}"}]
                                  :title "Clojure code"}

                     :browser {:title "Selenium operation"
                                :flds [{:kwd :file
                                        :re #"[a-zA-Z/\\0-9ñÑ].*"
                                        :label "File URL"
                                        :placeholder "Enter edn file URL"
                                        :info "Please specify the location of the edn configuration file"}
                                       {:kwd :driver-type
                                        :re #"chrome|firefox"
                                        :label "Type of driver (chrome|firefox)"
                                        :placeholder "Enter the type of driver"
                                        :info "Please specify type of driver (chrome|firefox)"}
                                       {:kwd :profile
                                        :re #"[a-zA-Z/\\0-9ñÑ\.\-\_]?.*"
                                        :label "Driver profile"
                                        :placeholder "Enter the path of the driver profile"
                                        :info "Please specify the location driver profile"}
                                       {:kwd :new-browser?
                                        :re          #"true|false"
                                        :label       "New browser?"
                                        :info        "Specify if a new browser is required E.g. true|false"
                                        :placeholder "New browser?"}
                                       {:kwd :close-it?
                                        :re          #"true|false"
                                        :label       "Close it?"
                                        :info        "Specify if the browser sould be closed at finish E.g. true|false"
                                        :placeholder "Close it?"}]}
                     :ldap {:title "LDAP operation"
                                :flds [{:kwd :host
                                        :re #"[a-zA-Z/\\0-9ñÑ].*"
                                        :label "Host name or ip"
                                        :placeholder "Enter host name/ip"
                                        :info "Please specify the name or ip od ladap server"}
                                       {:kwd         :port
                                        :re          #"[0-9]{1,4}"
                                        :label       "Port"
                                        :info        "Please specify where LDAP server is listening E.g. 389"
                                        :placeholder "Enter the LDAP port"}
                                       {:kwd         :bind-dn
                                        :re          #"[a-zA-Z].*"
                                        :label       "bind-dn"
                                        :info        "Please specify the bind-dn log log into LDAP server"
                                        :placeholder "Enter the bind-dn"}
                                       {:kwd         :password
                                        :re          #".*"
                                        :label       "Password"
                                        :info        "Please specify the password to log in the LDAP"
                                        :placeholder "Enter the password"}
                                       ]}
                     :mq {:title "IBM-MQ operation"
                          :flds [{:kwd :host
                                  :re #"[a-zA-Z/\\0-9ñÑ].*"
                                  :label "Host name or ip"
                                  :placeholder "Enter host name/ip"
                                  :info "Please specify the name or ip od ladap server"}
                                 {:kwd         :port
                                  :re          #"[0-9]{1,4}"
                                  :label       "Port"
                                  :info        "Please specify where MQ server is listening E.g. 1429"
                                  :placeholder "Enter the MQ port"}
                                 {:kwd         :qmanager
                                  :re          #"[a-zA-Z].*"
                                  :label       "qmanager"
                                  :info        "Please specify the qmanager E.g. QM.TESTING.ONE"
                                  :placeholder "Enter the qmanager"}
                                 {:kwd         :channel
                                  :re          #".*"
                                  :label       "channel"
                                  :info        "Please specify the MQ channel E.g. SYSTEM.DEF.SVRCONN"
                                  :placeholder "Enter channel"}
                                 {:kwd   :queues
                                  :label "Queue -> Max depth"
                                  :info  "Please specify the table queue -> max depth"
                                  :type  :params}
                                 ]}
                     :telegram-send   {:title "Telegram operation"
                                  :flds  [{:kwd         :bot-token
                                           :re          #"[a-zA-Z/\\0-9ñÑ].*"
                                           :label       "Bot token"
                                           :info        "Please specify your telegram bot token, E.g. 428918969:\nAAHVs7iVQlWpoqyELJ_\nbacv3a7rosx4PWW1, you can get one sending a message to @BotFather in telegram"
                                           :placeholder "Enter Telegram Bot Token"}
                                          {:kwd         :chat-tokens
                                           :re          #"^(\-?\d+[\, ]?)+$"
                                           :label       "Chat Tokens"
                                           :info        "Please specify the Telegram chat tokens to send message,must be comma separated E.g. 309085949, 309382943"
                                           :placeholder "Enter Chat Tokens"}
                                          {:kwd         :path
                                           :re          #".*"
                                           :label       "Image (Optional)"
                                           :info        "Please specify the image to be send with the message, this field is optional and can be a value stored in context, E.g. Hello, :context-key"
                                           :placeholder "Enter image path"}
                                          {:kwd         :message
                                           :re          #".*"
                                           :label       "Message"
                                           :info        "Please specify the Telegram message to be send, it can be a value stored in context, E.g. Hello, :context-key"
                                           :placeholder "Enter Message"}
                                          ]}
                     :telegram-get {:title "CMD Telegram operation"
                                      :flds  [{:kwd         :bot-token
                                               :re          #"[a-zA-Z/\\0-9ñÑ].*"
                                               :label       "Bot token"
                                               :info        "Please specify your telegram bot token, E.g. 428918969:\nAAHVs7iVQlWpoqyELJ_\nbacv3a7rosx4PWW1, you can get one sending a message to @BotFather in telegram"
                                               :placeholder "Enter Telegram Bot Token"}
                                              {:kwd         :chat-tokens
                                               :re          #"^\-?\d+([\, ]\d+)*$"
                                               :label       "Chat Tokens"
                                               :info        "Please specify the Telegram chat tokens to send message,must be comma separated E.g. 309085949, 309382943"
                                               :placeholder "Enter Chat Tokens"}
                                              ]}
                     :ibm3270    {:title "3270 operation"
                                  :flds  [{:kwd         :file
                                           :re          #"[a-zA-Z/\\0-9ñÑ].*"
                                           :label       "Url"
                                           :info        "Please specify the url of the CSV file to be executed, it can be key from context, E.g. :file3270, /home/iwrobot/3270.csv"
                                           :placeholder "Enter the URL"}
                                          {:kwd         :address
                                           :re          #"[a-zA-Z/\\0-9ñÑ].*"
                                           :label       "Host"
                                           :info        "Please specify the host to be checked, it can be a key from context E.g. :host3270, 172.168.1.1, myserver"
                                           :placeholder "Enter the host address"}
                                          {:kwd         :port
                                           :re          #"[0-9]{1,4}"
                                           :label       "Port"
                                           :info        "Please specify where 3270 server is listening E.g. 80, 22"
                                           :placeholder "Enter the 3270 port"}
                                          {:kwd         :ssl?
                                           :re          #"true|false"
                                           :label       "SSL"
                                           :info        "Specify if the port is expecting an SSL connection E.g. true|false"
                                           :placeholder "Is SSL?"}
                                          {:kwd         :protocol
                                           :re          #"[a-zA-Z/\\0-9ñÑ].*"
                                           :label       "Protocol"
                                           :info        "Please specify the type of procotol to be used for comunicating with 3270 server, E.g. IBM-3278-2"
                                           :placeholder "Enter the 3270 protocol"}
                                          ]}
                     :play-sound {:title "Play sound operation"
                                  :flds  [{:kwd         :path
                                           :re          #"[a-zA-Z/\\0-9ñÑ].*"
                                           :label       "File URL"
                                           :placeholder "Enter URL"
                                           :info        "Please specify the path of the sound file to be played (only WAV is supported), it can be a keyword E.g. :sound-alert, /home/iwrobot/alert.wav"}
                                          ]
                                  }
                     :selfie {:title "Selfie operation"
                              :flds [{:kwd :path
                                      :re #"[a-zA-Z/\\0-9ñÑ].*"
                                      :label "File URL"
                                      :placeholder "Enter URL of file to hold image"
                                      :info "Please specify the output path of the png screencapture to be taken, it can be a keyword, E.g. :selfie-path /home/iwrobot/screen.png"}
                                     ]
                              }
                     :os-cmd {:title "OS cmd operation"
                              :flds [{:kwd :shell
                                      :re #".*" ;[a-zA-Z/\\0-9ñÑ\.\:]
                                      :label "Cmd >"
                                      :placeholder "Enter OS command to execute!"
                                      :info "Please specify the shell command or the sh, bat, bash filename to be executed, it can be a keyword E.g. :shell /home/iwrobot/start-server.sh"}
                                     ]
                              }
                     :caudal {:title "Caudal operation"
                              :flds [{:kwd :host
                                      :re #"[a-zA-Z0-9][a-zA-Z0-9\.]*"
                                      :label "Caudal host "
                                      :placeholder "Enter caudal host or ip"
                                      :info "Please specify Host name or ip of caudal server"}
                                     {:kwd :port
                                      :re #"[0-9]{1,4}"
                                      :label "Caudal port "
                                      :placeholder "Enter caudal port"
                                      :info "Please specify the port where caudal is listening"}
                                     ;{:kwd :message
                                      ;:re #".*"
                                      ;:label "Caudal message "
                                      ;:placeholder "Enter caudal message"
                                      ;:info "Please specify message to attach to event, can have keywords form context"}
                                     ]
                              }
                     :switch-good {:title "Switch good operation"
                                  :flds  [{:kwd         :id
                                           :re          #"[a-zA-ZñÑ0-9\-_\.]+"
                                           :label       "Id"
                                           :placeholder "Enter id"
                                           :info        "Please specify the id of this mood change"}
                                          ]
                                  }
                     :switch-bad {:title "Switch bad operation"
                                  :flds  [{:kwd         :id
                                           :re          #"[a-zA-ZñÑ0-9\-_\.]+"
                                           :label       "Id"
                                           :placeholder "Enter id"
                                           :info        "Please specify the id of this mood change"}
                                          {:kwd         :minutes
                                           :re          #"[0-9]{1,4}"
                                           :label       "Minutes"
                                           :placeholder "Enter minutes"
                                           :info        "Please specify the number of minutes to wait before resending, it can be a keyword E.g. :minutes 30"}
                                          ]
                                  }
                     :socket     {:title "Socket operaation"
                                  :flds  [{:kwd         :host
                                           :re          #"[a-zA-Z/\\0-9ñÑ].*"
                                           :label       "Host"
                                           :info        "Host name or ip address to monitor"
                                           :placeholder "Host name/ ip"}
                                          {:kwd         :port
                                           :re          #"[0-9]{1,4}"
                                           :label       "Port"
                                           :info        "Port number on host to monitor"
                                           :placeholder "Port"}
                                          {:kwd         :timeout
                                           :re          #"[0-9]+"
                                           :label       "Timeout"
                                           :info        "Number of milliseconds to wait befor timeout"
                                           :placeholder "Milliseconds"}
                                          {:kwd         :retry-count
                                           :re          #"[0-9]+"
                                           :label       "Retry count"
                                           :info        "Number of times to retry"
                                           :placeholder "Count"}
                                          {:kwd         :retry-delay
                                           :re          #"[0-9]+"
                                           :label       "Retry delay"
                                           :info        "Number of milliseconds to wait befor retrying"
                                           :placeholder "Milliseconds"}]}
                     :sql   {:title "SQL read operation"
                                  :flds  [{:kwd         :classname
                                           :re          #"[a-zA-Z].*"
                                           :label       "Driver"
                                           :info        "Please specify the DB driver classname, it can be a keyword, E.g. :driver, com.mysql.jdbc.Driver"
                                           :placeholder "Enter the driver classname"}
                                          {:kwd         :subprotocol
                                           :re          #"[a-zA-Z].*"
                                           :label       "Subprotocol"
                                           :info        "Please specify the subprotocol of the database, it can be a keyword, E.g. :subprotocol, mysql, sqlserver"
                                           :placeholder "Enter the driver subbprotocol"}
                                          {:kwd         :subname
                                           :re          #".*"
                                           :label       "Subname"
                                           :info        "Please specify the subname of the database, it can be a keyword, E.g. :subname, //localhost:3306/strauzbatchdb"
                                           :placeholder "Enter the driver subname"}
                                          {:kwd         :user
                                           :re          #"[a-zA-Z].*"
                                           :label       "User"
                                           :info        "Please specify the username to log in the database, it can be a keyword, E.g. :username, root"
                                           :placeholder "Enter the username"}
                                          {:kwd         :password
                                           :re          #".*"
                                           :label       "Password"
                                           :info        "Please specify the password of the username to log in the database, it can be a keyword, E.g. :dbpassword, 123456"
                                           :placeholder "Enter the password"}
                                          {:kwd         :query
                                           :re          #"[a-zA-Z].*"
                                           :label       "Query"
                                           :info        "Please specify the query to be executed on the database, it can be a keyword, E.g. :query, SELECT * FROM mytable;"
                                           :placeholder "Enter the query"}
                                          {:kwd         :timeout
                                           :re          #"[0-9]+"
                                           :label       "Timeout"
                                           :info        "Number of milliseconds to wait befor timeout"
                                           :placeholder "Milliseconds"}
                                          {:kwd         :retry-count
                                           :re          #"[0-9]+"
                                           :label       "Retry count"
                                           :info        "Number of times to retry"
                                           :placeholder "Count"}
                                          {:kwd         :retry-delay
                                           :re          #"[0-9]+"
                                           :label       "Retry delay"
                                           :info        "Number of milliseconds to wait befor retrying"
                                           :placeholder "Milliseconds"}]}
                     :twitter    {:title "Twitter operation"
                                  :flds  [{:kwd         :app-consumer-key
                                           :re          #"[a-zA-Z/\\0-9ñÑ:-_@].*"
                                           :label       "App consumer key"
                                           :info        "Please specify the Consumer Key provided from Twitter, it can be a keyword, E.g. :ckey CIIAre8SjFkS8c3PYlri17a0E"
                                           :placeholder "Enter the consumer-key"}
                                          {:kwd         :app-consumer-secret
                                           :re          #"[a-zA-Z/\\0-9ñÑ:-_@].*"
                                           :label       "App consumer secret"
                                           :info        "Please specify the app-consumer-secret provided from Twitter, it can be a keyword, E.g. :app-secret 9BdSqpxEZlbItOQas0IrOpv0pGaJohVvovUNugtok5lr0SwLCH"
                                           :placeholder "Enter the consumer-secret"}
                                          {:kwd         :access-token
                                           :re          #"[a-zA-Z/\\0-9ñÑ:-_@].*"
                                           :label       "Access token"
                                           :info        "Please specify the access-token of the Twitter Application, it can be a keyword, E.g. :access-token 831926785675780097"
                                           :placeholder "Enter the access-token"}
                                          {:kwd         :access-token-secret
                                           :re          #"[a-zA-Z/\\0-9ñÑ:-_@].*"
                                           :label       "Access token secret"
                                           :info        "Please specify the Twitter application's access-token-secret , it can be a keyword, E.g. :access-token-secret, 79KvvV7Lss50i8hTaRNDL0YGf884uqÑvSTqElxocDKdfT"
                                           :placeholder "Enter the access-token-secret"}
                                          {:kwd         :message
                                           :re          #".*"
                                           :label       "Message"
                                           :info        "Please specify the message to tweet, it can be a conformed by keyword, E.g. Host :host is :alive"
                                           :placeholder "Enter the message"}]}
                     :mail-send  {:title "Send mail operation"
                                  :flds  [{:kwd         :from
                                           :re          #".+"
                                           :label       "From"
                                           :info        "Please specify the mail remittent, it can be a keyword, E.g. :mail, myuser@mymail.com"
                                           :placeholder "Enter the mail from"}
                                          {:kwd         :to
                                           :re          #".+"
                                           :label       "To"
                                           :info        "Please specify the mail that will receive the notification, it can be a keyword, E.g. :email, myuser@mymail.com"
                                           :placeholder "Enter the mail to"}
                                          {:kwd         :subject
                                           :re          #".+"
                                           :label       "Subject"
                                           :info        "Please specify the mail's subject, it can be a keyword, E.g. :error, Ocurrió un error"
                                           :placeholder "Enter subject"}
                                          {:kwd         :body
                                           :re          #".+"
                                           :label       "Body"
                                           :info        "Please specify the username to log in the database, it can be a keyword, E.g. :username, root"
                                           :placeholder "Enter body"}
                                          {:kwd         :host
                                           :re          #".+"
                                           :label       "Host"
                                           :info        "Please specify the host name/ip of mail server"
                                           :placeholder "Host name / ip"}
                                          {:kwd         :port
                                           :re          #"[0-9]*"
                                           :label       "Port"
                                           :info        "Please specify the port of the mail server"
                                           :placeholder "Port"}
                                          {:kwd         :user
                                           :re          #"[a-zA-Z].*"
                                           :label       "User"
                                           :info        "Please specify the username to connect to mail server"
                                           :placeholder "Enter the username"}
                                          {:kwd         :pass
                                           :re          #".*"
                                           :label       "Password"
                                           :info        "Please specify the password to connecto to mail server"
                                           :placeholder "Enter the password"}
                                          {:kwd         :ssl
                                           :re          #"true|false"
                                           :label       "SSL"
                                           :info        "Specify if the port is expecting an SSL connection E.g. true|false"
                                           :placeholder "Is SSL?"}
                                          {:kwd         :timeout
                                           :re          #"[0-9]+"
                                           :label       "Timeout"
                                           :info        "Number of milliseconds to wait befor timeout"
                                           :placeholder "Milliseconds"}
                                          {:kwd         :retry-count
                                           :re          #"[0-9]+"
                                           :label       "Retry count"
                                           :info        "Number of times to retry"
                                           :placeholder "Count"}
                                          {:kwd         :retry-delay
                                           :re          #"[0-9]+"
                                           :label       "Retry delay"
                                           :info        "Number of milliseconds to wait befor retrying"
                                           :placeholder "Milliseconds"}]}
                     :mail-get  {:title "Get-Mail operation"
                                  :flds  [{:kwd         :host
                                           :re          #".+"
                                           :label       "Host"
                                           :info        "Please specify the POP3 mail server host"
                                           :placeholder "Enter host"}
                                          {:kwd         :port
                                           :re          #"[0-9]+"
                                           :label       "Port"
                                           :info        "Please specify the POP3 mail server's port"
                                           :placeholder "Enter Port"}
                                          {:kwd         :protocol
                                           :re          #"pop3(s)?|imap(s)?"
                                           :label       "Protocol"
                                           :info        "Please specify the mail server protocol"
                                           :placeholder "Enter protocol"}
                                          {:kwd         :ssl
                                           :re          #"^true|false$"
                                           :label       "SSL"
                                           :info        "Please specify mail server uses SSL"
                                           :placeholder "Enter SSL"}
                                          {:kwd         :email
                                           :re          #".+"
                                           :label       "Mail Address"
                                           :info        "Please specify the Mail address to log in to mail server"
                                           :placeholder "Enter Mail Address"}
                                          {:kwd         :password
                                           :re          #".+"
                                           :label       "Password"
                                           :info        "Please specify the mail address password to log in to mail server"
                                           :placeholder "Enter the mail address password"}
                                          {:kwd         :subject-re
                                           :re          #".+"
                                           :label       "Subject RE"
                                           :info        "Please specify the regular expression for filtering Mails"
                                           :placeholder "Enter Regular Expression"}
                                          {:kwd         :from-re
                                           :re          #".+"
                                           :label       "From RE"
                                           :info        "Please specify the regular expression for filtering MailAddresses"
                                           :placeholder "Enter Regular Expression"}
                                          {:kwd         :timeout
                                           :re          #"[0-9]+"
                                           :label       "Timeout"
                                           :info        "Number of milliseconds to wait befor timeout"
                                           :placeholder "Milliseconds"}
                                          {:kwd         :retry-count
                                           :re          #"[0-9]+"
                                           :label       "Retry count"
                                           :info        "Number of times to retry"
                                           :placeholder "Count"}
                                          {:kwd         :retry-delay
                                           :re          #"[0-9]+"
                                           :label       "Retry delay"
                                           :info        "Number of milliseconds to wait befor retrying"
                                           :placeholder "Milliseconds"}
                                          ]}
                     :date-time  {:title "Date time operation"
                                  :flds  [{:kwd         :format
                                           :re          #".+"
                                           :label       "Format"
                                           :info        "Please specify the format time for storing the date, see java.text.SimpleDateFormat docs"
                                           :placeholder "yyyy-MM-dd'T'HH:mm:ss,SSS"}]}
                     :slack-send      {:title "Slack Operation"
                                  :flds  [{:kwd         :webhook
                                           :re          #".+"
                                           :label       "Webhook URL"
                                           :info        "Please specify the Webhook URL provided by Slack, it can be a keyword E.g :slack-webhook, https://hooks.slack.com/services/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"
                                           :placeholder "Enter the URL"}
                                          {:kwd         :message
                                           :re          #".+"
                                           :label       "Message"
                                           :info        "Please specify the message to be send, it can be a keyword E.g :slack-message, Hello world"
                                           :placeholder "Enter the message"}
                                          ]}
                     :http       {:title "Http Operations"
                                  :flds  [{:kwd         :uri
                                           :re          #".+"
                                           :label       "URI"
                                           :info        "Please specify the uri to send the HTTP petition"
                                           :placeholder "Enter the URI"}
                                          {:kwd         :method
                                           :re          #"^GET|POST|PUT|DELETE$"
                                           :label       "Method"
                                           :info        "Please specify the request method (GET, PUT, POST, DELETE)"
                                           :placeholder "Enter request method"}
                                          {:kwd   :params
                                           :label "Params"
                                           :info  "Please specify the parameters for the http request"
                                           :type  :params}
                                          {:kwd         :timeout
                                           :re          #"[0-9]+"
                                           :label       "Timeout"
                                           :info        "Number of milliseconds to wait befor timeout"
                                           :placeholder "Milliseconds"}
                                          {:kwd         :retry-count
                                           :re          #"[0-9]+"
                                           :label       "Retry count"
                                           :info        "Number of times to retry"
                                           :placeholder "Count"}
                                          {:kwd         :retry-delay
                                           :re          #"[0-9]+"
                                           :label       "Retry delay"
                                           :info        "Number of milliseconds to wait befor retrying"
                                           :placeholder "Milliseconds"}]}
                :gsheet-send {:title "Google Spreadsheet Appender"
                                  :flds  [{:kwd :client-secret-json
                                           :re #"[a-zA-Z/\\0-9ñÑ].*"
                                           :label "Google Secret File URL"
                                           :placeholder "Enter URL of file with Google Oauth2 secrets"
                                           :info "Please specify the client secrets JSON obtained via Google Console, E.g. /home/user/client_secrets.json"}
                                          {:kwd :oauth-store
                                           :re #"[a-zA-Z/\\0-9ñÑ].*"
                                           :label "OAuth Store File URL"
                                           :placeholder "Enter URL to save Cookie consent"
                                           :info "Please specify the place to save your consent cookie, E.g. /home/user/.store/oauth2"}
                                          {:kwd         :spreadsheet-id
                                           :re          #".+"
                                           :label       "SpreadSheetID"
                                           :info        "Please specify your ID from URL: https://docs.google.com/spreadsheets/d/<SPREADSHEET_ID>/edit"
                                           :placeholder "SPREADSHEET_ID"}
                                          {:kwd         :sheet-id
                                           :re          #".+"
                                           :label       "SheetID"
                                           :info        "Please specify the sheet to append data"
                                           :placeholder "Sheet 1"}
                                          {:kwd         :as-row?
                                           :re          #".+"
                                           :label       "Append as Row (true) or as Column (false)"
                                           :info        "true to append as Row"
                                           :placeholder "true"}
                                           {:kwd         :data
                                           :re          #".+"
                                           :label       "Text"
                                           :info        "Entry your text or keywords, E.g. 1, 2, :hi"
                                           :placeholder "value1, value2"}
                                          {:kwd         :timeout
                                           :re          #"[0-9]+"
                                           :label       "Timeout"
                                           :info        "Number of milliseconds to wait befor timeout"
                                           :placeholder "Milliseconds"}
                                          {:kwd         :retry-count
                                           :re          #"[0-9]+"
                                           :label       "Retry count"
                                           :info        "Number of times to retry"
                                           :placeholder "Count"}
                                          {:kwd         :retry-delay
                                           :re          #"[0-9]+"
                                           :label       "Retry delay"
                                           :info        "Number of milliseconds to wait befor retrying"
                                           :placeholder "Milliseconds"}]}
                     })

(defmethod dialog-state :default
  [selected-app state-id init-state {:keys [operation conf diagram flow]} opr-choices]
  (let [conf-atm (reagent/atom conf)
        ;params-atm (reagent/atom (if (string? (:params conf {}))
        ;                           (cljs.reader/read-string (:params conf {}))
        ;                           (:params conf {})))
        ]
    (fn [selected-app state-id init-state {:keys [operation conf diagram flow]}]
      (dialog-body (opr-diag-confs operation)
                   conf-atm
                   selected-app
                   state-id
                   init-state
                   {:operation operation :conf conf :diagram diagram :flow flow}
                   opr-choices
                   ;params-atm
                   ))))



(defn show-dialog [selected-app state-id init-state {:keys [operation conf diagram flow] :as states} opr-choices]
  (let [
        get-operation (fn [which?]
                        (first
                          (filter
                            (fn [oper]
                              (= (keyword (:id oper)) which?))
                            opr-choices)))

        operation-atm (reagent/atom (get-operation operation))
        name-atm (reagent/atom (subs (str state-id) 1))
        ]

    (fn [selected-app state-id init-state {:keys [operation conf diagram flow] :as states} opr-choices]
      [re-com/border
       :border "5px solid #559"
       :radius "15px"
       :child [re-com/v-box
               :width "740px"
               :height "auto"
               :style {:background-color "#d4e3f7" :border-radius "15px"
                       :padding-left     "1em"
                       :padding-right    "1em"
                       :padding-bottom   "1em"}
               :children [[re-com/h-box
                           :children [
                                      [:img {:src    (str "images/icons/operations/" (:image @operation-atm))
                                             :width  "25px"
                                             :height "25px"
                                             :style  {:margin-top   "1em"
                                                      :margin-right "0.5em"}}]
                                      [re-com/title :label (str (clojure.string/capitalize (:id @operation-atm)) " operation ")
                                       :style {:overflow "none"}
                                       :level :level2
                                       ]]]
                          [re-com/title :label (str "(" state-id ")")
                           :style {:font-weight "bold"}
                           :level :level3]

                          [re-com/title :label (str "Operation type: ")
                           :level :level3]
                          [re-com/single-dropdown
                           :choices opr-choices
                           :model (:id @operation-atm)
                           :placeholder "Operation type"
                           :width "100%"
                           :on-change (fn [id]
                                        (reset! operation-atm (get-operation (keyword id)))
                                        (re-frame/dispatch [:change-opr-type selected-app state-id (keyword id) ]))
                           :render-fn (fn [{:keys [id image]}]
                                        [re-com/h-box
                                         :children [[:img {:src    (str "images/icons/operations/" image)
                                                           :width  "25px"
                                                           :height "25px"}]
                                                    [re-com/gap
                                                     :size "0.5em"]
                                                    [:p id
                                                     ]]])]
                          ;[re-com/title :label "Operation name: "
                          ; :level :level3]
                          ;[re-com/input-text
                          ; :placeholder "operation name"
                          ; :width "100%"
                          ; :model name-atm
                          ; :status-icon? false
                          ; :change-on-blur? false
                          ; :status (if (seq @name-atm) :success :error)
                          ; :on-change (fn [txt]
                          ;              (reset! name-atm txt))]
                          [dialog-state
                           selected-app
                           state-id
                           init-state
                           states
                           opr-choices]]]])))
