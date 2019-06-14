(ns robot.util.selenium-common
  (:require
    [clojure.xml :as X]
    [clojure.walk :as w]
    [clojure.string :as S]
    [clojure.pprint :as pp]
    [clojure.tools.logging :as log]
    [clojure.java.shell :refer [sh]]
    [integrant.core :as ig]
    [robot.core.util :as U])
  (:import
   (org.openqa.selenium By NoSuchElementException WebDriver WebElement JavascriptExecutor)
   (org.openqa.selenium.firefox FirefoxDriver)
   (org.openqa.selenium.support.ui Select)))

(defn get-os []
  (let [os (System/getProperty "os.name")
        os (if (clojure.string/starts-with? os "Windows") "Windows" os)]
    os))

(defn suffix-driver []
  (let [os (System/getProperty "os.name")
        so (if (clojure.string/starts-with? os "Windows") "Windows" os)]
    (condp = so
      "Mac OS X" ""
      "Linux" "_linux_x64"
      "Windows" ".exe"
      (let [_ (log/warn "Selenium Driver binaries for OS " (pr-str os) " not registered" )] ""))))

(defmethod ig/init-key :robot.util.selenium-common/driver
  [_ {:keys [firefox chrome] :as conf}]
  (let [suffix (suffix-driver)
        firefox-driver (str (:driver firefox) suffix)
        firefox-args (:arguments firefox)
        chrome-driver (str (:driver chrome) suffix)
        chrome-args (:arguments chrome)]
    (log/info "Initializing selenium driver: ")
    (log/info {:firefox-driver firefox-driver :chrome-driver chrome-driver})

    (System/setProperty "webdriver.gecko.driver" firefox-driver)

    (System/setProperty "webdriver.chrome.driver"
                        chrome-driver)

    (System/setProperty "robot.gecko.args" (pr-str firefox-args))
    (System/setProperty "robot.chrome.args" (pr-str chrome-args))
    
    (System/setProperty "scrape.window" "true")))

(defmulti kill-port-pid
  (fn [port]
    (get-os)) :default :default)

(defmethod kill-port-pid :default [port]
  (log/warn "Selenium Driver clean process for OS " (get-os) " not registered" ))

(defmethod kill-port-pid "Mac OS X" [port]
  (let [command (sh "bash" "-c" (str "lsof -i:" port "| grep LISTEN"))
        out (S/replace (:out command) #"\n" "")
        [_ pid] (re-matches #".*\s+([0-9]+)\s+.*" out)]
    (log/info "Killing: " pid " using port: " port)
    (sh "bash" "-c" (str "kill -9 " pid))))

(defmethod kill-port-pid "Linux" [port]
  (let [command (sh "bash" "-c" (str "netstat -nltp | grep " port))
        _ (log/info "command:" (pr-str command))
        out (S/replace (:out command) #"\n" "")
        _ (log/info "OUT:" (pr-str out))
        [_ pid] (re-matches #".*\s+([0-9]+)/.*" out)]
    (log/info "Killing: " pid " using port: " port)
    (sh "bash" "-c" (str "kill -9 " pid))))
