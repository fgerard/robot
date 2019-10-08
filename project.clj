(defproject fgerard/robot "2.0.2"
  :description "Project robot"
  :url "https://fgerard.github.io/robot.docs"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}

  :plugins      [[lein-libdir "0.1.1"]
                 [lein-cljsbuild "1.1.4"]
                 [lein-less "1.7.5"]
                 [lein-asset-minifier "0.4.3"]]

  :dependencies [[org.clojure/clojure "1.10.1"]
                 [org.clojure/clojurescript "1.10.520" :exclusions [com.fasterxml.jackson.core/jackson-core]]
                 [org.clojure/core.async "0.4.500"]

                 [org.clojure/core.cache "0.6.5"]
                 [org.clojure/data.json "0.2.6"]
                 [org.clojure/data.xml "0.0.8"]
                 [org.clojure/data.codec "0.1.0"]

                 [commons-net/commons-net "3.6"]
                 [commons-codec/commons-codec "1.10"]
                 [commons-io/commons-io "2.5"]
                 [jagacy/jagacy "1.0.0"]
                 [clojure-csv/clojure-csv "2.0.1"]

                 [ring/ring-core "1.6.1"]
                 [org.clojure/java.jdbc "0.4.2"]
                 [ring-middleware-format "0.7.2"]
                 [hiccup "1.0.5"]
                 [bidi "2.0.16"]
                 [com.taoensso/sente "1.11.0"]
                 [org.clojure/tools.logging "0.3.1"]
                 [proto-repl "0.3.1"]
                 [riddley "0.1.14"]
                 [aleph "0.4.6" :exclusions [org.clojure/tools.logging]]
                 [jumblerg/ring.middleware.cors "1.0.1"]
                 [integrant "0.4.0"]
                 [cheshire "5.5.0"]
                 [org.apache.logging.log4j/log4j-core "2.8.1"]
                 [org.apache.logging.log4j/log4j-slf4j-impl "2.8.1"]

                 [javax.servlet/javax.servlet-api "3.1.0"]

                 ;; Java 11
                 [javax.xml.bind/jaxb-api "2.3.0"]
                 [com.sun.xml.bind/jaxb-core "2.3.0"]
                 [com.sun.xml.bind/jaxb-impl "2.3.0"]

                 [com.draines/postal "2.0.2"]
                 [twitter-api "1.8.0" :exclusions [org.bouncycastle/bcpkix-jdk15on io.netty/netty]]

                 [clj-tagsoup "0.3.0" :exclusions [org.clojure/clojure]] ;net.java.dev.stax-utils/stax-utils


                 [org.fusesource.leveldbjni/leveldbjni-all "1.8"]
                 [org.iq80.leveldb/leveldb-api "0.11"]      ;7

                 [org.seleniumhq.selenium/selenium-java "3.11.0" :exclusions [com.google.errorprone/error_prone_annotations]]

                 [re-frame "0.9.4" :exclusions [com.google.guava/guava]]
                 [re-com "2.1.0" :exclusions [com.google.guava/guava]]
                 [fipp "0.6.8"]
                 [cljs-http "0.1.43"]
                 [com.taoensso/sente "1.11.0"]
                 [com.andrewmcveigh/cljs-time "0.5.0"]
                 [cljsjs/codemirror "5.24.0-1"]
                 [javax.mail/mail "1.4.7", :exclusions [javax.activation/activation]]

                 [cljsjs/svgjs "2.2.5-0"]

                 [clj-jwt "0.1.1"]

                 [cljs-hash "0.0.2"]

                 [com.unboundid/unboundid-ldapsdk "3.2.0"]

                 [com.ibm/com.ibm.mq.commonservices "7.5.0.5"]
                 [com.ibm/com.ibm.mq.headers "7.5.0.5"]
                 [com.ibm/com.ibm.mq "7.5.0.5"]
                 [com.ibm/com.ibm.mq.jmqi "7.5.0.5"]
                 [com.ibm/com.ibm.mq.pcf "7.5.0.5"]
                 [com.ibm/com.ibm.mqjms "7.5.0.5"]
                 [com.sun/com.sun.connector "1.3.0"]

                 [com.google.apis/google-api-services-oauth2 "v2-rev131-1.23.0"]
                 [com.google.apis/google-api-services-sheets "v4-rev488-1.23.0"]
                 [com.google.http-client/google-http-client-jackson2 "1.23.0"]
                 [com.google.oauth-client/google-oauth-client-jetty "1.23.0"]
                 [org.clojure/java.jmx "0.3.4"]

                 ]

  ;:certificates ["./ci.interware.mx.pem"]

  :repositories [["central" "https://repo1.maven.org/maven2/"]
                 ["clojars" "https://repo.clojars.org/"]
                 ;["interware-3rdparty" {:url      "https://ci.interware.mx/nexus/repository/interware-3rdparty"
                 ;                       :releases {:checksum :fail :update :always}}]
                 ]

  :jvm-opts ~(concat
               ; Normal JVM opts to pass in
               ["-Xmx512m"]
               ; Java 9+ recognition, adding --add-modules. Java versions before 9
               ; had a different version syntax where they contained '.' delimiters,
               ; from Java 9 onwards it has a simple versioning scheme based on one
               ; number.
               (let [[mayor minor version] (clojure.string/split (System/getProperty "java.version") #"\.")
                     mayor (Integer/parseInt mayor)]
                 (if (> mayor 1)
                   ["--add-modules" "java.xml.bind"]
                   [])))

  :min-lein-version "2.5.3"


  :repl-options {:prompt (fn [ns] (str "<" ns "> "))
                 :welcome (println "Welcome to a higher state of consciousness!")
                 :init-ns robot.main.starter}

  :aot [robot.main.starter robot.util.selenium-direct]
 
  :source-paths ["src/clj"]
  :clean-targets ^{:protect false} ["resources/public/js/compiled" "target" "robot-distro/" "robot-distro.tgz"]

  :figwheel {:css-dirs ["resources/public/css"]
             :server-port 3333}


  :minify-assets [[:js {:source ["src/js/externs.js"] :target "resources/public/js/lib/externs.js" :optimizations :advanced}]]
  :less {:source-paths ["src/less/index.less"]
         :target-path "resources/public/css/index.css"}

  :profiles
  {:dev {:source-paths ["dev" "src" "test"]
         :dependencies [[org.clojure/tools.namespace "0.2.11"]
                        [binaryage/devtools "0.8.2"]]
         :plugins      [[lein-figwheel "0.5.7"]]
         :main robot.main.starter
         }
   :prod {:prep-tasks [["cljsbuild" "once" "min"] "compile"]
          :main robot.main.starter
          }}

  :cljsbuild
  {:builds
   [{:id           "dev"
     :source-paths ["src/cljs"]
     :figwheel     {:on-jsload "robot.ui.core/mount-root"}
     :compiler     {:main       robot.ui.core
                    :output-to            "resources/public/js/compiled/robot-ui.js"
                    :output-dir           "resources/public/js/compiled/out"
                    :asset-path           "js/compiled/out"
                    :source-map-timestamp true
                    :preloads             [devtools.preload]
                    :external-config      {:devtools/config {:features-to-install :all}}
                    }}


    {:id           "min"
     :source-paths ["src/cljs"]
     :jar true
     :compiler     {:main            robot.ui.core
                    :output-to       "resources/public/js/compiled/robot-ui.js"
                    :optimizations   :advanced ;:simple
                    :closure-defines {goog.DEBUG false}
                    :pretty-print    false}}

    ]}

  )
