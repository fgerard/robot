#!/bin/bash
echo "JDK list..."
/usr/libexec/java_home -V
echo "Selecting java version 11.0"
export JAVA_HOME=`/usr/libexec/java_home -v 11.0`
#export JAVA_HOME=`/usr/libexec/java_home -v 21.0`
java -version
echo "Building CSS files ..." && \
lein minify-assets && \
lein less once && \
echo "Building javascript & uberjar .." && \
lein with-profile prod uberjar
