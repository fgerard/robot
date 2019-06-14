#!/usr/bin/env bash

echo "Verifying JAVA instalation ..."
if type -p java; then
    echo "JAVA executable found in PATH"
    JAVA_BIN=java
elif [[ -n "$JAVA_HOME" ]] && [[ -x "$JAVA_HOME/bin/java" ]];  then
    echo "JAVA executable found in JAVA_HOME"
    JAVA_BIN="$JAVA_HOME/bin/java"
else
    echo "No JAVA installation found, please verify. Exiting ..."
    exit 1
fi

if [[ "$JAVA_BIN" ]]; then
    JAVA_VERSION=$("$JAVA_BIN" -version 2>&1 | awk -F '"' '/version/ {print $2}')
    echo "JAVA Version : $JAVA_VERSION"
fi

export BIN_PATH="$(cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
echo "BIN path $BIN_PATH"
export ROOT_PATH=$(dirname $BIN_PATH)
echo "Starting Robot from $ROOT_PATH"
export MININUM_MEMORY=1024m
export MAXIMUM_MEMORY=1024m
export TIMEZONE=America/Mexico_City

xvfb-run -s "-screen 0 1600x1200x24" $JAVA_BIN -Xms$MININUM_MEMORY -Xmx$MAXIMUM_MEMORY -Duser.timezone=$TIMEZONE -cp "./lib/*" -Djava.net.preferIPv4Stack=true  robot.main.starter
