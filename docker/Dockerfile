FROM ubuntu:24.04

ENV DEBIAN_FRONTEND=noninteractive

WORKDIR /opt

# openjdk-21-jdk-headless viene en los repos de Ubuntu 24.04 tanto para
# amd64 como arm64, asi que apt resuelve la arquitectura correcta sin
# condicionales por TARGETPLATFORM, y ya deja /usr/bin/java y /usr/bin/jar
# registrados via update-alternatives.
RUN apt-get update -qq && \
    apt-get install -y wget iputils-ping curl jq openssh-client openjdk-21-jdk-headless && \
    ln -s "$(dirname "$(dirname "$(readlink -f "$(which java)")")")" /opt/java && \
    rm -rf /var/lib/apt/lists/*

ENV JAVA_HOME=/opt/java

ENV LEIN_VERSION=2.12.0
ENV LEIN_INSTALL=/usr/local/bin/

WORKDIR /tmp

# Download the whole repo as an archive
RUN apt-get update && \
    apt-get install -y gnupg wget && \
    rm -rf /var/lib/apt/lists/* && \
    mkdir -p $LEIN_INSTALL && \
    wget -q https://raw.githubusercontent.com/technomancy/leiningen/$LEIN_VERSION/bin/lein-pkg && \
    echo "Comparing lein-pkg checksum ..." && \
    sha256sum lein-pkg && \
    echo "12a9c5e3a2471619ca3d64a7462f920fdf713ae8959eb4fcd6257c23332b5aa4 *lein-pkg" | sha256sum -c - && \
    mv lein-pkg $LEIN_INSTALL/lein && \
    chmod 0755 $LEIN_INSTALL/lein && \
    wget -q https://github.com/technomancy/leiningen/releases/download/$LEIN_VERSION/leiningen-$LEIN_VERSION-standalone.jar && \
    wget -q https://github.com/technomancy/leiningen/releases/download/$LEIN_VERSION/leiningen-$LEIN_VERSION-standalone.jar.asc && \
    gpg --batch --keyserver hkps://keyserver.ubuntu.com --recv-key 9D13D9426A0814B3373CF5E3D8A8243577A7859F && \
    echo "Verifying file PGP signature..." && \
    gpg --batch --verify leiningen-$LEIN_VERSION-standalone.jar.asc leiningen-$LEIN_VERSION-standalone.jar && \
    rm leiningen-$LEIN_VERSION-standalone.jar.asc && \
    mkdir -p /usr/share/java && \
    mv leiningen-$LEIN_VERSION-standalone.jar /usr/share/java/leiningen-$LEIN_VERSION-standalone.jar && \
    apt-get purge -y --auto-remove gnupg && \
    rm -rf /var/lib/apt/lists/*

ENV PATH=$PATH:$LEIN_INSTALL
ENV LEIN_ROOT=1

# Install clojure 1.12.5 so users don't have to download it every time
RUN echo '(defproject dummy "" :dependencies [[org.clojure/clojure "1.12.5"]])' > project.clj && \
    lein deps && rm project.clj

RUN mkdir -p /opt/quantumlabs/robot
WORKDIR /opt/quantumlabs/robot
COPY . .
ENV PATH=$PATH:/opt/quantumlabs/robot/bin
CMD ["robot", "start"]
