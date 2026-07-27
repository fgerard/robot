FROM ubuntu:18.04

ARG TARGETPLATFORM
ENV DEBIAN_FRONTEND=noninteractive

WORKDIR /opt


RUN apt-get update -qq && \
    apt-get install -y wget && \
    apt update -qq && \
    apt install -y iputils-ping curl jq && \
    apt install -y openssh-client && \
    if test "$TARGETPLATFORM" = 'linux/amd64'; then wget --no-check-certificate https://quantum-dataset.s3.amazonaws.com/DISTRO_zulu/zulu-11/zulu11.50.19-ca-jdk11.0.12-linux_x64.tar.gz && tar xzf zulu11.50.19-ca-jdk11.0.12-linux_x64.tar.gz && ln -s zulu11.50.19-ca-jdk11.0.12-linux_x64 java; fi && \
    if test "$TARGETPLATFORM" = 'linux/arm64'; then wget --no-check-certificate https://quantum-dataset.s3.amazonaws.com/DISTRO_zulu/zulu-11/zulu11.50.19-ca-jdk11.0.12-linux_aarch64.tar.gz && tar xzf zulu11.50.19-ca-jdk11.0.12-linux_aarch64.tar.gz && ln -s zulu11.50.19-ca-jdk11.0.12-linux_aarch64 java; fi && \
    rm zulu*tar.gz && \
    update-alternatives --install /usr/bin/java java /opt/java/bin/java 1 && \
    update-alternatives --install /usr/bin/jar jar /opt/java/bin/jar 1 && \
    update-alternatives --set java /opt/java/bin/java && \
    update-alternatives --set jar /opt/java/bin/jar && \
    rm -rf /var/lib/apt/lists/*

ENV JAVA_HOME=/opt/java

ENV LEIN_VERSION=2.9.6
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
    echo "094b58e2b13b42156aaf7d443ed5f6665aee27529d9512f8d7282baa3cc01429 *lein-pkg" | sha256sum -c - && \
    mv lein-pkg $LEIN_INSTALL/lein && \
    chmod 0755 $LEIN_INSTALL/lein && \
    wget -q https://github.com/technomancy/leiningen/releases/download/$LEIN_VERSION/leiningen-$LEIN_VERSION-standalone.zip && \
    wget -q https://github.com/technomancy/leiningen/releases/download/$LEIN_VERSION/leiningen-$LEIN_VERSION-standalone.zip.asc && \
    gpg --batch --keyserver hkps://keyserver.ubuntu.com --recv-key 20242BACBBE95ADA22D0AFD7808A33D379C806C3 && \
    echo "Verifying file PGP signature..." && \
    gpg --batch --verify leiningen-$LEIN_VERSION-standalone.zip.asc leiningen-$LEIN_VERSION-standalone.zip && \
    rm leiningen-$LEIN_VERSION-standalone.zip.asc && \
    mkdir -p /usr/share/java && \
    mv leiningen-$LEIN_VERSION-standalone.zip /usr/share/java/leiningen-$LEIN_VERSION-standalone.jar && \
    apt-get purge -y --auto-remove gnupg && \
    rm -rf /var/lib/apt/lists/*

ENV PATH=$PATH:$LEIN_INSTALL
ENV LEIN_ROOT=1

# Install clojure 1.10.1 so users don't have to download it every time
RUN echo '(defproject dummy "" :dependencies [[org.clojure/clojure "1.10.1"]])' > project.clj && \
    lein deps && rm project.clj

RUN mkdir -p /opt/quantumlabs/robot
WORKDIR /opt/quantumlabs/robot
COPY . .
ENV PATH=$PATH:/opt/quantumlabs/robot/bin
CMD ["robot", "start"]
