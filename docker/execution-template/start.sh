#!/bin/bash
# Corre en el servidor destino, junto al project.clj generado por
# docker/build_image.sh (mismo directorio) del cual se lee VERSION,
# para no tener que hardcodearla aqui.
BIN_DIR=$( cd $( dirname ${BASH_SOURCE[0]} ) && pwd )
PROJECT=$( grep "defproject" $BIN_DIR/project.clj )
VERSION=$( echo $PROJECT | awk '{print(substr($3,2,length($3)-2))}' )
HOSTNAME=`hostname`

docker run --name robot \
	        --restart=always \
		--network quantum-network \
		--log-opt max-size=1m \
		-e TIMEZONE=GMT-6:00 \
		-e MININUM_MEMORY=1G \
		-e MAXIMUM_MEMORY=3G \
		-e HOSTNAME=$HOSTNAME \
		-e ROBOT_CONFIG=/opt/quantumlabs/robot/data/config \
		-e ROBOT_LOGS=/opt/quantumlabs/robot/data/logs \
		-v /opt/quantum/robot/data:/opt/quantumlabs/robot/data \
		-v /opt/quantum/robot/data/app-log:/opt/quantumlabs/robot/app-log \
		-v /opt/quantum/robot/data/konservedb:/opt/quantumlabs/robot/konservedb \
		-v /opt/quantum/vision-stream/data/logs:/opt/quantum/vision-stream/data/logs \
		-v /opt/quantum/event-stream/data/logs:/opt/quantum/event-stream/data/logs \
		-v /opt/quantum/event-stream/data/relevantes:/opt/quantum/event-stream/data/relevantes \
		-p 14050:4050 \
		-p 18050:8050 \
		-d \
		quantumlabs/robot:$VERSION
