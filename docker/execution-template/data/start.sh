#!/bin/bash
# Corre en el servidor destino. Se autoubica y monta su propio directorio
# (este mismo, data/) como el volumen de datos del contenedor -- copiar
# execution-template/data/ a donde sea en el servidor y correr ./start.sh
# ahi alcanza, sin depender de un path fijo.
set -e

DATA=$( cd $( dirname ${BASH_SOURCE[0]} ) && pwd )

# VERSION, IMAGE_NAME, CONTAINER_NAME, TIMEZONE, MININUM_MEMORY,
# MAXIMUM_MEMORY, HTTPS_PORT y HTTP_PORT viven en config/instance.env para
# que este script tenga una sola fuente de verdad, editable sin tocar
# start.sh.
INSTANCE_ENV_PATH="$DATA/config/instance.env"
if [ ! -r "$INSTANCE_ENV_PATH" ]; then
  echo "Error: $INSTANCE_ENV_PATH no encontrado. Copialo desde docker/execution-template/data/config/instance.env." >&2
  exit 1
fi
source "$INSTANCE_ENV_PATH"

HOSTNAME=`hostname`

check_os() {
  UNAME=$( uname -s )
  case "${UNAME}" in
    Linux*)     OS=Linux;;
    Darwin*)    OS=Mac;;
    CYGWIN*)    OS=Cygwin;;
    MINGW*)     OS=MinGw;;
    *)          OS="UNKNOWN:${UNAME}"
  esac
  echo $OS
}

OS=$( check_os )
DOCKER=""
if test "$OS" = "Mac"; then
  if ! groups | grep -q "admin"; then
    echo "Unable to run docker, $USER must be added to admin group"
    exit 1
  fi
  DOCKER="docker"
fi
if test "$OS" = "Linux"; then
  if ! groups | grep -q "docker"; then
    if ! groups | grep -q "sudo"; then
      echo "Unable to run docker, $USER must be added to admin docker or sudo"
      exit 1
    else
      DOCKER="sudo docker"
    fi
  else
    DOCKER="docker"
  fi
fi
if test "$DOCKER" = ""; then
  echo "Your OS $OS is not supported by this script"
  exit 1
fi

# Arreglo (no string) para que rutas con espacios no se rompan al expandirse.
# HTTPS_PORT/HTTP_PORT siempre se publican en loopback; BIND_EXTRA_IPS
# (instance.env) agrega IPs adicionales del host abajo.
DOCKER_ARGS=(
              --name="$CONTAINER_NAME"
              --restart=always
              --network=quantum-network
              --add-host=host.docker.internal:host-gateway
              --log-opt max-size=1m
              -e TIMEZONE="$TIMEZONE"
              -e MININUM_MEMORY="$MININUM_MEMORY"
              -e MAXIMUM_MEMORY="$MAXIMUM_MEMORY"
              -e HOSTNAME="$HOSTNAME"
              -e ROBOT_CONFIG=/opt/quantumlabs/robot/data/config
              -e ROBOT_LOGS=/opt/quantumlabs/robot/data/logs
              -v "$DATA:/opt/quantumlabs/robot/data"
              -v "$DATA/app-log:/opt/quantumlabs/robot/app-log"
              -v "$DATA/konservedb:/opt/quantumlabs/robot/konservedb"
              -v /opt/quantum/vision-stream/data/logs:/opt/quantum/vision-stream/data/logs
              -v /opt/quantum/event-stream/data/logs:/opt/quantum/event-stream/data/logs
              -v /opt/quantum/event-stream/data/relevantes:/opt/quantum/event-stream/data/relevantes
              -p "127.0.0.1:${HTTPS_PORT}:4050"
              -p "127.0.0.1:${HTTP_PORT}:8050"
)

# El bind por IP es lo unico que de verdad limita quien alcanza la API: los
# puertos publicados por docker se DNATean en PREROUTING y pasan por
# FORWARD, no por INPUT, asi que ufw/iptables -INPUT NO los filtra. Por eso
# 0.0.0.0 aqui (permitido, no validamos) = API abierta a internet.
for BIND_IP in ${BIND_EXTRA_IPS:+${BIND_EXTRA_IPS//,/ }}; do
  echo "Publicando ademas en $BIND_IP:$HTTPS_PORT y $BIND_IP:$HTTP_PORT"
  DOCKER_ARGS+=( -p "${BIND_IP}:${HTTPS_PORT}:4050" -p "${BIND_IP}:${HTTP_PORT}:8050" )
done

$DOCKER run -d "${DOCKER_ARGS[@]}" quantumlabs/$IMAGE_NAME:$VERSION
