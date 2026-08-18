#!/bin/bash
#
# Compila el uberjar y el cljs de robot (v1) y robot2 (v2), y construye
# (y sube) la imagen docker quantumlabs/robot:$VERSION, con VERSION tomada
# de project.clj -- unica fuente de verdad para la version.
#
set -e

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

build_project() {
  # lein minify-assets esta roto con las versiones actuales de closure-compiler
  # (choque de classpath entre el closure-compiler viejo que trae lein-asset-minifier
  # y el que trae clojurescript -- ClassCastException en CompilerOptions$Environment).
  # resources/public/js/lib/externs.js ya esta generado y commiteado, y src/js/externs.js
  # no ha cambiado desde entonces, asi que no hace falta regenerarlo en cada build.
  printf "Compilando CSS ... \t\t"
  lein less once
  echo "OK"

  printf "Instalando dependencias npm (shadow-cljs, react, codemirror) ... \n"
  npm install

  printf "Compilando cljs robot v1 y robot2 v2 optimizados (shadow-cljs) ... \n"
  npx shadow-cljs release robot robot2

  printf "Compilando uberjar (backend) ... \n"
  lein with-profile prod uberjar
}

sync_instance_env() {
  ENV_FILE=$ROOT_DIR/docker/execution-template/data/config/instance.env
  printf "Actualizando VERSION/IMAGE_NAME en execution-template/data/config/instance.env ... \t"
  sed -i.bak -E "s/^VERSION=.*/VERSION=$VERSION/; s/^IMAGE_NAME=.*/IMAGE_NAME=$NAME/" $ENV_FILE
  rm -f $ENV_FILE.bak
  echo "OK"
}

build_docker_image() {
  OS=$( check_os )
  DOCKER=""
  if test "$OS" = "Mac"; then
    X=$( groups | grep "admin" 2>&1 )
    if test "$?" != 0; then
      echo "Unable to run docker, $USER must be added to admin group"
      exit 1
    fi
    DOCKER="docker"
  fi
  if test "$OS" = "Linux"; then
    X=$( groups | grep "docker" 2>&1 )
    if test "$?" != 0; then
      X=$( groups | grep "sudo" 2>&1 )
      if test "$?" != 0; then
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
    echo "Try to execute the following commands to create the image:"
    echo ""
    echo "cd $ROOT_DIR"
    echo "docker buildx build -f docker/Dockerfile -t quantumlabs/$NAME:$VERSION --platform linux/arm64,linux/amd64 --push ."
  else
    cd $ROOT_DIR
    $DOCKER buildx build -f docker/Dockerfile -t quantumlabs/$NAME:$VERSION --platform linux/arm64,linux/amd64 --push .
  fi
}

DOCKER_DIR=$( cd $( dirname ${BASH_SOURCE[0]} ) && pwd )
ROOT_DIR=$( dirname $DOCKER_DIR )
PROJECT=$( grep "defproject" $ROOT_DIR/project.clj )
NAME=$( echo $PROJECT | awk '{split($2,t0,"/"); print(t0[2])}' )
VERSION=$( echo $PROJECT | awk '{print(substr($3,2,length($3)-2))}' )

echo "ROOT_DIR=$ROOT_DIR"
echo "NAME=$NAME"
echo "VERSION=$VERSION"

cd $ROOT_DIR
lein clean
build_project
sync_instance_env
build_docker_image
