#!/bin/bash

DIRS_CUSTOM=( )

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

remove_env() {
  printf "Removing current environment ... \t"
  for TARGET in ${DIRS_FROM_BOT[@]}; do
    rm -rf $SRC_DIR/$TARGET;
  done
  for TARGET in ${DIRS_CUSTOM[@]}; do
    rm -rf $SRC_DIR/$TARGET;
  done
  echo "OK"
}

pull_submodule() {
  printf "Pulling submodule changes ... \t\t"
  X=$( ls -A $BOT_DIR 2>&1 )
  if test "$?" = 0; then
    ARGS="--recursive --remote" 
  else
    ARGS="--recursive --remote --init"
  fi
  X=$( git submodule update $ARGS )
  if test "$?" != 0; then
    echo "ERROR"
    echo "Failed pulling submodule"
    exit 1
  fi
  echo "OK"
}

make_env() {
  printf "Making environment ... \t\t\t"
  for TARGET in ${DIRS_FROM_BOT[@]}; do
    X=$( cp -r $BOT_DIR/$TARGET $SRC_DIR/$TARGET 2>&1 );
    if test "$?" != 0; then
      echo "ERROR"
      echo "Failed copying $BOT_DIR/$TARGET"
      exit 1
    fi
  done
  for TARGET in ${DIRS_CUSTOM[@]}; do
    mkdir $SRC_DIR/$TARGET;
    if test "$TARGET" = 'src'; then
      X=$( cp -r $BOT_DIR/$TARGET/py $SRC_DIR/$TARGET 2>&1 );
      if test "$?" != 0; then
        echo "ERROR"
        echo "Failed copying $BOT_DIR/$TARGET/py"
        exit 1
      fi
    fi
  done
  echo "OK"
}

download_jar() {
  rm -rf $SRC_DIR/lib
  mkdir  $SRC_DIR/lib
  aws s3 cp s3://quantum-dataset/DISTRO_robot/robot-$VERSION-standalone.jar $SRC_DIR/lib/
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
    echo "cd $SRC_DIR"
    echo "docker buildx build -t quantumlabs/$NAME:$VERSION --platform linux/arm64,linux/amd64 --push ."
  else
    cd $SRC_DIR
    $DOCKER buildx build -t quantumlabs/$NAME:$VERSION --platform linux/arm64,linux/amd64 --push .
  fi
}

remove_env
pull_submodule

BIN_DIR=$( cd $( dirname ${BASH_SOURCE[0]} ) && pwd )
SRC_DIR=$( dirname $BIN_DIR )/src
BOT_DIR=$( dirname $BIN_DIR )/robot
PROJECT=$( grep "defproject" $BOT_DIR/project.clj )
NAME=$( echo $PROJECT | awk '{split($2,t0,"/"); print(t0[2])}' )
VERSION=$( echo $PROJECT | awk '{print(substr($3,2,length($3)-2))}' )

echo "BIN_DIR=$BIN_DIR"
echo "SRC_DIR=$SRC_DIR"
echo "BOT_DIR=$BOT_DIR"
echo "PROJECT=$PROJECT"
echo "NAME=$NAME"
echo "VERSION=$VERSION"

make_env
download_jar
build_docker_image
