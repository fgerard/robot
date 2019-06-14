#!/usr/bin/env bash
#
# Distro Generator
#

export DKR_PATH="$(cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )" && \
export ROOT_PATH=$(dirname $DKR_PATH) && \
export PROJECT=$( grep "defproject" $ROOT_PATH/project.clj ) && \
export SERVICE_NAME=$( echo $PROJECT | awk '{split($2,t0,"/"); print(t0[2])}' ) && \
export SERVICE_VERSION=$( echo $PROJECT | awk '{print(substr($3,2,length($3)-2))}' ) && \
export DISTRO_DIR=$SERVICE_NAME-$SERVICE_VERSION && \
cd $ROOT_PATH && \
echo "Compiling in $ROOT_PATH" && \
lein clean && \
bin/make.sh && \
echo "Assembling components in $ROOT_PATH/$DISTRO_DIR" && \
rm -rvf $DISTRO_DIR* && \
mkdir -p $DISTRO_DIR/bin $DISTRO_DIR/lib $DISTRO_DIR/config $DISTRO_DIR/app-log && \
cp -v target/*standalone.jar $DISTRO_DIR/lib/ && \
cp -v bin/run-headless.sh $DISTRO_DIR/bin/ && \
cp -v bin/robot $DISTRO_DIR/bin/ && \
cp -rv config $DISTRO_DIR/ && \
cp -vr resources $DISTRO_DIR/ && \
cp -v project.clj $DISTRO_DIR/ && \
echo "Packaging $ROOT_PATH/$DISTRO_DIR.tar.gz" && \
tar cvzf $DISTRO_DIR.tgz $DISTRO_DIR 
#zip -r $DISTRO_DIR.zip $DISTRO_DIR
#tar -czvf $ROOT_PATH/distro/$SERVICE_NAME.tgz $SERVICE_NAME/project.clj $SERVICE_NAME/bin $SERVICE_NAME/config/caudal-config.clj $SERVICE_NAME/config/log4j2.xml $SERVICE_NAME/resources $SERVICE_NAME/security $ROOT_PATH/lib
