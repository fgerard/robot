#!/bin/bash

export BIN_PATH="$(cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
export ROOT_PATH=$(dirname $BIN_PATH)

export SERVICE_VERSION=$(sed -n -e '/^(defproject/p' $ROOT_PATH/project.clj | grep -Eo '"(.+)"' | grep -Eo '[a-zA-Z0-9\.-]+')
export SERVICE_NAME=$(sed -n -e '/^(defproject/p' $ROOT_PATH/project.clj | grep -Eo '/(.+) ' | grep -Eo '[a-zA-Z0-9\.-]+')

echo "Description found in project"
echo "Service name    : $SERVICE_NAME"
echo "Service version : $SERVICE_VERSION"

printf $SERVICE_NAME > $ROOT_PATH/NAME
printf $SERVICE_VERSION > $ROOT_PATH/VERSION

