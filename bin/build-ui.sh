#!/bin/bash
echo "Building javascript " && \
lein with-profile prod cljsbuild once
