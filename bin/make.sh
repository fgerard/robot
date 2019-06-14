#!/bin/bash
echo "Building CSS files ..." && \
lein minify-assets && \
lein less once && \
echo "Building javascript & uberjar .." && \
lein with-profile prod uberjar
