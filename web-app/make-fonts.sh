#!/bin/bash

mkdir tmp
curl -o tmp/font-awe.zip http://fontawesome.io/assets/font-awesome-4.7.0.zip
unzip -oj tmp/font-awe.zip '*/fonts/*' -d ../public/assets/fonts/awe/
rm -f tmp/font-awe.zip
