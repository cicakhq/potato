#!/bin/sh

set -e

./make-html.sh
./make-img.sh
./make-vendor.sh
./make-fonts.sh
./make-css.sh
