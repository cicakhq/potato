#!/bin/bash

if [ ! -f out/font-awe.zip ]; then
	echo "Download and copy fonts"
	mkdir out
	curl -o out/font-awe.zip http://fontawesome.io/assets/font-awesome-4.7.0.zip
	unzip -oj out/font-awe.zip '*/fonts/*' -d ../public/assets/fonts/awe/
else
	echo "Fonts already are installed"
fi
