#!/bin/bash

if [ ! -f out/font-awe.zip ] || [ ! -f out/font-ssp.zip ] || [ ! -f out/font-symb.zip ]; then
	echo "Download and copy fonts:"
	mkdir out
	echo "1. Font Awesome"
	curl -o out/font-awe.zip http://fontawesome.io/assets/font-awesome-4.7.0.zip
	mkdir -p ../public/assets/fonts/awe/
	unzip -oj out/font-awe.zip '*/fonts/*' -d ../public/assets/fonts/awe/
	echo "2. Adobe Source Sans Pro"
	curl -o out/font-ssp.zip https://codeload.github.com/adobe-fonts/source-sans-pro/zip/2.020R-ro/1.075R-it
	mkdir -p ../public/assets/fonts/ssp/
	unzip -oj out/font-ssp.zip '**/SourceSansPro*' -d ../public/assets/fonts/ssp/
	echo "3. Symbola for Emojis"
	curl -o out/font-symb.zip https://codeload.github.com/ChALkeR/Symbola-Emoji/zip/master
	mkdir -p ../public/assets/fonts/symb/
	unzip -oj out/font-symb.zip '**/Symbola-Emoji*' -d ../public/assets/fonts/symb/
else
	echo "Fonts already are installed"
fi
