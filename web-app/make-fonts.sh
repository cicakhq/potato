#!/bin/bash

set -e

CACHE=.fontcache
DEST=../public/assets/fonts/
if [ ! -d $DEST ] && [ -d ../public ]; then
	mkdir -p $DEST
fi

if [ ! -f $DEST/noto/NotoColorEmoji.ttf ] || [ ! -f $CACHE/font-awe.zip ] || [ ! -f $CACHE/font-ssp.zip ] \
   || [ ! -f $CACHE/font-symb.zip ] || [ ! -f $CACHE/NotoColorEmoji.ttf ]; then
	echo "Download and copy fonts:"
	if [ ! -d $CACHE ]; then mkdir $CACHE; fi
	echo "1. Font Awesome"
	curl -o $CACHE/font-awe.zip http://fontawesome.io/assets/font-awesome-4.7.0.zip
	if [ ! -d $DEST/awe ]; then mkdir -p $DEST/awe/; fi
	unzip -oj $CACHE/font-awe.zip '*/fonts/*' -d $DEST/awe/
	echo "2. Adobe Source Sans Pro"
	curl -o $CACHE/font-ssp.zip https://codeload.github.com/adobe-fonts/source-sans-pro/zip/2.020R-ro/1.075R-it
	if [ ! -d $DEST/ssp ]; then mkdir -p $DEST/ssp/; fi
	unzip -oj $CACHE/font-ssp.zip '**/SourceSansPro*' -d $DEST/ssp/
	echo "3. Symbola for Emojis"
	curl -o $CACHE/font-symb.zip https://codeload.github.com/ChALkeR/Symbola-Emoji/zip/master
	if [ ! -d $DEST/symb ]; then mkdir -p $DEST/symb/; fi
	unzip -oj $CACHE/font-symb.zip '**/Symbola-Emoji*' -d $DEST/symb/
	echo "4. Google Noto Color Emoji"
	curl -o $CACHE/NotoColorEmoji.ttf https://raw.githubusercontent.com/googlei18n/noto-emoji/master/fonts/NotoColorEmoji.ttf
	if [ ! -d $DEST/noto ]; then mkdir -p $DEST/noto/; fi
	cp $CACHE/NotoColorEmoji.ttf $DEST/noto/
else
	echo "Fonts already are installed"
fi
