#!/bin/sh

set -e

DEST=../public/assets/fonts/
if [ ! -d $DEST ] && [ -d ../public ]; then
	mkdir -p $DEST
fi

if [ ! -f $DEST/noto/NotoColorEmoji.ttf ] || [ ! -f out/font-awe.zip ] || [ ! -f out/font-ssp.zip ] \
   || [ ! -f out/font-symb.zip ] || [ ! -f out/NotoColorEmoji.ttf ]; then
	echo "Download and copy fonts:"
	if [ ! -d out ]; then mkdir out; fi
	echo "1. Font Awesome"
	curl -o out/font-awe.zip https://use.fontawesome.com/releases/v5.0.8/fontawesome-free-5.0.8.zip
#http://fontawesome.io/assets/font-awesome-4.7.0.zip
	if [ ! -d $DEST/awe ]; then mkdir -p $DEST/awe/; fi
	unzip -oj out/font-awe.zip '*/webfonts/*' -d $DEST/awe/
	echo "2. Adobe Source Sans Pro"
	curl -o out/font-ssp.zip https://codeload.github.com/adobe-fonts/source-sans-pro/zip/2.020R-ro/1.075R-it
	if [ ! -d $DEST/ssp ]; then mkdir -p $DEST/ssp/; fi
	unzip -oj out/font-ssp.zip '**/SourceSansPro*' -d $DEST/ssp/
	echo "3. Symbola for Emojis"
	curl -o out/font-symb.zip https://codeload.github.com/ChALkeR/Symbola-Emoji/zip/master
	if [ ! -d $DEST/symb ]; then mkdir -p $DEST/symb/; fi
	unzip -oj out/font-symb.zip '**/Symbola-Emoji*' -d $DEST/symb/
	echo "4. Google Noto Color Emoji"
	curl -o out/NotoColorEmoji.ttf https://raw.githubusercontent.com/googlei18n/noto-emoji/master/fonts/NotoColorEmoji.ttf
	if [ ! -d $DEST/noto ]; then mkdir -p $DEST/noto/; fi
	cp out/NotoColorEmoji.ttf $DEST/noto/
else
	echo "Fonts already are installed"
fi
