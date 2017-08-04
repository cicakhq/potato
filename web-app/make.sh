#!/bin/bash

set -e

DEST=../public/assets

### Images
DEST_IMG=$DEST/img/
if [ ! -d $DEST_IMG ] && [ -d ../public ]; then
        mkdir -p $DEST_IMG
fi

echo "Copy images"
cp -r src/images/* $DEST_IMG

### Fonts
CACHE=.fontcache
DEST_FONT=$DEST/fonts/
if [ ! -d $DEST_FONT ] && [ -d ../public ]; then
	mkdir -p $DEST_FONT
fi

if [ ! -f $DEST_FONT/noto/NotoColorEmoji.ttf ] || [ ! -f $CACHE/font-awe.zip ] || [ ! -f $CACHE/font-ssp.zip ] \
   || [ ! -f $CACHE/font-symb.zip ] || [ ! -f $CACHE/NotoColorEmoji.ttf ]; then
	echo "Download and copy fonts:"
	if [ ! -d $CACHE ]; then mkdir $CACHE; fi
	echo "1. Font Awesome"
	curl -o $CACHE/font-awe.zip http://fontawesome.io/assets/font-awesome-4.7.0.zip
	if [ ! -d $DEST_FONT/awe ]; then mkdir -p $DEST_FONT/awe/; fi
	unzip -oj $CACHE/font-awe.zip '*/fonts/*' -d $DEST_FONT/awe/
	echo "2. Adobe Source Sans Pro"
	curl -o $CACHE/font-ssp.zip https://codeload.github.com/adobe-fonts/source-sans-pro/zip/2.020R-ro/1.075R-it
	if [ ! -d $DEST_FONT/ssp ]; then mkdir -p $DEST_FONT/ssp/; fi
	unzip -oj $CACHE/font-ssp.zip '**/SourceSansPro*' -d $DEST_FONT/ssp/
	echo "3. Symbola for Emojis"
	curl -o $CACHE/font-symb.zip https://codeload.github.com/ChALkeR/Symbola-Emoji/zip/master
	if [ ! -d $DEST_FONT/symb ]; then mkdir -p $DEST_FONT/symb/; fi
	unzip -oj $CACHE/font-symb.zip '**/Symbola-Emoji*' -d $DEST_FONT/symb/
	echo "4. Google Noto Color Emoji"
	curl -o $CACHE/NotoColorEmoji.ttf https://raw.githubusercontent.com/googlei18n/noto-emoji/master/fonts/NotoColorEmoji.ttf
	if [ ! -d $DEST_FONT/noto ]; then mkdir -p $DEST_FONT/noto/; fi
	cp $CACHE/NotoColorEmoji.ttf $DEST_FONT/noto/
else
	echo "Fonts already are installed"
fi


### CSS
echo "Compile the CSS files"

SASSC=sassc
SCSS_SRC=src/css
CSS_OUT=$DEST/css
MANIFEST_DIR=$(realpath -e ../src/template/)
MANIFEST="$MANIFEST_DIR/manifest"

set +e
MD5SUM=`which md5sum`
set -e
if [ ! -x "$MD5SUM" ]; then
  MD5SUM='md5 -r'
fi

if [ ! -d $CSS_OUT ] && [ -d ../public ]; then
	 mkdir -p $CSS_OUT
fi

function make_scss {
  cd $1
  for f in *.scss
  do
	  BASE=${f%.*}
	  ${SASSC} $f $2/$BASE.css
	  CKSUM_NAME=$BASE-`$MD5SUM $2/$BASE.css | cut -b 26-32`.css
	  mv $2/$BASE.css $2/$CKSUM_NAME
	  echo '  ("'$BASE.css'" . "'$CKSUM_NAME'")' >> $MANIFEST
  done
}

function clean {
  rm -f $1/*.css
  rm -f $MANIFEST
}

clean $CSS_OUT
echo "(" > $MANIFEST
make_scss $(realpath -e $SCSS_SRC) $(realpath -e $CSS_OUT)
echo ")" >> $MANIFEST

