#!/bin/bash

echo "Compile the CSS files"

SASSC=sassc
SCSS_SRC=src/css
CSS_OUT=../public/assets/css
MANIFEST=$(readlink -e ../src/template/manifest/css.manifest)

function make_scss {
  cd $1
  for f in *.scss
  do
	  BASE=${f%.*}
	  ${SASSC} $f $2/$BASE.css
	  CKSUM_NAME=$BASE-`md5sum $2/$BASE.css | cut -b 26-32`.css
	  mv $2/$BASE.css $2/$CKSUM_NAME
	  echo "$BASE.css: $CKSUM_NAME" >> $MANIFEST
  done
}

function clean {
  rm -f $1/*.css
  rm -f $MANIFEST
}

clean $CSS_OUT
make_scss $(readlink -e $SCSS_SRC) $(readlink -e $CSS_OUT)
