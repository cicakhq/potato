#!/bin/bash

set -e

echo "Compile the CSS files"

SASSC=sassc
SCSS_SRC=src/css
CSS_OUT=../public/assets/css
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
