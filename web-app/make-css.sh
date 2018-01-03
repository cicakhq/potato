#!/bin/sh

set -e

echo "Compile the CSS files"

SASSC=sassc
SCSS_SRC=src/css
CSS_OUT=../public/assets/css
MANIFEST_DIR=$(readlink -e ../src/template/)
MANIFEST="$MANIFEST_DIR/manifest"

if [ ! -d $CSS_OUT ] && [ -d ../public ]; then
	 mkdir -p $CSS_OUT
fi

make_scss () {
  cd $1
  for f in *.scss
  do
	  BASE=${f%.*}
	  ${SASSC} $f $2/$BASE.css
	  CKSUM_NAME=$BASE-`md5sum $2/$BASE.css | cut -b 26-32`.css
	  mv $2/$BASE.css $2/$CKSUM_NAME
	  echo '  ("'$BASE.css'" . "'$CKSUM_NAME'")' >> $MANIFEST
  done
}

clean () {
  rm -f $1/*.css
  rm -f $MANIFEST
}

clean $CSS_OUT
echo "(" > $MANIFEST
make_scss $(readlink -e $SCSS_SRC) $(readlink -e $CSS_OUT)
echo ")" >> $MANIFEST
