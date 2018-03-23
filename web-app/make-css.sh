#!/bin/sh

set -e

echo "Compile the CSS files"

READLINK=readlink
MD5SUM=md5sum
if [ "`uname -s`" = 'Darwin' ]; then
	echo 'macOS - readlink and md5sum from coreutils'
	READLINK=greadlink
	MD5SUM=gmd5sum
fi
SASSC=sassc
SCSS_SRC=src/css
CSS_OUT=../public/assets/css
MANIFEST_DIR=$($READLINK -e ../src/template/)
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
	  CKSUM_NAME=$BASE-`$MD5SUM $2/$BASE.css | cut -b 26-32`.css
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
make_scss $($READLINK -e $SCSS_SRC) $($READLINK -e $CSS_OUT)
echo ")" >> $MANIFEST
