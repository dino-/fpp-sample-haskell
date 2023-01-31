#! /bin/bash

pngSrcFile=${1:?'ERROR must specify an image file'}

cd images

# Perform the image compression
pngquant --skip-if-larger --ext -qua.png $pngSrcFile

if [ $? != 0 ] ; then
   echo "Running pngquant on $pngSrcFile failed!" >> ../pngquant.log
else
   # Replace the original file with the new, smaller one
   pngQuantFile="$(basename $pngSrcFile .png)-qua.png"
   rm -f $pngSrcFile
   mv $pngQuantFile $pngSrcFile
fi
