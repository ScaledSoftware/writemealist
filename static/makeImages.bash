#!/bin/bash -le

defaultSize=16x16

if [ ! -e greyCheckLarge.png ] ; then
    curl http://www.clker.com/cliparts/f/6/3/2/1352836202549906166Apply%20Symbol.svg.hi.png > greyCheckLarge.png
    convert greyCheckLarge.png -resize 16x16 greyCheck16x16.png
fi

if [ ! -e blueCheckLarge.png ] ; then
    curl http://www.clker.com/cliparts/w/p/g/m/V/Z/check-hi.png > blueCheckLarge.png
    convert blueCheckLarge.png -resize 16x16 blueCheck16x16.png
fi

if [ ! -e emptyBoxLarge.png ] ; then
    curl http://www.clker.com/cliparts/e/q/p/N/s/G/checkbox-unchecked-hi.png > emptyBoxLarge.png
    convert emptyBoxLarge.png -resize 16x16 emptyBox16x16.png
fi

if [ ! -e checkedBoxLarge.png ] ; then
    curl http://www.clker.com/cliparts/z/Q/9/Y/Q/M/checkbox-checked-hi.png > checkedBoxLarge.png
    convert checkedBoxLarge.png -resize 16x16 checkedBox16x16.png
fi


if [ ! -e greenCheckLarge.png ] ; then
    curl http://www.clker.com/cliparts/e/2/a/d/1206574733930851359Ryan_Taylor_Green_Tick.svg.hi.png > greenCheckLarge.png
    convert greenCheckLarge.png -resize 12x12 greenCheck12x12.png
fi

convert -page +0+0 ./emptyBox16x16.png -page +4+1 greenCheck12x12.png -layers flatten checkedBox.png

rm *Large.png blueCheck16x16.png greenCheck12x12.png greyCheck16x16.png
touch ../Settings/StaticFiles.hs
