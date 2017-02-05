#!/bin/bash

for HTML in *.html;
do
  MD=`echo $HTML | sed "s/html$/md/g"`
  head -1 $HTML > $MD
  awk 'NR==2,/---/' $HTML >> $MD
  awk 'NR>2' $HTML | sed -e '1,/---/ d' > TMP1
  pandoc -s -r html TMP1 -o TMP2
  cat TMP2 >> $MD
  rm -f TMP*
done
