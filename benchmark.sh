#!/bin/bash

mkdir -p benchmark/dataset benchmark/output
for i in `seq 500 250 5000`; \
do 
  rm -vf benchmark/dataset/$$i.gbr ;
  stack exec -- generategerber --count $i benchmark/dataset/$i.gbr > /dev/null
  for cpu in 1 2 4
  do
    stack exec -- drawgerber benchmark/dataset/$i.gbr benchmark/output/$i.svg +RTS -N${cpu} ;
  done
  sleep 0.1
done

