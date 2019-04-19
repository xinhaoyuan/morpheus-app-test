#!/bin/bash

NAME=$1
RANGE_START=$2
RANGE_END=$3

make -C $NAME distclean

for i in `seq $RANGE_START $RANGE_END`; do
    rm -rf ${NAME}_copy_${i}
    cp -r $NAME ${NAME}_copy_${i}
    # make -C ${NAME}_copy_${i}
done
