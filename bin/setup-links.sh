#!/bin/bash

LOC=${PWD}
for filename in \.*; do
    if [[ -f "./$filename" ]]; then
        echo "ln -s $LOC/$filename $HOME/$filename"
        ln -s ${LOC}/${filename} $HOME/${filename}
    fi
done
