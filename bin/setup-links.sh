#!/bin/bash

for filename in \.*; do
    if [[ -f "./$filename" ]]
    then
        filepath=$(realpath "$filename")
        echo "ln -s $filepath $HOME/$filename"
        ln -s ${LOC}/${filename} $HOME/${filename}
    fi
done
