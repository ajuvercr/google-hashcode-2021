#!/bin/bash

echo "BUILDING"
stack build
zip out/source.zip src/* app/* stack.yaml package.yaml

for file in $(ls res | grep "\.in\$")
do
    echo $file
  ./practice-exe res/$file > out/$file
done
