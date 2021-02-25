#!/bin/bash

echo "BUILDING"
stack build

for file in $(ls res | grep "\.in\$")
do
    echo $file
  ./practice-exe res/$file > out/$file
done

zip out/source.zip src/* app/* stack.yaml package.yaml
