#!/bin/bash

echo "BUILDING"
stack build
zip out/source.zip src/* app/* stack.yaml package.yaml

for file in $(ls input | grep "\.txt\$")
do
    echo $file
  ./haskcode-exe input/$file > out/$file
done
