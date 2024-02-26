#!/bin/bash
parent_path=$( cd "$(dirname "${BASH_SOURCE[0]}")" ; pwd -P )
cd "$parent_path"

touch README.temp
cat header.md > README.temp
cat ../README.md >> README.temp
pandoc -N -f markdown -o README.pdf README.temp
rm README.temp
