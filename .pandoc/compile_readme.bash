#!/bin/bash
parent_path=$( cd "$(dirname "${BASH_SOURCE[0]}")" ; pwd -P )
filename="documentation"
cd "$parent_path"

touch ${filename}.temp
cat header.md > ${filename}.temp
cat ../${filename}.md >> ${filename}.temp
pandoc -N -f markdown -o ../${filename}.pdf ${filename}.temp
rm ${filename}.temp
