#!/bin/bash - 
set -o nounset                              # Treat unset variables as an error

find $1 -iname "*.org" -exec pandoc -s --toc -f org -t html -o {}.html {} \;


