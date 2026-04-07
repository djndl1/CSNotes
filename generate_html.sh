#!/bin/bash
find $1 -iname "*.org" -exec pandoc -s --toc -f org -t html -o {}.html {} \;


