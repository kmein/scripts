#!/bin/sh
python3 kevin.py author "$1" | pandoc -f markdown+smart --table-of-contents --toc-depth=6 --standalone --css=epub.css -o "$2"
