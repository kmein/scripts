#!/bin/sh

root=https://el.wikisource.org

curl -sSL "$root/wiki/%CE%93%CE%BB%CF%8E%CF%83%CF%83%CE%B1%CE%B9" \
  | htmlq --attribute href 'ol li a[title^="Γλ"]' \
  | while read -r slug; do
    curl -sSL "$root$slug" | htmlq dt,dd | tr -d '\n' | sed 's#\(<dt>\)#\n\1#g;s#<dt>\|</dd>##g;s#</dt><dd>#· #g;s#&lt;#<#g;s#&gt;#>#g'
  done
