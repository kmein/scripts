#!/bin/sh
root=https://www.orte-in-deutschland.de

extract_orte() {
  htmlq 'a[href*="-gemeinde-"], a[href*="-ort-"]' --attribute href \
    | sed "s#^#$root/#"
}

curl -sSL "$root/alphabetisches-ortsverzeichnis.html" \
  | htmlq '#suchindex li a[href^="orte-in"]' --attribute href \
  | while read -r slug; do
      curl -sSL "$root/$slug" \
        | htmlq '#subsuchindex li a[href^="orte-in"]' --attribute href \
        | while read -r slug; do
            first_page="$(curl -sSL "$root/$slug")"
            echo "$first_page" | extract_orte
            echo "$first_page" | htmlq 'a[href*="?seite="]' --attribute href | sed 's/.*seite=//' | sort -un | sed -n '1p;$p' | xargs seq 2>/dev/null | sed 's/^/?seite=/' | while read -r page; do
              curl -sSL "$root/$slug$page" | extract_orte
            done
          done
    done
