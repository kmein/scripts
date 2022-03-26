#!/bin/sh
root=https://www.orte-in-deutschland.de

extract_orte() {
  htmlq 'a[href*="-gemeinde-"], a[href*="-ort-"]' --attribute href
}

curl -sSL "$root/alphabetisches-ortsverzeichnis.html" \
  | htmlq '#suchindex li a[href^="orte-in"]' --attribute href \
  | while read -r slug; do
      curl -sSL "$root/$slug" \
        | htmlq '#subsuchindex li a[href^="orte-in"]' --attribute href \
        | while read -r slug; do
            first_page="$(curl -sSL "$root/$slug")"
            echo "$first_page" | extract_orte
            echo "$first_page" | htmlq 'a[href*="?seite="]' --attribute href | sort -u | while read -r page; do
              curl -sSL "$root/$slug$page" | extract_orte
            done
          done
    done
