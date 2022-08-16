#!/usr/bin/env bash
FILE=$(mktemp)
OUTDIR=$(mktemp -d)
curl -sSL 'https://www.mobileread.com/forums/attachment.php?attachmentid=135188&d=1424683732' -o "$FILE"
unzip "$FILE" -d "$OUTDIR"
sed '/^#/d' "$OUTDIR"/*.txt > input.txt
