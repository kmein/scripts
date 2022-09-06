#!/bin/sh
curl 'https://api.woerterbuchnetz.de/open-api/dictionaries/DWB/lemmata/*' \
  | jq -c '.result_set | .[] | {lemma: .lemma, id: .wbnetzid}' > dwb-compact.json
