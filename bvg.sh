#!/bin/sh

API="https://1.bvg.transport.rest"

JQ_PRELUDE='
  def ansicolor(code): "\u001b[\(code)m\(.)\u001b[0m";

  def prettytime: "\(.[0:19] | strptime("%FT%T") | mktime | strftime("%R"))" | ansicolor(90);

  def prettyline: "\u001b[\(
    if   .product == "subway"   then 34 # blue
    elif .product == "bus"      then 35 # magenta
    elif .product == "tram"     then 31 # red
    elif .product == "suburban" then 32 # green
    elif .product == "regional" then 37 # white
    else 37                             # white
    end
  )m\(.name)\u001b[0m";
'

find_id() {
  query="$1"
  curl -sG "${API}/locations" -d results=1 -d fuzzy=false -d poi=false -d addresses=false -d query="$query" \
    | jq -r ".[0].id"
}

station() {
  query="$1"
  curl -sG "${API}/locations" -d fuzzy=false -d poi=false -d addresses=false -d query="$query" \
    | jq -r '.[] | "\u001b[33m\(.id)\u001b[0m \(.name)"'
}

departures() {
  curl -sG "${API}/stations/$1/departures" -d remarks=false \
    | jq -r "$JQ_PRELUDE"'
      .[]
      | "\(.when | prettytime) \(.line | prettyline)\t\(.direction)"
    '
}

journeys() {
  origin_id="$1"
  destination_id="$2"
  curl -sG "${API}/journeys" -d results=1 -d from="$origin_id" -d to="$destination_id" \
    | jq -r "$JQ_PRELUDE"'
      .[0]
      | .legs
      | .[]
      | "\(.origin.name) \(.departure | prettytime) \(.line | prettyline) \(.arrival | prettytime) \(.destination.name)"
    '
}

case $1 in
  find)
    shift
    station "$@"
  ;;
  d:id|dep:id|departures:id)
    shift
    departures "$@"
  ;;
  j:id|jny:id|journey:id)
    shift
    departures "$@"
  ;;
  j|jny|journey)
    shift
    journeys "$(find_id "$1")" "$(find_id "$2")"
  ;;
  d|dep|departures)
    shift
    departures "$(find_id "$1")"
  ;;
  *)
    echo "Usage: $0 [find STATION NAME | jny ORIGIN DESTINATION | dep QUERY]"
  ;;
esac
