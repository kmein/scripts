#!/bin/sh

page() {
  tr -dc "ABCDEFGHILMNOPQRSTVXYZ ,." < /dev/urandom \
    | fold -w 80 \
    | head -n 40 \
    | awk '{print $0} END { print "" }'
}

for _ in $(seq 1 410); do
  page
done
