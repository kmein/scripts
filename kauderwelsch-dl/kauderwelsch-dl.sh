#!/bin/sh
mkdir -p kauderwelsch

for volume in $(seq -w 1 200); do
  if language_name="$(curl -sSL "https://www.reise-know-how.de/kauderwelsch/$volume" | grep -oP '<title>\K[^<]*' | sed 's![/]!_!g;s! - Wort für Wort!!g')"; then
    test -n "$language_name" || continue

    echo "$volume = $language_name"
    directory="kauderwelsch/$volume $language_name"

    if [ ! -d "$directory" ]; then # dont run if directory already exists
      mkdir -p "$directory"
      for track in $(seq -w 1 20); do
        url="https://www.reise-know-how.de/kauderwelsch/$volume/Track-$track"
        path="$directory/$track.mp3"
        curl -sSL "$url" -o "$path"
        if [ -f "$path" ] && [ "$(stat -c %s "$path")" = 50 ]; then # on failure, reise-know-how.de returns 50 bytes
          rm -f "$path"
        else
          echo "+ Track $track"
        fi
      done
    else
      echo "$directory skipped"
    fi
  fi
done
