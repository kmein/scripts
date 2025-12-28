#!/bin/sh
set -efu

tokenize() {
  tr -cs '[:alpha:]' '\n' | tr '[:upper:]' '[:lower:]'
}

text="Once upon a time, in a quiet village nestled between rolling hills and sparkling rivers, there lived a clever fox named Felix. Felix was known throughout the village for his cunning tricks and playful antics. Every morning, he would sneak through the meadows, darting past rabbits and chickens, always careful to avoid the farmer's watchful eyes. Despite his mischievous ways, Felix had a kind heart and often shared his clever solutions with friends in need. One day, a heavy storm swept through the valley, leaving many paths muddy and rivers swollen. Felix saw his chance to help: he guided lost ducklings back to their pond, and showed the frightened kittens how to find shelter under the sturdy oak trees. The villagers watched in amazement as the fox moved gracefully through the rain-soaked fields, his orange fur glistening and his sharp eyes alert. By the time the storm passed, the village had gained a newfound respect for Felix. Tales of his bravery spread far and wide, carried by wandering merchants and whispered by children as they played near the cobblestone streets. Nights in the village were quiet once more, but the story of Felix, the fox who danced through storm and shadow, continued to inspire laughter, cleverness, and courage in the hearts of all who heard it."

echo "$text" | tokenize | python3 sort.py
