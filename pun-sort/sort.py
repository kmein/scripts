#!/usr/bin/env python3

import sys
import string
import subprocess
from functools import lru_cache
import argparse

# -------------------------
# IPA helpers
# -------------------------

def get_ipa(word, lang="en"):
    try:
        out = subprocess.check_output(
            ["espeak-ng", "-v", lang, "-q", "--ipa=3", word],
            stderr=subprocess.DEVNULL,
            text=True
        )
        return out.strip().strip("/")
    except Exception:
        return ""

def ipa_tokenize(ipa):
    tokens = []
    i = 0
    while i < len(ipa):
        ch = ipa[i]
        if ch in "ˈˌ":
            i += 1
            continue
        if i + 1 < len(ipa) and ipa[i:i+2] in {"aɪ", "aʊ", "eɪ", "oʊ", "ɔɪ"}:
            tokens.append(ipa[i:i+2])
            i += 2
        else:
            tokens.append(ch)
            i += 1
    return tokens

# -------------------------
# Distance
# -------------------------

VOWELS = set("aeiouəɪʊɔɛɜɑæ")

def sub_cost(a, b):
    if a == b:
        return 0.0
    if a in VOWELS and b in VOWELS:
        return 0.6
    if a in VOWELS or b in VOWELS:
        return 2.0
    return 1.0

@lru_cache(maxsize=None)
def phonetic_distance(a, b):
    a = tuple(a)
    b = tuple(b)
    n, m = len(a), len(b)
    dp = [[0] * (m + 1) for _ in range(n + 1)]

    for i in range(n + 1):
        dp[i][0] = i
    for j in range(m + 1):
        dp[0][j] = j

    for i in range(1, n + 1):
        for j in range(1, m + 1):
            dp[i][j] = min(
                dp[i - 1][j] + 1,
                dp[i][j - 1] + 1,
                dp[i - 1][j - 1] + sub_cost(a[i - 1], b[j - 1])
            )

    return dp[n][m]

# -------------------------
# Seriation
# -------------------------

def seriate(words, ipas):
    unused = set(words)
    path = [words[0]]
    unused.remove(words[0])

    while unused:
        cur = path[-1]
        nxt = min(
            unused,
            key=lambda w: phonetic_distance(ipas[cur], ipas[w]) / max(len(ipas[cur]), len(ipas[w]), 1)
        )
        path.append(nxt)
        unused.remove(nxt)

    return path

# -------------------------
# Main
# -------------------------



def tokenize_stdin():
    """
    Reads stdin and returns a list of lowercase words.
    Handles:
      - Unicode letters (ä, ö, ü, ß, é, ñ, etc.)
      - Ignores punctuation
    """
    text = sys.stdin.read()
    tokens = text.translate(str.maketrans('', '', string.punctuation)).split()
    return tokens


def main():
    parser = argparse.ArgumentParser(description="Pun-sort words by phonetic similarity")
    parser.add_argument(
        "--lang", "-l",
        type=str,
        default="en",
        help="Language code for espeak-ng (default: en)"
    )

    args = parser.parse_args()
    LANG = args.lang

    words = tokenize_stdin()
    words = list(dict.fromkeys(words))
    ipas = {w: tuple(ipa_tokenize(get_ipa(w, lang=LANG))) for w in words}

    ordered = seriate(words, ipas)

    for w in ordered:
        print(f"{w}\t/{''.join(ipas[w])}/")

if __name__ == "__main__":
    main()
