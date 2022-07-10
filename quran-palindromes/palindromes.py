import sys
import json

THRESHOLD = 5


def expand(s, low, high):
    while low >= 0 and high < len(s) and s[low] == s[high]:
        yield s[low : high + 1]
        low -= 1
        high += 1


def find_palindromes(s):
    for i in range(len(s)):
        yield from expand(s, i, i)
        yield from expand(s, i, i + 1)


if __name__ == "__main__":
    with open("quran.json", "r") as quran_file:
        quran = json.load(quran_file)
        for surah in quran:
            for aya in surah["verses"]:
                text = aya["text"].translate(
                    dict.fromkeys(
                        list(range(0x64B, 0x65F + 1))
                        + list(range(0x6E1, 0x6EF + 1))
                        + [
                            0x20,
                        ],
                        None,
                    )
                )
                palindromes = list(
                    pal
                    for pal in find_palindromes(text)
                    if len(pal.strip()) > THRESHOLD
                )
                if len(palindromes) > 0:
                    print(surah["id"], aya["id"], palindromes)
