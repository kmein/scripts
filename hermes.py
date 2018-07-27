#!/usr/bin/env python3
import itertools
import subprocess

from PyDictionary import PyDictionary

dictionary = PyDictionary()

def translate_en_de(word):
    return dictionary.translate(word, "de")

def pronunciation(word, lang="de"):
    command = ["espeak", "-x", "-q", "-v", lang, word]
    return subprocess.check_output(command).decode("utf-8").strip()

def is_rhyme(word1, word2, lang="de"):
    coda1 = itertools.dropwhile(lambda c: c != "'", pronunciation(word1, lang))
    coda2 = itertools.dropwhile(lambda c: c != "'", pronunciation(word2, lang))
    return list(coda1) == list(coda2)

if __name__ == "__main__":
    print(pronunciation("hallo welt"))
    print(translate_en_de("woe"))
    print(is_rhyme("welt", "geld"))
