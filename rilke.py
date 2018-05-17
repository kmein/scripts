from argparse import ArgumentParser
from enum import Enum
from bs4 import BeautifulSoup
import re
import requests
from typing import Iterator, NewType

class Language(Enum):
    EN = "en"
    DE = "de"

DEFAULT_LANG = Language.DE
Url = NewType("Url", str)

def cook_soup(url: Url) -> BeautifulSoup:
    return BeautifulSoup(requests.get(url).text, "lxml")

def base_url(lang: Language = DEFAULT_LANG) -> Url:
    return Url("https://{}.wiktionary.org".format(lang.value))

def entry_url(word: str, lang: Language = DEFAULT_LANG) -> Url:
    return Url(base_url(lang) + "/wiki/" + word)

def rhymes_url(entry_url: Url, lang: Language = DEFAULT_LANG) -> Url:
    soup = cook_soup(entry_url)
    result_url = base_url(lang)
    try:
        if lang == Language.DE:
            result_url += soup.find("a", href=re.compile(r"/wiki/Reim:Deutsch:.*"))["href"]
        elif lang == Language.EN:
            result_url += soup.find("a", href=re.compile(r"/wiki/Rhymes:English/.*"))["href"]
        return result_url
    except:
        raise ValueError("Entry at {} not found.".format(entry_url))

def find_rhymes(rhymes_url: Url) -> Iterator[str]:
    soup = cook_soup(rhymes_url)
    for li in soup.select("div#content ul > li > a"):
        try:
            yield li.text
        except KeyError:
            continue

if __name__ == "__main__":
    parser = ArgumentParser(description="Find your rhyme in no time")
    parser.add_argument("entry", metavar="WORD", type=str, help="the word to rhyme")
    parser.add_argument("-l", "--lang", help="the language", choices=["de", "en"], nargs="?", default="de")
    args = parser.parse_args()

    entry = entry_url(args.entry, lang=Language(args.lang))
    rhymes = rhymes_url(entry, lang=Language(args.lang))
    for rhyme in find_rhymes(rhymes):
        print(rhyme)
