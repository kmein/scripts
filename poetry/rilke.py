from argparse import ArgumentParser
from bs4 import BeautifulSoup
from enum import Enum
from sys import stderr
from typing import Iterator, NewType
import re
import requests

Url = NewType("Url", str)
class Language(Enum):
    EN = "en"
    DE = "de"

DEFAULT_LANG = Language.DE

def cook_soup(url: Url) -> BeautifulSoup:
    return BeautifulSoup(requests.get(url).text, "lxml")

def base_url(lang: Language = DEFAULT_LANG) -> Url:
    return Url("https://{}.wiktionary.org".format(lang.value))

def entry_url(word: str, lang: Language = DEFAULT_LANG) -> Url:
    return Url(base_url(lang) + "/wiki/" + word)

def rhymes_url(entry_url: Url, lang: Language = DEFAULT_LANG) -> Url:
    soup = cook_soup(entry_url)
    result_url = base_url(lang)
    if lang == Language.DE:
        pattern = re.compile(r"/wiki/Reim:Deutsch:.*")
    elif lang == Language.EN:
        pattern = re.compile(r"/wiki/Rhymes:English/.*")
    anchor = soup.find("a", href=pattern)
    if anchor is not None:
        return result_url + anchor["href"]
    else:
        raise ValueError("Entry at {} not found.".format(entry_url))

def find_rhymes(rhymes_url: Url) -> Iterator[str]:
    soup = cook_soup(rhymes_url)
    for li in soup.select("div#content ul > li > a"):
        yield li.text

if __name__ == "__main__":
    parser = ArgumentParser(description="Find your rhyme in no time")
    parser.add_argument("entry", metavar="WORD", type=str, help="the word to rhyme")
    parser.add_argument("-l", "--lang", help="the language", choices=["de", "en"], nargs="?", default="de")
    args = parser.parse_args()
    try:
        entry = entry_url(args.entry, lang=Language(args.lang))
        rhymes = rhymes_url(entry, lang=Language(args.lang))
        for rhyme in find_rhymes(rhymes):
            print(rhyme)
    except ValueError as error:
        stderr.write("{}\n".format(error))
