from bs4 import BeautifulSoup
import requests
import re

DEFAULT_LANG = "de"

def cook_soup(url):
    return BeautifulSoup(requests.get(url).text, "lxml")

def base_url(lang=DEFAULT_LANG):
    return "https://{}.wiktionary.org".format(lang)

def entry_url(word, lang=DEFAULT_LANG):
    return base_url(lang) + "/wiki/" + word

def find_rhymes_url(entry_url, lang=DEFAULT_LANG):
    entry_html = requests.get(entry_url).text
    soup = BeautifulSoup(entry_html, "lxml")
    result_url = base_url(lang)
    if lang == "de":
        result_url += soup.find("a", href=re.compile(r"/wiki/Reim:Deutsch:.*"))["href"]
    elif lang == "en":
        result_url += soup.find("a", href=re.compile(r"/wiki/Rhymes:English/.*"))["href"]
    else:
        pass
    return result_url

def find_rhymes(rhymes_url):
    rhymes_html = requests.get(rhymes_url).text
    soup = BeautifulSoup(rhymes_html, "lxml")
    for li in soup.select("div#content ul > li > a"):
        try:
            yield li.text
        except KeyError:
            continue

if __name__ == "__main__":
    entry_url = entry_url("stabil", lang="de")
    rhymes_url = find_rhymes_url(entry_url, lang="de")
    for rhyme in find_rhymes(rhymes_url):
        print(rhyme)
