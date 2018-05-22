#!/usr/bin/env python3
from argparse import ArgumentParser
from bs4 import BeautifulSoup
from datetime import datetime
from typing import List
import re
import requests


def soup_from(url):
    return BeautifulSoup(requests.get(url).text, "lxml")


class Author:
    def __init__(self, author_id: int) -> None:
        author_texts_url = "https://www.keinverlag.de/autorentexte.php?start=0&limit=1000000&sortby=tnr&autor={}".format(author_id)
        soup = soup_from(author_texts_url)
        self.texts = []  # type: List[Text]
        for text in soup.select("ul.textliste > li > a[href$=\".text\"]"):
            # strip off the last five characters (".text")
            text_id = int(text["href"][:-5])
            try:
                self.texts.append(Text(text_id))
            except ValueError:
                continue

    def markdown(self, *, with_type: bool = False) -> str:
        name = self.texts[0].author

        def __gen():
            yield "% {}".format(name)
            for text in self.texts:
                yield "\n\n* * *\n\n"
                yield text.markdown(with_author=False, with_type=with_type)

        return "\n".join(__gen())


class Text:
    def __init__(self, text_id: int) -> None:
        normalization = {132: "\"", 147: "\"", 0x96: "--", 0x91: "'", 0x92: "'", 0x97: "---"}
        text_url = "https://www.keinverlag.de/{}.text".format(text_id)
        soup = soup_from(text_url)
        try:
            self.title = soup.select("h1 > span")[0].text.translate(normalization)
            content = str(soup.select(".fliesstext > span")[0])
            content = re.sub(r'<span style="font-style: italic;">(([\n\r]|.)*?)</span>', r"_\1_", content)
            self.content = BeautifulSoup(content, "lxml").text.translate(normalization)
            self.author = soup.select("h3 > a")[2].text
            self.type = soup.select("h1 ~ h3")[0].text
        except IndexError:
            raise ValueError("Text {} not available.".format(text_id))

    def markdown(self, *, with_author: bool = True, with_type: bool = False) -> str:
        return "#### {maybe_author}{title}{maybe_type}\n\n{content}".format(
            title=self.title,
            maybe_author=self.author + ": " if with_author else "",
            maybe_type=" ("+self.type+")" if with_type else "",
            content="\n".join(line + "\\" for line in self.content.splitlines()))


if __name__ == "__main__":
    parser = ArgumentParser()
    parser.add_argument("--type", help="Include text type", action="store_true")
    subparsers = parser.add_subparsers()

    handle_text = subparsers.add_parser("text", help="Handle one text")
    handle_text.add_argument("tid", help="KeinVerlag text id", type=int)
    handle_text.set_defaults(func=lambda a: print(Text(a.tid).markdown(with_type=a.type)))

    handle_author = subparsers.add_parser("author", help="Handle all texts by an author")
    handle_author.add_argument("aid", help="KeinVerlag author id", type=str)
    handle_author.set_defaults(func=lambda a: print(Author(a.aid).markdown(with_type=a.type)))

    args = parser.parse_args()
    args.func(args)
