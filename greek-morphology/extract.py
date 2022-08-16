#!/usr/bin/env python3
import unicodedata
import sys
import csv
from tqdm.auto import tqdm

PATH = "input.txt"


def parse_interpretation(interpretation):
    try:
        lemma, translation, analysis = interpretation.split(" : ")
        return {
            "lemma": lemma.strip() or None,
            "translation": translation.strip() or None,
            "analysis": analysis.strip() or None,
        }
    except:
        print(interpretation, file=sys.stderr)
        sys.exit(1)


def purge_accents(string):
    return (
        unicodedata.normalize("NFD", string)
        .lower()
        .translate(dict([key, None] for key in range(0x300, 0x380)))
    )


def parse(plain_text):
    print("parsing", file=sys.stderr)
    result = []
    for entry in tqdm(plain_text.split("\n\n")):
        key_value = entry.split("\n")
        key = key_value[0]
        value = key_value[1:]
        result.append(
            {
                "form": key.split("|"),
                "simple": purge_accents(key.split("|")[0]),
                "interpretations": [
                    parse_interpretation(interpretation)
                    for interpretations in value
                    for interpretation in interpretations.split("<br>")
                ],
            }
        )
    return result


with open(PATH, "r") as file:
    plain_text = file.read()

    forms = [f"form{n}" for n in range(1, 6)]
    fieldnames = (
        ["simple"]
        + forms
        + [
            "lemma",
            "translation",
            "analysis",
        ]
    )

    csv_writer = csv.DictWriter(sys.stdout, fieldnames=fieldnames)
    csv_writer.writeheader()
    print("writing", file=sys.stderr)
    for word in tqdm(parse(plain_text)):
        for interpretation in word["interpretations"]:
            csv_writer.writerow(
                dict(zip(forms, word["form"]))
                | {
                    "simple": word["simple"],
                    "lemma": interpretation["lemma"],
                    "translation": interpretation["translation"],
                    "analysis": interpretation["analysis"],
                }
            )
