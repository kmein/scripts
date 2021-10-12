#!/usr/bin/env python3
"""Output SVG maps of surnames.
Powered by Christoph Stöpel's Geogen API.

Usage:
  geogen.py relative <name>... [--color=<color>]
  geogen.py absolute <name>... [--color=<color>]
  geogen.py (-h | --help)

Options:
  -h --help          Show this screen.
  --color=<color>  Diagram accent colour.
"""
from docopt import docopt
import geogen.client


def cli():
    arguments = docopt(__doc__)
    df = geogen.client.create_data_frame(arguments["<name>"])
    color = arguments["--color"] or "navy"
    if arguments["relative"]:
        print(geogen.client.generate_map(df, "relative", fill_color=color))
    elif arguments["absolute"]:
        print(geogen.client.generate_map(df, "absolute", fill_color=color))
