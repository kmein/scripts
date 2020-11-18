#!/usr/bin/env python
from setuptools import setup

setup(
    name="Geogen",
    version="1.0",
    description="Geogen (geogen.stoepel.net) API wrapper",
    author="Kierán Meinhardt",
    author_email="kmein@posteo.de",
    packages=[],
    scripts=["geogen"],
    install_requires=[
        "requests >=2.24.0, <3.0.0",
        "requests-cache >=0.5.2, <1.0.0",
        "pandas >=1.1.1, <2.0.0",
        "docopt >=0.6.2, <1.0.0",
    ],
)
