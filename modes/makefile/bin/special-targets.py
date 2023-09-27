#!/usr/bin/env python3

import requests
from bs4 import BeautifulSoup

MAN_URL = "https://www.gnu.org/software/make/manual/html_node/Special-Targets.html"


def get_special_targets(url):
    response = requests.get(url)
    response.raise_for_status()

    soup = BeautifulSoup(response.content, "html.parser")

    # Find all <dt> elements within <dl> elements
    elems = soup.find_all("dl")[-1].find_all("dt")
    return [e.text.split(" ")[0].strip() for e in elems]


if __name__ == "__main__":
    targets = get_special_targets(MAN_URL)
    print('\n'.join(targets))
