#!/usr/bin/env python3

import requests
import sys
from bs4 import BeautifulSoup

MAN_URL = "https://www.gnu.org/software/make/manual/html_node/Special-Targets.html"


def get_special_targets(url):
    try:
        response = requests.get(url, timeout=3)
        response.raise_for_status()

        soup = BeautifulSoup(response.content, "html.parser")

        # Find all <dt> elements within <dl> elements
        elems = soup.find_all("dl")[-1].find_all("dt")
        return [e.text.split(" ")[0].strip() for e in elems]
    except requests.exceptions.Timeout:
        print(f"Timed out waiting for {MAN_URL}", file=sys.stderr)
        return None
    except requests.exceptions.RequestException as e:
        print(f"Error: {e}", file=sys.stderr)
        return None


if __name__ == "__main__":
    targets = get_special_targets(MAN_URL)
    if targets:
        print('\n'.join(targets))
