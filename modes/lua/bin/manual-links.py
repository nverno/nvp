#!/usr/bin/env python3

"""
Extract the Lua manual links and dump as JSON to stdout
"""

import json
import re
import sys

import requests
from bs4 import BeautifulSoup

# URL of the Lua 5.4 Manual
url = "https://www.lua.org/manual/5.4/manual.html"
response = requests.get(url)
soup = BeautifulSoup(response.content, "html.parser")

# Get links with names starting with "pdf-"
manual_links = {
    a.attrs["name"][4:]: a.text
    for a in soup.find_all("a", {"name": re.compile("^pdf-")})
}
manual_links

json.dump(manual_links, sys.stdout)
