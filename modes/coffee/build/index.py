#!/usr/bin/env python

"""
Get data from from http://coffeescript.org
"""

import sys
from bs4 import BeautifulSoup

if sys.version_info[0] == 3:
    import urllib.request as urllib
    from urllib.error import HTTPError
else:
    import urllib2 as urllib
    from urllib2 import HTTPError


class Index:
    """
    Extract index from table of contents.
    """

    def __init__(self, url):
        self.refs = {}
        self.soup = None
        self.url = url
        try:
            html = urllib.urlopen(url).read()
            self.soup = BeautifulSoup(html)

        except HTTPError as e:
            print(e)

    def get_toc(self):
        menu = self.soup.find("div", {"class": "contents menu"})
        for l in menu.findAll("a"):
            if 'href' in l.attrs:
                name = l.get_text()
                refid = l.attrs['href']

                self.refs[name] = refid

ip = Index("http://coffeescript.org/")

if __name__ == '__main__':
    import json
    ip = Index("http://coffeescript.org/")
    ip.get_toc()
    with open("index.json", "w") as f:
        json.dump(ip.refs, f)
