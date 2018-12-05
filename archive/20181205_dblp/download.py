import requests
from bs4 import BeautifulSoup
from pathlib import Path


def get_osdi_url_dict(year):
    r = requests.get('https://dblp.uni-trier.de/db/conf/osdi/osdi{}.html'
                     .format(year))
    soup = BeautifulSoup(r.text, 'html.parser')
    headers = [x.h2.text for x in soup.find_all('header')[1:]]
    uls = soup.find_all('ul', class_='publ-list')[1:]
    d = dict()
    for header, ul in zip(headers, uls):
        lis = ul.find_all('li', recursive=False)
        d[header] = ['osdi{}-'.format(year % 100) +
                     x.nav.find('a')['href'].split('/')[-1].replace('_', '-')
                     for x in lis]
    return d


def download_osdi(path, year, url_prefix=''):
    base_url = (url_prefix +
                'https://www.usenix.org/system/files/conference/osdi{}/'
                .format(year % 100))
    if path.strip()[0] == '~':
        path = Path(path).expanduser()
    else:
        path = Path(path)
    d = get_osdi_url_dict(year)
    for header in d:
        p = path/header
        p.mkdir()
        for paper in d[header]:
            pdf = paper + '.pdf'
            r = requests.get(base_url + pdf)
            with open(p/pdf, 'wb') as f:
                for chunk in r:
                    f.write(chunk)
