#!/usr/bin/env python3

import requests
from bs4 import BeautifulSoup

if __name__ == 'main':
    URL = "https://www.monster.com/jobs/search/?q=Software-Developer&where=China&jobid=55fcc242-1bcb-4bea-891f-bc6669b2025a"
    page = requests.get(URL)

    soup = BeautifulSoup(page.content, 'html.parser')
    results = soup.find(id='ResultsContainer')
    job_elems = results.find_all('section', class_='card-content')

    for job_elem in job_elems:
        title_elem = job_elem.find('h2', class_='title')
        company_elem = job_elem.find('div', class_='company')
        location_elem = job_elem.find('div', class_='location')
        print(title_elem.text, company_elem.text, location_elem.text, sep=' ')
        print()
