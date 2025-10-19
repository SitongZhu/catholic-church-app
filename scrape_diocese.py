import requests
from bs4 import BeautifulSoup
import re
from datetime import datetime
import csv
import html
import unicodedata
import codecs

# Read the diocese_links_with_names.csv file to get the URL and diocese name
urls_and_names = []
with open('diocese_links_with_names.csv', 'r', encoding='utf-8') as f:
    lines = f.readlines()
    for line in lines[1: ]: 
        parts = line.strip().split(',')
        if len(parts) >= 2:
            url = parts[0].strip()
            diocese_name = parts[1].strip()
            if url and diocese_name:
                urls_and_names.append((url, diocese_name))

all_rows = []
header_row = None 

# 1. Maintain all General Info fields that have appeared in the loop
all_general_info_keys = set()

# 2. Collect General Info in each URL loop
general_info_dict = {}

def normalize_text(text):
    """Normalize text, handle special characters"""
    if not text:
        return text
    # First decode HTML entities
    text = html.unescape(text)
    # Then normalize Unicode characters
    text = unicodedata.normalize('NFKC', text)
    # Handle possible encoding issues
    try:
        text = text.encode('latin1').decode('utf-8')
    except:
        pass
    return text

all_general_info_keys = set()
all_stats_headers = None
all_data = []

for idx, (url, diocese_name) in enumerate(urls_and_names, 1):
    headers = {
        'User-Agent': 'Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/91.0.4472.124 Safari/537.36',
        'Accept-Charset': 'ISO-8859-1'
    }
    response = requests.get(url, headers=headers)
    # Set the correct encoding
    response.encoding = 'ISO-8859-1'
    # Use html5lib parser instead of html.parser
    soup = BeautifulSoup(response.text, 'html5lib')

    # 1. Get the bishop name and age
    bishop_name = None
    bishop_age = None
    bishop_h3 = soup.find('h3', string=lambda s: s and 'Bishop(s)' in s)
    if bishop_h3:
        bishop_ul = bishop_h3.find_next('ul')
        if bishop_ul:
            # Use regex to find bishop links
            bishop_links = bishop_ul.find_all('a', href=re.compile(r'^/bishop/'))
            for link in bishop_links:
                # Check if the link text contains the bishop name
                # Use normalize_text to handle special characters
                name = normalize_text(link.get_text(separator=' ', strip=True))
                if name and not name.lower().startswith('vacant'):
                    # Get the age information
                    parent_li = link.find_parent('li')
                    if parent_li:
                        # First find writeage
                        script = parent_li.find('script')
                        age = None
                        if script:
                            match = re.search(r'writeage\((\d+),(\d+),(\d+)\)', script.string or "")
                            if match:
                                try:
                                    year, month, day = map(int, match.groups())
                                    if 1 <= month <= 12 and 1 <= day <= 31:  # Validate the validity of the month and day
                                        birth = datetime(year, month, day)
                                        today = datetime.now()
                                        age = (today - birth).days / 365.2425
                                        age = f"{age:.2f}"
                                    else:
                                        age = None
                                except ValueError:
                                    age = None
                        if not age:
                            all_brackets = re.findall(r'\(([^)]+)\)', parent_li.get_text())
                            age = all_brackets[-1] if all_brackets else None
                        bishop_name = name
                        bishop_age = age
                        break

    # General Information
    general_info_dict = {}
    info_h3 = soup.find('h3', string=lambda s: s and 'General Information' in s)
    if info_h3:
        info_ul = info_h3.find_next('ul')
        if info_ul:
            for li in info_ul.find_all('li'):
                text = normalize_text(li.get_text(separator=' ', strip=True))
                if ':' in text:
                    key, value = text.split(':', 1)
                    key = key.strip()
                    value = value.strip()
                    general_info_dict[key] = value
                    all_general_info_keys.add(key)

    # Statistics table
    stats_data = []
    stats_headers = []
    stats_anchor = soup.find('a', attrs={'name': 'stats'})
    stats_table = None
    if stats_anchor:
        stats_table = stats_anchor.find_next('table')
        if stats_table:
            header_th = stats_table.find('tr').find_all('th')
            stats_headers = [normalize_text(th.get_text(strip=True)) for th in header_th]
            if all_stats_headers is None:
                all_stats_headers = stats_headers
            for tr in stats_table.find_all('tr')[1:]:
                stats_row = [normalize_text(cell.get_text(strip=True)) if cell.get_text(strip=True) else 'NA' for cell in tr.find_all('td')]
                # Save each row data, including the diocese name
                all_data.append({
                    'url': url,
                    'diocese': diocese_name,
                    'general_info': general_info_dict,
                    'stats': dict(zip(stats_headers, stats_row))
                })

    # After processing each URL, output the progress
    print(f'Processed {idx}/{len(urls_and_names)} URLs: {diocese_name}')

# Generate the header, add the diocese column
header_row = ['url', 'diocese'] + sorted(list(all_general_info_keys)) + all_stats_headers

# Fill the data and write to CSV
all_rows = [header_row]
for item in all_data:
    row = [item['url'], item['diocese']]
    # General Information
    for key in sorted(list(all_general_info_keys)):
        row.append(item['general_info'].get(key, 'NA'))
    # Statistics
    for stat_key in all_stats_headers:
        row.append(item['stats'].get(stat_key, 'NA'))
    all_rows.append(row)

# Print or save
with open('stats_with_bishop.csv', 'w', newline='', encoding='utf-8-sig') as f:
    writer = csv.writer(f)
    writer.writerows(all_rows)
print('Saved as stats_with_bishop.csv')