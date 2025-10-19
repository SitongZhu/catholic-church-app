import requests
from bs4 import BeautifulSoup
import csv
import time

def get_all_base_urls():
    # Generate lca.html to lcz.html, and add 2.html after pages with 2
    base_urls = []
    with_2 = ['b', 'c', 'm', 's']
    for i in range(ord('a'), ord('z')+1):
        ch = chr(i)
        base_urls.append(f"https://www.catholic-hierarchy.org/diocese/lc{ch}.html")
        if ch in with_2:
            base_urls.append(f"https://www.catholic-hierarchy.org/diocese/lc{ch}2.html")
    return base_urls

def get_diocese_links_and_names_from_url(url):
    """
    Get diocese links and corresponding city names from the specified URL
    Returns: [(url, city_name), ...]
    """
    diocese_data = []
    try:
        response = requests.get(url, timeout=30)
        response.raise_for_status()
        soup = BeautifulSoup(response.text, 'html.parser')
        
        anchor = soup.find('a', attrs={'name': 'list'})
        if anchor:
            ul = anchor.find_next('ul')
            if ul:
                for li in ul.find_all('li'):
                    a = li.find('a')
                    if a:
                        href = a.get('href', '')
                        city_name = a.get_text(strip=True)
                        
                        # Filter out country links, keep only diocese links
                        if not href.startswith('/country/') and href.endswith('.html'):
                            full_url = f"https://www.catholic-hierarchy.org/diocese/{href}"
                            diocese_data.append((full_url, city_name))
    except Exception as e:
        print(f"Error processing {url}: {e}")
        return []
    
    return diocese_data

def main():
    all_data = set()  # Use set to avoid duplicates
    
    print("Starting to scrape diocese links and city names...")
    
    for i, url in enumerate(get_all_base_urls()):
        print(f"Processing ({i+1}/30): {url}")
        
        # Add delay to avoid too frequent requests
        if i > 0:
            time.sleep(1)
        
        data = get_diocese_links_and_names_from_url(url)
        all_data.update(data)
        
        print(f"  Retrieved {len(data)} dioceses from {url}")
    
    # Convert to list and sort
    sorted_data = sorted(list(all_data), key=lambda x: x[1])  # Sort by city name
    
    # Save to CSV file
    output_file = 'diocese_links_with_names.csv'
    with open(output_file, 'w', newline='', encoding='utf-8') as csvfile:
        writer = csv.writer(csvfile)
        writer.writerow(['url', 'diocese'])
        
        for url, city_name in sorted_data:
            writer.writerow([url, city_name])
    
    print(f"\nScraping completed!")
    print(f"Total dioceses collected: {len(sorted_data)}")
    print(f"Data saved to: {output_file}")
    
    # Show first few examples
    if sorted_data:
        print("\nFirst 5 examples:")
        for i, (url, city_name) in enumerate(sorted_data[:5]):
            print(f"  {i+1}. {city_name} -> {url}")

if __name__ == "__main__":
    main()
