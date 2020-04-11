import argparse, datetime
import pandas as pd
from selenium import webdriver
from selenium.webdriver.firefox.options import Options
from bs4 import BeautifulSoup

# parsing and configuration
def parse_args():
    desc = "Scrape historical prices from marketsinsider"
    parser = argparse.ArgumentParser(description=desc)
    parser.add_argument('--item', type=str)
    return parser.parse_args()

args = parse_args()
item = args.item

today = datetime.date.today()
today_date = f'{today.day}.{today.month}.{today.year}'

markets_url = 'https://markets.businessinsider.com/commodities/historical-prices'
item_url = f'{markets_url}/{item}-price/usd/22.1.2020_{today_date}'

print(f'Accessing Webpage for {item} from 22.1.2020 to {today_date}')
# need geckodriver installed for this to run
browser = webdriver.Firefox(executable_path='C:/Users/Steph/bin/geckodriver.exe')
browser.get(item_url)

search_form = browser.find_element_by_id('historic-price-list')
test = browser.find_element_by_css_selector('table.table.instruments')
soup = BeautifulSoup(test.get_attribute("outerHTML"), 'lxml')

dates = []
close_prices = []
for tr in soup.find_all('tr')[2:]:
    tds = tr.find_all('td')
    dates.append(tds[0].text.strip())
    close_prices.append(tds[1].text.strip())

browser.close()

price_df = pd.DataFrame({'Date':dates, 'Close':close_prices})
price_df.to_csv(f'data/{item}_prices.csv', index=False)
print(f'Prices Obtained for {item}')
