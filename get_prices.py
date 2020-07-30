import argparse, datetime
import pandas as pd
from selenium import webdriver
from selenium.webdriver.firefox.options import Options
from bs4 import BeautifulSoup

# parsing and configuration
def parse_args():
    desc = "Scrape historical prices from marketsinsider"
    parser = argparse.ArgumentParser(description=desc)
    parser.add_argument('--query', type=str)
    return parser.parse_args()

args = parse_args()
query = args.query

def get_prices(query, query_class, start_date, end_date):

    markets_url = f'https://markets.businessinsider.com/{query_class}/historical-prices'

    if(query_class=='index'):
        query_url = f'{markets_url}/{query}/{start_date}_{end_date}'
    else:
        query_url = f'{markets_url}/{query}/usd/{start_date}_{end_date}'

    print(f'Accessing Webpage for {query} from {start_date} to {end_date}')
    # need geckodriver installed for this to run
    browser = webdriver.Firefox(executable_path='C:/Users/Steph/bin/geckodriver.exe')
    browser.get(query_url)

    search_form = browser.find_element_by_id('historic-price-list')
    test = browser.find_element_by_xpath('//*[@id="historic-price-list"]/div/div[2]/table')
    #test = browser.find_element_by_css_selector('table.table.instruments')
    soup = BeautifulSoup(test.get_attribute("outerHTML"), 'lxml')

    dates = []
    close_prices = []
    for tr in soup.find_all('tr')[2:]:
        tds = tr.find_all('td')
        dates.append(tds[0].text.strip())
        close_prices.append(tds[1].text.strip())

    browser.close()

    price_df = pd.DataFrame({
        'Date':dates, 'Close':close_prices,
        'Query_class':query_class, 'Query':query})

    #price_df.to_csv(f'data/{query}_prices.csv', index=False)
    print(f'Prices Obtained for {query} from {start_date} to {end_date}')

    return(price_df)
