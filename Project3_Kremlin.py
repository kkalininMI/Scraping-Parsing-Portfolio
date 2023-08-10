# -*- coding: utf-8 -*-
"""
Created on Sat Jun 18 10:47:09 2022

@author: Kirill
"""
#Web Scraper for http://en.kremlin.ru/  (based on Clayton Besaw's script)

from bs4 import BeautifulSoup
import requests
import os
import json
import nltk
import re
from googletrans import Translator, constants
from pprint import pprint
from datetime import datetime
from nltk.tokenize import sent_tokenize, word_tokenize
import tqdm
import time
import pickle
import pandas as pd


os.chdir('C:/Users/Kirill/Desktop/Data for NLP/Presidential speeches')
headers = {'User-Agent': 'Mozilla/5.0 (Macintosh; Intel Mac OS X 10_10_1) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/39.0.2171.95 Safari/537.36'}
  

#STAGE 1: Scraping links
page_vector = range(1,455,1)
links = []
info_list1 = []
pbar = tqdm.tqdm(total = len(page_vector))

for pageN in page_vector:
    info_dict1 = {}
    urlS = "http://en.kremlin.ru/events/president/transcripts/page/{pageN}".format(pageN=pageN)    
    print(urlS)
    time.sleep(10)
    page = requests.get(urlS, headers=headers, timeout=30)
    contents = page.content
    soup = BeautifulSoup(contents, 'html.parser')
    transcript_links = soup.find_all('a',
                                      {'class':["tabs_article item big", 
                                                 "tabs_article item medium"]})
    for tag in transcript_links:
        links.append(tag.get('href'))
        info_dict1["url"] = tag

#save link_list
#with open('links.pkl', 'wb') as fp:
#    pickle.dump(links, fp)



#STAGE 2: Scrape data
#with open('links.pkl', 'rb') as fp:
#    links = pickle.load(fp)

#this collects text and URL
base_url = 'http://www.en.kremlin.ru'
info_list = []
pbar = tqdm.tqdm(total = len(links))

def cleantxt(txt):
    txt = txt.replace(u'\xa0', u' ') 
    txt = txt.replace('\n', ' ')
    return(txt)


for l in links: 
    info_dict = {}
    transcript_url = l
    full_url = base_url + transcript_url[:29] + '/copy' + transcript_url[29:]
    page = requests.get(full_url, headers=headers, timeout=30)
    contents = page.content
    transcript = BeautifulSoup(contents, 'html.parser')
    text_body = transcript.find('textarea')
    text_body2 = text_body.text
    text_body2 = cleantxt(text_body2)
    text_body2 = text_body2.strip()
    info_dict["Text"] = text_body2
    info_dict["URL"] = full_url
    info_dict["URL2"] = l
    
    info_list.append(info_dict)
    time.sleep(4)
    pbar.update(1)
pbar.close()

#this part collects the metadata
info_list = []
pbar = tqdm.tqdm(total = len(links))
i=0
for l in links:   #len(links)
    i = i + 1
    info_dict = {}
    transcript_url = l
    full_url = base_url + transcript_url
    page = requests.get(full_url, headers=headers, timeout=30)
    contents = page.content
    transcript = BeautifulSoup(contents, 'html.parser')
    
    #save BS
    with open("downloaded_speeches2/file_"+str(i)+".html", "w", encoding="utf-8") as file:
        file.write(str(transcript))
    
    #extract title
    key_name1 = 'Transcript Title'
    try:
        title = transcript.find('h1', attrs={'class':'entry-title p-name'})
        info_dict[key_name1] = title.text
    except AttributeError:
        info_dict[key_name1] = ' '
    #summary
    key_name2 = 'Summary'
    try:
        summary = transcript.find('div', attrs={'class':'read__lead entry-summary p-summary'})
        info_dict[key_name2] = summary.text
    except AttributeError:
        info_dict[key_name2] = ' '   
    #date
    key_name3 = 'Date'
    try:
        date = transcript.find('time', attrs={'itemprop':'datePublished'})
        info_dict[key_name3] = date.text
    except AttributeError:
        info_dict[key_name3] = ' '
    #time-published
    key_name4 = 'Pub-Time'
    try:
        ptime = transcript.find('div', attrs={'class':'read__time'})
        info_dict[key_name4] = ptime.text
    except AttributeError:
        info_dict[key_name4] = 'Unknown'
    
    #datetime
    key_name34 = "datetime"
    try:
        datetimeV = date.text + " " + ptime.text
        datetimeV = datetime.strptime(datetimeV, '%B %d, %Y %H:%M')
        datetimeV = datetimeV.strftime('%Y-%m-%dT%H:%M:%S')
        info_dict[key_name34] = datetimeV
    except AttributeError:
        info_dict[key_name34] = 'Unknown'
       
    #location
    key_name5 = "Location"
    try:
        loc = transcript.find('div', attrs={'class':'read__place p-location'})
        info_dict[key_name5] = loc.text
    except AttributeError:
        info_dict[key_name5] = 'Unknown'
    #text
    key_name6 = 'Text'
    try:
        text = transcript.find('div', attrs={'itemprop':'articleBody'})
        info_dict[key_name6] = cleantxt(text.text) 
    except AttributeError:
        info_dict[key_name6] = ' '
    #url
    key_name7 = "URL"
    info_dict[key_name7] = full_url
    #orig_url
    key_name8 = "URL2"
    info_dict[key_name8] = l    
    #topics
    key_name9 = "Topics"
    try:
        topics = transcript.find_all('li', attrs={'class':'p-category'})
        topics = ", ".join([i.text.strip() for i in topics])
        info_dict[key_name9] = topics
    except AttributeError:
        info_dict[key_name9] = ' '
    
    key_name10 = "ID"
    info_dict[key_name10] = i
     
    #append to master list
    info_list.append(info_dict)
    pbar.update(1)
    time.sleep(2)
    
    if i%10==0:
        with open('data.pkl', 'wb') as fp:
            pickle.dump(info_list, fp)
    
pbar.close()    

#save json files
with open('en_Kremlin.json', 'w') as f:
    json.dump(info_list, f)

#now create pandas data frame and export to .csv
#df = pd.DataFrame(info_list)  
#df.to_csv("putin_scrape.csv", index = False, header = True, 
#         encoding = 'utf-8')
#df.to_pickle("putin_scape.pkl")


