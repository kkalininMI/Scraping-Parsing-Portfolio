# -*- coding: utf-8 -*-
"""
Created on Thu Jun 16 16:05:39 2022

@author: Kirill
"""

#Web Scraper for https://navalny.com

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
import time

os.chdir('C:/Navalny')

translator = Translator()
indexv = 1
en_Navalnydict = {}
ru_Navalnydict = {}

def split_sentences(bpost_text): 
    sent_text = sent_tokenize(bpost_text) 
    
    list_chunks = []
    acc_chunk = ""
    for sentence in sent_text:
        if len(acc_chunk) < 4000:
            newl = len(acc_chunk) + len(sentence)
            if newl < 4000:
                acc_chunk = acc_chunk + " " + sentence
            else:
                list_chunks.append(acc_chunk) 
                acc_chunk = sentence
    list_chunks.append(acc_chunk)    
    return(list_chunks)
        

for pageN in range(1,296+1,1):  #range(1,296+1,1)

    urlS = "https://navalny.com/?p={pageN}".format(pageN=pageN)    
    print(urlS)
    page = requests.get(urlS)
    contents = page.content
    soup = BeautifulSoup(contents, 'html.parser')
    
    bposts = soup.find_all("div", class_="b-post")
    
    for bpost in bposts:
        
        indexv +=1
        
        bpost_title = bpost.find("h2", class_="b-title").get_text()
        bpost_title = bpost_title.replace('\n', "")
        bpost_title_en = translator.translate(bpost_title, src="ru", dest="en").text

        bpost_date = bpost.find("div", class_="b-post__info__item").get_text()
        bpost_date = re.sub('(\n)|(\s+)', "", bpost_date)
        bpost_date = datetime.strptime(bpost_date, '%d.%m.%Y,%H:%M')
        bpost_date = bpost_date.strftime('%Y-%m-%dT%H:%M:%S')
        
        bpost_text = bpost.find("div", class_="b-post__content").get_text()
        bpost_text = bpost_text.replace('\n','');
        bpost_text_en = split_sentences(bpost_text)
                
        bpost_textA=[]
        for s in bpost_text_en:
            bpost_textA.append(translator.translate(s, src="ru", dest="en").text)
            time.sleep(1)
                              
        bpost_text_enM = " ".join(bpost_textA)

        res_ru = {"page":pageN, "date":bpost_date, "title":bpost_title, "text":bpost_text}
        ru_Navalnydict["id_{indexv}".format(indexv=indexv)] = res_ru
        res_en = {"page":pageN, "date":bpost_date, "title":bpost_title_en, "text":bpost_text_enM}
        en_Navalnydict["id_{indexv}".format(indexv=indexv)] = res_en
        
#save json files
with open('en_Navalny.json', 'w') as f:
    json.dump(en_Navalnydict, f)

with open('ru_Navalny.json', 'w') as f:
    json.dump(ru_Navalnydict, f)
    
fileJson = [json.loads(line) for line in open('en_Navalny.json','r', encoding="utf-8")]
