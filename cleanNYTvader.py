# -*- coding: utf-8 -*-
"""CleanNYTvader.ipynb

Automatically generated by Colaboratory.

Original file is located at
    https://colab.research.google.com/drive/10Lr-gVM0-BhyhK85_jv7UJI_esWOvCUs
"""

# Commented out IPython magic to ensure Python compatibility.
from io import StringIO
import os
import re
import numpy as np
from linecache import getline
import time
import datetime
import sys
import pandas as pd 
import pdb
!pip install xlsxwriter
import xlsxwriter
import nltk
nltk.download('punkt')
from nltk import tokenize
import matplotlib.pyplot as plt
from wordcloud import WordCloud, STOPWORDS, ImageColorGenerator
#google collab specific line to connect to google drive
from google.colab import drive 
drive.mount('/content/gdrive')
#change directoy to .txt files directory
# %cd /content/gdrive/My\ Drive/PDF Data Thesis/Text Data
#Create list of text files in directory that the program will read from
files=[i for i in os.listdir() if i.endswith('txt')]
#Extract the title, date and body of the transcript from the text file. Common pattern: Title is always one line above "New York Times" header, 
#Date is alwauys after. Transcript always starts with "Body"
data = pd.DataFrame({'Title':[],'Date':[],'article':[]})
for file in files :
  fileX= open(file, "r")
  text = fileX.read()
  text = text.split("End of Document")
  count = 0
  for episode in text :
    if count > 0 :
      newrow={'Title':title, 'Date':date,'article':body}
      data=data.append(newrow, ignore_index=True)
    getLines = False
    body = []
    episode = episode.splitlines()
    episode = [episode for episode in episode if episode.strip() != '']
    for i , line in enumerate(episode,start=0) :
      line = line.strip()
      if ("The New York Times" == line) : 
        if (i == 2) :
          title = episode[i - 2].strip() + " " + episode[i - 1].strip()
        else :
          title = episode[i - 1].strip()
        date = re.findall(r'(?:\d{2} )?(?:Jan|Feb|Mar|Apr|May|Jun|Jul|Aug|Sep|Oct|Nov|Dec)[a-z]* (?:\d{1,2}, )?\d{4}',episode[i + 1])
      if line.startswith("Body") :
        getLines = True
      if getLines :
        body.append(line)
    count = count + 1
#A dictionary from which we expand contractions
CONTRACTION_MAP = {
"ain't": "is not",
"aren't": "are not",
"can't": "cannot",
"can't've": "cannot have",
"'cause": "because",
"could've": "could have",
"couldn't": "could not",
"couldn't've": "could not have",
"didn't": "did not",
"doesn't": "does not",
"don't": "do not",
"hadn't": "had not",
"hadn't've": "had not have",
"hasn't": "has not",
"haven't": "have not",
"he'd": "he would",
"he'd've": "he would have",
"he'll": "he will",
"he'll've": "he he will have",
"he's": "he is",
"how'd": "how did",
"how'd'y": "how do you",
"how'll": "how will",
"how's": "how is",
"I'd": "I would",
"I'd've": "I would have",
"I'll": "I will",
"I'll've": "I will have",
"I'm": "I am",
"I've": "I have",
"i'd": "i would",
"i'd've": "i would have",
"i'll": "i will",
"i'll've": "i will have",
"i'm": "i am",
"i've": "i have",
"isn't": "is not",
"it'd": "it would",
"it'd've": "it would have",
"it'll": "it will",
"it'll've": "it will have",
"it's": "it is",
"let's": "let us",
"ma'am": "madam",
"mayn't": "may not",
"might've": "might have",
"mightn't": "might not",
"mightn't've": "might not have",
"must've": "must have",
"mustn't": "must not",
"mustn't've": "must not have",
"needn't": "need not",
"needn't've": "need not have",
"o'clock": "of the clock",
"oughtn't": "ought not",
"oughtn't've": "ought not have",
"shan't": "shall not",
"sha'n't": "shall not",
"shan't've": "shall not have",
"she'd": "she would",
"she'd've": "she would have",
"she'll": "she will",
"she'll've": "she will have",
"she's": "she is",
"should've": "should have",
"shouldn't": "should not",
"shouldn't've": "should not have",
"so've": "so have",
"so's": "so as",
"that'd": "that would",
"that'd've": "that would have",
"that's": "that is",
"there'd": "there would",
"there'd've": "there would have",
"there's": "there is",
"they'd": "they would",
"they'd've": "they would have",
"they'll": "they will",
"they'll've": "they will have",
"they're": "they are",
"they've": "they have",
"to've": "to have",
"wasn't": "was not",
"we'd": "we would",
"we'd've": "we would have",
"we'll": "we will",
"we'll've": "we will have",
"we're": "we are",
"we've": "we have",
"weren't": "were not",
"what'll": "what will",
"what'll've": "what will have",
"what're": "what are",
"what's": "what is",
"what've": "what have",
"when's": "when is",
"when've": "when have",
"where'd": "where did",
"where's": "where is",
"where've": "where have",
"who'll": "who will",
"who'll've": "who will have",
"who's": "who is",
"who've": "who have",
"why's": "why is",
"why've": "why have",
"will've": "will have",
"won't": "will not",
"won't've": "will not have",
"would've": "would have",
"wouldn't": "would not",
"wouldn't've": "would not have",
"y'all": "you all",
"y'all'd": "you all would",
"y'all'd've": "you all would have",
"y'all're": "you all are",
"y'all've": "you all have",
"you'd": "you would",
"you'd've": "you would have",
"you'll": "you will",
"you'll've": "you will have",
"you're": "you are",
"you've": "you have"
}

import requests
from bs4 import BeautifulSoup
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import seaborn as sns
import os
import spacy
import nltk
from nltk.tokenize.toktok import ToktokTokenizer
import re
import unicodedata
#Function to expand contractions
def expand_contractions(text, contraction_mapping=CONTRACTION_MAP):
    
    contractions_pattern = re.compile('({})'.format('|'.join(contraction_mapping.keys())), 
                                      flags=re.IGNORECASE|re.DOTALL)
    def expand_match(contraction):
        match = contraction.group(0)
        first_char = match[0]
        expanded_contraction = contraction_mapping.get(match)\
                                if contraction_mapping.get(match)\
                                else contraction_mapping.get(match.lower())                       
        expanded_contraction = first_char+expanded_contraction[1:]
        return expanded_contraction
        
    expanded_text = contractions_pattern.sub(expand_match, text)
    expanded_text = re.sub("'", "", expanded_text)
    return expanded_text


data_text = data[['article']]
data_text['article'] = data_text['article'].apply(', '.join)

documents = data_text
processed_docs = documents['article'].map(expand_contractions)

#The open source sentiment analysis tool vader is used here because it combines language heuritics (including negation rules) with
# a simple scoring approach done on a per sentence basis. Each sentence in the article is scored, and the average is taken. 
!pip install vaderSentiment
import vaderSentiment
import statistics
from vaderSentiment.vaderSentiment import SentimentIntensityAnalyzer
analyser = SentimentIntensityAnalyzer()

def sentiment_analyzer_scores(sentence):
    score = analyser.polarity_scores(sentence)
    return score['compound']

averageValues = []
for article in processed_docs : 
  sampleDoc = article.split(",")
  oneString = ",".join(sampleDoc)
  oneString = oneString.replace(',','')
  sentenceSplit = tokenize.sent_tokenize(oneString)
  values = []
  for sentence in sentenceSplit : 
    sentValue = sentiment_analyzer_scores(sentence)
    values.append(sentValue)
  average = statistics.mean(values)
  averageValues.append(average)
#Rejoin the "Body" column to the date and title columns and add the average sentiment score as a fourth column
#Make sure the date is in the proper datetime format for ease of plotting in the future
df = data[['Date']]
df['Date'] = [','.join(map(str, l)) for l in df['Date']]
df['Date']= pd.to_datetime(df['Date']) 
df2 = pd.DataFrame(averageValues)
df3 = pd.concat([df,df2], axis = 1)
df3.columns = ["Date","Average Score"]
#Export to excel
df3.to_excel("initialResults.xlsx",index=False,encoding="utf-8",engine='xlsxwriter')