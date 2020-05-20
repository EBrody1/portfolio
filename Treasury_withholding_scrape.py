# -*- coding: utf-8 -*-
"""
Created on Wed May 20 16:16:35 2020

@author: brody
"""

import pandas as pd
#import numpy as np
from openpyxl import load_workbook
#import datetime
from datetime import date, timedelta
# calculate today's date and then the target of treasury report which is 2 or
# 4 days behind depending on weekends
today = date.today() 
day = today.strftime('%A')
if day == 'Monday' or day == "Tuesday":
    Target = today - timedelta(days=4)
else:
    Target = today - timedelta(days=2)
targ = Target.strftime("%m/%d/%y")
# format the url of the xlsx file
yr = targ.split('/')[2]
d = targ.split('/')[1]
mon = targ.split('/')[0]
ext = yr+mon+d+'00.xlsx'
head = 'https://fsapps.fiscal.treasury.gov/dts/files/'
url = head+ext

# grab daily report to add to the records
df = pd.read_excel(url)

# extract the date
d = df.columns[0]


# drop empty column that appears on some uploads of the data set
typ = 'A'
if df.columns[-1] == 'Unnamed: 6':
    df.drop('Unnamed: 6', axis=1, inplace=True)
    typ = 'B'

# rename the columns
df.columns = ['Table','Account', 'Closing today', 'Today', 'Month to date', 'FYTD' ]

#Track Fed MLF loans

# find entry for MLF seems to change every time one time b was 73
MLF = df[df['Account']=='ESF - Economic Recovery Programs']


data = pd.DataFrame()

data = data.append(MLF)



data['Date'] = d
data.columns = [['Table','Account', 'Closing today', 'Today', 'Month to date', 'FYTD','Date']]



# code was adopted from here: https://medium.com/better-programming/using-python-pandas-with-excel-d5082102ca27

writer = pd.ExcelWriter('ESF.xlsx', engine='openpyxl')
# try to open an existing workbook
writer.book = load_workbook('ESF.xlsx')
# copy existing sheets
writer.sheets = dict((ws.title, ws) for ws in writer.book.worksheets)
# read existing file
reader = pd.read_excel(r'ESF.xlsx')
# write out the new sheet
data.to_excel(writer,index=False,header=False,startrow=len(reader)+1)

writer.close()

### Track witholdings from income and employment tax

# find entry for withholdings 

WH = df[df['Account'] =='Withheld Income and Employment Taxes']


# same as code above
data = pd.DataFrame()

data = data.append(WH)
data['Date'] = d

data.columns = [['Table','Account', 'Closing today', 'Today', 'Month to date', 'FYTD','Date']]



writer = pd.ExcelWriter('Withholdings.xlsx', engine='openpyxl')
# try to open an existing workbook
writer.book = load_workbook('Withholdings.xlsx')
# copy existing sheets
writer.sheets = dict((ws.title, ws) for ws in writer.book.worksheets)
# read existing file
reader = pd.read_excel(r'Withholdings.xlsx')
# write out the new sheet
data.to_excel(writer,index=False,header=False,startrow=len(reader)+1)

writer.close()

#pull up excel and sort by date
cs = pd.read_excel('Withholdings.xlsx')

# in pandas to convert a column of strings into date object
cs['Date'] = pd.to_datetime(cs['Date'])

# sort
cs = cs.sort_values('Date')

# these numbers are in millions better for dashboard to convert to millions
def convert(num):
    if num < 1000000:
        res = num * 1000000
        return res
    else:
        return num
cs['Today'] = cs['Today'].apply(convert)
cs['Month to date'] = cs['Month to date'].apply(convert)


cs.to_csv('Withheld.csv')

df = pd.read_excel('ESF.xlsx')

# in convert  strings into date object
df['Date'] = pd.to_datetime(df['Date'])

# sort
df = df.sort_values('Date')

# these numbers are in millions better for dashboard to convert to millions
def convert(num):
    if num < 1000000:
        res = num * 1000000
        return res
    else:
        return num
df['Today'] = df['Today'].apply(convert)
df['Month to date'] = df['Month to date'].apply(convert)
df['FYTD'] = df['FYTD'].apply(convert)


df.to_csv('ESF.csv')