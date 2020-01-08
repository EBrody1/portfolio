#!/usr/bin/env python
# coding: utf-8

# ## Maaser Calculator 

# In[ ]:
import datetime
import sqlite3

def maaser(netincome):        
    '''A function designed to take income and calculate maaser based on expenses
    and other types of income  '''
    miles = 0
    while True:
        x = input('enter miles driven or type "cont" : ')
        if x == "cont":
            break
        else:
            try:
                miles += float(x)
                print("miles driven: {}".format(miles))
            except:
                print('error non number entered')
                      
    netincome -= miles/2
    while True: 
        try:
            dental = float(input("enter dental deduction: "))
        except:
            print('error non number entered')
        else: break 
    while True: 
        try:
            health = float(input("enter health insurance deduction: "))
        except:
            print('error non number entered')
        else: break 
    while True: 
        try:
            disability = float(input("enter disability deduction: "))
        except:
            print('error non number entered')
        else: break 
          
    netincome += (dental) + (disability) + (health)
    while True:
        try:
            milage = float(input('enter mileage reimbursement: '))
        except:
            print('error non number entered')
        else: break 
    while True:
        try:
            phone = float(input('enter phone reimbursement: '))
        except:
            print('error non number entered')
        else: break 
        
    netincome -= (milage) + (phone)
       
    expenses = 0
    while True:
        x = input('enter any expenses encountered such as babysitting or type "cont" : ')
        if x == "cont":
            break
        else:
            try:
                expenses += float(x) 
            except:
                print('error non number entered')
    print("expenses were: {}".format(expenses))
    netincome -= expenses
    M = round(netincome/10, 2)
    Date = datetime.date.today() 
    D = Date.strftime("%m/%d/%y")      
    

    conn = sqlite3.connect('maaser.sqlite')
    cur = conn.cursor()

# Make some fresh tables using executescript()
    cur.execute('''CREATE TABLE IF NOT EXISTS Maaser_Owed (
    Date TEXT, Amount_Owed INTEGER)''')
    
    cur.execute('''INSERT INTO Maaser_Owed
        (Date, Amount_owed)
        VALUES ( ?, ?)''',
        ( D, M ) )

    conn.commit()
    
    fout = open('maaser2.csv', 'a')
    fout.write('{}, {}'.format(D, M))
    fout.close()    
    
    return "you owe ${} in maaser. Tizku Limitzvos!".format(round(netincome/10, 2)) 
    
# In[ ]:
print(maaser(1712.77))





# In[ ]:




