
# Script assumes database called company exists and is loaded with data
# which was entered using the week_6 bussines_units scrip

# use the database
USE company;

# the 2012 data was imported from csv using the import wizard and loaded into a  table named 2012pd
# the CSV for 2014 data was imported using the import wizard and the table and loaded in a table named 2014pd
# script loads data only from those 2 years

# drop the table if it exists previously
DROP TABLE IF EXISTS g2_table;
# create a table to hold the  data and call it G2_Table
CREATE TABLE G2_Table

# query for 2012 data
# returns designation, bu name, product name, region, year, month, total quantity sold and total value it was sold for
# with columns of commas in between each column from bussines units table, product bu table and table created from csv for 2012
# excluding declining orders


# return columns designation, bu name, product name, region, year and call it year,  month,
# total quantity sold and total value and label each column as such with columns of commas in between all columns
# these columns are named so they can fit in the table
SELECT  A.BU_Designation, ',' AS comma,
        A.BU_Name, ',' AS comma2,
        C.Product, ','AS comma3,
        C.Region,  ','AS comma4,
        B.Prod_BU_Year AS 'Year', ',' AS comma5,
        C.Month,  ',' AS comma6,
        SUM(C.Quantity) AS  'Sum of Quantity',  ',' AS comma7,
        SUM(C.`Order Total`) AS 'Order Total'
# bring data from bussiness units table only entries that match the product bu table with the same bu name and line up those matches
FROM business_unit AS A
INNER JOIN  product_bu AS B
      ON A.BU_Name = B.BU_Name
# bring 2012 csv data only entries that match procut name of product bu table and line up those matches
INNER JOIN  2012pd AS c
      ON B.Product_Name = C.Product
# exclude bu designation decline and filter results to year 2012
WHERE A.BU_Designation <> 'Decline'
    AND B.Prod_BU_Year = 2012
# create units for groups that the sum columns can aggregate using these columns
GROUP BY A.BU_Designation,
         A.BU_Name,
         C.Product,
         C.Region,
         B.Prod_BU_Year,
         C.Month
# filter results to enliminate order quantities of 0
HAVING SUM(C.Quantity) > 0
# concantenate the above query with the one below to create one output table
UNION

# Query for 2014 data
# returns designation, bu name, product name, region, year, month, total quantity sold and total value it was sold for
# from bussines units table, product bu table and table created from csv for 2014
# excluding declining orders

# return columns designation, bu name, product name, region, year and call it year,  month,
# total quantity sold and total value which is order price minus bulk discount and label each column as such
# with columns of commas in between all columns
SELECT  A.BU_Designation, ',' AS comma,
        A.BU_Name, ',' AS comma2,
        C.Product, ','AS comma3,
        C.Region,  ','AS comma4,
        B.Prod_BU_Year AS 'Year', ',' AS comma5,
        C.Month,  ',' AS comma6,
        SUM(C.Quantity) AS  'Sum of Quantity', ',' AS comma7,
        SUM(C.`Order Subtotal`) - SUM(C.`Quantity Discount`) AS 'Order Total'
# bring data from bussiness units table only entries that match the product bu table with the same bu name and line up those matches
FROM business_unit AS A
INNER JOIN  product_bu AS B
   ON A.BU_Name = B.BU_Name
# bring 2014 csv data only entries that match procut name of product bu table and line up those matches
INNER JOIN 2014pd AS c
    ON B.Product_Name = C.Product
# exclude bu designation decline and filter results to year 2014
WHERE A.BU_Designation <> 'Decline'
   AND B.Prod_BU_Year = 2014
# create units for groups that the sum columns can aggregate using these columns
GROUP BY A.BU_Designation,
         A.BU_Name,
         C.Product,
         C.Region,
         B.Prod_BU_Year,
         C.Month
# filter results to enliminate order quantities of 0
HAVING SUM(C.Quantity) > 0
#sort the query ascending by columns starting with columns on left of the list
ORDER BY BU_Designation,
         BU_Name,
         Product,
         Region,
         Year,
         Month,
         'Sum of Quantity',
         'Order Total';

# End Script
