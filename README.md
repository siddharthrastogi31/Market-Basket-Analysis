# Market-Basket-Analysis
Using Apriori algorithm to find interesting association rules, using support, confidence threshold and standardised lift for a grocery dataset.
This algorithm can be used for grocery stores : Tesco, Lidl etc.

## Problem

Sometimes, it is difficult to organise and place items in grocery stores, if we use apriori algorithm to identify association between items -> this approach can be used to solve this problem.

![](Result/Grocery_store.jpeg)

## Package
The `arules` package in R can be used to mine association rules from transaction data.

To install package 'arules' :
install.packages("arules") is used.

To load library :
library(arules) is used.

## Data-set
Groceries data has been used to perform the analysis, data is stored in a particular format, as a "transactions" matrix - This representation is a very eﬀicient way of storing large binary matrices.

## Result

After performing, association rules mining. Here, support theshold has been considered as 0.01, confidence threshhold as 0.5.
Most interesting rules based on association value (considered as standardized lift) are :

![](Result/Association_Rules.png)

Higher the standardized lift value denotes that higher is the association between the itemsets.
