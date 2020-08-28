# Project 1: Market Basket Analysis
rm(list=ls())
library(arules)
library(viridisLite)
library(arulesViz)
library(dplyr)

setwd("C:/Users/cchousal/Documents/Data Science/FINAL PROJECT WORK/PROJECT-1-8211-MARKET-BASKET-ANALYSIS-R")

basket<-read.transactions(
  file = 'P1_market_basket.csv',
  sep=",",
  header = TRUE,
  quote="",
  rm.duplicates = TRUE,
  format = 'basket'
)

summary(basket)

##Total Transactions
transactions <- nrow(basket)

##Items in Inventory
inv_items <- ncol(basket)

##Items purchased = rows X cols X density

pur_items <- transactions * inv_items * 0.0009915565

## Plot of 10 Most Frequently  purchased Items.

library(RColorBrewer)

itemFrequencyPlot(x =basket,
                  topN = 10,
                  type = 'absolute',
                  horiz = TRUE,
                  col = brewer.pal(10,'Spectral')
                 )


# 1st set of Association Rules & sort by conf
rule1 <- basket %>% apriori(parameter = list(supp = 0.005, conf = 0.8)) %>% sort(by= 'confidence')
summary(rule1)
rule1 %>% head(n=5) %>% inspect
rule1 %>% tail(n=5) %>% inspect

# sort by lift
rule1 <- rule1 %>% sort(by= 'lift')
rule1 %>% head(n=5) %>% inspect

#Plot
plot(rule1,engine = "htmlwidget", jitter = 0)
plot(rule1, method = "two-key",engine = "htmlwidget", jitter = 0)
plot(rule1, method = "graph", engine = "htmlwidget")


# 2nd set of Association Rules & sort by conf
rule2 <- basket %>% apriori(parameter = list(supp = 0.009, conf = 0.3)) %>% sort(by= 'confidence')
summary(rule2)
rule2 %>% head(n=5) %>% inspect
rule2 %>% tail(n=5) %>% inspect


#Plot
plot(rule2,engine = "htmlwidget", jitter = 0)
plot(rule2, method = "two-key",engine = "htmlwidget", jitter = 0)
plot(rule2, method = "graph", engine = "htmlwidget")


# 3rd set of Association Rules & sort by conf
rule3 <- basket %>% apriori(parameter = list(supp = 0.02, conf = 0.5)) %>% sort(by= 'support')
summary(rule3)
rule3 %>% head(n=5) %>% inspect
rule3 %>% tail(n=5) %>% inspect


#Plot
plot(rule3,engine = "htmlwidget", jitter = 0)
plot(rule3, method = "two-key",engine = "htmlwidget", jitter = 0)
plot(rule3, method = "graph", engine = "htmlwidget")
