rm(list=ls())

##Apriori algorithm 
setwd("C:\\Jaya\\GL\\MRA\\GA")

#Step 1: Read the data
df_cafe <- read.csv("Cafe_data.csv",header = TRUE, sep = ",")
summary(df_cafe)

#Step 2: Data cleaning and manipulations
df_sorted <- df_cafe[order(df_cafe$Bill.Number),]
df_sorted <- as.numeric(df_sorted$Bill.Number)

#Make sure that you do not have package 'dplyr' attached to the session
library(plyr); library(dplyr)
if(sessionInfo()['basePkgs']=="dplyr" | sessionInfo()['otherPkgs']=="dplyr"){
  detach(package:dplyr, unload=TRUE)
}

library(plyr)

#convert the dataframe into basket format
df_itemList <- ddply(df_cafe,c("Bill.Number","Date"),
                     function(df1)paste(df1$Item.Desc, 
                                        collapse = ","))
  
#remove date and bill number
df_itemList_bkp <- df_itemList

df_itemList$Bill.Number <- NULL
df_itemList$Date <- NULL

#Rename column headers for ease of use
colnames(df_itemList) <- c("itemList")

#Write dataframe to a csv file to get row numbers
write.csv(df_itemList,"ItemList.csv", quote = FALSE, row.names = TRUE)
-------------------------------------------------------------------------------------------------------
#Step 3: Find the association rules
pacman::p_load(arules,RColorBrewer)

#convert csv file to basket format
txn = read.transactions(file="ItemList.csv", rm.duplicates= TRUE, format="basket",sep=",",cols=1)
dim(txn)
str(txn)
summary(txn)
head(txn@itemInfo,n=12)

#The Item Frequency Histogram
arules::itemFrequencyPlot(txn,topN=20,col=brewer.pal(8,'Pastel2'),main='Absolute Item Frequency Plot',type="absolute",ylab="Item Frequency (Absolute)")
arules::itemFrequencyPlot(txn,topN=10,col=brewer.pal(8,'Pastel2'),main='Relative Item Frequency Plot',type="relative",ylab="Item Frequency (Relative)")
#sparse matrix
image(sample(txn,100))

#remove quotes if any from transactions
txn@itemInfo$labels <- gsub("\"","",txn@itemInfo$labels)

#run apriori algorithm
basket_rules <- apriori(txn,parameter = list(sup = 0.0001, conf = 0.70,target="rules"))
basket_rules <- sort(basket_rules, by='count', decreasing = TRUE)
basket_rules
#set of 29 rules
summary(basket_rules)
#rule length distribution (lhs + rhs):sizes
#2  3  4  5 
#3  7 17  2 
#3 rules have only two items,while 7 rules have 3 items and 17 have 4 items.

write(basket_rules, file = "basketrules.csv", sep = ",", quote = TRUE, row.names = FALSE)

#detach tm package if you have it
if(sessionInfo()['basePkgs']=="tm" | sessionInfo()['otherPkgs']=="tm"){
  detach(package:sentiment, unload=TRUE)
  detach(package:tm, unload=TRUE)
}

#view rules
inspect(basket_rules[1:10])
inspect(sort(basket_rules, by = "confidence")[1:10])
write(groceryrules, file = "groceryrules.csv", sep = ",", quote = TRUE, row.names = FALSE)
sambucarules <- subset(basket_rules, items %in% "SAMBUCA")
inspect(sambucarules)
topRules <- basket_rules[1:15]

#Graphical Representation
plot(topRules, method = "graph",control = list(type = "items"))
plot(basket_rules, method = "graph",control = list(type = "items"))

#convert to datframe and view; optional
df_basket <- as(basket_rules,"data.frame")
df_basket$confidence <- df_basket$confidence*100
df_basket$support <- df_basket$support*nrow(df_basket)
df_basket

# Mining rules for recommendations:
# split lhs and rhs into two columns
library(reshape2)
df_basket <- transform(df_basket, rules = colsplit(rules, pattern = "=>", names = c("lhs","rhs")))

# Remove curly brackets around rules
df_basket$rules$lhs <- gsub("[[:punct:]]", "", df_basket$rules$lhs)
df_basket$rules$rhs <- gsub("[[:punct:]]", "", df_basket$rules$rhs)

# convert to chracter
df_basket$rules$lhs <- as.character(df_basket$rules$lhs)
df_basket$rules$rhs <- as.character(df_basket$rules$rhs)

library(stringi)
library(dplyr)
df_basket$rules %>%
  filter(stri_detect_fixed(lhs, "N R G HOOKAH")) %>%
  select(rhs)


#plot the rules
pacman::p_load(arulesViz)
plot(basket_rules)

set.seed(8000)
plot(basket_rules, method = "grouped", control = list(k = 5))

plot(basket_rules[1:29,], method="graph", control=list(type="items"))

plot(basket_rules[1:29], method="paracoord",  control=list(reorder=TRUE))

itemFrequencyPlot(txn, topN = 10)

arulesViz::plotly_arules(basket_rules)

plot(basket_rules[1:10,],measure=c("support","lift"),shading="confidence",interactive=T)
