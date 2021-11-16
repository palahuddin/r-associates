#import Library
if(sessionInfo()['basePkgs']=="dplyr" | sessionInfo()['otherPkgs']=="dplyr"){
  detach(package:dplyr, unload=TRUE)
}

if(sessionInfo()['basePkgs']=="tm" | sessionInfo()['otherPkgs']=="tm"){
  detach(package:sentiment, unload=TRUE)
  detach(package:tm, unload=TRUE)
}

library(plyr)
library(arules)
library(arulesViz)


#Impor Dataset
onlineritel <- read.csv("/data/OnlineRetail_dataset.csv")
class(onlineritel)

#Data Cleaning and Exploration
str(onlineritel)
head(onlineritel)

#checking NA values
sum(is.na(onlineritel))

#Convert member number to numeric
sorted <- onlineritel[order(onlineritel$Member_number),]

#Convert item description to categorical format
sorted$Member_number <- as.numeric(sorted$Member_number)
str(sorted)

#Group all the items that were bought together by the same customer on the same date
itemList <- ddply(sorted, c("Member_number","Date"), function(df1)paste(df1$itemDescription,collapse = ","))
                  
head(itemList,15)

#Remove member number and date
itemList$Member_number <- NULL
itemList$Date <- NULL
colnames(itemList) <- c("itemList")

#Write To CSV File Itemlist
write.csv(itemList, "/data/ItemList.csv", quote=FALSE, row.names=TRUE)
head(itemList, 10)

#Convert CSV file to Basket Format
txn = read.transactions(file="/data/ItemList.csv", rm.duplicates= TRUE, format="basket",sep=",",cols=1);
print(txn)

#Remove quotes from Transaction
txn@itemInfo$labels <- gsub("\"","",txn@itemInfo$labels)
basket_rules <- apriori(txn, parameter = list(minlen=2, sup = 0.001, conf = 0.05, target="rules"))


#Total rules generated
print(length(basket_rules))
summary(basket_rules)

#Inspecting the basket rules¶
inspect(basket_rules[1:20])

#write rules and save to csv
View(basket_rules)

#Visualizing the Association Rules¶
plot(basket_rules, jitter = 0)
plot(basket_rules, method = "grouped", control = list(k = 5))


#Graph of first 20 rules¶
plot(basket_rules[1:20], method="graph")

#Graph of first 50 rules
plot(basket_rules[1:50], method="graph")


#Parallel coordinates plot
plot(basket_rules[1:20], method="paracoord")

#Most Frequent Products
itemFrequencyPlot(txn, topN = 20)

