#title: "Final Project"
#author: "Tyler McAfee"
#date: "7/20/2020" - 9/15/2020
#output: pdf_document

  
#  The following data is from https://catalog.data.gov/dataset/nyc-citywide-annualized-calendar-sales-update. 
#  This document introduces into the data and explores the prices of homes compared to different variables. 
#  I hope you enjoy.


#setwd("C:\\Users\\tmcafee\\tmcafee\\Education\\Summer 2020\\IST719\\FinalProject")

library(dplyr)
library(lubridate)
library(stringr)
library(rgdal)
library(ggplot2)
library(zoo)
library(ggpubr)
library(ggthemes)

# Read in the NYC housing dataset
housing <- read.csv("C:/Users/tmcafee/tmcafee/Education/Summer 2020/IST719/FinalProject/NYC_Citywide_Annualized_Calendar_Sales_Update.csv"
                    , header = T
                    , stringsAsFactors = F)

# Get an idea of the data set
#par(mfrow=c(2,1))
View(housing)
head(housing)
colnames(housing)
str(housing)
dim(housing)

# For this project I will just focus on family dwellings, to get an idea of housing real estate
familyHousing <- housing %>% filter(str_detect(BUILDING.CLASS.CATEGORY, 'FAMILY DWELLINGS'))

#Removes some columns
familyHousing <- familyHousing[ , -which(names(familyHousing) %in% c("EASE.MENT", "Community.Board", "Council.District", "Census.Tract","TAX.CLASS.AS.OF.FINAL.ROLL", "TAX.CLASS.AT.TIME.OF.SALE"))]
str(familyHousing)
unique(familyHousing$TAX.CLASS.AS.OF.FINAL.ROLL)
unique(familyHousing$TAX.CLASS.AT.TIME.OF.SALE)

#Remove NAs from the data
familyHousing <- na.omit(familyHousing)

# Initial calculation of the dataset score
dataSetScore <- (ncol(familyHousing)*4)*(nrow(familyHousing)/100)  
dataSetScore

# As indicated above, I just will focus on housing real estate, so I want to know the sale price for houses.
# Will filter out records that don't have a sale price
familyHousing[familyHousing==0] <- NA
familyHousingSales <- familyHousing[!is.na(familyHousing$SALE.PRICE),]
familyHousingSales <- familyHousingSales[!is.na(familyHousingSales$LAND.SQUARE.FEET),]


# Need to clean up the Building.Class.Category variable. This variable will be used in my analysis.
familyHousingSales[familyHousingSales$BUILDING.CLASS.CATEGORY == "01  ONE FAMILY DWELLINGS", "BUILDING.CLASS.CATEGORY"] <- "ONE FAMILY DWELLINGS"
familyHousingSales[familyHousingSales$BUILDING.CLASS.CATEGORY == "01 ONE FAMILY DWELLINGS", "BUILDING.CLASS.CATEGORY"] <- "ONE FAMILY DWELLINGS"
familyHousingSales[familyHousingSales$BUILDING.CLASS.CATEGORY == "02  TWO FAMILY DWELLINGS", "BUILDING.CLASS.CATEGORY"] <- "TWO FAMILY DWELLINGS"
familyHousingSales[familyHousingSales$BUILDING.CLASS.CATEGORY == "02 TWO FAMILY DWELLINGS", "BUILDING.CLASS.CATEGORY"] <- "TWO FAMILY DWELLINGS"
familyHousingSales[familyHousingSales$BUILDING.CLASS.CATEGORY == "03  THREE FAMILY DWELLINGS", "BUILDING.CLASS.CATEGORY"] <- "THREE FAMILY DWELLINGS"
familyHousingSales[familyHousingSales$BUILDING.CLASS.CATEGORY == "03 THREE FAMILY DWELLINGS", "BUILDING.CLASS.CATEGORY"] <- "THREE FAMILY DWELLINGS"


# Add buckets for the home price
familyHousingSales$Price.Size <- ""
familyHousingSales[which(familyHousingSales$SALE.PRICE < 500000),]$Price.Size <- "<500k"
familyHousingSales[which(familyHousingSales$SALE.PRICE >= 500000 & familyHousingSales$SALE.PRICE <= 1000000),]$Price.Size <- "500k-1million"
familyHousingSales[which(familyHousingSales$SALE.PRICE > 1000000),]$Price.Size <- ">1million"

unique(familyHousingSales$Price.Size)

# Convert date to POSIX
familyHousingSales$SALE.DATE <- as.POSIXct(familyHousingSales$SALE.DATE, format = "%m/%d/%Y")

###################### Further cleaning #########################################

# Will create a copy of my df now for reference
familyHousingSalesSmall <- familyHousingSales

# Focusing on only single family dwellings
familyHousingSalesSmall <- familyHousingSalesSmall[which(familyHousingSalesSmall$BUILDING.CLASS.CATEGORY == "ONE FAMILY DWELLINGS"),]

# Add buckets for land size
familyHousingSalesSmall$Land.Size <- ""
familyHousingSalesSmall[which(familyHousingSalesSmall$LAND.SQUARE.FEET < 25000),]$Land.Size <- "<25k sq ft"
familyHousingSalesSmall[which(familyHousingSalesSmall$LAND.SQUARE.FEET >= 25000 & familyHousingSalesSmall$LAND.SQUARE.FEET <= 50000),]$Land.Size <- "25k-50k sq ft"
familyHousingSalesSmall[which(familyHousingSalesSmall$LAND.SQUARE.FEET > 50000),]$Land.Size <- ">50k sq ft"

# Add borough names
familyHousingSalesSmall$BoroughName <- ""
familyHousingSalesSmall[which(familyHousingSalesSmall$BOROUGH == 1),]$BoroughName <- "Manhattan"
familyHousingSalesSmall[which(familyHousingSalesSmall$BOROUGH == 2),]$BoroughName <- "Bronx"
familyHousingSalesSmall[which(familyHousingSalesSmall$BOROUGH == 3),]$BoroughName <- "Brooklyn"
familyHousingSalesSmall[which(familyHousingSalesSmall$BOROUGH == 4),]$BoroughName <- "Queens"
familyHousingSalesSmall[which(familyHousingSalesSmall$BOROUGH == 5),]$BoroughName <- "Staten Island"



########################Exploratory plots#################################
# Define some colors
myColors <- c("#6BAED6", "#4292C6", "firebrick1", "#08519C", "#084594")
names(myColors) <- levels(familyHousingSalesSmall$BoroughName)
colScale <- scale_colour_manual(name = "BoroughName",values = myColors)

# Scatter plot : showing us an idea of the data
ggplot(familyHousingSalesSmall, aes(x=SALE.DATE, y=SALE.PRICE, colour = BoroughName)) + theme_classic() +
  geom_point() + colScale + scale_y_continuous(limits = c(0,41000000), labels = c("0", "10million", "20million", "30million", "40million")) + 
  theme(text = element_text(color = "black", size = 15, face = "bold"),
        legend.background = element_rect(fill = "#EAF6F9",
                                         colour = "#EAF6F9") 
        , panel.background = element_rect(fill = "#EAF6F9",
                                          colour = "#EAF6F9")
        , plot.background = element_rect(fill = "#EAF6F9"))
# Now will focus on sale prices less than 5 million
familyHousingSalesSmall <- familyHousingSalesSmall[which(familyHousingSalesSmall$SALE.PRICE < 5000000),]

# Checking the dataset score again to verify it still meets the minimum requirement
dataSetScore1 <- (ncol(familyHousingSalesSmall)*4)*(nrow(familyHousingSalesSmall)/100)  
dataSetScore1
dim(familyHousingSales)

#####################Donut land size plots#####################################

### Manhattan
Manhattan <- familyHousingSalesSmall[which(familyHousingSalesSmall$BoroughName == "Manhattan"),]

ManhattanSize <- as.data.frame(table(Manhattan$Land.Size))
colnames(ManhattanSize) <- c("LandSize", "Count")

blue.color.vec <- c("#6BAED6", "#2171B5", "#084594")
ManhattanSize$LandSize <- factor(ManhattanSize$LandSize, levels = c("<25k sq ft", "25k-50k sq ft", ">50k sq ft"))
ManhattanSize <- ManhattanSize %>%
  arrange(desc(LandSize)) %>%
  mutate(lab.ypos = cumsum(Count) - 0.5*Count)

ggplot(ManhattanSize, aes(x = 2, y = Count, fill = LandSize)) +
  geom_bar(stat = "identity", color = "white") +
  coord_polar(theta = "y", start = 0)+
  geom_text(aes(y = lab.ypos, label = Count), color = "white", size = 7)+ 
  scale_fill_manual(values = blue.color.vec) +
  theme_void()+
  xlim(0.5, 2.5) + 
  theme(legend.title = element_text(color = "black", size = 20),
        legend.text = element_text(color = "black", size = 20),
        legend.key.size = unit(1.5, "cm"),
        legend.key.width = unit(1.5,"cm"),
        plot.margin = margin(0, .2, 0, .2, "cm"),
        title = element_text(size=20))+
  ggtitle("Manhattan")

### Bronx
Bronx <- familyHousingSalesSmall[which(familyHousingSalesSmall$BoroughName == "Bronx"),]

BronxSize <- as.data.frame(table(Bronx$Land.Size))
colnames(BronxSize) <- c("LandSize", "Count")
BronxSize$LandSize <- factor(BronxSize$LandSize, levels = c("<25k sq ft", "25k-50k sq ft", ">50k sq ft"))
BronxSize <- BronxSize %>%
  arrange(desc(LandSize)) %>%
  mutate(lab.ypos = cumsum(Count) - 0.5*Count)

ggplot(BronxSize, aes(x = 2, y = Count, fill = LandSize)) +
  geom_bar(stat = "identity", color = "white") +
  coord_polar(theta = "y", start = 0)+
  geom_text(aes(y = lab.ypos, label = Count), color = "white", size = 6)+ 
  scale_fill_manual(values = blue.color.vec) +
  theme_void()+
  xlim(0.5, 2.5) + 
  theme(legend.title = element_text(color = "black", size = 20),
        legend.text = element_text(color = "black", size = 20),
        legend.key.size = unit(1.5, "cm"),
        legend.key.width = unit(2,"cm"),
        plot.margin = margin(0, .2, 0, .2, "cm"),
        title = element_text(size=20))+
  ggtitle("Bronx")

#### Brooklyn
Brooklyn <- familyHousingSalesSmall[which(familyHousingSalesSmall$BoroughName == "Brooklyn"),]

BrooklynSize <- as.data.frame(table(Brooklyn$Land.Size))
colnames(BrooklynSize) <- c("LandSize", "Count")
BrooklynSize$LandSize <- factor(BrooklynSize$LandSize, levels = c("<25k sq ft", "25k-50k sq ft", ">50k sq ft"))
BrooklynSize <- BrooklynSize %>%
  arrange(desc(LandSize)) %>%
  mutate(lab.ypos = cumsum(Count) - 0.5*Count)

ggplot(BrooklynSize, aes(x = 2, y = Count, fill = LandSize)) +
  geom_bar(stat = "identity", color = "white") +
  coord_polar(theta = "y", start = 0)+
  geom_text(aes(y = lab.ypos, label = Count), color = "white", size = 6)+ 
  scale_fill_manual(values = blue.color.vec) +
  theme_void()+
  xlim(0.5, 2.5) + 
  theme(legend.title = element_text(color = "black", size = 20),
        legend.text = element_text(color = "black", size = 20),
        legend.key.size = unit(1.5, "cm"),
        legend.key.width = unit(2,"cm"),
        plot.margin = margin(0, .2, 0, .2, "cm"),
        title = element_text(size=20))+
  ggtitle("Brooklyn")

#### Queens
Queens <- familyHousingSalesSmall[which(familyHousingSalesSmall$BoroughName == "Queens"),]

QueensSize <- as.data.frame(table(Queens$Land.Size))
colnames(QueensSize) <- c("LandSize", "Count")
QueensSize$LandSize <- factor(QueensSize$LandSize, levels = c("<25k sq ft", "25k-50k sq ft", ">50k sq ft"))
QueensSize <- QueensSize %>%
  arrange(desc(LandSize)) %>%
  mutate(lab.ypos = cumsum(Count) - 0.5*Count)

ggplot(QueensSize, aes(x = 2, y = Count, fill = LandSize)) +
  geom_bar(stat = "identity", color = "white") +
  coord_polar(theta = "y", start = 0)+
  geom_text(aes(y = lab.ypos, label = Count), color = "white", size = 3.5)+ 
  scale_fill_manual(values = blue.color.vec) +
  theme_void()+
  xlim(0.5, 2.5) + 
  theme(legend.title = element_text(color = "black", size = 20),
        legend.text = element_text(color = "black", size = 20),
        legend.key.size = unit(1.5, "cm"),
        legend.key.width = unit(2,"cm"),
        plot.margin = margin(0, .2, 0, .2, "cm"),
        title = element_text(size=20))+
  ggtitle("Queens")

#### Staten Island
Staten <- familyHousingSalesSmall[which(familyHousingSalesSmall$BoroughName == "Staten Island"),]

StatenSize <- as.data.frame(table(Staten$Land.Size))
colnames(StatenSize) <- c("LandSize", "Count")
StatenSize$LandSize <- factor(StatenSize$LandSize, levels = c("<25k sq ft", "25k-50k sq ft", ">50k sq ft"))
StatenSize <- StatenSize %>%
  arrange(desc(LandSize)) %>%
  mutate(lab.ypos = cumsum(Count) - 0.5*Count)

ggplot(StatenSize, aes(x = 2, y = Count, fill = LandSize)) +
  geom_bar(stat = "identity", color = "white") +
  coord_polar(theta = "y", start = 0)+
  geom_text(aes(y = lab.ypos, label = Count), color = "white", size = 4.8)+ 
  scale_fill_manual(values = blue.color.vec) +
  theme_void()+
  xlim(0.5, 2.5) + 
  theme(legend.title = element_text(color = "black", size = 20),
        legend.text = element_text(color = "black", size = 20),
        legend.key.size = unit(1.5, "cm"),
        legend.key.width = unit(2,"cm"),
        plot.margin = margin(0, .2, 0, .2, "cm"),
        title = element_text(size=20))+
  ggtitle("Staten Island")


#########################Donut price size plots################################

### Manhattan
Manhattan <- familyHousingSalesSmall[which(familyHousingSalesSmall$BoroughName == "Manhattan"),]

ManhattanPriceSize <- as.data.frame(table(Manhattan$Price.Size))
colnames(ManhattanPriceSize) <- c("PriceSize", "Count")

ManhattanPriceSize$PriceSize <- factor(ManhattanPriceSize$PriceSize, levels = c("<500k", "500k-1million", ">1million"))
ManhattanPriceSize <- ManhattanPriceSize %>%
  arrange(desc(PriceSize)) %>%
  mutate(lab.ypos = cumsum(Count) - 0.5*Count)

ggplot(ManhattanPriceSize, aes(x = 2, y = Count, fill = PriceSize)) +
  geom_bar(stat = "identity", color = "white") +
  coord_polar(theta = "y", start = 0)+
  geom_text(aes(y = lab.ypos, label = Count), color = "white", size = 7)+ 
  scale_fill_manual(values = blue.color.vec) +
  theme_void()+
  xlim(0.5, 2.5) + 
  theme(legend.title = element_text(color = "black", size = 20),
        legend.text = element_text(color = "black", size = 20),
        legend.key.size = unit(1.5, "cm"),
        legend.key.width = unit(2,"cm"),
        plot.margin = margin(0, .2, 0, .2, "cm"),
        title = element_text(size=20))+
  ggtitle("Manhattan")

### Bronx
Bronx <- familyHousingSalesSmall[which(familyHousingSalesSmall$BoroughName == "Bronx"),]

BronxPriceSize <- as.data.frame(table(Bronx$Price.Size))
colnames(BronxPriceSize) <- c("PriceSize", "Count")
BronxPriceSize$PriceSize <- factor(BronxPriceSize$PriceSize, levels = c("<500k", "500k-1million", ">1million"))
BronxPriceSize <- BronxPriceSize %>%
  arrange(desc(PriceSize)) %>%
  mutate(lab.ypos = cumsum(Count) - 0.5*Count)

ggplot(BronxPriceSize, aes(x = 2, y = Count, fill = PriceSize)) +
  geom_bar(stat = "identity", color = "white") +
  coord_polar(theta = "y", start = 0)+
  geom_text(aes(y = lab.ypos, label = Count), color = "white", size = 7)+ 
  scale_fill_manual(values = blue.color.vec) +
  theme_void()+
  xlim(0.5, 2.5) + 
  theme(legend.title = element_text(color = "black", size = 20),
        legend.text = element_text(color = "black", size = 20),
        legend.key.size = unit(1.5, "cm"),
        legend.key.width = unit(2,"cm"),
        plot.margin = margin(0, .2, 0, .2, "cm"),
        title = element_text(size=20))+
  ggtitle("Bronx")

#### Brooklyn
Brooklyn <- familyHousingSalesSmall[which(familyHousingSalesSmall$BoroughName == "Brooklyn"),]

BrooklynPriceSize <- as.data.frame(table(Brooklyn$Price.Size))
colnames(BrooklynPriceSize) <- c("PriceSize", "Count")
BrooklynPriceSize$PriceSize <- factor(BrooklynPriceSize$PriceSize, levels = c("<500k", "500k-1million", ">1million"))
BrooklynPriceSize <- BrooklynPriceSize %>%
  arrange(desc(PriceSize)) %>%
  mutate(lab.ypos = cumsum(Count) - 0.5*Count)

ggplot(BrooklynPriceSize, aes(x = 2, y = Count, fill = PriceSize)) +
  geom_bar(stat = "identity", color = "white") +
  coord_polar(theta = "y", start = 0)+
  geom_text(aes(y = lab.ypos, label = Count), color = "white", size = 7)+ 
  scale_fill_manual(values = blue.color.vec) +
  theme_void()+
  xlim(0.5, 2.5) + 
  theme(legend.title = element_text(color = "black", size = 20),
        legend.text = element_text(color = "black", size = 20),
        legend.key.size = unit(1.5, "cm"),
        legend.key.width = unit(2,"cm"),
        plot.margin = margin(0, .2, 0, .2, "cm"),
        title = element_text(size=20))+
  ggtitle("Brooklyn")

#### Queens
Queens <- familyHousingSalesSmall[which(familyHousingSalesSmall$BoroughName == "Queens"),]

QueensPriceSize <- as.data.frame(table(Queens$Price.Size))
colnames(QueensPriceSize) <- c("PriceSize", "Count")
QueensPriceSize$PriceSize <- factor(QueensPriceSize$PriceSize, levels = c("<500k", "500k-1million", ">1million"))
QueensPriceSize <- QueensPriceSize %>%
  arrange(desc(PriceSize)) %>%
  mutate(lab.ypos = cumsum(Count) - 0.5*Count)

ggplot(QueensPriceSize, aes(x = 2, y = Count, fill = PriceSize)) +
  geom_bar(stat = "identity", color = "white") +
  coord_polar(theta = "y", start = 0)+
  geom_text(aes(y = lab.ypos, label = Count), color = "white", size = 7)+ 
  scale_fill_manual(values = blue.color.vec) +
  theme_void()+
  xlim(0.5, 2.5) + 
  theme(legend.title = element_text(color = "black", size = 20),
        legend.text = element_text(color = "black", size = 20),
        legend.key.size = unit(1.5, "cm"),
        legend.key.width = unit(2,"cm"),
        plot.margin = margin(0, .2, 0, .2, "cm"),
        title = element_text(size=20))+
  ggtitle("Queens")

#### Staten Island
Staten <- familyHousingSalesSmall[which(familyHousingSalesSmall$BoroughName == "Staten Island"),]

StatenPriceSize <- as.data.frame(table(Staten$Price.Size))
colnames(StatenPriceSize) <- c("PriceSize", "Count")
StatenPriceSize$PriceSize <- factor(StatenPriceSize$PriceSize, levels = c("<500k", "500k-1million", ">1million"))
StatenPriceSize <- StatenPriceSize %>%
  arrange(desc(PriceSize)) %>%
  mutate(lab.ypos = cumsum(Count) - 0.5*Count)

ggplot(StatenPriceSize, aes(x = 2, y = Count, fill = PriceSize)) +
  geom_bar(stat = "identity", color = "white") +
  coord_polar(theta = "y", start = 0)+
  geom_text(aes(y = lab.ypos, label = Count), color = "white", size = 7)+ 
  scale_fill_manual(values = blue.color.vec) +
  theme_void()+
  xlim(0.5, 2.5) + 
  theme(legend.title = element_text(color = "black", size = 20),
        legend.text = element_text(color = "black", size = 20),
        legend.key.size = unit(1.5, "cm"),
        legend.key.width = unit(2,"cm"),
        plot.margin = margin(0, .2, 0, .2, "cm"),
        title = element_text(size=20))+
  ggtitle("Staten Island")

###########################Time series############################

# Looking at another exploratory plot, these time series plots will show us average price over the months for each borough

# Group Bronx by month, year, price
aggDataBronx <- aggregate(Bronx$SALE.PRICE, list(lubridate::month(Bronx$SALE.DATE),lubridate::year(Bronx$SALE.DATE)), mean)
colnames(aggDataBronx) <- c("Month", "Year", "MEAN.PRICE")
my.months <- c(rep(c("January","February","March","April","May","June","July","August","September","October","November","December"),4))
aggDataBronx$monthName <- my.months
aggDataBronx$BoroughName <- c(rep(c("Bronx"),48))

# Group Brooklyn by month, year, price
aggDataBrooklyn <- aggregate(Brooklyn$SALE.PRICE, list(lubridate::month(Brooklyn$SALE.DATE),lubridate::year(Brooklyn$SALE.DATE)), mean)
colnames(aggDataBrooklyn) <- c("Month", "Year", "MEAN.PRICE")
my.months <- c(rep(c("January","February","March","April","May","June","July","August","September","October","November","December"),4))
aggDataBrooklyn$monthName <- my.months
aggDataBrooklyn$BoroughName <- c(rep(c("Brooklyn"),48))


#######
# It was found that Manhattan has limited data for each month. There are actually 4 months that have no sales, so need to give them values for place holders in plot
aggDataManhattan <- aggregate(Manhattan$SALE.PRICE, list(lubridate::month(Manhattan$SALE.DATE),lubridate::year(Manhattan$SALE.DATE)), mean)
colnames(aggDataManhattan) <- c("Month", "Year", "MEAN.PRICE")
View(aggDataManhattan)

# Empty months in Manhattan data frame so need to make a function to insert values in those rows
insertRow <- function(aggDataManhattan, newrow, r) {
  aggDataManhattan[seq(r+1,nrow(aggDataManhattan)+1),] <- aggDataManhattan[seq(r,nrow(aggDataManhattan)),]
  aggDataManhattan[r,] <- newrow
  aggDataManhattan
}


# Will add values for 4 rows in the data set. Will set the value to the average of the mean price from month before/after
r <- 14
newrow <- c("2", "2017", mean(c(aggDataManhattan$MEAN.PRICE[13],aggDataManhattan$MEAN.PRICE[15])))
aggDataManhattan <- insertRow(aggDataManhattan, as.numeric(newrow), r)

r <- 16
newrow <- c("4", "2017", mean(c(aggDataManhattan$MEAN.PRICE[15],aggDataManhattan$MEAN.PRICE[17])))
aggDataManhattan <- insertRow(aggDataManhattan, as.numeric(newrow), r)

r <- 18
newrow <- c("6", "2017", mean(c(aggDataManhattan$MEAN.PRICE[17],aggDataManhattan$MEAN.PRICE[19])))
aggDataManhattan <- insertRow(aggDataManhattan, as.numeric(newrow), r)

r <- 48
newrow <- c("12", "2019", mean(aggDataManhattan$MEAN.PRICE[47]))
aggDataManhattan <- insertRow(aggDataManhattan, as.numeric(newrow), r)

# Created an extra row so removing
aggDataManhattan <- aggDataManhattan[-c(49), ] 


my.months <- c(rep(c("January","February","March","April","May","June","July","August","September","October","November","December"),4))
aggDataManhattan$monthName <- my.months
aggDataManhattan$BoroughName <- c(rep(c("Manhattan"),48))

##############

# Group Queens by month, year, price
aggDataQueens <- aggregate(Queens$SALE.PRICE, list(lubridate::month(Queens$SALE.DATE),lubridate::year(Queens$SALE.DATE)), mean)
colnames(aggDataQueens) <- c("Month", "Year", "MEAN.PRICE")
my.months <- c(rep(c("January","February","March","April","May","June","July","August","September","October","November","December"),4))
aggDataQueens$monthName <- my.months
aggDataQueens$BoroughName <- c(rep(c("Queens"),48))

# Group Staten  Island by month, year, price
aggDataStaten <- aggregate(Staten$SALE.PRICE, list(lubridate::month(Staten$SALE.DATE),lubridate::year(Staten$SALE.DATE)), mean)
colnames(aggDataStaten) <- c("Month", "Year", "MEAN.PRICE")
my.months <- c(rep(c("January","February","March","April","May","June","July","August","September","October","November","December"),4))
aggDataStaten$monthName <- my.months
aggDataStaten$BoroughName <- c(rep(c("Staten Island"),48))

# Will combine the 4 boroughs that exclude Manhattan (will split these borughs so that data is represented better)
aggDataAll <- rbind(aggDataBronx, aggDataBrooklyn, aggDataQueens, aggDataStaten)

# Define some colors for these plots
my_colors <- RColorBrewer::brewer.pal(9,"Blues")[5:9]

# Data frame and time series plot for 4 boroughs
dat1 = data.frame(date = seq(as.Date("2016-01-15"), as.Date("2019-12-15"), "1 month"),
                  MEAN.PRICE = aggDataAll$MEAN.PRICE, Borough = aggDataAll$BoroughName)
dat1$date = as.yearmon(dat1$date)

ggplot(dat1, aes(date, label=TRUE, abbr=TRUE, 
                 MEAN.PRICE, group=aggDataAll$BoroughName, colour=aggDataAll$BoroughName)) +
  geom_line(size = 1) +
  scale_color_manual(values = c("#6BAED6", "#084594","#2171B5", "#4292C6"))+
  geom_point(size = 2, stroke = 1, shape = 19) +
  labs(x="Time (Each point is a month)", colour="Borough") +
  theme_classic() +  scale_y_continuous(limits= c(0,1200000), breaks = c(0,200000,400000,600000,800000,1000000,1200000), labels = c("0", "200k", "400k", "600k", "800k", "1million", "1.2million")) +
  theme(legend.title = element_text(color = "black", size = 15, face = "bold"),
        text = element_text(color = "black", size = 15, face = "bold"),
        legend.key.size = unit(1.5, "cm"),
        legend.key.width = unit(1.5,"cm"),
        legend.background = element_rect(fill = "#EAF6F9",
                                         colour = "#EAF6F9") 
        , panel.background = element_rect(fill = "#EAF6F9",
                                          colour = "#EAF6F9")
        , plot.background = element_rect(fill = "#EAF6F9"))

# Data frame and time series plot for Manhattan
dat2 = data.frame(date = seq(as.Date("2016-01-15"), as.Date("2019-12-15"), "1 month"),
                  MEAN.PRICE = aggDataManhattan$MEAN.PRICE, Borough = aggDataManhattan$BoroughName)

dat2$date = as.yearmon(dat2$date)
ggplot(dat2, aes(date, label=TRUE, abbr=TRUE, 
                 MEAN.PRICE, group=aggDataManhattan$BoroughName, colour=aggDataManhattan$BoroughName)) +
  geom_line(size=1) +
  scale_color_manual(values = c("#08519C"))+
  geom_point(size = 2, stroke = 1, shape = 19) +
  labs(x="Time (Each point is a month)", colour="Borough") +
  theme_classic() +  scale_y_continuous(limits= c(0,5000000), breaks = c(0,1000000,2000000,3000000,4000000,5000000), labels = c("0", "1million", "2million", "3million", "4million", "5million")) +
  theme(legend.title = element_text(color = "black", size = 15, face = "bold"),
        text = element_text(color = "black", size = 15, face = "bold"),
        legend.key.size = unit(1.5, "cm"),
        legend.key.width = unit(1.5,"cm"),
        legend.background = element_rect(fill = "#EAF6F9",
                                         colour = "#EAF6F9")
        , panel.background = element_rect(fill = "#EAF6F9",
                                          colour = "#EAF6F9")
        , plot.background = element_rect(fill = "#EAF6F9"))

###################Exploratory plot honed in #######################################

# Boxplot of data by borough
ggplot(familyHousingSalesSmall, aes(x=BoroughName, y=SALE.PRICE)) + theme_classic() +
  scale_y_continuous(limits= c(0,5000000), breaks = c(0,1000000,2000000,3000000,4000000,5000000), labels = c("0", "1million", "2million", "3million", "4million", "5million")) +
  geom_boxplot(fill = c("#6BAED6", "#4292C6", "#2171B5", "#08519C", "#084594"), outlier.alpha = 0.2, size =1) + 
  theme(text = element_text(color = "black", size = 15, face = "bold"),axis.line = element_line(colour = "black")
        , panel.background = element_rect(fill = "#EAF6F9",
                                          colour = "#EAF6F9")
        , plot.background = element_rect(fill = "#EAF6F9"))

# Boxplot of data by year
familyHousingSalesSmall$SALE.YEAR <- as.factor(lubridate::year(familyHousingSalesSmall$SALE.DATE))

ggplot(familyHousingSalesSmall, aes(x=SALE.YEAR, y=SALE.PRICE)) + theme_classic() +
  scale_y_continuous(limits= c(0,5000000), breaks = c(0,1000000,2000000,3000000,4000000,5000000), labels = c("0", "1million", "2million", "3million", "4million", "5million")) +
  geom_boxplot(fill = c("#6BAED6", "#4292C6", "#2171B5", "#084594"), outlier.alpha = 0.2, size =1) + #coord_flip() +
  theme(text = element_text(color = "black", size = 15, face = "bold"),axis.line = element_line(colour = "black")
        , panel.background = element_rect(fill = "#EAF6F9",
                                          colour = "#EAF6F9")
        , plot.background = element_rect(fill = "#EAF6F9"))


############################# Map Section #########################################


# install.packages("ggthemes")

Project.dir <- "C:\\Users\\tmcafee\\tmcafee\\Education\\Summer 2020\\IST719\\FinalProject\\"

# Read in the NYCC shape data
nyc <- readOGR(paste0(Project.dir, "nybb_20c")
               , "nybb", stringsAsFactors = F)

# Zoom in on map
plot(nyc, lwd=0.5, asp=1)
nyc_map <- fortify(nyc, 0.05)

# This shape data has borough values that differ from my data set, so need to update
nyc_map$id <- as.numeric(nyc_map$id)
nyc_map$id <- nyc_map$id+1
nycData <- as.data.frame(nyc$BoroCode,nyc$BoroName)
nycData$BOROUGH <- rownames(nycData)
rownames(nycData) <- c()
dim(nycData)
colnames(nycData) <- c("id")
str(nycData)
nycData$id<- as.numeric(nycData$id)
nycData[order(nycData$id),]

SalePrice <- aggregate(familyHousingSalesSmall$SALE.PRICE, list(familyHousingSalesSmall$BOROUGH), mean)
SalePrice$idnew <- c("3","1","4","5","2")
colnames(SalePrice) <- c("idOld", "MEAN.PRICE", "id")

# Merging nyc map data and average sale price data
mergedata <- merge(x = nycData, y = SalePrice, by = "id")
str(mergedata)

my.map <- merge(x = nyc_map, y = SalePrice, by = "id")

# Creating NYC plot with geom_map
ggplot() + geom_map(data = my.map, map = my.map) + 
  aes(x = long, y = lat, map_id = id, group = group
      , fill = MEAN.PRICE) + theme_void() +
  theme(legend.title = element_text(color = "black", size = 20, face = "bold"),
        legend.text = element_text(color = "black", size = 20, face = "bold"),
        legend.key.size = unit(1.5, "cm"),
        legend.key.width = unit(1.5,"cm") )


