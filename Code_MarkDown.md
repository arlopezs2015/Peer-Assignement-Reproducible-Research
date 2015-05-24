#Reproducible Research: Peer Assessment 2
##Analysis of US National Weather Service Storm Data
##Synopsys

Storms and other severe weather events can cause both public health and economic problems for communities and municipalities. Many severe events can result in fatalities, injuries, and property damage, and preventing such outcomes to the extent possible is a key concern.

This project involves exploring the U.S. National Oceanic and Atmospheric Administration’s (NOAA) storm database. This database tracks characteristics of major storms and weather events in the United States, including when and where they occur, as well as estimates of any fatalities, injuries, and property damage.


#Data
The data for this assignment come in the form of a comma-separated-value file compressed via the bzip2 algorithm to reduce its size. You can download the file from the course web site:

Storm Data [47Mb] There is also some documentation of the database available. Here you will find how some of the variables are constructed/defined.

National Weather Service Storm Data Documentation

National Climatic Data Center Storm Events FAQ

The events in the database start in the year 1950 and end in November 2011. In the earlier years of the database there are generally fewer events recorded, most likely due to a lack of good records. More recent years should be considered more complete.

#Data Processing

##Loading Data

First charge the libraries required
```{r, echo=FALSE}
##install.packages("dplyr")
##install.packages("data.table")
library(dplyr)
library(plyr)
library(stringr)
library(ggplot2)
library(data.table)


summary(cars)
```
Then, download and unzip the file

```{r, echo=FALSE}
##if(!file.exists("./data")){dir.create("./data")}
##fileurl <- "http://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2"
setwd("~/data")
arch<-file.path(getwd(),"repdata-data-StormData.csv.bz2")  
##download.file(fileurl,arch)
data <- read.csv(bzfile(arch), stringsAsFactors = FALSE)
data <- data.table(read.table(bzfile(arch), header = TRUE, sep = ",",stringsAsFactors = FALSE))

```

##Variables needed
A tidy data set is needed to select the columns and the data to be analized to answer the questions asked:

1. Across the United States, which types of events (as indicated in the EVTYPE variable) are most harmful with respect to population health?

2. Across the United States, which types of events have the greatest economic consequences?


Selecting the colulmns to apply the analisys

```{r}
columns_need <- c("EVTYPE","FATALITIES", "INJURIES","PROPDMG", "PROPDMGEXP","CROPDMG", "CROPDMGEXP")   
data_cols_need<-data[,columns_need]
summary(data_cols_need)
```

#Calculate injuries

```{r}
data_injuries <- ddply(data, .(EVTYPE), summarise, fatalities = sum(FATALITIES), injuries = sum(INJURIES)  )
head(data_injuries)

names(data_injuries) <- c("EventType", "fatalities", "injuries")
```

Order by fatalities
```{r}
data_injuries_ord <- data_injuries[with(data_injuries, order(-fatalities, injuries)), ]
```

Omit the data without fatalities


```{r}
data_injuries_0 <- data_injuries_ord[(data_injuries_ord$fatalities & data_injuries_ord$injuries > 0), ] 
```

#Calcualte total damage

Calculate Property damage

```{r}
data_damage <- ddply(data, .(EVTYPE), summarise, property = sum(PROPDMG), crop = sum(CROPDMG))
names(data_damage) <- c("EventType", "property", "crop")
```

Order ascendig the total damage

```{r}
data_damage_ord <- data_damage[with(data_damage, order(-property, crop)), ] 
head(data_damage_ord)
```


#Results


Top 15 type of injuries

Type of harmful events with respect to the population

```{r}
head(data_injuries_0,15)
```

As can be seen on the list table a tornado has caused the majority of casualties

```{r}
top_15_inj <- data_injuries_0 [1:15,]
ggplot(top_15_inj, aes(x = EventType, y = fatalities)) + geom_histogram(stat = "identity", fill = "red") + 
            theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
            xlab("Event Type") + 
            ylab("number of fatalities") + 
            ggtitle("Fatalities per type")
```
![rplot1](https://cloud.githubusercontent.com/assets/10600024/7790129/e2e4ce3e-026a-11e5-8226-4e15be004877.png)

Type of events have the greatest economic consequences

Herein is presented the poperty damage caused by event, in which the damage caused is greater in tornado flash flood

```{r}
head(data_damage_ord,15)

top_15_dmg <- data_damage_ord[1:15,]
```


```{r, echo=FALSE}
ggplot(top_15_dmg, aes(x = EventType, y = property)) + geom_histogram(stat = "identity", fill = "green") + 
            theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
            xlab("Event") + 
            ylab("property damage") + 
            ggtitle("Damage per event")
```

Analogous to the property damage, a crop is similar in the events tornado flash flood

```{r, echo=FALSE}
ggplot(top_15_dmg, aes(x = EventType, y = crop)) + geom_histogram(stat = "identity", fill = "orange") + 
            theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
            xlab("Event") + 
            ylab("crop damage") + 
            ggtitle("Crop damage per event")
```



