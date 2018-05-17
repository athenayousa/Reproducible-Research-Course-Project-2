---
title: "Exploratory Data Analysis of major storms and weather events on Public Health and Economy using NOAA Storm Database in the US"
author: "Sreenivasulu Parimi"
date: "May 13, 2018"
output: html_document
keep_md: true
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

#  Reproducible Research: Peer Assessment 2

## 1. Synopsis
Storms and other severe weather events can cause both public health and economic problems for communities and municipalities. Many severe events can result in fatalities, injuries, and property damage, and preventing such outcomes to the extent possible is a key concern.

This project involves exploring the U.S. National Oceanic and Atmospheric Administration's (NOAA) storm database. This database tracks characteristics of major storms and weather events in the United States, including when and where they occur, as well as estimates of any fatalities, injuries, and property damage.

An analysis of the NOAA Storm Data has been performed with respect to population health and economic consequences due to weather events. The analysis shows that major reason for INJURIES was TORNADOES while main reason for FATALITIES was EXCESSIVE HEAT. Further, it shows that major reason for CROP damage was DROUGHT while main reason for PROPERTY damage was FLASH FLOOD.

## 2. Data Processing

#### 2.1 Load Packages
Load the required packages
```{r}
library(R.utils)
library(dplyr)
library(ggplot2)
library(gridExtra)
```

#### 2.2 Download and Read File
Download the bz2files format data file and unzip if it doesn't exist.
```{r}
url <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2"
file_name <- "StormData.csv.bz2"
if(!file.exists(file_name)) {
      download.file(url, file_name)
}

if(!file.exists('StormData.csv')) {
      bunzip2("StormData.csv.bz2", overwrite=T, remove=F)
}

raw_storm_data <- read.csv('StormData.csv')
```

#### 2.3 Explore the Storm US Data
Run the required data exploration activities

```{r}
dim(raw_storm_data)
head(raw_storm_data, n = 2)
str(raw_storm_data)
summary(raw_storm_data)
```
The raw storm data contains 902297 rows and 37 columns.

#### 2.4 Select Required Observations and Variables for the analysis

We took the following variables those are sufficient for this analysis.

##### Health variables:
1 **FATALITIES**: approx. number of deaths
2 **INJURIES**: approx. number of injuries

##### Economic variables:
1 **PROPDMG**: approx. property damags
2 **PROPDMGEXP**: the units for property damage value
3 **CROPDMG**: approx. crop damages
4 **CROPDMGEXP**: the units for crop damage value

##### Other Variables:
1 **EVTYPE**: weather event (Tornados, Wind, Snow, Flood, etc..)
2 **BGN_DATE**: Years

```{r}
required_vars <- c( "BGN_DATE", "EVTYPE", "FATALITIES", "INJURIES", "PROPDMG", "PROPDMGEXP", "CROPDMG", "CROPDMGEXP")
tidy_storm_data <- raw_storm_data[, required_vars]
tidy_storm_data$EVTYPE <- toupper(tidy_storm_data$EVTYPE)
```

Added additional new variable called YEAR which stores the proper format of date.
```{r}
tidy_storm_data$YEAR <- as.numeric(format(as.Date(tidy_storm_data$BGN_DATE, format = "%m/%d/%Y %H:%M:%S"), "%Y"))
```
The events in the database start in the year 1950 and end in November 2011. In the earlier years of the database there are generally fewer events recorded, most likely due to a lack of good records. More recent years should be considered more complete.

Look at the histogram of the data Yearly wise:
```{r}
hist(tidy_storm_data$YEAR, col = "blue", breaks = 30, main="Number of Weather EVENTs per Year", xlab="Year", ylab="Frequency")
```

As for the above histogram, we can see that the number of events tracked starts to significantly increase from around 1990. So, we can use the subset of the data from 1990 to 2011 to get most out of good records.

```{r}
tidy_storm_data <- tidy_storm_data[tidy_storm_data$YEAR >= 1990, ]
dim(tidy_storm_data)
```

Look for the blank values in the data:
```{r}
sum(is.na(tidy_storm_data$BGN_DATE))
sum(is.na(tidy_storm_data$EVTYPE))
sum(is.na(tidy_storm_data$INJURIES))
sum(is.na(tidy_storm_data$PROPDMG))
sum(is.na(tidy_storm_data$PROPDMGEXP))
sum(is.na(tidy_storm_data$CROPDMG))
sum(is.na(tidy_storm_data$CROPDMGEXP))
```


#### 2.5 Handling Exponential variables PROPDMGEXP and CROPDMGEXP

we can see that the PROPDMG and CROPDMG variables each one has an associated EXP variable. We need to handle this so that we can have numbers that we can use. The factors we need to handle with are:
```{r}
levels(tidy_storm_data$PROPDMGEXP)
levels(tidy_storm_data$CROPDMGEXP)
```

We can write a function which calculates the actual cost taking into account the correct exponent. The conversion is based on the information found at [How To Handle Exponent Value of PROPDMGEXP and CROPDMGEXP](https://rstudio-pubs-static.s3.amazonaws.com/58957_37b6723ee52b455990e149edde45e5b6.html).

```{r}
exp_calc <- function(x, exponent) {
      if (exponent %in% c("h", "H"))
            return(x * (10 ** 2))
      else if (exponent %in% c("k", "K"))
            return(x * (10 ** 3))
      else if (exponent %in% c("m", "M"))
            return(x * (10 ** 6))
      else if (exponent %in% c("b", "B"))
            return(x * (10 ** 9))
      else if (!is.na(as.numeric(exponent)))
            return(x * (10 ** as.numeric(exponent)))
      else if (exponent %in% c("+"))
            return(x * 10)
      else if (exponent %in% c("", "-", "?"))
            return(x) # Actually x * 10^0 = x * 1 = x
      else {
            stop("It is not a valid value")
      }
}
```

Now we can use the mapply function along with the exp_calc function to calculate the actual damage costs for crops and properties and store the results in two new variables CROPDMG_calc and PROPDMG_calc respectively:
```{r}
tidy_storm_data$PROPDMG_calc <- mapply(exp_calc, tidy_storm_data$PROPDMG, tidy_storm_data$PROPDMGEXP)
tidy_storm_data$CROPDMG_calc <- mapply(exp_calc, tidy_storm_data$CROPDMG, tidy_storm_data$CROPDMGEXP)
dim(tidy_storm_data)
head(tidy_storm_data)
```
Now the tidy Storm Data contains 751740 rows and 11 columns.

#### 2.6 Processed Data Summary

Now we can store the final required summarised data in a tibble.
```{r}
tidy_storm_data_summary <- tidy_storm_data %>%
      group_by(EVTYPE) %>%
      summarise( total_fatalities = sum(FATALITIES), total_injuries = sum(INJURIES), 
total_properties_damage = sum(PROPDMG_calc), total_crop_damage = sum(CROPDMG_calc) )

dim(tidy_storm_data_summary)
head(tidy_storm_data_summary)
```

## 3. Results

##### 3.1 Across the United States, which types of events (as indicated in the ```EVTYPE``` variable) are most harmful with respect to population health?

Fetch the top 5 causes of fatalities and store the results in a new tibble:
```{r}
fatalities <- tidy_storm_data_summary %>%
      arrange_(~ desc(total_fatalities)) %>%
      select(EVTYPE, total_fatalities) %>%
      slice(1:5)
```

Fetch the top 5 causes of injuries and store the results in a new tibble:
```{r}
injuries <- tidy_storm_data_summary %>%
      arrange_(~ desc(total_injuries)) %>%
      select(EVTYPE, total_injuries) %>%
      slice(1:5)
```

Plotting the data for visualization:
```{r}
# First plot
plot1 <- ggplot(data = injuries, aes(x = reorder(EVTYPE, total_injuries), y = total_injuries)) + geom_bar(fill = "blue", stat = "identity") + coord_flip() + ylab("Total number of injuries") + xlab("Event type") + ggtitle("Fatalities and injuries caused by weather in the US - Top 10") + theme(legend.position = "none")

# Second plot
plot2 <- ggplot(data = fatalities, aes(x = reorder(EVTYPE, total_fatalities), y = total_fatalities)) + geom_bar(fill = "red", stat = "identity") + coord_flip() + ylab("Total number of fatalities") + xlab("Event type") + theme(legend.position = "none")

# Combine both plots in a grid
grid.arrange(plot1, plot2, nrow = 2)
```

The above plot shows that the ```EXCESSIVE HEAT``` and ```TORNADO``` were caused most FATALITIES while ```TORNADO``` caused most INJURIES in the US from 1990 to 2011.

##### 3.2 Across the United States, which types of events have the greatest economic consequences?

Fetch the top 5 causes of crop damage and store the results in a new tibble:
```{r}
crop_damage <- tidy_storm_data_summary %>%
      arrange_(~ desc(total_crop_damage)) %>%
      select(EVTYPE, total_crop_damage) %>%
      slice(1:5)
```

Fetch the top 5 causes of properties damage and store the results in a new tibble:
```{r}
prop_damage <- tidy_storm_data_summary %>%
      arrange_(~ desc(total_properties_damage)) %>%
      select(EVTYPE, total_properties_damage) %>%
      slice(1:5)
```

Plot the data for visualization:
```{r}
# First plot
plot3 <- ggplot(data = crop_damage, aes(x = reorder(EVTYPE, total_crop_damage), y = total_crop_damage)) + geom_bar(fill = "blue", stat = "identity") + coord_flip() + ylab("Economic consequence of crop damage in USD") + xlab("Event type") + ggtitle("Crop and property damage caused by weather in the US - Top 10") + theme(legend.position = "none")

# Second plot
plot4 <- ggplot(data = prop_damage, aes(x = reorder(EVTYPE, total_properties_damage), y = total_properties_damage)) + geom_bar(fill = "red", stat = "identity") + coord_flip() + ylab("Economic consequence of property damage in USD") + xlab("Event type") + theme(legend.position = "none")

# Combine both plots in a grid
grid.arrange(plot3, plot4, nrow = 2)
```

The above plot shows that the ```DROUGHT``` and ```FLOOD``` were caused most CROP damage while ```FLASH FLOOD``` and ```THUNDERSTORM WINDS``` were caused most PROPERTY damage in the US from 1990 to 2011.

### Conclusion
From the analysis, we can say that ```EXCESSIVE HEAT``` and ```TORNADO``` were most harmful with respect to population health, whereas ```DROUGHT```, ```THUNDERSTORM WINDS```, and ```FLOOD``` have the greatest economic consequences.