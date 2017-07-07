library(dplyr)
library(ggplot2)
library(lubridate)
library(data.table)
library(gridExtra)
library(plyr)
library(dynlm)
setwd("/Users/kraus/Documents/health/")

## Read data
rxn_raw  <- read.csv("./rxn.csv")
trig_raw <- read.csv("./trigger.csv")

# Generic Linear Model ----------------------------------------------------

## Read data
rxn <- rxn_raw
trig <- trig_raw

## Transform timestamps to hour
rxn$timez <- strptime(rxn$TIMESTAMP, "%m/%d/%y %H:%M")
trig$timez <- strptime(trig$TIMESTAMP, "%m/%d/%y %H:%M")
trig$hour <- as.POSIXct(trunc(trig$timez, "hour"))
rxn$hour <- as.POSIXct(trunc(rxn$timez, "hour"))
rxn$TIMESTAMP <- mdy_hm(rxn$TIMESTAMP)
trig$TIMESTAMP <- mdy_hm(trig$TIMESTAMP)
rxn$time_epoch <- as.integer(rxn$TIMESTAMP)
trig$time_epoch <- as.integer(trig$TIMESTAMP)
rxn$ep_hour <- round((rxn$time_epoch - 1477852560)/3600)
trig$ep_hour <- round((trig$time_epoch - 1477852560)/3600)
trig$CHILD <- tolower(trig$CHILD)
trig$CHILD <- gsub(" ","_",trig$CHILD)

## Transform and merge data
setDT(trig)
setDT(rxn)
trig <- dcast(trig, ep_hour~CHILD)
rxn <- rxn[, .(rxi=sum(INTENSITY)), by=.(ep_hour)]
data <- merge(x = rxn, y = trig, by = "ep_hour", all = TRUE)

## Filter
data[2:6,2] <- c(4,10,20,90,150) # fake rxn data for 6 hour window
setDT(data)
data <- data[ep_hour < 7]
data <- data %>% select(-ep_hour)

## Create GLM Model
str(data)
model <- glm(formula = rxi~running+fruit+magnesium+corn_masa_flour, data = data)
#model <- lm(rxi~., data) # predict reaction from all child triggers
summary(model)


# Time Plot: --------------------------------------------------------------

## Read data
rxn <- rxn_raw
trig <- trig_raw

rxn$TIMESTAMP <- mdy_hm(rxn$TIMESTAMP)
rxn$time_epoch <- as.integer(rxn$TIMESTAMP)
trig$TIMESTAMP <- mdy_hm(trig$TIMESTAMP)
trig$time_epoch <- as.integer(trig$TIMESTAMP)
trig$name <- trig$CATEGORY
rxn$name <- rxn$REACTION

setDT(trig)
trig <- trig[, .(value=.N), by=.(name,time_epoch)]

setDT(rxn)
#rxn <- rxn[, .(value=sum(INTENSITY)), by=.(name,time_epoch)]
rxn <- rxn[, .(value=sum(INTENSITY)), by=.(time_epoch)]
rxn[, name := "symptom"]
rxn <- rxn[, .(name,time_epoch,value)]

x <- rbind(rxn,trig)

ggplot(x, aes(x=time_epoch, y=value, color = name)) +
  geom_point(size=3) +
  geom_line(size=1.5) +
  ggtitle("Reaction Intensity") +
    theme(plot.title = element_text(size = 20, face = 'bold', hjust = .5)
        , panel.background = element_blank() )

