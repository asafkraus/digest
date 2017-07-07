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
#        , panel.grid.major = element_blank()
        #, panel.grid.minor = element_blank()
        , panel.background = element_blank() )

rxn$TIMESTAMP <- strptime(rxn$TIMESTAMP, "%m/%d/%y %H:%M")
trig$TIMESTAMP <- strptime(trig$TIMESTAMP, "%m/%d/%y %H:%M")
max(trig$TIMESTAMP)
max(rxn$TIMESTAMP)
#setDT(rxn)
#rxn[rxn$TIMESTAMP < "2016-10-31 00:47:00",]
rxn[rxn$TIMESTAMP < max(trig$TIMESTAMP),]



rxn$TIMESTAMP
rxn$time1 <- mdy_hm(rxn$TIMESTAMP)
rxn$hour <- hour(rxn$time1)
View(rxn)
class(rxn$time1)
class(rxn$time2)
plot(x = rxn$time1, y = rxn$INTENSITY)
lines(x = rxn$time1, y = rxn$INTENSITY)


rxn$timez <- strptime(rxn$TIMESTAMP, "%m/%d/%y %H:%M")
trig$timez <- strptime(trig$TIMESTAMP, "%m/%d/%y %H:%M")
trig$hour <- as.POSIXct(trunc(trig$timez, "hour"))
rxn$hour <- as.POSIXct(trunc(rxn$timez, "hour"))

setDT(trig)
trig <- trig[, .(count=.N), by=.(CATEGORY,hour)]

setDT(rxn)
rxn <- rxn[, .(INTENSITY=sum(INTENSITY)), by=.(hour)]




setDT(trig)
trig <- trig[, .(count=.N), by=.(CATEGORY,hour)]

setDT(rxn)
rxn <- rxn[, .(INTENSITY=sum(INTENSITY)), by=.(hour)]

a <- ggplot(rxn, aes(x=hour, y=INTENSITY)) +
  geom_line() +
  ggtitle("Reaction Intensity")
b <- ggplot(trig, aes(x=hour, y=count)) +
  geom_bar(position="dodge",stat="identity", aes(x=hour,fill=CATEGORY)) +
  theme(legend.position="bottom") +
  ggtitle("Triggers")

gridExtra::grid.arrange(a,b)

trig$type <- "trig"
rxn$type <- "rxn"
rxn$CATEGORY <- "-"
trig <- trig %>% select(hour,INTENSITY,CATEGORY,type,cnt)
rxn <- rxn %>% select(hour,INTENSITY,CATEGORY,type,cnt)
data <- rbind(rxn,trig)


names(rxn)

ggplot(data) +
  geom_smooth(aes(x=timez, y=INTENSITY)) +
  facet_grid(~type)
  geom_line(trig, aes(x=timez, y=INTENSITY))
