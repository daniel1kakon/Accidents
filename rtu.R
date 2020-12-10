library(tidyverse)
library(ggplot2)
library(scales)
library(lubridate)
library(readr)
locale("he")

Sys.setlocale("LC_ALL", "Hebrew")
knitr::opts_chunk$set(echo = TRUE)
infodata <- read_csv(file = "C:/Users/Roei/Desktop/RR/accidents_fixed.csv", locale = locale(date_names = "he", encoding = "UTF-8"))



ABC<-aggregate(c(infodata["מס' תושבים"],infodata["סהכ תאונות דרכים"]), by=list(infodata$מחוז), FUN=sum)
ABC["תושבים פר תאונה"]<-round(ABC$מס..תושבים/ABC$סהכ.תאונות.דרכים, 2)
colnames(ABC) <- c("מחוז", "מס תושבים", "סהכ תאונות דרכים","תושבים פר תאונה")
clabel=paste0(unlist(ABC["תושבים פר תאונה"]) , sep="")
ggplot(ABC, aes(y = unlist(ABC$`תושבים פר תאונה`), x=unlist(ABC$מחוז))) +
  theme_bw(base_rect_size = 0) +
  geom_bar(position = 'dodge', stat="identity", width=0.8, fill="darkgray") +
  labs(y = "מספר תאונות",
       x = "מחוז")+
  geom_text(aes(label=clabel), position=position_dodge(width=0.1), vjust=-0.25)+
  labs(title = "כמות תאונות פר תושב על פי מחוז" , x = "" , y = "כמות תאונות פר תושב") +
  theme(axis.text.x = element_text(size=11, angle=0, vjust=1), plot.title = element_text(hjust = 0.5))+
  ylim(0,60)



###    טבלת עמודות של מוערבים בתאונה במצב קשה (פצועים קשה והרוגים מתוך סך נפגעים) בת.ד. בארץ 
BIOKOT <- aggregate(c(infodata["סהכ נפגעים"], infodata["הרוגים"]+infodata$`פצועים קשה`), by=list(infodata$מחוז), FUN=sum)
#Summing seekers by district.
colnames(BIOKOT) <- c("מחוז", "סך נפגעים", "BIK")

BIOKOT <- mutate(BIOKOT, BIOKOT["BIK"] <- (100*BIOKOT["BIK"] / sum(BIOKOT["סך נפגעים"]))) #Percentage col. of TOTAL job seekers.
#Sorting by percentage
BIOKOT <- BIOKOT[order(BIOKOT["BIK"], decreasing = TRUE),]
BIOKOT$מחוז <- factor(BIOKOT$מחוז, levels = BIOKOT$מחוז)
tlabel=paste0(round(BIOKOT$BIK,3),"%")
ggplot(BIOKOT, aes(y = unlist(BIOKOT["BIK"]), x = unlist(BIOKOT["מחוז"]))) +
  geom_bar(position = 'dodge', stat="identity", width=0.8, fill="navy") +
  geom_text(aes(label=tlabel),
            position=position_dodge(width=0.1), vjust=-0.25) +
  
  labs(title = "אחוז מוערבים בתאונה במצב קשה על פי מחוז" , x = "" , y = "נפגעים קשה והרוגים %") +
  theme(axis.text.x = element_text(size=11, angle=0, vjust=1), plot.title = element_text(hjust = 0.5))+
ylim(0,0.75)
###



infodata$ישוב<-as.factor(infodata$ישוב)
infodata["מחוז"]<-as.factor(infodata$מחוז)

