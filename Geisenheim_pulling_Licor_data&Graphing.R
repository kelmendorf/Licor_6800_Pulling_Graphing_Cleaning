library(tidyverse)
library(stringr)
library(stringi)
library(dplyr)
library(readxl)
library(data.table)
library(ggplot2)
library(writexl)
library(utils)
library(writexl)
library(base)

#choose.dir()
#Read in August Data - - Choose August Directory. Need to be read in differently because data start at different rows
licor6800.point_measurement.compiler<-function(){
  filenames <- list.files("C:/Users/kehel/Desktop/Thesis/Geisenheim_ACi_Licor_August", pattern="*.csv", full.names=TRUE)
  for (filename in filenames) {
    meas<-read.csv(filename, skip = 15, fileEncoding='latin1', check.names=F, header = TRUE)[-1:-2,] 
    list<-cbind(filename, meas)
    list<-list%>%select("filename","date","A","Ca", "Ci","E", "CO2_s", "CO2_r", "Qin", "gsw","gtc", "Pa","Qin","RHcham","Tleaf", "PhiPS2", "Fo", "Fm", "Fv/Fm", "Adark", "Fs", "Fm")%>%
      mutate("date" = str_split_fixed(list$date, "[:blank:](?=\\d)", 2)[,1])%>%
      mutate("date" = str_sub(date,start = 5))%>%
      mutate("Time" = str_split_fixed(list$date, "[:blank:](?=\\d)", 2)[,2])%>%
      rename("Date" = date )
  list$Date<- sub("(?<=^..)", "\\.", list$Date, perl=TRUE)
  completeA6800 <- if(exists('completeA6800') == TRUE){rbind(completeA6800, list)} else{list}
  }
  return(completeA6800)
}
View(completeA6800)

#choose.dir()
#Read in June Data - Choose June Directory. Need to be read in differently because data start at different rows
licor6800.point_measurement.compiler<-function(){
  filenames <- list.files("C:/Users/kehel/Desktop/Thesis/Geisenheim_ACi_Licor_June", pattern="*.csv", full.names=TRUE)
  for (filename in filenames) {
    meas<-read.csv(filename, skip = 13, fileEncoding='latin1', check.names=F, header = TRUE)[-1:-2,] 
    list<-cbind(filename, meas)
    list<-list%>%select("filename","date","A","Ca", "Ci","E", "CO2_s", "CO2_r", "Qin", "gsw","gtc", "Pa","Qin","RHcham","Tleaf", "PhiPS2", "Fo", "Fm", "Fv/Fm", "Adark", "Fs", "Fm")%>%
      mutate("date" = str_split_fixed(list$date, "[:blank:](?=\\d)", 2)[,1])%>%
      mutate("date" = str_sub(date,start = 5))%>%
      mutate("Time" = str_split_fixed(list$date, "[:blank:](?=\\d)", 2)[,2])%>%
      rename("Date" = date )
    list$Date<- sub("(?<=^..)", "\\.", list$Date, perl=TRUE)
    completeJ6800 <- if(exists('completeJ6800') == TRUE){rbind(completeJ6800, list)} else{list}
  }
  return(completeJ6800)
}
#View(completeJ6800)

# Bind June and August #
complete6800<- rbind(completeA6800, completeJ6800)
View(complete6800)

write_xlsx(complete6800, "Complete6800.xlsx")

# Get list of file names #
file <- cbind(filenames)
#View(file)
file3 <- str_split_fixed(file,"/",10)
#View(file2)
write_xlsx(x=as.data.frame(file3), path = "file3.xlsx")

#extract file name data as columns for graphing
complete6800[c('path', 'ACi', 'Licor', 'Date', 'RingID','RowNumber', 'VineNumber', 'Variety')] <- str_split_fixed(complete6800$filename, '_', 8)
complete6800[c('Month', 'Date')] <- str_split_fixed(complete6800$Date, '/', 2)
complete6800$Variety <- gsub(".csv", "", complete6800$Variety)
DF <- complete6800[grep("K|D|L|E|B|G", complete6800$RingID),]
DF$CO2Level <- ifelse(grepl("E|B|G", DF$RingID), "elevated", "ambient")

#Discard negatives
dd_wreps <- do.call(bind_rows, DF)
Nonegs<-dd_wreps%>%filter(gsw >0 & Ci>0)%>%
  arrange(Date,Time)

#Make columns numeric values
Nonegs$gsw<-as.numeric(Nonegs$gsw)
Nonegs$A<-as.numeric(Nonegs$A)
Nonegs$Ci<-as.numeric(Nonegs$Ci)

#Establish and Remove Outliers
CiQ1 <- quantile(Nonegs$Ci, probs=.25, na.rm = TRUE) #"type 7" default
CiQ3 <- quantile(Nonegs$Ci, probs=.75, na.rm = TRUE)
AQ1 <- quantile(Nonegs$A, probs=.25, na.rm = TRUE)
AQ3 <- quantile(Nonegs$A, probs=.75, na.rm = TRUE)
IQRCi <- IQR(Nonegs$Ci, na.rm=TRUE, type = 7)
IQRA <- IQR(Nonegs$A, na.rm=TRUE)

no_outliers <- subset(Nonegs, Nonegs$Ci > (CiQ1 - 1.5*IQRCi) 
                      & Nonegs$Ci< (CiQ3 + 1.5*IQRCi) 
                      & Nonegs$A > (AQ1 - 1.5*IQRA) 
                      & Nonegs$A < (AQ3 + 1.5*IQRA))

#Sorting Isotope data 
isotope <- subset(no_outliers, no_outliers$CO2_s>=399 & no_outliers$CO2_s<=401)
isotope <- distinct(isotope, Ca, .keep_all=TRUE)
#View(isotope)
#write_xlsx(isotope, 'Isotope_data_for_gm_calculator.xlsx')

#Subset Data for graphing 
R <- no_outliers[which(no_outliers$Variety == "R"),]
CS <- no_outliers[which(no_outliers$Variety == "CS"),]
June <- no_outliers[which(no_outliers$Month == "June"),]
August <- no_outliers[which(no_outliers$Month == "August"),]

no_outliers$MonthVariety <- paste(no_outliers$Month, no_outliers$Variety, sep="_")
view(no_outliers)

June$JVarietyCO2 <- paste(June$Variety, June$CO2Level, sep="_")
August$AVarietyCO2 <- paste(August$Variety, August$CO2Level, sep="_")

###### PLOT ######

ggplot(data=June, aes(x=Ci, y=A, col= as.factor(JVarietyCO2)))+
  geom_point()+
  geom_smooth()+
  labs(title = "FACE June Ambient v.s. Elevated by Variety")

ggplot(data=August, aes(x=Ci, y=A, col= as.factor(AVarietyCO2)))+
  geom_point()+
  geom_smooth()+
  labs(title = "FACE August Ambient v.s. Elevated by Variety")

ggplot(data=no_outliers, aes(x=Ci, y=A, col= as.factor(MonthVariety)))+
  geom_point()+
  geom_smooth()+
  labs(title = "FACE Month_Variety")


ggplot(data=June, aes(x=Ci, y=A, col= as.factor(CO2Level)))+
         geom_point()+
         geom_smooth()+
        labs(title = "FACE June Data")

ggplot(data=August, aes(x=Ci, y=A, col= as.factor(CO2Level)))+
  geom_point()+
  geom_smooth()+
  labs(title = "FACE August Data")

ggplot(data=R, aes(x=Ci, y=A, col= as.factor(CO2Level)))+
  geom_point()+
  geom_smooth()+
  labs(title = "FACE Riesling Data")

ggplot(data=CS, aes(x=Ci, y=A, col= as.factor(CO2Level)))+
  geom_point()+
  geom_smooth()+
  labs(title = "FACE Cabernet Data")




###Save as csv and manually remove mistakes in scanning 
#Followed this protocol: remove obvious scanning mistakes by hand if scan 4+ was more similar to the following plant's gsw values, or if scan 1 was more similar to the previous.

#setwd(choose.dir())
write.csv(dd_wreps_noneg%>%arrange(Date, Time),"gsw_6800_pre-clean.csv")

#Read in licor 6800 point measurements with obvious outliers removed 
pointsclean<-read.csv("gsw_6800_cleaned.csv")
#View(pointsclean)

#look at histograms of gsw for each treatment of each genotype on each day
pointsclean<-add_column(pointsclean, Geno_Date_Treat = paste(pointsclean$Genotype,pointsclean$Treatment,pointsclean$Date,sep = "_"), .after = "Date")
pointsclean$Geno_Date_Treat
codedates<-unique(pointsclean$Geno_Date_Treat)

for (i in codedates) {
  a<-pointsclean%>%filter(Geno_Date_Treat == i)%>%
    ggplot()+
    geom_histogram(aes(x = gsw_6800,fill = Treatment), bins = 3)+
    ggtitle(i)
  print(a)
}


#View(de)

