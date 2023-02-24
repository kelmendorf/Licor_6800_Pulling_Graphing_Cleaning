library(tidyverse)
library(stringr)
library(stringi)
library(dplyr)
library(readxl)
library(data.table)
library(ggplot2)
library(writexl)
library(utils)
library(base)


###############################################################
    # Bring in data from Licor 6800 csv files #
###############################################################

#choose.dir()
    #######Read in August Data - - Choose August Directory. Need to be read in differently because data start at different rows
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
#View(completeA6800)

#choose.dir()
    #######Read in June Data - Choose June Directory. Need to be read in differently because data start at different rows
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

  ###### Bind June and August #
complete68001<- rbind(completeA6800, completeJ6800)
complete6800 <- distinct(complete68001)

#View(complete6800)
#write_xlsx(complete6800, "Complete6800.xlsx")

###############################################################
    # Get list of file names #
###############################################################

#file <- cbind(filenames)
#View(file)
#file3 <- str_split_fixed(file,"/",10)
#View(file2)
#write_xlsx(x=as.data.frame(file3), path = "file3.xlsx")

###############################################################
    # Edit and Add Column Names #
###############################################################
    #######extract file name data as columns for graphing
complete6800[c('path', 'ACi', 'Licor', 'Date', 'RingID','RowNumber', 'VineNumber', 'Variety')] <- str_split_fixed(complete6800$filename, '_', 8)
complete6800[c('Month', 'Date')] <- str_split_fixed(complete6800$Date, '/', 2)
complete6800$Variety <- gsub(".csv", "", complete6800$Variety)
DF <- complete6800[grep("K|D|L|E|B|G", complete6800$RingID),]
DF$CO2Level <- ifelse(grepl("E|B|G", DF$RingID), "elevated", "ambient")

###############################################################
    # Discard Negative gsw and Ci #
###############################################################
dd_wreps <- do.call(bind_rows, DF)
Nonegs<-dd_wreps%>%filter(gsw >0 & Ci>0)%>%
  arrange(Date,Time)

view(no_outliers)
###############################################################
    # Make Columns Numeric Values #
###############################################################
Nonegs$gsw<-as.numeric(Nonegs$gsw)
Nonegs$A<-as.numeric(Nonegs$A)
Nonegs$Ci<-as.numeric(Nonegs$Ci)
Nonegs$Ca<-as.numeric(Nonegs$Ca)

###############################################################
    # Establish and Remove Outliers Using Quartiles #
###############################################################
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

no_outliers$MonthVariety <- paste(no_outliers$Month, no_outliers$Variety, sep="_")
no_outliers$LeafID <- paste(no_outliers$RingID, no_outliers$RowNumber, no_outliers$VineNumber, no_outliers$MonthVariety, sep="_")

###############################################################
    # Sub-setting Data For Graphing #
###############################################################
R <- no_outliers[which(no_outliers$Variety == "R"),]
CS <- no_outliers[which(no_outliers$Variety == "CS"),]
June <- no_outliers[which(no_outliers$Month == "June"),]
August <- no_outliers[which(no_outliers$Month == "August"),]
#K7133CSJune <- no_outliers[which(no_outliers$LeafID == "K_71_33_June_CS"),]
#View(K7133CSJune)

June$JVarietyCO2 <- paste(June$Variety, June$CO2Level, sep="_")
August$AVarietyCO2 <- paste(August$Variety, August$CO2Level, sep="_")

###############################################################
    # Sorting CO2_s Data for GM Calculator #
###############################################################
#isotope <- subset(June, June$CO2_s>=398 & June$CO2_s<=402)
#isotope <- distinct(isotope, Ca, .keep_all=TRUE)
#View(isotope)
#write_xlsx(isotope, 'Isotope_data_for_gm_calculator2.xlsx')


      ################################################
###############################################################
                    # Plotting Data #
###############################################################
      ################################################

###############################################################
# Individual Plots Using 'for' loop. See Fitting ACi Curves: plantecophys #
###############################################################

#individal_curves<-function(){
#  filenames <- list.files("C:/Users/kehel/Desktop/Thesis/Geisenheim_ACi_Licor_June", pattern="*.csv", full.names=TRUE)
#  for (filename in filenames) {
#    meas<-read.csv(filename, skip = 13, fileEncoding='latin1', check.names=F, header = TRUE)[-1:-2,] 
#    list<-cbind(filename, meas)
#    list<-list%>%select("filename","date","A","Ca","Ci","E", "CO2_s", "CO2_r", "Qin", "gsw","gtc", "Pa","Qin","RHcham","Tleaf", "PhiPS2", "Fo", "Fm", "Fv/Fm", "Adark", "Fs", "Fm")
#    ggplot(data = list, aes(x=Ci, y=A))+ geom_smooth()+ labs(title="filename")
#    individual_curves <- if(exists('individual_curves') == TRUE){rbind(individual_curves, list)} else{list}
#  }
#  return(individual_curves)
#}
#
#for (LeafID in no_outliers) {
#    ggplot(data=no_outliers, aes(x=Ci, y=A, col= as.factor(Variety)))+
#    geom_point()+
#    geom_smooth()+
#    labs(title = "FACE June Ambient v.s. Elevated by Variety")
#}

###############################################################
    # FACET PLOT #
###############################################################
individal_curves_facet <- ggplot(no_outliers, 
     aes(x = Ci, y = A, color = LeafID, group = LeafID)) +
     facet_wrap(~ LeafID) +
     geom_point() + 
     geom_line() +
     theme_bw(base_size = 14)
individal_curves_facet

###############################################################
    # Average ggplots #
###############################################################
ggplot(data=June, aes(x=Ci, y=A, col= as.factor(JVarietyCO2)))+
  geom_point()+
  geom_smooth()+
  labs(title = "FACE June Ambient v.s. Elevated by Variety")

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


      ################################################
###############################################################
            # Fitting ACi Curves: plantecophys #
###############################################################
      ################################################

  #devtools::install_github("poales/msuRACiFit")
  #library(htmltools)
  #msuRACiFit::genApp()
  #install.packages("plantecophys")

library(plantecophys)
library(devtools)

###############################################################
    # Make A, Tleaf, and Qin numeric #
###############################################################
June$A <- as.numeric(as.character(June$A))
June$Tleaf <- as.numeric(as.character(June$Tleaf))
June$Qin <- as.numeric(as.character(June$Qin))
summary(June)

no_outliers$A <- as.numeric(as.character(no_outliers$A))
no_outliers$Tleaf <- as.numeric(as.character(no_outliers$Tleaf))
no_outliers$Qin <- as.numeric(as.character(no_outliers$Qin))

###############################################################
    # Make individual tibbles that I can extract using fitac #
###############################################################
tibblegroup <- split(no_outliers, no_outliers$LeafID)
names(tibblegroup) #use names to find names of tibbles
summary(tibblegroup)


#df <- names(tibblegroup) #create a df & excel to extract LeafIDs
#write_xlsx(x=as.data.frame(df), path = "file16.xlsx")

fitB_38_34_August_CS <- fitaci(tibblegroup$B_38_34_August_CS, varnames=list(ALEAF="A", Ci="Ci", Tleaf="Tleaf", PPFD="Qin"))
fitB_38_34_August_CS
#plot(fitB_38_34_August_CS, addlegend = TRUE)
#fitB_38_34_August_CS$Photosyn(600)
fitB_38_34_August_CS$Ci(0) #compensation point
coef(fitB_38_34_August_CS)
#fitted(fitB_38_34_August_CS)

L_89_2_June_CS<- fitaci(tibblegroup$L_89_2_June_CS, varnames=list(ALEAF="A", Ci="Ci", Tleaf="Tleaf", PPFD="Qin"))
L_89_2_June_CS
L_89_2_June_CS$Ci(0) #compensation point

###############################################################
# Find Amax #
###############################################################

Amax <- 
no_outliers %>%
  group_by(LeafID) %>%
  summarise(max(A))

View(Amax)
write_xlsx(Amax, "Amax.xlsx")

###############################################################
# Fit Many Curves at Once with fitacis (does not work) #
###############################################################

fit1 <- fitacis(no_outliers, "LeafID", varnames=list(ALEAF="A", Ci="Ci", Tleaf="Tleaf", PPFD="Qin"))

fit1 <- fitacis(no_outliers, "LeafID")

fit <- fitacis(df, "Group")
fit

df <- data.frame('Group' = no_outliers$LeafID, 
                 'Ci' = no_outliers$Ci, 
                 'ALEAF' = no_outliers$A, 
                 'PPFD' = no_outliers$Qin, 
                 'Tleaf' = no_outliers$Tleaf)
view(df)



eight <- df[which(df$Group == "K_74_31_August_R"),]
fit3 <- fitacis(eight, "Group")
fit3
view(eight)

no_outliers[1:10, ]
view(no_outliers)

testicularcancer<-function(){
  filenames <- list.files("C:/Users/kehel/Desktop/Thesis/Geisenheim_ACi_Licor_June", pattern="*.csv", full.names=TRUE)
  for (filename in filenames) {
    meas<-read.csv(filename, skip = 13, fileEncoding='latin1', check.names=F, header = TRUE)[-1:-2,] 
    list<-cbind(filename, meas)
    list<-list%>%select(ALEAF="A", Ci="Ci", Tleaf="Tleaf", PPFD="Qin")%>%
    fitaci(filenames, "LeafID")
    testicular <- if(exists('testicular') == TRUE){rbind(testicular, list)} else{list}
  }
  return(testicular)
}
view(testicular)

is.atomic(complete6800$Qin)

list$Qin <- is.numeric(list$Qin)
list$PPFD <- is.numeric(list$PPFD)

fit1

