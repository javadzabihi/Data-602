
library(treemap)
library(dplyr)
library(tidyverse)
library(lubridate)
library(caTools)
library(ggplot2)
library(ggthemes)
library(reshape2)
library(data.table)
library(tidyr)
library(corrgram)       
library(corrplot)
library(formattable)
library(cowplot)
library(ggpubr)
library(gdata)

# Volume of request for each community?
# type, volume, time taken to solve a request
#Most important requests to work on:
#street lights, hgw, sanitation, vacant structure, maintenance structure, cleaning, dirty street, dirty alley
# what is the reason? a specific request takes more time to be solved in one neighborhood in contrast to the other neighborhood
# the reasons may be : geography, politics and things like that may play a role in this trend
# Higher number of requests filed in a rich community?
# Normalizing with population 
# status date and create date


# Data_Census
data_census_2015 <- read.csv("Census_Demographics__2015__data.csv")

str(data_census_2015)
colnames(data_census_2015)[3] <- "Community"
unique(data_census_2015$Community)
any(is.na(data_census_2015))
mydata_census_2015 <- tbl_df(data_census_2015)
head(mydata_census_2015)
unique_Community <- data_census_2015 %>% select(Community) %>% arrange(Community) %>% unique()

# Removing communities which did not have related neighborhoods in 311 dataset

data_census_2015 <- data_census_2015[ !(data_census_2015$Community %in% c('Greenmount East','Hamilton','Northwood','South Baltimore',
                                                                          'Southern Park Heights','Southwest Baltimore','Southeastern')), ]



str(data_census_2015)
table(data_census_2015$Community)

# Defining levels and labels for the community column

data_census_2015$Community = factor(data_census_2015$Community,
                                    levels = c('Allendale/Irvington/S. Hilton', 'Beechfield/Ten Hills/West Hills', 
                                               'Belair-Edison', 'Brooklyn/Curtis Bay/Hawkins Point', 'Canton', 
                                               'Cedonia/Frankford', 'Cherry Hill', 'Chinquapin Park/Belvedere', 
                                               'Claremont/Armistead', 'Clifton-Berea', 'Cross-Country/Cheswolde', 
                                               'Dickeyville/Franklintown', 'Dorchester/Ashburton', 'Downtown/Seton Hill', 
                                               'Edmondson Village', 'Fells Point', 'Forest Park/Walbrook', 'Glen-Fallstaff', 
                                               'Greater Charles Village/Barclay', 'Greater Govans', 'Greater Mondawmin', 
                                               'Greater Roland Park/Poplar Hill', 'Greater Rosemont', 'Harbor East/Little Italy',
                                               'Harford/Echodale', 'Highlandtown', 'Howard Park/West Arlington', 
                                               'Inner Harbor/Federal Hill', 'Lauraville', 'Loch Raven', 'Madison/East End', 
                                               'Medfield/Hampden/Woodberry/Remington', 'Midtown', 'Midway/Coldstream', 
                                               'Morrell Park/Violetville', 'Mount Washington/Coldspring', 
                                               'North Baltimore/Guilford/Homeland', 'Oldtown/Middle East', 'Orangeville/East Highlandtown',
                                               'Patterson Park North & East', 'Penn North/Reservoir Hill', 'Pimlico/Arlington/Hilltop',
                                               'Poppleton/The Terraces/Hollins Market', 'Sandtown-Winchester/Harlem Park', 'The Waverlies',
                                               'Upton/Druid Heights',  'Washington Village/Pigtown', 'Westport/Mount Winans/Lakeland'),
                                    labels = c(1:48))





# Data_311

data_311 <- read.csv("311_Customer_Service_Requests.csv")

str(data_311)
summary(data_311)

# The command below gives us a vector of unique neighborhoods in alphabetical order
unique_Neighborhood <- data_311 %>% select(Neighborhood) %>% arrange(Neighborhood) %>% unique()

unique(data_311$SRStatus)
any(is.na(data_311))
table(data_311$Neighborhood)
tail(data_311)
head(data_311)

#This line gives the length of unique values for every column in our dataset
sapply(data_311, function(x) length(unique(x)) )

#The following output is much more informative and compact than what we would get if we
# printed the original data frame (data_311) to the console.

mydata_311 <- tbl_df(data_311)
head(mydata_311)

# Removing "SRRecordID", "ServiceRequestNum", and "GeoLocation" columns from the dataset

data_311 <- data_311[,3:15]
head(data_311)


#Providing a subset of 311 dataset which has the specific neighborhoods related to
#the community areas in census dataset

new_data_311 <- data_311 %>% filter(Neighborhood =='ALLENDALE'| Neighborhood =='IRVINGTON'|Neighborhood =='BEECHFIELD'|Neighborhood =='TEN HILLS'|Neighborhood =='WEST HILLS'
                                      |Neighborhood =='BELAIR-EDISON'| Neighborhood =='BROOKLYN'| Neighborhood=='CURTIS BAY'|Neighborhood =='HAWKINS POINT' 
                                      |Neighborhood =='CANTON'| Neighborhood =='CEDONIA'|Neighborhood =='FRANKFORD'|Neighborhood == 'CHERRY HILL'|Neighborhood == 'CHINQUAPIN PARK-BELVEDERE'
                                      |Neighborhood =='CLAREMONT-FREEDOM' | Neighborhood =='ARMISTEAD GARDENS'|Neighborhood == 'CLIFTON PARK' |Neighborhood == 'BEREA'
                                      |Neighborhood =='CROSS COUNTRY'|Neighborhood =='CHESWOLDE'|Neighborhood == 'DICKEYVILLE'|Neighborhood =='FRANKLINTOWN'|Neighborhood == 'DORCHESTER'
                                      |Neighborhood =='ASHBURTON'|Neighborhood == 'DOWNTOWN'|Neighborhood =='SETON HILL'|Neighborhood == 'EDMONDSON VILLAGE'|Neighborhood =='FELLS POINT'
                                      |Neighborhood =='FOREST PARK'|Neighborhood =='WALBROOK'|Neighborhood == 'GLEN' |Neighborhood =='FALLSTAFF'|Neighborhood == 'CHARLES VILLAGE'|Neighborhood =='BARCLAY'
                                      |Neighborhood =='MID-GOVANS'|Neighborhood =='ROSEBANK'|Neighborhood == 'WINSTON-GOVANS'|Neighborhood =='WOODBOURNE-MCCABE'|Neighborhood =='RICHNOR SPRINGS'
                                      |Neighborhood =='MONDAWMIN'|Neighborhood == 'GREATER ROLAND PARK'|Neighborhood == 'NORTH ROLAND PARK/POPLAR HILL'|Neighborhood == 'ROSEMONT'
                                      |Neighborhood =='LITTLE ITALY'|Neighborhood == 'HARFORD-ECHODALE'|Neighborhood =='PERRING PARKWAY'|Neighborhood == 'HIGHLANDTOWN'|Neighborhood == 'HOWARD PARK'
                                      |Neighborhood =='WEST ARLINGTON'|Neighborhood == 'INNER HARBOR'|Neighborhood =='FEDERAL HILL'|Neighborhood =='LAURAVILLE'|Neighborhood =='LOCH RAVEN'|Neighborhood =='MADISON-EASTEND'
                                      |Neighborhood =='HAMPDEN'|Neighborhood =='WOODBERRY'|Neighborhood =='REMINGTON'|Neighborhood =='MIDTOWN-EDMONDSON'|Neighborhood =='COLDSTREAM HOMESTEAD MONTEBELLO'
                                      |Neighborhood =='MORRELL PARK'|Neighborhood =='VIOLETVILLE'|Neighborhood =='MOUNT WASHINGTON'|Neighborhood =='COLDSPRING'|Neighborhood =='GUILFORD'|Neighborhood =='HOMELAND'|Neighborhood == 'OLDTOWN'
                                      |Neighborhood =='MIDDLE EAST'|Neighborhood == 'ORANGEVILLE'|Neighborhood == 'PATTERSON PARK'|Neighborhood == 'PENN NORTH'
                                      |Neighborhood =='RESERVOIR HILL'|Neighborhood == 'PIMLICO GOOD NEIGHBORS'|Neighborhood =='ARLINGTON'|Neighborhood == 'POPPLETON'|Neighborhood =='HOLLINS MARKET'
                                      |Neighborhood =='SANDTOWN-WINCHESTER'|Neighborhood =='HARLEM PARK'|Neighborhood == 'WAVERLY'|Neighborhood == 'UPTON'|Neighborhood =='DRUID HEIGHTS' 
                                      |Neighborhood =='WASHINGTON VILLAGE'|Neighborhood =='WESTPORT'|Neighborhood =='MOUNT WINANS'|Neighborhood =='LAKELAND')


#Defining levels and labels for these selected neighborhoods    
# This will help us to work easier 
#when writing the code to add household median income for each neighborhood
new_data_311$Neighborhood <- factor(new_data_311$Neighborhood,
                                      levels = c('ALLENDALE','IRVINGTON','BEECHFIELD','TEN HILLS','WEST HILLS',
                                                 'BELAIR-EDISON', 'BROOKLYN','CURTIS BAY','HAWKINS POINT', 
                                                 'CANTON', 'CEDONIA','FRANKFORD', 'CHERRY HILL', 'CHINQUAPIN PARK-BELVEDERE',
                                                 'CLAREMONT-FREEDOM' , 'ARMISTEAD GARDENS', 'CLIFTON PARK' , 'BEREA', 
                                                 'CROSS COUNTRY','CHESWOLDE', 'DICKEYVILLE','FRANKLINTOWN', 'DORCHESTER',
                                                 'ASHBURTON', 'DOWNTOWN','SETON HILL', 'EDMONDSON VILLAGE', 'FELLS POINT',
                                                 'FOREST PARK','WALBROOK', 'GLEN' , 'FALLSTAFF', 'CHARLES VILLAGE','BARCLAY',
                                                 'MID-GOVANS', 'ROSEBANK', 'WINSTON-GOVANS', 'WOODBOURNE-MCCABE', 'RICHNOR SPRINGS',
                                                 'MONDAWMIN', 'GREATER ROLAND PARK', 'NORTH ROLAND PARK/POPLAR HILL', 'ROSEMONT',
                                                 'LITTLE ITALY', 'HARFORD-ECHODALE','PERRING PARKWAY', 'HIGHLANDTOWN', 'HOWARD PARK',
                                                 'WEST ARLINGTON', 'INNER HARBOR','FEDERAL HILL','LAURAVILLE','LOCH RAVEN','MADISON-EASTEND',
                                                 'HAMPDEN','WOODBERRY','REMINGTON','MIDTOWN-EDMONDSON','COLDSTREAM HOMESTEAD MONTEBELLO',
                                                 'MORRELL PARK','VIOLETVILLE','MOUNT WASHINGTON','COLDSPRING','GUILFORD','HOMELAND', 'OLDTOWN',
                                                 'MIDDLE EAST', 'ORANGEVILLE', 'PATTERSON PARK', 'PENN NORTH',
                                                 'RESERVOIR HILL', 'PIMLICO GOOD NEIGHBORS','ARLINGTON', 'POPPLETON','HOLLINS MARKET',
                                                 'SANDTOWN-WINCHESTER','HARLEM PARK', 'WAVERLY', 'UPTON','DRUID HEIGHTS', 
                                                 'WASHINGTON VILLAGE','WESTPORT','MOUNT WINANS','LAKELAND'),
                                      labels = c(1:84))

table(new_data_311$Neighborhood)  
str(new_data_311)

#Creating "Median_Household_Income" column in 311 dataset with an empty vector
 
new_data_311$Median_Household_Income = NA

#Assigning "Median_Household_Income" values to this created column 
#according to the relation between community in census dataset and related neighborhoods in 311 dataset

new_data_311$Median_Household_Income[which(new_data_311$Neighborhood==1 | new_data_311$Neighborhood==2)] <- data_census_2015$Median.Household.Income[which(data_census_2015$Community==1)]
new_data_311$Median_Household_Income[which(new_data_311$Neighborhood==3 | new_data_311$Neighborhood==4 | new_data_311$Neighborhood==5)] <- data_census_2015$Median.Household.Income[which(data_census_2015$Community==2)]
new_data_311$Median_Household_Income[which(new_data_311$Neighborhood==6) ] <- data_census_2015$Median.Household.Income[which(data_census_2015$Community==3)]
new_data_311$Median_Household_Income[which(new_data_311$Neighborhood==7 | new_data_311$Neighborhood==8 | new_data_311$Neighborhood==9)] <- data_census_2015$Median.Household.Income[which(data_census_2015$Community==4)]
new_data_311$Median_Household_Income[which(new_data_311$Neighborhood==10) ] <- data_census_2015$Median.Household.Income[which(data_census_2015$Community==5)]
new_data_311$Median_Household_Income[which(new_data_311$Neighborhood==11 | new_data_311$Neighborhood==12)] <- data_census_2015$Median.Household.Income[which(data_census_2015$Community==6)]
new_data_311$Median_Household_Income[which(new_data_311$Neighborhood==13) ] <- data_census_2015$Median.Household.Income[which(data_census_2015$Community==7)]
new_data_311$Median_Household_Income[which(new_data_311$Neighborhood==14) ] <- data_census_2015$Median.Household.Income[which(data_census_2015$Community==8)]
new_data_311$Median_Household_Income[which(new_data_311$Neighborhood==15 | new_data_311$Neighborhood==16)] <- data_census_2015$Median.Household.Income[which(data_census_2015$Community==9)]
new_data_311$Median_Household_Income[which(new_data_311$Neighborhood==17 | new_data_311$Neighborhood==18)] <- data_census_2015$Median.Household.Income[which(data_census_2015$Community==10)]
new_data_311$Median_Household_Income[which(new_data_311$Neighborhood==19 | new_data_311$Neighborhood==20)] <- data_census_2015$Median.Household.Income[which(data_census_2015$Community==11)]
new_data_311$Median_Household_Income[which(new_data_311$Neighborhood==21 | new_data_311$Neighborhood==22)] <- data_census_2015$Median.Household.Income[which(data_census_2015$Community==12)]
new_data_311$Median_Household_Income[which(new_data_311$Neighborhood==23 | new_data_311$Neighborhood==24)] <- data_census_2015$Median.Household.Income[which(data_census_2015$Community==13)]
new_data_311$Median_Household_Income[which(new_data_311$Neighborhood==25 | new_data_311$Neighborhood==26)] <- data_census_2015$Median.Household.Income[which(data_census_2015$Community==14)]
new_data_311$Median_Household_Income[which(new_data_311$Neighborhood==27) ] <- data_census_2015$Median.Household.Income[which(data_census_2015$Community==15)]
new_data_311$Median_Household_Income[which(new_data_311$Neighborhood==28) ] <- data_census_2015$Median.Household.Income[which(data_census_2015$Community==16)]
new_data_311$Median_Household_Income[which(new_data_311$Neighborhood==29 | new_data_311$Neighborhood==30)] <- data_census_2015$Median.Household.Income[which(data_census_2015$Community==17)]
new_data_311$Median_Household_Income[which(new_data_311$Neighborhood==31 | new_data_311$Neighborhood==32)] <- data_census_2015$Median.Household.Income[which(data_census_2015$Community==18)]
new_data_311$Median_Household_Income[which(new_data_311$Neighborhood==33 | new_data_311$Neighborhood==34)] <- data_census_2015$Median.Household.Income[which(data_census_2015$Community==19)]
new_data_311$Median_Household_Income[which(new_data_311$Neighborhood==35 | new_data_311$Neighborhood==36 | new_data_311$Neighborhood==37 | new_data_311$Neighborhood==38 | new_data_311$Neighborhood==39)] <- data_census_2015$Median.Household.Income[which(data_census_2015$Community==20)]
new_data_311$Median_Household_Income[which(new_data_311$Neighborhood==40) ] <- data_census_2015$Median.Household.Income[which(data_census_2015$Community==21)]
new_data_311$Median_Household_Income[which(new_data_311$Neighborhood==41 | new_data_311$Neighborhood==42)] <- data_census_2015$Median.Household.Income[which(data_census_2015$Community==22)]
new_data_311$Median_Household_Income[which(new_data_311$Neighborhood==43) ] <- data_census_2015$Median.Household.Income[which(data_census_2015$Community==23)]
new_data_311$Median_Household_Income[which(new_data_311$Neighborhood==44) ] <- data_census_2015$Median.Household.Income[which(data_census_2015$Community==24)]
new_data_311$Median_Household_Income[which(new_data_311$Neighborhood==45 | new_data_311$Neighborhood==46)] <- data_census_2015$Median.Household.Income[which(data_census_2015$Community==25)]
new_data_311$Median_Household_Income[which(new_data_311$Neighborhood==47) ] <- data_census_2015$Median.Household.Income[which(data_census_2015$Community==26)]
new_data_311$Median_Household_Income[which(new_data_311$Neighborhood==48 | new_data_311$Neighborhood==49)] <- data_census_2015$Median.Household.Income[which(data_census_2015$Community==27)]
new_data_311$Median_Household_Income[which(new_data_311$Neighborhood==50 | new_data_311$Neighborhood==51)] <- data_census_2015$Median.Household.Income[which(data_census_2015$Community==28)]
new_data_311$Median_Household_Income[which(new_data_311$Neighborhood==52) ] <- data_census_2015$Median.Household.Income[which(data_census_2015$Community==29)]
new_data_311$Median_Household_Income[which(new_data_311$Neighborhood==53) ] <- data_census_2015$Median.Household.Income[which(data_census_2015$Community==30)]
new_data_311$Median_Household_Income[which(new_data_311$Neighborhood==54) ] <- data_census_2015$Median.Household.Income[which(data_census_2015$Community==31)]
new_data_311$Median_Household_Income[which(new_data_311$Neighborhood==55 | new_data_311$Neighborhood==56 | new_data_311$Neighborhood==57)] <- data_census_2015$Median.Household.Income[which(data_census_2015$Community==32)]
new_data_311$Median_Household_Income[which(new_data_311$Neighborhood==58 )] <- data_census_2015$Median.Household.Income[which(data_census_2015$Community==33)]
new_data_311$Median_Household_Income[which(new_data_311$Neighborhood==59 )] <- data_census_2015$Median.Household.Income[which(data_census_2015$Community==34)]
new_data_311$Median_Household_Income[which(new_data_311$Neighborhood==60 | new_data_311$Neighborhood==61)] <- data_census_2015$Median.Household.Income[which(data_census_2015$Community==35)]
new_data_311$Median_Household_Income[which(new_data_311$Neighborhood==62 | new_data_311$Neighborhood==63)] <- data_census_2015$Median.Household.Income[which(data_census_2015$Community==36)]
new_data_311$Median_Household_Income[which(new_data_311$Neighborhood==64 | new_data_311$Neighborhood==65)] <- data_census_2015$Median.Household.Income[which(data_census_2015$Community==37)]
new_data_311$Median_Household_Income[which(new_data_311$Neighborhood==66 | new_data_311$Neighborhood==67)] <- data_census_2015$Median.Household.Income[which(data_census_2015$Community==38)]
new_data_311$Median_Household_Income[which(new_data_311$Neighborhood==68 )] <- data_census_2015$Median.Household.Income[which(data_census_2015$Community==39)]
new_data_311$Median_Household_Income[which(new_data_311$Neighborhood==69 )] <- data_census_2015$Median.Household.Income[which(data_census_2015$Community==40)]
new_data_311$Median_Household_Income[which(new_data_311$Neighborhood==70 | new_data_311$Neighborhood==71)] <- data_census_2015$Median.Household.Income[which(data_census_2015$Community==41)]
new_data_311$Median_Household_Income[which(new_data_311$Neighborhood==72 | new_data_311$Neighborhood==73)] <- data_census_2015$Median.Household.Income[which(data_census_2015$Community==42)]
new_data_311$Median_Household_Income[which(new_data_311$Neighborhood==74 | new_data_311$Neighborhood==75)] <- data_census_2015$Median.Household.Income[which(data_census_2015$Community==43)]
new_data_311$Median_Household_Income[which(new_data_311$Neighborhood==76 | new_data_311$Neighborhood==77)] <- data_census_2015$Median.Household.Income[which(data_census_2015$Community==44)]
new_data_311$Median_Household_Income[which(new_data_311$Neighborhood==78 )] <- data_census_2015$Median.Household.Income[which(data_census_2015$Community==45)]
new_data_311$Median_Household_Income[which(new_data_311$Neighborhood==79 | new_data_311$Neighborhood==80)] <- data_census_2015$Median.Household.Income[which(data_census_2015$Community==46)]
new_data_311$Median_Household_Income[which(new_data_311$Neighborhood==81)] <- data_census_2015$Median.Household.Income[which(data_census_2015$Community==47)]
new_data_311$Median_Household_Income[which(new_data_311$Neighborhood==82 | new_data_311$Neighborhood==83 | new_data_311$Neighborhood==84)] <- data_census_2015$Median.Household.Income[which(data_census_2015$Community==48)]


# Let's get back to the name of neighborhoods instead of having numbers as our labels
new_data_311$Neighborhood <- factor(new_data_311$Neighborhood,
                                    levels = c(1:84),
                                    labels = c('ALLENDALE','IRVINGTON','BEECHFIELD','TEN HILLS','WEST HILLS',
                                               'BELAIR-EDISON', 'BROOKLYN','CURTIS BAY','HAWKINS POINT', 
                                               'CANTON', 'CEDONIA','FRANKFORD', 'CHERRY HILL', 'CHINQUAPIN PARK-BELVEDERE',
                                               'CLAREMONT-FREEDOM' , 'ARMISTEAD GARDENS', 'CLIFTON PARK' , 'BEREA', 
                                               'CROSS COUNTRY','CHESWOLDE', 'DICKEYVILLE','FRANKLINTOWN', 'DORCHESTER',
                                               'ASHBURTON', 'DOWNTOWN','SETON HILL', 'EDMONDSON VILLAGE', 'FELLS POINT',
                                               'FOREST PARK','WALBROOK', 'GLEN' , 'FALLSTAFF', 'CHARLES VILLAGE','BARCLAY',
                                               'MID-GOVANS', 'ROSEBANK', 'WINSTON-GOVANS', 'WOODBOURNE-MCCABE', 'RICHNOR SPRINGS',
                                               'MONDAWMIN', 'GREATER ROLAND PARK', 'NORTH ROLAND PARK/POPLAR HILL', 'ROSEMONT',
                                               'LITTLE ITALY', 'HARFORD-ECHODALE','PERRING PARKWAY', 'HIGHLANDTOWN', 'HOWARD PARK',
                                               'WEST ARLINGTON', 'INNER HARBOR','FEDERAL HILL','LAURAVILLE','LOCH RAVEN','MADISON-EASTEND',
                                               'HAMPDEN','WOODBERRY','REMINGTON','MIDTOWN-EDMONDSON','COLDSTREAM HOMESTEAD MONTEBELLO',
                                               'MORRELL PARK','VIOLETVILLE','MOUNT WASHINGTON','COLDSPRING','GUILFORD','HOMELAND', 'OLDTOWN',
                                               'MIDDLE EAST', 'ORANGEVILLE', 'PATTERSON PARK', 'PENN NORTH',
                                               'RESERVOIR HILL', 'PIMLICO GOOD NEIGHBORS','ARLINGTON', 'POPPLETON','HOLLINS MARKET',
                                               'SANDTOWN-WINCHESTER','HARLEM PARK', 'WAVERLY', 'UPTON','DRUID HEIGHTS', 
                                               'WASHINGTON VILLAGE','WESTPORT','MOUNT WINANS','LAKELAND'))

# And change it to character again
new_data_311$Neighborhood <- as.character(new_data_311$Neighborhood)


# Calculating time taken to solve a request

CreatedDate <- mdy_hms(new_data_311$CreatedDate, tz = "America/New_York")
statusDate <- mdy_hms(new_data_311$StatusDate, tz = "America/New_York")
new_data_311$TimeTaken_minutes <- interval(CreatedDate, statusDate)/dminutes()
new_data_311$CreatedDay = weekdays(CreatedDate)
# ------------Remove all rows containing Rat Rubout-------------

new_data_311 <- new_data_311[ grep("SW-Rat Rubout", new_data_311$SRType, invert = TRUE) , ]
new_data_311 <- new_data_311[ grep("SSW-Bulk-Scheduled", new_data_311$SRType, invert = TRUE) , ]
new_data_311 <- new_data_311[ grep("SW-Bulk-Scheduled", new_data_311$SRType, invert = TRUE) , ]
high_freq_srtype <- data.frame(new_data_311 %>% group_by(new_data_311$SRType) %>% tally())
high_freq_srtype_order <- high_freq_srtype[order(-high_freq_srtype$n),]
head(high_freq_srtype_order,20)

#------------------SORT---------------------
high_freq_srtype[order(-high_freq_srtype$n),]



#Data based on top ten high frequency srtypes
srtype_high_frequency <- filter(new_data_311, SRType %in% 
                                  c("HCD-Sanitation Property","SW-Dirty Alley","TRS-Parking Complaints","SW-HGW") & SRStatus == "CLOSED")
                                                                                   


# Remove unnecessary columns

srtype_high_frequency <- srtype_high_frequency[,-c(4,5,7,8,9,10,11,12,13)]




srtype_high_frequency$SRType <- factor(srtype_high_frequency$SRType,
                                          levels = c("HCD-Sanitation Property","SW-Dirty Alley",
                                                     "TRS-Parking Complaints","SW-HGW"))
  
srtype_high_frequency$CreatedDay <- as.factor(srtype_high_frequency$CreatedDay)
unique(srtype_high_frequency$MethodReceived)

srtype_high_frequency$MethodReceived <- factor(srtype_high_frequency$MethodReceived,
                                               levels = c("Interface","Internet",
                                                          "Mass Entry", "Mobile Apps","Phone"))



srtype_high_frequency$CreatedDay <-  factor(srtype_high_frequency$CreatedDay,
                                            levels = c("Sunday","Monday","Tuesday",
                                                       "Wednesday", "Thursday",
                                                       "Friday","Saturday"))                                                         
srtype_high_frequency <- na.omit(srtype_high_frequency)
str(srtype_high_frequency)

# Rounding TimeTaken_hours
srtype_high_frequency$TimeTaken_minutes <- round(srtype_high_frequency$TimeTaken_minutes)
range(srtype_high_frequency$TimeTaken_minutes)

# Creating intervals for time taken which will help us to create different plots
group_TimeTaken <- function(Time){
  
  # TimeTaken_minutes <= 1 hour
  if (Time <= 60){
    return('TimeTaken <= 1 hour')
    
    # 1 hour < TimeTaken_minutes <= 1 day  
  }else if(Time > 60 & Time <= 60*24){
    return('1 hour < TimeTaken <= 1 day')
    
    # 1 day < TimeTaken_minutes <= 1 week  
  }else if(Time > 60*24 & Time <= 60*24*7){
    return('1 day < TimeTaken <= 1 week')
    
    # 1 week < TimeTaken_minutes <= 1 month 
  }else if(Time > 60*24*7 & Time <= 60*24*31){
    return('1 week < TimeTaken <= 1 month')
    
    # TimeTaken_minutes > 1 month
  }else{
    return('TimeTaken > 1 month')
  }
}
srtype_high_frequency$TimeTaken <- sapply(srtype_high_frequency$TimeTaken_minutes, group_TimeTaken)

srtype_high_frequency$TimeTaken <- as.factor(srtype_high_frequency$TimeTaken)
srtype_high_frequency$TimeTaken <- factor(srtype_high_frequency$TimeTaken,
                                       levels = c("TimeTaken <= 1 hour","1 hour < TimeTaken <= 1 day",
                                                  "1 day < TimeTaken <= 1 week","1 week < TimeTaken <= 1 month",
                                                  "TimeTaken > 1 month"))



# creating income intervals which will help to categorize our dataset regarding median income
group_income <- function(income){
  
  # Median_Household_Income <= 20000
  if (income <= 20000){
    return('Income <= 20k')
    
    # 20k < Median_Household_Income <= 40k  
  }else if(income > 20000 & income <= 40000){
    return('20k < Income <= 40k')
    
    # 40k < Median_Household_Income <= 60k  
  }else if(income > 40000 & income <= 60000){
    return('40k < Income <= 60k')
    
    # 60k < Median_Household_Income <= 80k  
  }else if(income > 60000 & income <= 80000){
    return('60k < Income <= 80k')
    
    # Median_Household_Income > 80000
  }else{
    return('Income > 80k')
  }
}
srtype_high_frequency$Income <- sapply(srtype_high_frequency$Median_Household_Income, group_income)

srtype_high_frequency$Income <- as.factor(srtype_high_frequency$Income)
srtype_high_frequency$Income <- factor(srtype_high_frequency$Income,
                                       levels = c("Income <= 20k","20k < Income <= 40k",
                                                  "40k < Income <= 60k","60k < Income <= 80k",
                                                  "Income > 80k"))

# Top 10 neighborhoods by population
neighbor <- c("FRANKFORD", "BELAIR-EDISON", "CANTON", "BROOKLYN", 
              "SANDTOWN-WINCHESTER", "CHERRY HILL", "CHARLES VILLAGE", 
              "GLEN", "COLDSTREAM HOMESTEAD MONTEBELLO", "HAMPDEN") 

srtype_high_frequency <- srtype_high_frequency[srtype_high_frequency$Neighborhood %in% neighbor,]
################### Visualization
# make sure "plyr" package is not called.
# calling dplyr and plyr together will hinder the work of dplyr package!

png(filename="tree.png",width=800, height=800)
srtype_high_frequency %>% group_by(Neighborhood, SRType) %>%
  summarise(nr = length(SRType)) %>%
  top_n(100,wt=nr) %>% ungroup() %>%
  treemap(
    index=c("Neighborhood","SRType"), 
    type="value",
    vSize = "nr",  
    vColor = "nr",
    palette = "RdBu",  
    title=sprintf("Frequency of SRTypes per Neighborhood"), 
    title.legend = "Number of SRTypes",
    fontsize.title = 14 
  )
dev.off()


# Count of Top Ten High Frequency SRTypes by Neighborhoods

ggplot(srtype_high_frequency,aes(Neighborhood))+
  geom_bar(fill='blue') + theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1,size=8)) +
  labs(title = 'Sum of Top 4 high frequency SRTypes by Neighborhoods')

gg0 <- ggplot(srtype_high_frequency,aes(Neighborhood))+
  geom_bar(aes(fill=Income)) + theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1,size=8)) +
  labs(title = "Sum of Top 4 high frequency SRTypes by Neighborhoods")

gg00 <- ggplot(srtype_high_frequency,aes(Neighborhood, fill=SRType))+
  geom_bar(position = "dodge") + theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1,size=8)) +
  labs(title = "Top 4 high frequency SRTypes by Neighborhoods")

ggarrange(gg0, gg00, nrow=2,ncol=1)
# It turned out that Belair-Edison, Canton, Coldstream,
# Sandtown-Winchester, and Washington village have the highest number of requests.
# Canton is a rich neighborhood, people live in Washington village have an average income,
# And the other neighborhoods with highest number of requests are below average income.

# Let's see the number of each of top 10 high frequency requests by neighborhood 

ggplot(subset(srtype_high_frequency, SRType == "HCD-Sanitation Property"),aes(Neighborhood))+
  geom_bar(aes(fill=Income)) + theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1,size=8)) +
  labs(title = 'Count of "HCD-Sanitation Property" Requests by Neighborhoods')
# Belair-Edison, Sandtown-winchester, Washington village, Canton, and Brooklyn have
# the highest number of Sanitation requests.


ggplot(subset(srtype_high_frequency, SRType == "SW-Dirty Alley"),aes(Neighborhood))+
  geom_bar(aes(fill=Income)) + theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1,size=8)) +
  labs(title = 'Count of "SW-Dirty Alley" Requests by Neighborhoods')
# Belair-Edison, Coldstream, Brooklyn, Washington village, and Sandtown-winchester
# have the highest number of Dirty Alley requests.


ggplot(subset(srtype_high_frequency, SRType == "TRS-Parking Complaints"),aes(Neighborhood))+
  geom_bar(aes(fill=Income)) + theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1,size=8)) +
  labs(title = 'Count of "TRS-Parking Complaints" Requests by Neighborhoods')
# Canton, Fellspoint, Downtown, Hampden, and Federal Hill have the highest number of
# Parking Complaints requests.


ggplot(subset(srtype_high_frequency, SRType == "ECC-Vehicle Look Up"),aes(Neighborhood))+
  geom_bar(aes(fill=Income)) + theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1,size=8)) +
  labs(title = 'Count of "ECC-Vehicle Look Up Reuests"" by Neighborhoods')

ggplot(subset(srtype_high_frequency, SRType == "SW-HGW"),aes(Neighborhood))+
  geom_bar(aes(fill=Income)) + theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1,size=8)) +
  labs(title = 'Count of "SW-HGW" Requests by Neighborhoods')

ggplot(subset(srtype_high_frequency, SRType == "SW-Dirty Street - Proactive"),aes(Neighborhood))+
  geom_bar(aes(fill=Income)) + theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1,size=8)) +
  labs(title = 'Count of "SW-Dirty Street - Proactive" Requests by Neighborhoods')

ggplot(subset(srtype_high_frequency, SRType == "SW-Dirty Street"),aes(Neighborhood))+
  geom_bar(aes(fill=Income)) + theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1,size=8)) +
  labs(title = 'Count of "SW-Dirty Street" Requests by Neighborhoods')

ggplot(subset(srtype_high_frequency, SRType == "BGE-StLight(s) Out"),aes(Neighborhood))+
  geom_bar(aes(fill=Income)) + theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1,size=8)) +
  labs(title = 'Count of "BGE-StLight(s) Out" Requests by Neighborhoods')

ggplot(subset(srtype_high_frequency, SRType == "TRS-Abandoned Vehicle"),aes(Neighborhood))+
  geom_bar(aes(fill=Income)) + theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1,size=8)) +
  labs(title = 'Count of "TRS-Abandoned Vehicle" Requests by Neighborhoods')

ggplot(subset(srtype_high_frequency, SRType == "WW Water Leak (Exterior)"),aes(Neighborhood))+
  geom_bar(aes(fill=Income)) + theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1,size=8)) +
  labs(title = 'Count of "WW Water Leak (Exterior)" Requests by Neighborhoods')



# High frequency SRTypes by Income
gg1 <- ggplot(srtype_high_frequency,aes(Income))+
  geom_bar(aes(fill=Income)) + theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1,size=8)) +
  labs(title = 'Count of Top 4 high frequency SRTypes by Income')
# Neighborhoods with income below average have the higher number of requests.

# High frequency SRTypes by Income
gg2 <- ggplot(srtype_high_frequency,aes(SRType))+
  geom_bar(aes(fill=Income)) + theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1,size=8)) +
  labs(title = 'Count of Top 4 high frequency SRTypes by Income')
# Except Parking Complaint in which around half of requests belong to the neighborhoods with
# Income above average, the other high frequency SRTypes took place in neighborhoods with
# below average income.

ggarrange(gg1, gg2, nrow=2,ncol=1,common.legend = TRUE, legend = "bottom")


# High frequency SRTypes by TimeTaken
gg3 <- ggplot(srtype_high_frequency,aes(TimeTaken))+
  geom_bar(aes(fill=TimeTaken)) + theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1,size=8)) +
  labs(title = 'Sum of Top 4 high frequency SRTypes by TimeTaken') 
# High frequency SRTypes by TimeTaken
gg4 <- ggplot(srtype_high_frequency,aes(SRType))+
  geom_bar(aes(fill=TimeTaken)) + theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1,size=8)) +
  labs(title = 'Sum of Top 4 high frequency SRTypes by TimeTaken') 

ggarrange(gg3, gg4, nrow=1,ncol=2,common.legend = TRUE, legend = "bottom")




# Let's see the frequency of our 10 srtypes visually
ggplot(srtype_high_frequency,aes(SRType))+
  geom_bar(fill='blue') + theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1,size=8)) +
  labs(title = 'Count of Top 10 high frequency SRTypes') 

# Count of Top 10 high frequency SRTypes by MethodReceived
ggplot(srtype_high_frequency,aes(SRType))+
  geom_bar(aes(fill=MethodReceived)) + theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1,size=8)) +
  labs(title = 'Count of Top 10 high frequency SRTypes by MethodReceived') 


# Let's see which days are most frequent regarding the number of srtypes happening
# in those days
ggplot(srtype_high_frequency,aes(CreatedDay))+
  geom_bar(fill='blue') + theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1,size=8)) +
  labs(title = 'Count of Top 10 high frequency SRTypes by CreatedDay') 
# It seems weekends have the lowest number of requests,
# and weekdays share fairely the same number of requests.

# Count of Top 10 high frequency SRTypes by CreatedDay and Income
ggplot(srtype_high_frequency,aes(CreatedDay))+
  geom_bar(aes(fill=Income)) + theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1,size=8)) +
  facet_wrap(~ SRType) +
  labs(title = 'Count of Top 10 high frequency SRTypes by CreatedDay and Income') 



# Which agency deals with higher number of requests?
#Count of Top 10 high frequency SRTypes by Agency
gg5 <- ggplot(srtype_high_frequency,aes(Agency))+
  geom_bar(aes(fill=Agency)) + theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size=8)) +
  labs(title = 'Count of Top 4 high frequency SRTypes by agency') 
#Count of Top 10 high frequency SRTypes by Agency
gg6 <- ggplot(srtype_high_frequency,aes(SRType))+
  geom_bar(aes(fill=Agency)) + theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size=8)) +
  labs(title = 'Count of Top 4 high frequency SRTypes by agency') 

ggarrange(gg5, gg6, nrow=2,ncol=1,common.legend = TRUE, legend = "bottom")
# Apparently Bureau of Solid Waste and Bureau of Water and Waste Water
# deal with the highest and lowest number of requests respectively.

# Which method is more popular among people to report their request?
gg7 <- ggplot(srtype_high_frequency,aes(MethodReceived))+
  geom_bar(aes(fill=MethodReceived)) + theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1,size=8)) +
  labs(title = 'Count of Top 4 high frequency SRTypes by MethodReceived') 
# Most requests have been reported by phone.

gg8 <- ggplot(srtype_high_frequency,aes(Income))+
  geom_bar(aes(fill=MethodReceived)) + theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1,size=8))  +
  labs(title = 'Count of Top 4 high frequency SRTypes by Income') 

ggarrange(gg7, gg8, nrow=1,ncol=2,common.legend = TRUE, legend = "bottom")
# Apparently, rich people prefer phone to the other methods.



# SRType frequency according to income
ggplot(srtype_high_frequency,aes(SRType))+
  geom_bar(aes(fill=Income)) + theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1,size=8)) +
  labs(title = 'Count of Top 10 high frequency SRTypes by Income') 

# In general, in neighborhoods with the income between 20k and 40k, requests
# are more frequent than the other neighborhoods.
# Neighborhoods with income more that 80k experience the lowest rate of requests,
# but there is one exception for these neighborhoods.
# "TRS-Parking Complaint" requests are so frequent in rich neighborhoods.

# Count of Top 10 high frequency SRTypes by CreatedDay and Income
ggplot(srtype_high_frequency,aes(SRType))+
  geom_bar(aes(fill=Income)) + theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1,size=8)) +
  facet_wrap(~ CreatedDay) +
  labs(title = 'Count of Top 10 high frequency SRTypes by CreatedDay and Income') 

# Count of Top 10 high frequency SRTypes by Income
ggplot(srtype_high_frequency,aes(SRType))+
  geom_bar(fill='blue') + theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1,size=8)) +
  facet_wrap(~ Income) +
  labs(title = 'Count of Top 10 high frequency SRTypes by Income') 


#Top 10 high frequency SRTypes by Median Household Income
gg9 <- ggplot(srtype_high_frequency,aes(SRType,Median_Household_Income))+
  geom_boxplot(aes(fill=SRType)) + theme_bw() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1,size=8)) +
  labs(title = 'Top 4 high frequency SRTypes by Median Household Income') 
# Top 10 high frequency SRTypes by Median Household Income
gg10 <-ggplot(srtype_high_frequency,aes(SRType,Median_Household_Income))+
  geom_violin(aes(fill=SRType)) + theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1,size=8)) +
  labs(title = 'Top 4 high frequency SRTypes by Median Household Income') 

ggarrange(gg9, gg10, ncol = 1, nrow = 2,common.legend = TRUE, legend = "bottom")


# Top 10 high frequency SRTypes by Median Household Income and CreatedDay
gg11 <- ggplot(srtype_high_frequency,aes(CreatedDay,Median_Household_Income))+
  geom_boxplot(aes(fill=CreatedDay)) + theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1,size=8)) +
  labs(title = 'Top 4 high frequency SRTypes by Median Household Income and CreatedDay')
# Top 10 high frequency SRTypes by Median Household Income and CreatedDay
gg12<- ggplot(srtype_high_frequency,aes(CreatedDay,Median_Household_Income))+
  geom_violin(aes(fill=CreatedDay)) + theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1,size=8)) +
  labs(title = 'Top 4 high frequency SRTypes by Median Household Income and CreatedDay')

ggarrange(gg11, gg12, ncol = 1, nrow = 2,common.legend = TRUE, legend = "bottom")

# Top 10 high frequency SRTypes by Median Household Income and MethodReceived
gg13 <- ggplot(srtype_high_frequency,aes(MethodReceived,Median_Household_Income))+
  geom_boxplot(aes(fill=MethodReceived)) + theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1,size=8)) +
  labs(title = 'Top 4 high frequency SRTypes by Median Household Income and MethodReceived')

# Top 10 high frequency SRTypes by Median Household Income and MethodReceived
gg14 <- ggplot(srtype_high_frequency,aes(MethodReceived,Median_Household_Income))+
  geom_violin(aes(fill=MethodReceived)) + theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1,size=8)) +
  labs(title = 'Top 4 high frequency SRTypes by Median Household Income and MethodReceived')

ggarrange(gg13, gg14, ncol = 1, nrow = 2,common.legend = TRUE, legend = "bottom")

######## subset

ggplot(subset(srtype_high_frequency, SRType == "SW-Dirty Alley"), aes(CreatedDay)) +
  geom_bar(aes(fill = Income)) + theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1,size=8)) +
  labs(title = 'Count of "SW-Dirty Alley" Requests by Income')


ggplot(srtype_high_frequency,aes(SRType,TimeTaken_minutes))+
  geom_boxplot(aes(fill=SRType)) + theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1,size=8)) +
  labs(title = 'Top 10 high frequency SRTypes by TimeTaken')


srtype_high_frequency$Neighborhood <- as.factor(srtype_high_frequency$Neighborhood)

# Average of time taken to solve a request based on specific
# requests and specific neighborhoods
srtype_average <- srtype_high_frequency %>%
  select(c(1,3,7,9)) %>%
  group_by(SRType, Neighborhood, Income) %>%
  summarise_at(vars(TimeTaken_minutes), funs(round(mean(., na.rm=TRUE))))

srtype_average <- as.data.frame(srtype_average)


group_AverageTimeTaken <- function(Time){
  
  # TimeTaken_minutes <= 1 hour
  if (Time <= 60){
    return('AverageTimeTaken <= 1 hour')
    
    # 1 hour < TimeTaken_minutes <= 1 day  
  }else if(Time > 60 & Time <= 60*24){
    return('1 hour < AverageTimeTaken <= 1 day')
    
    # 1 day < TimeTaken_minutes <= 1 week  
  }else if(Time > 60*24 & Time <= 60*24*7){
    return('1 day < AverageTimeTaken <= 1 week')
    
    # 1 week < TimeTaken_minutes <= 1 month 
  }else if(Time > 60*24*7 & Time <= 60*24*31){
    return('1 week < AverageTimeTaken <= 1 month')
    
    # TimeTaken_minutes > 1 month
  }else{
    return('AverageTimeTaken > 1 month')
  }
}
srtype_average$AverageTimeTaken <- sapply(srtype_average$TimeTaken_minutes, group_AverageTimeTaken)

srtype_average$AverageTimeTaken <- as.factor(srtype_average$AverageTimeTaken)
srtype_average$AverageTimeTaken <- factor(srtype_average$AverageTimeTaken,
                                          levels = c("AverageTimeTaken <= 1 hour","1 hour < AverageTimeTaken <= 1 day",
                                                     "1 day < AverageTimeTaken <= 1 week","1 week < AverageTimeTaken <= 1 month",
                                                     "AverageTimeTaken > 1 month"))

# ##############

gg15 <- ggplot(subset(srtype_average, SRType =="HCD-Sanitation Property"), aes(Neighborhood,TimeTaken_minutes)) + 
  geom_bar(aes(fill=Income), stat="identity") +    
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8)) +
  labs(title = "HCD-Sanitation Property", y = "Average time taken")

gg16 <- ggplot(subset(srtype_average, SRType =="SW-Dirty Alley"), aes(Neighborhood,TimeTaken_minutes)) + 
  geom_bar(aes(fill=Income), stat="identity") +    
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8)) +
  labs(title = "SW-Dirty Alley", y = "Average time taken")

gg17 <- ggplot(subset(srtype_average, SRType =="TRS-Parking Complaints"), aes(Neighborhood,TimeTaken_minutes)) + 
  geom_bar(aes(fill=Income), stat="identity") +    
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8)) +
  labs(title = "TRS-Parking Complaints", y = "Average time taken")

gg18 <- ggplot(subset(srtype_average, SRType =="SW-HGW"), aes(Neighborhood,TimeTaken_minutes)) + 
  geom_bar(aes(fill=Income), stat="identity") +    
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8)) +
  labs(title = "SW-HGW", y = "Average time taken")

ggarrange(gg15, gg16, gg17, gg18, ncol = 2, nrow = 2,common.legend = TRUE, legend = "bottom")

ggplot(subset(srtype_average, SRType =="ECC-Vehicle Look Up"), aes(Neighborhood,TimeTaken_minutes)) + 
  geom_bar(aes(fill=AverageTimeTaken), stat="identity") +    
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 8)) +
  labs(title = "ECC-Vehicle Look Up", y = "Average time taken")

ggplot(subset(srtype_average, SRType =="SW-Dirty Street - Proactive"), aes(Neighborhood,TimeTaken_minutes)) + 
  geom_bar(aes(fill=AverageTimeTaken), stat="identity") +    
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 8)) +
  labs(title = "SW-Dirty Street - Proactive", y = "Average time taken")

ggplot(subset(srtype_average, SRType =="SW-Dirty Street"), aes(Neighborhood,TimeTaken_minutes)) + 
  geom_bar(aes(fill=AverageTimeTaken), stat="identity") +    
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 8)) +
  labs(title = "SW-Dirty Street", y = "Average time taken")

ggplot(subset(srtype_average, SRType =="BGE-StLight(s) Out"), aes(Neighborhood,TimeTaken_minutes)) + 
  geom_bar(aes(fill=AverageTimeTaken), stat="identity") +    
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 8)) +
  labs(title = "BGE-StLight(s) Out", y = "Average time taken")

ggplot(subset(srtype_average, SRType =="TRS-Abandoned Vehicle"), aes(Neighborhood,TimeTaken_minutes)) + 
  geom_bar(aes(fill=AverageTimeTaken), stat="identity") +    
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 8)) +
  labs(title = "TRS-Abandoned Vehicle", y = "Average time taken")

ggplot(subset(srtype_average, SRType =="WW Water Leak (Exterior)"), aes(Neighborhood,TimeTaken_minutes)) + 
  geom_bar(aes(fill=AverageTimeTaken), stat="identity") +    
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 8)) +
  labs(title = "WW Water Leak (Exterior)", y = "Average time taken")



##### Population dataset

Population <- read.xls("Population By Neighborhood.xlsx", sheet=2)

Unique.Neighborhood <- unique(srtype_high_frequency$Neighborhood)
Unique.Neighborhood <- as.data.frame(Unique.Neighborhood)
Unique.Neighborhood <- arrange(Unique.Neighborhood, (Unique.Neighborhood))

Population$Name <- sapply(Population$Name, toupper)

Population_data <- Population %>% filter(Name %in% Unique.Neighborhood[,1])
Population_data$Population[Population_data$Name=="CLIFTON PARK"] <- 32276
Population_data$Population[Population_data$Name=="HAWKINS POINT"] <- 10
Population_data$Population[Population_data$Name=="PATTERSON PARK"] <- 49031
colnames(Population_data)[3] <- "Neighborhood"

ggplot(Population_data, aes(Neighborhood,Population)) + 
  geom_bar(stat="identity",fill='blue') +    
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 8)) +
  labs(title = "Population by neighborhood", x = "Neighborhood")


Neighborhood.Income <- srtype_high_frequency %>% select(Neighborhood, Income)%>% unique() %>% arrange(Neighborhood) 

Population_Income <- merge(Neighborhood.Income, Population_data, by = "Neighborhood")

ggplot(Population_Income, aes(Neighborhood,Population)) + 
  geom_bar(aes(fill=Income), stat="identity") +    
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 8)) +
  labs(title = "Population by neighborhood", x = "Neighborhood")



# Merging "srtype_average" and "Neighborhood.Income"
srtype.average.income <- merge(srtype_average, Neighborhood.Income, by ="Neighborhood")

ggplot(subset(srtype.average.income, SRType =="SW-Dirty Alley"), aes(Neighborhood,TimeTaken_minutes)) + 
  geom_bar(aes(fill=Income), stat="identity") +    
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 8)) +
  labs(title = "SW-Dirty Alley", y = "Average time taken")

################### "SW-Dirty Alley"

Dirty_Alley <- srtype_high_frequency %>% filter(SRType == "SW-Dirty Alley")
 
Top10_Neighborhood <- Dirty_Alley %>% group_by(Neighborhood) %>%
  summarise(nr = length(SRType)) %>% arrange(desc(nr))  %>% top_n(10,wt=nr)

Dirty_Alley <- Dirty_Alley %>% filter(Neighborhood %in% Top10_Neighborhood$Neighborhood)

Dirty_Alley$Neighborhood <- as.factor(Dirty_Alley$Neighborhood)

str(Dirty_Alley)

### Adding Population as a column to the dataset

Dirty_Alley$Population <- NA

Neighborhood_Population <- function(name){
  if(name == 'WASHINGTON VILLAGE'){
    return(Dirty_Alley$Population[Dirty_Alley$Neighborhood==name] <- Population$Population[Population$Name=='WASHINGTON VILLAGE/PIGTOWN'])
  }else if(name == 'HARLEM PARK'){
    return(Dirty_Alley$Population[Dirty_Alley$Neighborhood==name]<- Population$Population[Population$Name=='HARLEM PARK'])
  }else if(name == 'COLDSTREAM HOMESTEAD MONTEBELLO'){
    return(Dirty_Alley$Population[Dirty_Alley$Neighborhood==name] <- Population$Population[Population$Name=='COLDSTREAM HOMESTEAD MONTEBELLO'])
  }else if(name == 'CANTON'){
    return(Dirty_Alley$Population[Dirty_Alley$Neighborhood==name] <- Population$Population[Population$Name=='CANTON'])
  }else if(name == 'BROOKLYN'){
    return(Dirty_Alley$Population[Dirty_Alley$Neighborhood==name] <- Population$Population[Population$Name=='BROOKLYN'])
  }else if(name == 'BELAIR-EDISON'){
    return(Dirty_Alley$Population[Dirty_Alley$Neighborhood==name] <- Population$Population[Population$Name=='BELAIR-EDISON'])
  }else if(name == 'SANDTOWN-WINCHESTER'){
    return(Dirty_Alley$Population[Dirty_Alley$Neighborhood==name] <- Population$Population[Population$Name=='SANDTOWN-WINCHESTER'])
  }else if(name == 'BARCLAY'){
    return(Dirty_Alley$Population[Dirty_Alley$Neighborhood==name] <- Population$Population[Population$Name=='BARCLAY'])
  }else if(name == 'MIDDLE EAST'){
    return(Dirty_Alley$Population[Dirty_Alley$Neighborhood==name] <- Population$Population[Population$Name=='MIDDLE EAST'])
  }else{
    return(Dirty_Alley$Population[Dirty_Alley$Neighborhood==name] <- Population$Population[Population$Name=='HIGHLANDTOWN'])
  }
}

Dirty_Alley$Population <- sapply(Dirty_Alley$Neighborhood, Neighborhood_Population)

Dirty_Alley <- Dirty_Alley[,-c(1,2)]

Dirty_Alley <- Dirty_Alley %>%
  select(Neighborhood, Population, Median_Household_Income, TimeTaken_minutes, everything())

#-----------Crime dataset-----------
crime_data <- read.csv("crime.csv")

#-------------Creating an empty dataframe-------------------------
df.crime <- data.frame(Count.Crime=rep(as.integer(NA),279),
                       stringsAsFactors=FALSE)

##### Adding Neighborhoods to this dataframe
df.crime$Neighborhood <- unique (crime_data$Neighborhood)

########################### Count of crime
j=0
for (i in unique (crime_data$Neighborhood)){
  j=j+1
  count <- length(which(crime_data$Neighborhood == i & crime_data$Description %in% unique(crime_data$Description) ))
  df.crime$Count.Crime[j] <- count}
df.crime$Neighborhood <- sapply(df.crime$Neighborhood, toupper)

### Adding Population as a column to the dataset

Dirty_Alley$CrimeCount <- NA

Neighborhood_CrimeCount <- function(name){
  if(name == 'WASHINGTON VILLAGE'){
    return(Dirty_Alley$CrimeCount[Dirty_Alley$Neighborhood==name] <- df.crime$Count.Crime[df.crime$Neighborhood=='WASHINGTON VILLAGE/PIGTOW'])
  }else if(name == 'HARLEM PARK'){
    return(Dirty_Alley$CrimeCount[Dirty_Alley$Neighborhood==name]<- df.crime$Count.Crime[df.crime$Neighborhood=='HARLEM PARK'])
  }else if(name == 'COLDSTREAM HOMESTEAD MONTEBELLO'){
    return(Dirty_Alley$CrimeCount[Dirty_Alley$Neighborhood==name] <- df.crime$Count.Crime[df.crime$Neighborhood=='COLDSTREAM HOMESTEAD MONT'])
  }else if(name == 'CANTON'){
    return(Dirty_Alley$CrimeCount[Dirty_Alley$Neighborhood==name] <- df.crime$Count.Crime[df.crime$Neighborhood=='CANTON'])
  }else if(name == 'BROOKLYN'){
    return(Dirty_Alley$CrimeCount[Dirty_Alley$Neighborhood==name] <- df.crime$Count.Crime[df.crime$Neighborhood=='BROOKLYN'])
  }else if(name == 'BELAIR-EDISON'){
    return(Dirty_Alley$CrimeCount[Dirty_Alley$Neighborhood==name] <- df.crime$Count.Crime[df.crime$Neighborhood=='BELAIR-EDISON'])
  }else if(name == 'SANDTOWN-WINCHESTER'){
    return(Dirty_Alley$CrimeCount[Dirty_Alley$Neighborhood==name] <- df.crime$Count.Crime[df.crime$Neighborhood=='SANDTOWN-WINCHESTER'])
  }else if(name == 'BARCLAY'){
    return(Dirty_Alley$CrimeCount[Dirty_Alley$Neighborhood==name] <- df.crime$Count.Crime[df.crime$Neighborhood=='BARCLAY'])
  }else if(name == 'MIDDLE EAST'){
    return(Dirty_Alley$CrimeCount[Dirty_Alley$Neighborhood==name] <- df.crime$Count.Crime[df.crime$Neighborhood=='MIDDLE EAST'])
  }else{
    return(Dirty_Alley$CrimeCount[Dirty_Alley$Neighborhood==name] <- df.crime$Count.Crime[df.crime$Neighborhood=='HIGHLANDTOWN'])
  }
}

Dirty_Alley$CrimeCount <- sapply(Dirty_Alley$Neighborhood, Neighborhood_CrimeCount)


Dirty_Alley <- Dirty_Alley %>%
  select(Neighborhood, Population, CrimeCount, everything())

### Converting TimeTaken to hour
Dirty_Alley$TimeTaken_minutes <- round((Dirty_Alley$TimeTaken_minutes)/60)
colnames(Dirty_Alley)[5] <- "TimeTaken"
colnames(Dirty_Alley)[8] <- "TimeTaken.Interval"
colnames(Dirty_Alley)[9] <- "Income.Interval"

Dirty_Alley.AverageTime <- Dirty_Alley %>% 
  select(Neighborhood, TimeTaken) %>%
  group_by(Neighborhood) %>%
  summarise_at(vars(TimeTaken), funs(mean(., na.rm=TRUE)))

ggplot(Dirty_Alley.AverageTime, aes(y=TimeTaken, x=Neighborhood)) + 
  geom_bar( stat="identity", fill='blue') +    
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size=8)) +
  labs(title = 'Average time taken to solve "SW-Dirty Alley" in different neighborhoods', 
       y = "Average Time Taken(hours)") + coord_flip() + theme_bw()

################### Machine Learning

# We do not need Agency, because each agency works on a specific request.

dataset <- Dirty_Alley[,2:5]


# Splitting the dataset into the Training set and Test set

library(caTools)
set.seed(123)
split = sample.split(dataset$TimeTaken, SplitRatio = 0.8)
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)


############# Multiple Linear Regression

# Fitting Multiple Linear Regression to the Training set

regressor_lm = lm(formula = TimeTaken ~ .,
                  data = training_set)
summary(regressor_lm)

####### Predicting the Test set results
y_pred_lm = predict(regressor_lm, newdata = test_set)

Pred_Actual_lm <- as.data.frame(cbind(Prediction = y_pred_lm, Actual = test_set$TimeTaken))

gg.lm <- ggplot(Pred_Actual_lm, aes(Actual, Prediction )) +
  geom_point() + theme_bw() + geom_abline() +
  labs(title = "Multiple Linear Regression", x = "Actual Time Taken",
       y = "Predicted Time Taken") +
  theme(plot.title = element_text(family = "Helvetica", face = "bold", size = (15)), 
        axis.title = element_text(family = "Helvetica", size = (10)))
gg.lm

MSE.lm <- sum((test_set$TimeTaken - y_pred_lm)^2)/nrow(test_set)

########################################## Random Forest Regression

# Fitting Random Forest Regression to the dataset
# install.packages('randomForest')
library(randomForest)
set.seed(1234)
regressor_rf = randomForest(x = dataset[-4],
                            y = dataset$TimeTaken,
                            ntree = 100)

# Predicting a new result with Random Forest Regression
y_pred_rf = predict(regressor_rf, newdata = test_set)

Pred_Actual_rf <- as.data.frame(cbind(Prediction = y_pred_rf, Actual = test_set$TimeTaken))


gg.rf <- ggplot(Pred_Actual_rf, aes(Actual, Prediction )) +
  geom_point() + theme_bw() + geom_abline() +
  labs(title = "Random Forest Regression", x = "Actual Time Taken",
       y = "Predicted Time Taken") +
  theme(plot.title = element_text(family = "Helvetica", face = "bold", size = (15)), 
        axis.title = element_text(family = "Helvetica", size = (10)))

gg.rf
#############

############################### SVR

# Fitting SVR to the dataset
library(e1071)
regressor_svr = svm(formula = TimeTaken ~ .,
                    data = dataset,
                    type = 'eps-regression',
                    kernel = 'radial')



# Predicting a new result
y_pred_svr = predict(regressor_svr,  newdata = test_set)

Pred_Actual_svr <- as.data.frame(cbind(Prediction = y_pred_svr, Actual = test_set$TimeTaken))


Pred_Actual_lm.versus.svr <- cbind(Prediction.lm = y_pred_lm, Prediction.svr = y_pred_svr, Actual = test_set$TimeTaken_minutes)


gg.svr <- ggplot(Pred_Actual_svr, aes(Actual, Prediction )) +
  geom_point() + theme_bw() + geom_abline() +
  labs(title = "SVR", x = "Actual Time Taken",
       y = "Predicted Time Taken") +
  theme(plot.title = element_text(family = "Helvetica", face = "bold", size = (15)), 
        axis.title = element_text(family = "Helvetica", size = (10)))

gg.svr


######################

mm <- model.matrix( 
  ~  MethodReceived + CreatedDay + Income + TimeTaken, 
  data = training_set 
)

flags1 <- data.frame(Reduce(cbind, 
                  lapply(levels(dataset$MethodReceived), function(x){(dataset$MethodReceived == x)*1})
))
names(flags1) = levels(dataset$MethodReceived)

flags2 <- data.frame(Reduce(cbind, 
                            lapply(levels(dataset$CreatedDay), function(x){(dataset$CreatedDay == x)*1})
))
names(flags2) = levels(dataset$CreatedDay)

flags3 <- data.frame(Reduce(cbind, 
                            lapply(levels(dataset$Income), function(x){(dataset$Income == x)*1})
))
names(flags3) = levels(dataset$Income)

flags4 <- data.frame(Reduce(cbind, 
                            lapply(levels(dataset$TimeTaken), function(x){(dataset$TimeTaken == x)*1})
))
names(flags4) = levels(dataset$TimeTaken)

names.flags <- paste(c(names(flags1),names(flags2),names(flags3),names(flags4)))


levelnames = paste(names.flags, collapse = " + ")


# Fitting K-NN to the Training set and Predicting the Test set results
library(class)
y_pred = knn(train = training_set[, -1],
             test = test_set[, -1],
             cl = training_set[, 1],
             k = 5)

# Making the Confusion Matrix
cm = table(test_set[, 3], y_pred)

# Neural Net
nn <- neuralnet( paste("SRType ~ ", levelnames),
                data=dataset,hidden=10,linear.output=TRUE)

nnpredicted = compute(nn, training_set[-1])

nnpredictedvalues = nnpredicted$net.result*(max(training_set$SRType) - min(training_set$SRType)) + min(training_set$SRType)


# Let's consider TimeTaken_minutes as the dependent variable

# Multiple linear regression

# Splitting the dataset into the Training set and Test set
# install.packages('caTools')
library(caTools)
set.seed(123)
dataset <- srtype_high_frequency[1:7]
split = sample.split(dataset$TimeTaken_minutes, SplitRatio = 0.8)
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)


############# Multiple Linear Regression

# Fitting Multiple Linear Regression to the Training set
regressor_lm = lm(formula = TimeTaken_minutes ~ .,
                  data = training_set)

summary(regressor_lm)

####### Predicting the Test set results
y_pred_lm = predict(regressor_lm, newdata = test_set)

Pred_Actual_lm <- as.data.frame(cbind(Prediction = y_pred_lm, Actual = test_set$TimeTaken_minutes))

gg.lm <- ggplot(Pred_Actual_lm, aes(Actual, Prediction )) +
  geom_point() + theme_bw() + geom_abline() +
  labs(title = "Multiple Linear Regression", x = "Actual Time Taken",
       y = "Predicted Time Taken") +
  theme(plot.title = element_text(family = "Helvetica", face = "bold", size = (15)), 
        axis.title = element_text(family = "Helvetica", size = (10)))

MSE.lm <- sum((test_set$TimeTaken_minutes - y_pred_lm)^2)/nrow(test_set)




# instead of neighborhoods use population, income, number of crimes
# use top 10 neighborhoods based on population and higher number of requests


