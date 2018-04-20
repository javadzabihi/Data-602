# Data-602 Final Project

# This code is in the process of revising and will be updated so soon!

setwd("../R Programming Folder/Data 602 Final Project")


library(tidyverse)
library(dplyr)
library(ggplot2)
library(ggthemes)
library(plyr)
library(reshape2)
library(lubridate)
library(caTools)
library(data.table)
library(tidyr)



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

#This line gives the unique values for every column in our dataset
sapply(data_311, function(x) length(unique(x)) )

distinct(select(data_311, Neighborhood))

#The following output is much more informative and compact than what we would get if we
# printed the original data frame (data_311) to the console.

mydata_311 <- tbl_df(data_311)
head(mydata_311)

#Check for Missing values

missing_values <- data_311 %>% summarize_all(funs(sum(is.na(.))/n()))

missing_values <- gather(missing_values, key="feature", value="missing_pct")
missing_values %>% 
  ggplot(aes(x=reorder(feature,-missing_pct),y=missing_pct)) +
  geom_bar(stat="identity",fill="red")+
  coord_flip()+theme_bw()
  
  # Converting "CreatedDate", "StatusDate", "DueDate" columns to Date 
#data_311$CreatedDate <- dmy_hms(data_311$CreatedDate)
#data_311$StatusDate <- dmy_hms(data_311$StatusDate)
#data_311$DueDate <- dmy_hms(data_311$DueDate)

# Removing "SRRecordID", "ServiceRequestNum", and "GeoLocation" columns from the dataset

new_data_311 <- new_data_311[,3:15]
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
new_data_311$Neighborhood = factor(new_data_311$Neighborhood,
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



#Making several plots to get a clear idea about how our data is distributed

#Plotting histogram
hist(new_data_311$Median_Household_Income, col='grey')

#Using frequencies or densities
#Creating a density plot
mydatadens <- density(new_data_311$Median_Household_Income)
plot(mydatadens)

       
       
boxplot(Median_Household_Income~Neighborhood,data = new_data_311)   




# Adding another column, "SRType_date", by converting "CreatedDate" column to Date and removing its time portion
# This new column will help us in the process of combining "data_311" and "crime_data"

data_311$SRType_date <- as.Date(data_311$CreatedDate, format = "%m/%d/%Y")
str(data_311)

range(data_311$SRType_date)

# Calculating time taken to solve a request

createdDate <- mdy_hms(data_311$CreatedDate, tz = "America/New_York")
statusDate <- mdy_hms(data_311$StatusDate, tz = "America/New_York")
data_311$TimeTaken_minutes <- interval(createdDate, statusDate)/dminutes()
head(data_311)


# crime_dataset

crime_data <- read.csv("BPD_Part_1_Victim_Based_Crime_Data.csv")
str(crime_data)
summary(crime_data)
table(crime_data$Description)
unique(crime_data$Neighborhood)
head(crime_data)
tail(crime_data)
table(sort(crime_data$Neighborhood))
table(sort(data_311$Neighborhood))
# More informative and compact overview of the output

mydata_crime <- tbl_df(crime_data)
head(mydata_crime)
tail(mydata_crime)
       
# Finding the correlation between numerical columns
Num.cols <- sapply(crime_data, is.numeric)
Cor.data <- cor(crime_data[, Num.cols])
       
install.packages("corrgram") 
library(corrgram)       
install.packages("corrplot")  
library(corrplot)
 
 corrplot(cor.data, method = 'color')      
 
 corrgram(crime_data, order = TRUE, lower.panel = panel.shade,
         upper.panel = panel.pie, text.panel = panel.txt)      

# Removing "CrimeCode" column from the dataset

crime_data <- crime_data[,-3]
head(crime_data)

#Convert "CrimeDate column to Dates

crime_data$CrimeDate <- as.Date(crime_data$CrimeDate, format = "%m/%d/%Y")
str(crime_data)

range(crime_data$CrimeDate)






# ------------Remove all rows containing Rat Rubout-------------

data_311 <- data_311[ grep("SW-Rat Rubout", data_311$SRType, invert = TRUE) , ]
data_311 <- data_311[ grep("SSW-Bulk-Scheduled", data_311$SRType, invert = TRUE) , ]
high_freq_srtype <- data.frame(data_311 %>% group_by(data_311$SRType) %>% tally())
high_freq_srtype_order <- high_freq_srtype[order(-high_freq_srtype$n),]
head(high_freq_srtype_order)

#------------------SORT---------------------
high_freq_srtype[order(-high_freq_srtype$n),]



#-------set max print options
options(max.print=1000)

data_311$Agency %>% summary()



#----------------Data based on SRType = ECC-Vehicle Look Up------------

srtype_vehicle <- filter(data_311, SRType == "ECC-Vehicle Look Up" & SRStatus == "CLOSED")
str(srtype_vehicle)

# Calculate time take
createdDate <- mdy_hms(srtype_vehicle$CreatedDate, tz = "America/New_York")
statusDate <- mdy_hms(srtype_vehicle$StatusDate, tz = "America/New_York")
TimeTaken = interval(createdDate, statusDate)/dminutes()    # interval in minutes

srtype_vehicle$TimeTaken <- TimeTaken
srtype_vehicle$createdDay = weekdays(createdDate)
srtype_vehicle$StatusDay = weekdays(statusDate)
srtype_vehicle$createdDay <- as.factor(srtype_vehicle$createdDay)
names(srtype_vehicle)
summary(srtype_vehicle) 
nrow(srtype_vehicle)    

vehicle_var <- c("TimeTaken","MethodReceived","createdDay","Neighborhood")
vehicle_table <- srtype_vehicle[vehicle_var]
glimpse(vehicle_table)
str(vehicle_table)
table(vehicle_table$MethodReceived)
range(vehicle_table$TimeTaken)
#-------set max print options-----------
options(max.print=1000)

# Multiple Linear Regression


# Encoding categorical data
vehicle_table$MethodReceived = factor(vehicle_table$MethodReceived,
                         levels = c('E-Mail', 'Phone'),
                         labels = c(1, 2))


vehicle_table$createdDay = factor(vehicle_table$createdDay,
                                  levels = c('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Fridday', 'Saturday', 'Sunday'),
                                  labels = c(1, 2, 3, 4, 5, 6, 7))

# Splitting the dataset into the Training set and Test set
# install.packages('caTools')
library(caTools)
set.seed(123)
split = sample.split(vehicle_table$TimeTaken, SplitRatio = 0.8)
training_set = subset(vehicle_table, split == TRUE)
test_set = subset(vehicle_table, split == FALSE)

# Feature Scaling
# training_set = scale(training_set)
# test_set = scale(test_set)

# Fitting Multiple Linear Regression to the Training set
regressor = lm(formula = TimeTaken ~ createdDay + MethodReceived,
               data = training_set)

# Predicting the Test set results
y_pred = predict(regressor, newdata = test_set)



# Building the optimal model using Backward Elimination
regressor = lm(formula = TimeTaken ~ Neighborhood + MethodReceived + createdDay,
               data = vehicle_table)
summary(regressor)


regressor = lm(formula = TimeTaken ~ Neighborhood + createdDay,
               data = vehicle_table)
summary(regressor)
regressor = lm(formula = TimeTaken ~ Neighborhood,
               data = vehicle_table)
summary(regressor)

# Visualising the Regression Model results
# install.packages('ggplot2')


library(ggplot2)
ggplot() +
  geom_point(aes(x = vehicle_table$createdDay, y = vehicle_table$TimeTaken),
            colour = 'red') +
  geom_boxplot(aes(x = vehicle_table$createdDay, y = predict(regressor, newdata = vehicle_table)),
            colour = 'blue') +
  ggtitle('Regression Model') +
  scale_y_continuous(limits = c(0, 5))+
  xlab('createdDay') +
  ylab('TimeTaken')





