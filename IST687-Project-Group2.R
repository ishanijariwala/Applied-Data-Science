#####################################################
# IST 687 Project
# Group 2
# Integrated code
#####################################################

#Installing required packages

install.packages('xlsx')
library(xlsx)
install.packages('ggplot2')
library(ggplot2)
install.packages('reshape2')
library(reshape2)
install.packages('stringr')
library(stringr)

#####################################################
# DATA Q1 : 1.	WHAT IS THE TREND/PATTERN OF CUSTOMERS IN USING HOTEL'S SERVICES 
# AND THE DIFFERENCE, IF ANY, BASED ON CUSTOMER'S PURPOSE OF VISIT?
#####################################################

# Loading in the data from file - hyatt_Jun_Dec_14.csv"
AnalysisDF <- read.csv("C:/Users/Ishani/Desktop/Syracuse University/Spring2016/IST 687/Project/hyatt_Jun_Dec_14.csv")
# Retaining required columns in the dataset
AnalysisDF1 <- AnalysisDF[,which(names(AnalysisDF) %in% c("POV_CODE_C","Likelihood_Recommend_H","Overall_Sat_H","Guest_Room_H","Tranquility_H","Condition_Hotel_H","Customer_SVC_H","Staff_Cared_H","Internet_Sat_H","Check_In_H","F.B_FREQ_H","NPS_Type","State_PL","Country_PL"))]

# Business Question: Purpose of visit analysis
POV <- AnalysisDF1[,c("POV_CODE_C","NPS_Type","State_PL","Country_PL")]

nrow(POV)
#Total number of records in data set - 3643250

# Removing NA's from dataset
POV <- na.omit(POV)

nrow(POV)
#Total number of records in data set after removing NA's - 3643250

#Converting column NPS_Type from factor to character to remove blanks
POV$NPS_Type <- as.character(POV$NPS_Type)

#Removing blanks
POV <- POV[which(POV$NPS_Type != ""),]

nrow(POV)
#Total number of records in data set after removing blank values - 150853

# To encode vector as factor 
POV$NPS_Type <- factor(POV$NPS_Type)

# Taking data only for United States
POV <- POV[which(POV$Country_PL == "United States"),]

# To remove duplicate elements/rows
unique(POV$State_PL)

table(POV$POV_CODE_C)
# Business - 100014
# Leisure - 20314

# Getting the count for purpose of visit
StatewiseNPS <- table(POV$State_PL,POV$NPS_Type)

StatewiseNPS <- as.data.frame.matrix(StatewiseNPS)

nrow(StatewiseNPS)

str(StatewiseNPS)

View(StatewiseNPS)

# Writung code in csv
write.csv(StatewiseNPS, file = "C:/Users/Ishani/Desktop/Syracuse University/Spring2016/IST 687/Project/StatewiseNPS.csv")

# Removing rooms with 0 frequency
StatewiseNPS <- StatewiseNPS[-which(StatewiseNPS$Detractor == 0 & StatewiseNPS$Passive == 0 & StatewiseNPS$Promoter == 0),]

# Calculating difference between promoter and detractor
StatewiseNPS$Difference <- StatewiseNPS$Promoter - StatewiseNPS$Detractor

# Calculating Frequency for each state
StatewiseNPS$Frequency <- StatewiseNPS$Detractor+StatewiseNPS$Passive+StatewiseNPS$Promoter

# Calculating NPS for each state
StatewiseNPS$NPS <- (((StatewiseNPS$Promoter/StatewiseNPS$Frequency)*100) -((StatewiseNPS$Detractor/StatewiseNPS$Frequency)*100))

# Sorting based on frequency in decreasing order
StatewiseNPS <- StatewiseNPS[order(StatewiseNPS$NPS),]

# Bar Plot for state - to study which state has least NPS
barplot(StatewiseNPS$NPS, names.arg = rownames(StatewiseNPS),col = "dodgerblue3", las=3, ylab="Net Promoter Score(%)",cex.names=.5)

# Analysis of Rhode Island - Looking for detractor
RhodeIslandAnalysis <- AnalysisDF1[which(AnalysisDF1$State_PL == "Rhode Island" & AnalysisDF1$NPS_Type=="Detractor"),]

nrow(RhodeIslandAnalysis)

str(RhodeIslandAnalysis)

RhodeIslandAnalysis <- na.omit(RhodeIslandAnalysis)

# Guest Room
table(RhodeIslandAnalysis$Guest_Room_H)
length(which(RhodeIslandAnalysis$Guest_Room_H <= 6))
length(which(RhodeIslandAnalysis$Guest_Room_H >= 9))

# Tranquility
table(RhodeIslandAnalysis$Tranquility_H)
length(which(RhodeIslandAnalysis$Tranquility_H <= 6))
length(which(RhodeIslandAnalysis$Tranquility_H >= 9))

# Hotel Condition
table(RhodeIslandAnalysis$Condition_Hotel_H)
length(which(RhodeIslandAnalysis$Condition_Hotel_H <= 6))
length(which(RhodeIslandAnalysis$Condition_Hotel_H >= 9))

# Customer Service
table(RhodeIslandAnalysis$Customer_SVC_H)
length(which(RhodeIslandAnalysis$Customer_SVC_H <= 6))
length(which(RhodeIslandAnalysis$Customer_SVC_H >= 9))

# Staff Cared
table(RhodeIslandAnalysis$Staff_Cared_H)
length(which(RhodeIslandAnalysis$Staff_Cared_H <= 6))
length(which(RhodeIslandAnalysis$Staff_Cared_H >= 9))

# Internet Service
table(RhodeIslandAnalysis$Internet_Sat_H)
length(which(RhodeIslandAnalysis$Internet_Sat_H <= 6))
length(which(RhodeIslandAnalysis$Internet_Sat_H >= 9))

# Check-in Process
table(RhodeIslandAnalysis$Check_In_H)
length(which(RhodeIslandAnalysis$Check_In_H <= 6))
length(which(RhodeIslandAnalysis$Check_In_H >= 9))

# Food and beverages
table(RhodeIslandAnalysis$F.B_FREQ_H)
length(which(RhodeIslandAnalysis$F.B_FREQ_H <= 6))
length(which(RhodeIslandAnalysis$F.B_FREQ_H >= 9))

View(POV)

# Creating columns
DrivingFactors <- c("Guest_Room_H","Tranquility_H","Condition_Hotel_H","Customer_SVC_H","Staff_Cared_H","Internet_Sat_H","Check_In_H","F.B_FREQ_H")
DetractorCount <- c(length(which(RhodeIslandAnalysis$Guest_Room_H <= 6)),length(which(RhodeIslandAnalysis$Tranquility_H <= 6)),length(which(RhodeIslandAnalysis$Condition_Hotel_H <= 6)),length(which(RhodeIslandAnalysis$Customer_SVC_H <= 6)),length(which(RhodeIslandAnalysis$Staff_Cared_H <= 6)),length(which(RhodeIslandAnalysis$Internet_Sat_H <= 6)),length(which(RhodeIslandAnalysis$Check_In_H <= 6)),length(which(RhodeIslandAnalysis$F.B_FREQ_H <= 6)))
PromoterCount <- c(length(which(RhodeIslandAnalysis$Guest_Room_H >= 9)),length(which(RhodeIslandAnalysis$Tranquility_H >= 9)),length(which(RhodeIslandAnalysis$Condition_Hotel_H >= 9)),length(which(RhodeIslandAnalysis$Customer_SVC_H >= 9)),length(which(RhodeIslandAnalysis$Staff_Cared_H >= 9)),length(which(RhodeIslandAnalysis$Internet_Sat_H >= 9)),length(which(RhodeIslandAnalysis$Check_In_H >= 9)),length(which(RhodeIslandAnalysis$F.B_FREQ_H >= 9)))

# Making data frame
drivingdf <- data.frame(DrivingFactors,DetractorCount,PromoterCount)

View(drivingdf)

# Using melt function to stack a set of columns into a single column of data
meltedDriveDf <- melt(drivingdf,id=c("DrivingFactors"))

# HeatMap
ggplot(meltedDriveDf,aes(x=variable,y=DrivingFactors))+
  geom_tile(aes(fill = value),color = "red") + 
  geom_text(aes(fill = value,label =value))+
  theme(axis.title.y = element_blank())


#####################################################
# DATA Q5 :	Distribution of customers travelling on business 
# based on the number of times they used the food and beverages in the outlets. 
#####################################################

#Creating dataframe 
Q1 <- data.frame(AnalysisDF$LENGTH_OF_STAY_R,AnalysisDF$LENGTH_OF_STAY_CATEGORY_R,AnalysisDF$LENGTH_OF_STAY_C,AnalysisDF$Language_H,
                 AnalysisDF$Guest_Room_H,AnalysisDF$Tranquility_H,AnalysisDF$Condition_Hotel_H,AnalysisDF$Staff_Cared_H,AnalysisDF$Internet_Sat_H,
                 AnalysisDF$Currency_H,AnalysisDF$Currency_PL,AnalysisDF$ROOM_TYPE_CODE_C,AnalysisDF$CC_Type_H,
                 AnalysisDF$ROOM_TYPE_CODE_R,AnalysisDF$Gender_H,AnalysisDF$Guest_Country_H,AnalysisDF$Guest_State_H,AnalysisDF$Country_PL,
                 AnalysisDF$LENGTH_OF_STAY_R,AnalysisDF$LENGTH_OF_STAY_CATEGORY_R,AnalysisDF$LENGTH_OF_STAY_C,AnalysisDF$POV_CODE_C,
                 AnalysisDF$Likelihood_Recommend_H,AnalysisDF$Guest.NPS.Goal_PL,AnalysisDF$NPS_Type,AnalysisDF$Customer_SVC_H, AnalysisDF$Overall_Sat_H,AnalysisDF$F.B_FREQ_H,
                 AnalysisDF$F.B_Overall_Experience_H,AnalysisDF$Guest.NPS.Goal_PL,AnalysisDF$State_PL,AnalysisDF$Customer_SVC_H,
                 AnalysisDF$Condition_Hotel_H)

#Filtering on the basis of Country
Q1Data<-subset(Q1, Q1$AnalysisDF.Country_PL== "United States")
str(Q1Data)
Q1datana <- na.omit(Q1Data)
str(Q1datana)
write.csv(Q1datana, file = "CleanedQ1DataNA.csv")

#united states and promotors
Q1DataNaPromotor <- Q1datana[which(Q1datana$AnalysisDF.NPS_Type == "Promoter"),]
Table1 <- data.frame(Q1DataNaPromotor,Q1DataNaPromotor$AnalysisDF.State_PL)
str(Q1DataNaPromotor)

#united states and promoters and business
Q1DataNaPromotorBusiness <- Q1DataNaPromotor[which(Q1DataNaPromotor$AnalysisDF.POV_CODE_C == "BUSINESS"),]
str(Q1DataNaPromotorBusiness)
summary(Q1DataNaPromotorBusiness$AnalysisDF.State_PL)

#united states and Passive
Q1DataNaPassive <- Q1datana[which(Q1datana$AnalysisDF.NPS_Type == "Passive"),]
str(Q1DataNaPassive)
#united states and promoters and business
Q1DataNaPassiveBusiness <- Q1DataNaPassive[which(Q1DataNaPassive$AnalysisDF.POV_CODE_C == "BUSINESS"),]
str(Q1DataNaPassiveBusiness)

#united states and Detractor
Q1DataNaDetractor <- Q1datana[which(Q1datana$AnalysisDF.NPS_Type == "Detractor"),]
str(Q1DataNaDetractor)
#united states and detractors and business
Q1DataNaDetractorBusiness <- Q1DataNaDetractor[which(Q1DataNaDetractor$AnalysisDF.POV_CODE_C == "BUSINESS"),]
str(Q1DataNaDetractorBusiness)

#Storing the data in variable 
tbfood1 <- c(Q1datana$AnalysisDF.F.B_Overall_Experience_H,Q1datana$AnalysisDF.NPS_Type,Q1datana$AnalysisDF.F.B_FREQ_H)
write.csv(tbfood1, file = "tbfood1.csv")
tbgoal1 <- c(Q1datana$AnalysisDF.Likelihood_Recommend_H,Q1datana$AnalysisDF.Guest.NPS.Goal_PL,Q1datana$AnalysisDF.State_PL,Q1datana$AnalysisDF.Guest.NPS.Goal_PL)
write.csv(tbgoal1, file = "tbgoal1.csv")
A <- read.csv('tbfood1.csv')
#Plotting graph
unique(A$AnalysisDF.F.B_Overall_Experience_H)
unique(A$AnalysisDF.NPS_Type)
unique(A$AnalysisDF.F.B_FREQ_H)
#Correlation between Likelihood to Recommendation and Overall Satisfaction
cor(A$AnalysisDF.F.B_FREQ_H, A$AnalysisDF.F.B_Overall_Experience_H)
#Scatter plot: x=Food Beverages Overall Experience, y=Food Bevereges Frequency, color=NPS type
A$AnalysisDF.F.B_FREQ_H <- A$AnalysisDF.F.B_FREQ_H + runif(nrow(A), min=0, max=.9)
A$AnalysisDF.F.B_Overall_Experience_H<- A$AnalysisDF.F.B_Overall_Experience_H + runif(nrow(A), min=0, max=.9)
g <- ggplot(data=A, aes(x=A$AnalysisDF.F.B_Overall_Experience_H))
g <- g + geom_point(aes(y=A$AnalysisDF.F.B_FREQ_H, color=A$AnalysisDF.NPS_Type))
g <- g + labs(x = "Food Beverages Overall Experience")
g <- g + labs(y = "Food Bevereges Frequency")
g <- g + labs(colour = "NPS Type")
g <- g + ggtitle("Food Beverages Overall Experience vs. Food Bevereges Frequency")
g



#####################################################
# DATA Q2 : COMAPRING AVERAGE NPS VALUE OF EACH STATE WITH ITS 
# TARGET NPS GOAL VALUE (THIS VALUE IS SET BY THE HYATT GROUP FOR EACH AND EVERY HOTEL)
#####################################################


goalData <- read.csv('tbgoal1.csv')
NPSdata <- read.csv('StatewiseNPS.csv')

# Removing rooms with 0 frequency
NPSdata <- NPSdata[-which(NPSdata$Detractor == 0 & NPSdata$Passive == 0 & NPSdata$Promoter == 0),]

# Calculating difference between promoter and detractor
NPSdata$Difference <- NPSdata$Promoter - NPSdata$Detractor

# Calculating Frequency for each state
NPSdata$Frequency <- NPSdata$Detractor+NPSdata$Passive+NPSdata$Promoter

# Calculating NPS for each state
NPSdata$NPS <- (((NPSdata$Promoter/NPSdata$Frequency)*100) -((NPSdata$Detractor/NPSdata$Frequency)*100))

# Sorting based on frequency in decreasing order
NPSdata <- NPSdata[order(NPSdata$NPS),]

NPSdata <- NPSdata[-2:-6]
require(ggplot2)
data(goalData)
# check the dataset
head(goalData)
# plot it 

str(goalData)
# Aggregating both the datasets Goal NPS and grouping it statewise
NPSGoal <- aggregate(goalData$Guest.NPS.Goal_PL ~ State_PL, goalData, mean)
# Renaming the columns of the dataframe
colnames(NPSGoal)<- c('State_PL','Goal NPS')

#Merging the dataframes 
aggData <- merge(NPSdata,NPSGoal, by.x = "X", by.y = "State_PL")
# Melting the Goal NPS and actual NPS as per each state
aggData.m <- melt(aggData, id.vars="X")
#Plotting the bar plot 
g <- ggplot(data=aggData.m, aes(x=X, y=value))
g <- g + geom_bar(aes(fill=variable), position="dodge", stat="identity")
g <- g + ggtitle("Statewise Distribution - Actual NPS vs. Goal NPS")
g <- g + theme(axis.text.x=element_text(angle=90, hjust=1))
g <- g + ylim(0,100)
g


#####################################################
# DATA Q3 : 3.	WHICH PROPERTY (GEOG. LOCATION) 
# IS MOST PREFERRED DEPENDING ON STAY, FOOD, SERVICES, MEMBERSHIP DETAILS?
#####################################################

#For Map
Q4data1 <- read.csv('/Users/KJ/Desktop/IST 687 Applied Data Science/Q4dataNew.csv')

Q4data <- Q4data1
Q4data <- Q4data1[complete.cases(Q4data1),]
colnames(Q4data) <- c("SR no", "HotelName", "HotelID","HotelCondition", "Tranquility", "LikelihoodRecommend","CustomerSVC","Region", "City", "Country", "State_PL", "NPSType")

head(Q4data)
Q4data <- Q4data[Q4data$Country == 'United States',]
Q4data <- Q4data[Q4data$NPSType == 'Detractor',] 
Q4data <- na.omit(Q4data)


Map1Data <- data.frame(Q4data$LikelihoodRecommend, Q4data$Tranquility, Q4data$HotelCondition, Q4data$CustomerSVC, Q4data$State_PL)
LikelihoodPerState <- aggregate(Q4data.LikelihoodRecommend ~ Q4data.State_PL, Map1Data, mean)
TranquilityperState <- aggregate(Q4data.Tranquility ~ Q4data.State_PL, Map1Data, mean)
HotelConditionPerState <- aggregate(Q4data.HotelCondition ~ Q4data.State_PL, Map1Data, mean)
CustomerServicePerState <- aggregate(Q4data.CustomerSVC ~ Q4data.State_PL, Map1Data, mean)

Map1DataNew <- merge(LikelihoodPerState,TranquilityperState, by.x = "Q4data.State_PL", by.y = "Q4data.State_PL")
Map2DataNew<- merge(LikelihoodPerState,HotelConditionPerState, by.x = "Q4data.State_PL", by.y = "Q4data.State_PL")
Map3DataNew<- merge(LikelihoodPerState,CustomerServicePerState, by.x = "Q4data.State_PL", by.y = "Q4data.State_PL")

Map1DataNew <- Map1DataNew[-1,]
Map1DataNew$Q4data.State_PL <- tolower(Map1DataNew$Q4data.State_PL)
Map1DataNew$latlon <- geocode(Map1DataNew$Q4data.State_PL)

Map2DataNew <- Map2DataNew[-1,]
Map2DataNew$Q4data.State_PL <- tolower(Map2DataNew$Q4data.State_PL)
Map2DataNew$latlon <- geocode(Map2DataNew$Q4data.State_PL)

Map3DataNew <- Map1DataNew[-1,]
Map3DataNew$Q4data.State_PL <- tolower(Map3DataNew$Q4data.State_PL)
Map3DataNew$latlon <- geocode(Map3DataNew$Q4data.State_PL)

US <- map_data("state")
colnames(Map1DataNew) <- c("state", "LikelihoodToRecommend","tranquility", "latlon")
colnames(Map2DataNew) <- c("state", "LikelihoodToRecommend", "hotelcondition", "latlon")
colnames(Map3DataNew) <- c("state", "LikelihoodToRecommend", "CustomerSVC", "latlon")

#U.S. map, representing the color with the Likelihood to recommend of that state
map.popColor1	<- ggplot(Map1DataNew,	aes(map_id	=	state))		
map.popColor1	<- map.popColor1+		geom_map(map	=	US,	aes(fill=Map1DataNew$LikelihoodToRecommend))
map.popColor1	<- map.popColor1	+	expand_limits(x	=	US$long,	y	=	US$lat)
map.popColor1	<- map.popColor1+	coord_map()	+	ggtitle("Likelihood to Recommend")
map.popColor1

#U.S. map, representing the color with the Tranquility Rating of that state
map.popColor1	<- ggplot(Map1DataNew,	aes(map_id	=	state))		
map.popColor1	<- map.popColor1+		geom_map(map	=	US,	aes(fill=tranquility))
map.popColor1	<- map.popColor1	+	expand_limits(x	=	US$long,	y	=	US$lat)
map.popColor1	<- map.popColor1+	coord_map()	+	ggtitle("Tranquiliy Rating")
map.popColor1


#U.S. map, representing the color with the Likelihood to recommend of that state
map.popColor2	<- ggplot(Map2DataNew,	aes(map_id	=	state))		
map.popColor2	<- map.popColor2+		geom_map(map	=	US,	aes(fill=Map2DataNew$LikelihoodToRecommend))
map.popColor2	<- map.popColor2	+	expand_limits(x	=	US$long,	y	=	US$lat)
map.popColor2	<- map.popColor2+	coord_map()	+	ggtitle("Likelihood to Recommend")
map.popColor2


#U.S. map, representing the color with the Hotel Condition Rating of that state
map.popColor2	<- ggplot(Map2DataNew,	aes(map_id	=	state))		
map.popColor2	<- map.popColor2+		geom_map(map	=	US,	aes(fill=hotelcondition))
map.popColor2	<- map.popColor2+	expand_limits(x	=	US$long,	y	=	US$lat)
map.popColor2	<- map.popColor2+	coord_map()	+	ggtitle("Hotel Condition Rating")
map.popColor2

#U.S. map, representing the color with the Likelihood to recommend of that state
map.popColor3	<- ggplot(Map3DataNew,	aes(map_id	=	state))		
map.popColor3	<- map.popColor3+		geom_map(map	=	US,	aes(fill=Map3DataNew$LikelihoodToRecommend))
map.popColor3	<- map.popColor3+	expand_limits(x	=	US$long,	y	=	US$lat)
map.popColor3	<- map.popColor3+	coord_map()	+	ggtitle("Likelihood to Recommend")
map.popColor3


#U.S. map, representing the color with the Customer Service Rating of that state
map.popColor3	<- ggplot(Map3DataNew,	aes(map_id	=	state))		
map.popColor3	<- map.popColor3+		geom_map(map	=	US,	aes(fill=CustomerSVC))
map.popColor3	<- map.popColor3	+	expand_limits(x	=	US$long,	y	=	US$lat)
map.popColor3	<- map.popColor3+	coord_map()	+	ggtitle("Customer Service Rating")
map.popColor3


#Applying Linear Modelling for the above columns to find the dependency:

lm_lik_CS<-lm(Q4data$LikelihoodRecommend~Q4data$CustomerSVC, data=Q4data)
summary(lm_lik_CS)

lm_lik_TR<-lm(Q4data$LikelihoodRecommend~Q4data$Tranquility, data=Q4data)
summary(lm_lik_TR)

lm_lik_HC<-lm(Q4data$LikelihoodRecommend~Q4data$HotelCondition, data=Q4data)
summary(lm_lik_HC)

lm_lik_ALL<-lm(Q4data$LikelihoodRecommend~Q4data$Tranquility+Q4data$HotelCondition+Q4data$CustomerSVC, data=Q4data)
summary(lm_lik_ALL)
plot(lm_lik_ALL)

#####################################################
# DATA Q4:	UNDERSTAND THE DIFFERENT REVENUE TRENDS AND PATTERNS 
# ACROSS HOTELS IN UNITED STATES. WE FIRST ANALYZED GROSS, NETT AND ROOM REVENUES, 
# LENGTH OF STAY AND LIKELIHOOD TO RECOMMEND BY AGGREGATING ALL 
# THE RECORDS AS PER EACH STATE AND THEN FINDING THE MEAN BY USING COUNT OF EACH RECORD PER STATE.
#####################################################

# Start by drilling down from bigger csv file
b1 <- read.csv('USStates.csv')
#Made Reference variable
dftest1<-b1 
#Removed NA Values
dftest <- na.omit(dftest1)
#Created subset of only US States 
USData<-subset(dftest, dftest$df.Country_PL== "United States")
#Created data table to perform aggregation functions
install.packages('data.table')
library(data.table)
#Created data table
DF<- data.table(USData)
#Created reference data frame
a<-data.frame(USData)
#Aggregating the gross revenue as per state
Gross_Rev<-aggregate(a$df.Gross_Rev_H,by=list(Category=a$df.State_PL), FUN=sum)
#Aggregating the net revenue as per state
Net_Rev<-aggregate(a$df.Net_Rev_H,by=list(Category=a$df.State_PL), FUN=sum)
#Aggregating the room revenue as per state
Room_Rev<-aggregate(a$df.Room_Rev_H,by=list(Category=a$df.State_PL), FUN=sum)
#Aggregating the length of stay as per state
Length_Stay<- aggregate(a$df.Length_Stay_H,by=list(Category=a$df.State_PL), FUN=sum)
#Using the data table to find count of states
countstate<- table(a$df.State_PL)

countstated<- as.data.frame((countstate))
countstated<-countstated[-c(1,3,6,38,41), ]
#Calculations for Gross Revenue
#Changing column names
setnames(countstated,"Var1", "State")
setnames(Gross_Rev,"Category", "State")
#Merging aggregate revenues with count of each state
net<- merge(Gross_Rev,countstated,by="State")
setnames(net,"x", "GRevenue")
#Creating a new column average with combined Revenue value and Count of states for aggregation
net$Average<-(net$GRevenue/net$Freq)
#Rounding off the values to remove decimals
net$Average<-round(net$Average)
#Creating a bar plot of the same
barplot(net$Average,col="blueviolet", names.arg=net$State,las=2.5)

#Calculations for Nett Revenue
#Changing column names
setnames(Net_Rev,"Category", "State")
#Merging aggregate revenues with count of each state
#Creating a bar plot of the same
net1<- merge(Net_Rev,countstated,by="State")
setnames(net1,"x", "NRevenue")
#Creating a new column average with combined Revenue value and Count of states for aggregation
net1$Average<-(net1$NRevenue/net1$Freq)
net1$Average<-round(net1$Average)
#Rounding off the values to remove decimals
barplot(net1$Average,names.arg=net1$State,col= "cyan4", las=2.5)

#Calculations for Room Revenue
#Changing column names
setnames(Room_Rev,"Category", "State")
#Merging aggregate revenues with count of each state
net2<- merge(Room_Rev,countstated,by="State")
setnames(net2,"x", "RRevenue")
#Creating a new column average with combined Revenue value and Count of states for aggregation
net2$Average<-(net2$RRevenue/net2$Freq)
#Rounding off the values to remove decimals
net2$Average<-round(net2$Average)
#Creating a bar plot of the same
barplot(net2$Average,names.arg=net2$State,col="sandybrown", las=2.5)

#Calculations for Length of stay
setnames(Length_Stay,"x", "Duration")
setnames(Length_Stay,"Category", "State")
#Changing column names
#Merging aggregate revenues with count of each state
net4<- merge(Length_Stay,countstated,by="State")
#Creating a new column average with combined Revenue value and Count of states for aggregation
net4$Average<-(net4$Duration/net4$Freq)
#Rounding off the values to remove decimals
net4$Average<-round(net4$Average)
#Creating a bar plot of the same
barplot(net4$Average,names.arg=net4$State, col= "deepskyblue4", las=2.5)

#Dividing gross revenue per state with length of stay
net5<- net
#Taking both data frame average
net5$GrossDay<- (net2$Average/net4$Average)
net5$GrossDay<-round(net5$GrossDay)
#Creating the barplot
barplot(net5$GrossDay,names.arg=net4$State, las=2.5)
#Plot to get aggregate data for gross vs gross/day
aggData <- merge(net4,net5, by.x = "State", by.y = "State")
#Creating new data frames
test1<-data.frame(net$State,net$Average)
test2<-data.frame(net5$State, net5$GrossDay)
setnames(test2,"net5.State","net.State")
#New similar data frames created to perform aggregate function
test<- data.frame(test1, test2)
setnames(test,"net.State.1","net.State")
setnames(test,"net.Average","GrossRevenue")
setnames(test,"net5.GrossDay","GrossRevenue/Day")
#Melting data into one variable
aggData.m <- melt(test, id.vars="net.State")
install.packages("ggplot2")
library(ggplot2)
#Code to create bi-variate bar plot
g <- ggplot(data=aggData.m, aes(x=net.State, y=value))
g <- g + geom_bar(aes(fill=variable), position="dodge", stat="identity")
g <- g + ggtitle("Gross Revenue vs Gross Revenue Per Day- Per Transaction per State")
g <- g + theme(axis.text.x=element_text(angle=90, hjust=1))
g <- g + ylim(0,2100)
g





