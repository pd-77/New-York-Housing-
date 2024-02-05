
#Loading the Packages

library(odbc)
library(DBI)
library(tidyverse)
library(lubridate)

#Connecting to SQL Server
alldata<- dbConnect(odbc(),
                    Driver = "sql server",
                    Server = "met-sql19.bu.edu",
                    Database ="NYC real estate",
                    Port = 1433)

#Bringing Tables to Environment Pane              
Neighbourhood <- dbReadTable(alldata,"NEIGHBORHOOD")
Borough <- dbReadTable(alldata,"BOROUGH")
Building_Class <- dbReadTable(alldata,"BUILDING_CLASS")
Transaction_data <- dbReadTable(alldata,"NYC_TRANSACTION_DATA")

#Joining Tables & Filtering
MyTable_Residential <- Transaction_data%>%
  left_join(Neighbourhood,by=c("NEIGHBORHOOD_ID"="NEIGHBORHOOD_ID"))%>%
  left_join(Building_Class,by=c("BUILDING_CLASS_FINAL_ROLL"="X.BUILDING_CODE_ID"))%>%
  filter(TYPE=="RESIDENTIAL",
         NEIGHBORHOOD_ID==15|NEIGHBORHOOD_ID==17|NEIGHBORHOOD_ID==19
         |NEIGHBORHOOD_ID==24|NEIGHBORHOOD_ID==35,SALE_PRICE>0,GROSS_SQUARE_FEET>0)%>%
  group_by(YEAR=year(SALE_DATE))%>% 
  filter((YEAR>2010&YEAR<2021))%>%
  group_by(NEIGHBORHOOD_ID)

#Finding sale for Each Neighborhood
Total_SALE_PER_NEIGHBORHOOD <- summarise(MyTable_Residential,Total_Sales_Price=sum(SALE_PRICE),
                                         Area_Per_SqFt=sum(GROSS_SQUARE_FEET),
                                         Mean_per_NB=mean(SALE_PRICE),Standard_Deviation=sd(SALE_PRICE),
                                         Mean_SqFT=mean(GROSS_SQUARE_FEET))%>%
  mutate(sales_price_per_sqft=Total_Sales_Price/Area_Per_SqFt)

#Descriptive Summary of Each Neighborhood
Neighborhood15 <- MyTable_Residential%>%
  filter(NEIGHBORHOOD_ID==15)
summary(Neighborhood15$SALE_PRICE)
summary(Neighborhood15$GROSS_SQUARE_FEET)

Neighborhood17 <- MyTable_Residential%>%
  filter(NEIGHBORHOOD_ID==17)
summary(Neighborhood17$SALE_PRICE)
summary(Neighborhood17$GROSS_SQUARE_FEET)

Neighborhood19 <- MyTable_Residential%>%
  filter(NEIGHBORHOOD_ID==19)
summary(Neighborhood19$SALE_PRICE)
summary(Neighborhood19$GROSS_SQUARE_FEET)

Neighborhood24 <- MyTable_Residential%>%
  filter(NEIGHBORHOOD_ID==24)
summary(Neighborhood24$SALE_PRICE)
summary(Neighborhood24$GROSS_SQUARE_FEET)

Neighborhood35<- MyTable_Residential%>%
  filter(NEIGHBORHOOD_ID==35)
summary(Neighborhood35$SALE_PRICE)
summary(Neighborhood35$GROSS_SQUARE_FEET)

#Distribution of Properties Sold
#Joining Tables & Filtering
Distribution_Property_Type <- Transaction_data%>%
  left_join(Neighbourhood,by=c("NEIGHBORHOOD_ID"="NEIGHBORHOOD_ID"))%>%
  left_join(Building_Class,by=c("BUILDING_CLASS_FINAL_ROLL"="X.BUILDING_CODE_ID"))%>%
  filter(NEIGHBORHOOD_ID==15|NEIGHBORHOOD_ID==17|NEIGHBORHOOD_ID==19
         |NEIGHBORHOOD_ID==24|NEIGHBORHOOD_ID==35,SALE_PRICE>0,GROSS_SQUARE_FEET>0)%>%
  group_by(YEAR=year(SALE_DATE))%>% 
  filter((YEAR>2010&YEAR<2021))%>%
  group_by(NEIGHBORHOOD_ID)%>%
  group_by(TYPE)%>%
  summarise(proportions=n())


#Finding Corelations 
cor(Neighborhood15$SALE_PRICE,Neighborhood15$GROSS_SQUARE_FEET)
cor(Neighborhood17$SALE_PRICE,Neighborhood17$GROSS_SQUARE_FEET)
cor(Neighborhood19$SALE_PRICE,Neighborhood19$GROSS_SQUARE_FEET)
cor(Neighborhood24$SALE_PRICE,Neighborhood24$GROSS_SQUARE_FEET)
cor(Neighborhood35$SALE_PRICE,Neighborhood35$GROSS_SQUARE_FEET)


#Finding KPI for All Neighborhoods

Borough2<- Transaction_data%>%
  left_join(Neighbourhood,by=c("NEIGHBORHOOD_ID"="NEIGHBORHOOD_ID"))%>%
  left_join(Building_Class,by=c("BUILDING_CLASS_FINAL_ROLL"="X.BUILDING_CODE_ID"))%>%
  filter(BOROUGH_ID==2,SALE_PRICE>0,GROSS_SQUARE_FEET>0)%>%
  group_by(YEAR=year(SALE_DATE))%>% 
  filter((YEAR>2010&YEAR<2021))%>%
  group_by(NEIGHBORHOOD_ID)

Borough2KPI <- Borough2%>%
  summarise(TotalUnits=n(),MedianSalePrice=median(SALE_PRICE),SD_of_Sales=sd(SALE_PRICE),
            SumOfUnit=sum(RESIDENTIAL_UNITS),PricePerSqft=sum(SALE_PRICE/sum(GROSS_SQUARE_FEET)))

#Finding Z Score
zscores <- scale(Borough2KPI[c(-1)]) %>%
  as.data.frame()
zscores <- replace(zscores,is.na(zscores),0)
k <- kmeans(zscores,centers=4)
k_means <- cbind(Borough2KPI,k$cluster)
ggplot(k_means) +geom_point(mapping=aes(x=MedianSalePrice,y=TotalUnits,size=PricePerSqft,color = k$cluster))
#My Neighborhood cluster are 3,4,1,3,3


#T-Test for Neighborhoods
t.test(x=Neighborhood15$SALE_PRICE,y = Neighborhood17$SALE_PRICE,alternative = "t",conf.level = 0.95)
t.test(x=Neighborhood19$SALE_PRICE,y = Neighborhood24$SALE_PRICE,alternative = "t",conf.level = 0.95)
t.test(x=Neighborhood15$SALE_PRICE,y = Neighborhood35$SALE_PRICE,alternative = "t",conf.level = 0.95)
















