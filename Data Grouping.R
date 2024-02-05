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
MyTable <- Transaction_data%>%
  left_join(Neighbourhood,by=c("NEIGHBORHOOD_ID"="NEIGHBORHOOD_ID"))%>%
  left_join(Building_Class,by=c("BUILDING_CLASS_FINAL_ROLL"="X.BUILDING_CODE_ID"))%>%
  filter(TYPE=="RESIDENTIAL",
         NEIGHBORHOOD_ID==15|NEIGHBORHOOD_ID==17|NEIGHBORHOOD_ID==19
         |NEIGHBORHOOD_ID==24|NEIGHBORHOOD_ID==35,SALE_PRICE>0,GROSS_SQUARE_FEET>0)%>%
  group_by(YEAR=year(SALE_DATE))%>%
  group_by(NEIGHBORHOOD_ID)

#Analysing Data to Find Per Sqaure Feet Area
Total_SALE_PER_NEIGHBORHOOD <- summarise(MyTable,Total_Sales_Price=sum(SALE_PRICE),
                                         Area_Per_SqFt=sum(GROSS_SQUARE_FEET))%>%
  mutate(sales_price_per_sqft=Total_Sales_Price/Area_Per_SqFt)
ggplot(Total_SALE_PER_NEIGHBORHOOD,aes(x=NEIGHBORHOOD_ID,y=sales_price_per_sqft)
       )+geom_bar(stat='identity')

