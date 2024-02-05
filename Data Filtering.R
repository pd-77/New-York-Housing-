library(dplyr)
library(lubridate)
library(magrittr)
library(tidyverse)


#Bringing Data
NYC_TRANSACTION_DATA <- read_csv("/users/priyamdholiya/downloads/NYC_TRANSACTION_DATA.csv")
BUILDING_CLASS <- read_csv("/users/priyamdholiya/downloads/BUILDING_CLASS.csv")
NEIGHBORHOOD <- read_csv("/users/priyamdholiya/downloads/NEIGHBORHOOD.csv")
BOROUGH <-read_csv("/users/priyamdholiya/downloads/BOROUGH.csv")


#Filtering by Type
transactions_residential <- NYC_TRANSACTION_DATA %>%
left_join (BUILDING_CLASS, by=c("BUILDING_CLASS_FINAL_ROLL"="BUILDING_CODE_ID")) 

transactions_residential <- transactions_residential %>%
filter( TYPE =="RESIDENTIAL")

#Work on NB ID
NB_ID <- NEIGHBORHOOD %>% 
  filter(NEIGHBORHOOD_NAME == "BATHGATE")

#Finding Bathgate
BA1 <- transactions_residential %>% 
  filter(NEIGHBORHOOD_ID == NB_ID$NEIGHBORHOOD_ID)

colnames(BA1)

#Adding Sales year
sales_year <- as.Date(BA1$SALE_DATE)

sales_year  <- year(sales_year)

BA1 <- BA1 %>% mutate(SALES_YEAR = sales_year)

#Question 1 Answer
BA2 <- BA1 %>% group_by(SALES_YEAR) %>%
  summarise(avg_1_sqfet = mean(SALE_PRICE/GROSS_SQUARE_FEET, na.rm = T))

BA2

#Removing NA values.

BA1 <- BA1 %>% filter(SALE_PRICE!=0, GROSS_SQUARE_FEET!=0)

#Finding Minimum
min(BA1$SALE_PRICE)
BA1 <- BA1 %>% filter(SALE_PRICE > 10)

min(BA1$GROSS_SQUARE_FEET)


#Question 3 and 4 Answer
BA3 <- BA1 %>%
  group_by(SALES_YEAR) %>%
  summarise(avg_1_sqfet = mean(SALE_PRICE/GROSS_SQUARE_FEET, na.rm = T))

BA3

#Working for All Neighborhoods 

neighbors <- NEIGHBORHOOD %>%
  left_join(NB_ID, by="BOROUGH_ID") %>%
  filter(BOROUGH_ID==NB_ID$BOROUGH_ID)

neighbors$NEIGHBORHOOD_NAME.x

colnames(NYC_TRANSACTION_DATA)

transactions <- transactions_residential %>% 
  mutate(NEIGHBORHOOD_SALES_YEAR = year(as.Date(transactions_residential$SALE_DATE)))%>% 
  filter(SALE_PRICE>50)

NB_ID_Bathgate <- neighbors %>% filter(NEIGHBORHOOD_NAME.x=="BATHGATE")
NB_ID_Baychester <- neighbors %>% filter(NEIGHBORHOOD_NAME.x=="BAYCHESTER")
NB_ID_Bedford <- neighbors %>% filter(NEIGHBORHOOD_NAME.x=="BEDFORD PARK/NORWOOD")
NB_ID_Belmont <- neighbors %>% filter(NEIGHBORHOOD_NAME.x=="BELMONT")
NB_ID_Bronx_Park <- neighbors %>% filter(NEIGHBORHOOD_NAME.x=="BRONX PARK")


#Result of "BATHGATE"
transaction_Bathgate <- transactions %>%
  filter(NEIGHBORHOOD_ID == NB_ID_Bathgate$NEIGHBORHOOD_ID.x) %>%
  filter(SALE_PRICE!=0, GROSS_SQUARE_FEET!=0) %>%
  group_by(NEIGHBORHOOD_SALES_YEAR) %>%
  summarise(avg_1_sqfet = mean(SALE_PRICE/GROSS_SQUARE_FEET, na.rm = T))

transaction_Bathgate

#Result of "BAYCHESTER"
transaction_Baychester <- transactions %>%
  filter(NEIGHBORHOOD_ID == NB_ID_Baychester$NEIGHBORHOOD_ID.x) %>%
  filter(SALE_PRICE!=0, GROSS_SQUARE_FEET!=0) %>%
  group_by(NEIGHBORHOOD_SALES_YEAR) %>%
  summarise(avg_1_sqfet = mean(SALE_PRICE/GROSS_SQUARE_FEET, na.rm = T))

transaction_Baychester

#Result of "BEDFORD PARK/NORWOOD"
transaction_Bedford <- transactions %>%
  filter(NEIGHBORHOOD_ID == NB_ID_Bedford$NEIGHBORHOOD_ID.x) %>%
  filter(SALE_PRICE!=0, GROSS_SQUARE_FEET!=0) %>%
  group_by(NEIGHBORHOOD_SALES_YEAR) %>%
  summarise(avg_1_sqfet = mean(SALE_PRICE/GROSS_SQUARE_FEET, na.rm = T))

transaction_Bedford

#Result of "BELMONT"
transaction_Belmont <- transactions %>%
  filter(NEIGHBORHOOD_ID == NB_ID_Belmont$NEIGHBORHOOD_ID.x) %>%
  filter(SALE_PRICE!=0, GROSS_SQUARE_FEET!=0) %>%
  group_by(NEIGHBORHOOD_SALES_YEAR) %>%
  summarise(avg_1_sqfet = mean(SALE_PRICE/GROSS_SQUARE_FEET, na.rm = T))

transaction_Belmont

#Result of "BRONX PARK"
transaction_Bronx_Park <- transactions %>%
  filter(NEIGHBORHOOD_ID == NB_ID_Bronx_Park$NEIGHBORHOOD_ID.x) %>%
  filter(SALE_PRICE!=0, GROSS_SQUARE_FEET!=0) %>%
  group_by(NEIGHBORHOOD_SALES_YEAR) %>%
  summarise(avg_1_sqfet = mean(SALE_PRICE/GROSS_SQUARE_FEET, na.rm = T))

transaction_Bronx_Park

#Plotting for Question 6

ggplot() + geom_line(data=transaction_Bedford, aes(x= NEIGHBORHOOD_SALES_YEAR,y= avg_1_sqfet), colour = "red") +
  geom_line(data=transaction_Bathgate, aes(x= NEIGHBORHOOD_SALES_YEAR,y= avg_1_sqfet), colour = "blue") +
  geom_line(data=transaction_Baychester, aes(x= NEIGHBORHOOD_SALES_YEAR,y= avg_1_sqfet), colour = "orange")+
  geom_line(data=transaction_Belmont, aes(x= NEIGHBORHOOD_SALES_YEAR,y= avg_1_sqfet), colour = "green")+
  geom_line(data=transaction_Bronx_Park, aes(x= NEIGHBORHOOD_SALES_YEAR,y= avg_1_sqfet), colour = "gray")+
  xlab("Sales Year") + ylab("Average price per Square Foot")
