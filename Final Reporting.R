library(odbc)
library(DBI)
library(tidyverse)
library(lubridate)
library(ggplot2)
library(forecast)
library(fpp3)


NYC_TRANSACTION_DATA <- read_csv("/users/priyamdholiya/downloads/NYC_TRANSACTION_DATA.csv")
BUILDING_CLASS <- read_csv("/users/priyamdholiya/downloads/BUILDING_CLASS.csv")
NEIGHBORHOOD <- read_csv("/users/priyamdholiya/downloads/NEIGHBORHOOD.csv")
BOROUGH <-read_csv("/users/priyamdholiya/downloads/BOROUGH.csv")

#Figuring High Low Neighborhood = Borough 2
data = inner_join(NYC_TRANSACTION_DATA, 
                  NEIGHBORHOOD %>% filter(BOROUGH_ID == 2),
                  by = "NEIGHBORHOOD_ID") %>% 
  filter(year(SALE_DATE) >= 2011) 

HL_DATA_BOR.2 = data %>% 
  filter(NEIGHBORHOOD_ID %in% c(15, 17, 19, 24, 35)) %>% 
  filter(RESIDENTIAL_UNITS > 0) %>% 
  group_by(NEIGHBORHOOD_ID) %>% 
  mutate(transactions = n()) %>% 
  ungroup() %>% 
  filter(transactions %in% c(min(transactions), max(transactions))) %>% 
  mutate(volume = ifelse(transactions == max(transactions), "High", "Low"))

#_________________________________________________________________________
#PART 1
#Filtered Table with High Low Data of Neighborhoods
#(Total Dollar Monthly Sales)
NB_HIGH_LOW<-NYC_TRANSACTION_DATA %>%
  left_join(NEIGHBORHOOD,by=c("NEIGHBORHOOD_ID"="NEIGHBORHOOD_ID"))%>%
  left_join(BUILDING_CLASS,by=c("BUILDING_CLASS_FINAL_ROLL"="BUILDING_CODE_ID"))%>%
  filter(NEIGHBORHOOD_ID==c(17,35),TYPE=='RESIDENTIAL',SALE_PRICE>0,GROSS_SQUARE_FEET>0)%>%
  mutate(YEAR=year(SALE_DATE),)%>%
  mutate(MONTH=month(SALE_DATE))%>%
  filter(YEAR>2011)%>%
  mutate(Quarter = quarter(SALE_DATE))%>%
  group_by(NEIGHBORHOOD_NAME, NEIGHBORHOOD_ID,Quarter,MONTH,YEAR)%>%
  summarise(TotalSales=sum(SALE_PRICE))

NB_HIGH_LOW[is.na(NB_HIGH_LOW)]=0

#Neighborhood 17 Summary
H_17_NB<-NB_HIGH_LOW%>%
  mutate(NEIGHBORHOOD_NAME)%>%
  filter(NEIGHBORHOOD_ID==17)%>%
  summarise(TotalSales,YEAR)%>%
  mutate(Quarter)
H_17_NB$pd<-1:120
m.17<-lm(formula=TotalSales~pd,
      data=H_17_NB)
summary(m.17)

#Neighborhood 35 Summary
L_35_NB<-NB_HIGH_LOW%>%
  mutate(NEIGHBORHOOD_NAME)%>%
  mutate(YEAR)%>%
  filter(NEIGHBORHOOD_ID==35)%>%
  summarise(TotalSales,YEAR)%>%
  mutate(Quarter)
L_35_NB$pd<-1:4
m.35<-lm(formula=TotalSales~pd,
      data=L_35_NB)
summary(m.35)

#Making Regression Models
regmodel17<-lm(H_17_NB,formula=TotalSales~YEAR+Quarter)
summary(regmodel17)

regmodel35<-lm(L_35_NB,formula=TotalSales~YEAR+Quarter)
summary(regmodel35)


#Plotting the Graph BAYCHESTER
plot(H_17_NB$pd,H_17_NB$TotalSales)
#No Trend , only additive seasonality (Hence running MNA)

ggplot(H_17_NB,aes(x=H_17_NB$pd,y=H_17_NB$TotalSales))+geom_line()
ggplot(H_17_NB, aes(x = H_17_NB$pd, y =H_17_NB$TotalSales) ) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)

time_line17<-ts(data=H_17_NB$TotalSales,start=2011,end = 2022,frequency=4)

r17<-ets(y=time_line17,model="MNA")
plot(r17)

NB_17_FC <- forecast(r17,8)
plot(r17)

plot(NB_17_FC)
#___________________________________________________
#Plotting the Graph for Bronx Park
plot(L_35_NB$pd,L_35_NB$TotalSales)
#Trend Available, but no seasonality, hence letting the function decide the model
#R studio choses the ANA model, showing trend.
ggplot(L_35_NB,aes(x=L_35_NB$pd,y=L_35_NB$TotalSales))+geom_line()
ggplot(L_35_NB, aes(x = L_35_NB$pd, y =L_35_NB$TotalSales) ) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)

time_line35<-ts(data=L_35_NB$TotalSales,start=2011,end = 2021,frequency=4)

r35<-ets(y=time_line35)
plot(r35)

NB_35_FC <- forecast(r35,8)
plot(r35)

plot(NB_35_FC)

#__________________________________________________________________________________
#PART 2
HL_DATA_BOR.2_ts = HL_DATA_BOR.2 %>% 
  mutate(date = yearmonth(format(SALE_DATE, "%Y-%m"))) %>% 
  group_by(date, NEIGHBORHOOD_NAME) %>% 
  summarise(sales = sum(SALE_PRICE), .groups = "drop")

fit = HL_DATA_BOR.2_ts %>%
  filter(NEIGHBORHOOD_NAME == "BAYCHESTER") %>%
  as_tsibble(index=date) %>%
  model(fit1 = TSLM(sales ~ trend()),
        fit2 = TSLM(sales ~ trend() + season()))
report(fit$fit1[[1]])
report(fit$fit2[[1]])


fit = HL_DATA_BOR.2_ts %>%
  filter(NEIGHBORHOOD_NAME == "BRONX PARK") %>%
  as_tsibble(index=date) %>%
  model(fit1 = TSLM(sales ~ trend()),
        fit2 = TSLM(sales ~ trend() + season()))
report(fit$fit1[[1]])
report(fit$fit2[[1]])

#________________________________________________________________________________
#PART 3
fit = lm(SALE_PRICE ~ SALE_DATE + YEAR_BUILT + BUILDING_CLASS_FINAL_ROLL + 
           GROSS_SQUARE_FEET + RESIDENTIAL_UNITS,
         data = HL_DATA_BOR.2)
summary(fit)

car::vif(fit)

HL_DATA_BOR.2$predicted_price = predict(fit, HL_DATA_BOR.2)
HL_DATA_BOR.2[which.max(HL_DATA_BOR.2$predicted_price-HL_DATA_BOR.2$SALE_PRICE),] %>% 
  select(SALE_ID, SALE_PRICE, predicted_price)
HL_DATA_BOR.2[which.min(HL_DATA_BOR.2$predicted_price-HL_DATA_BOR.2$SALE_PRICE),] %>% 
  select(SALE_ID, SALE_PRICE, predicted_price)



