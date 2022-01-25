rm(list=ls())
install.packages("quantmod")
install.packages("ggplot2")
install.packages("tidyquant")
install.packages("tidyverse")
install.packages("dplyr")
library(quantmod)
library(ggplot2)
library(tidyquant)
library(tidyverse)
library(dplyr)
library(COVID19)

#Uses the COVID19 library
covid2 <- covid19()
covid2 <- subset(covid19(), administrative_area_level_1 == "United States")
covid2 <- select(covid2, id, date, confirmed)
covid2$daily <- ave(covid2$confirmed, covid2$id, FUN=function(x) c(0, diff(x)))
covid2 %>%
  group_by(id) %>%
  arrange(date, .by_group = TRUE) %>%
  mutate(daily = confirmed - lag(confirmed, default = first(confirmed)))
covid2 %>%
  ggplot(aes(x = date, y = daily, group=1)) +
  ggtitle("Daily Confirmed Cases of COVID-19 in the United States") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_date(date_labels = "%b %y", date_breaks = "1 month") +
  geom_line()



#Test, not used for project
tsla <- getSymbols("TSLA", src = "yahoo", from = "2020-01-01", to = "2020-12-31", auto.assign = FALSE)
pbr <- getSymbols("PBR", src = "yahoo", from = "2020-01-01", to = "2020-12-31", auto.assign = FALSE)

#Not necessary to run
#nasdaq <- tq_exchange("NASDAQ")







### IMPORTANT ###
#This next block of code no longer functions correctly. This is because the NASDAQ dataset
#has been changed since I worked on this project. It no longer has a "sector" variable, which was
#replaced by "Industry" variable. It not an easy fix because the "sector" variable had high
#level categories such Finance, Health Care, etc. The industry variable has more detailed
#categories that makes just swapping the variable names no possible. In the future, since this
#is historical data, I will download the portion of the dataset that I used so that the code
#will continue to function correctly if the dataset is changed in anyway
###

#Creates a data set for each respective sector to get tickers
nasdaq_finance <- subset(tq_exchange("NASDAQ"), sector == "Finance")
nasdaq_healthcare <- subset(tq_exchange("NASDAQ"), sector == "Health Care")
nasdaq_energy <- subset(tq_exchange("NASDAQ"), sector == "Energy")
nasdaq_technology <- subset(tq_exchange("NASDAQ"), sector == "Technology")
nasdaq_transportation <- subset(tq_exchange("NASDAQ"), sector == "Transportation")

#Creates stock price data set for each respective sector
nasdaq_finance_prices2 <- tq_get(nasdaq_finance[,1], from = "2019-01-01", to = "2020-12-31", get = "stock.prices")
nasdaq_healthcare_prices2 <- tq_get(nasdaq_healthcare[,1], from = "2019-01-01", to = "2020-12-31", get = "stock.prices")
nasdaq_energy_prices2 <- tq_get(nasdaq_energy[,1], from = "2019-01-01", to = "2020-12-31", get = "stock.prices")
nasdaq_technology_prices2 <- tq_get(nasdaq_technology[,1], from = "2019-01-01", to = "2020-12-31", get = "stock.prices")
nasdaq_transportation_prices2 <- tq_get(nasdaq_transportation[,1], from = "2019-01-01", to = "2020-12-31", get = "stock.prices")

#Creates data set that gives mean of each sector based off of adjusted price
nasdaq_finance_prices_mean <- aggregate(adjusted ~ date, nasdaq_finance_prices, mean)
nasdaq_healthcare_prices_mean <- aggregate(adjusted ~ date, nasdaq_healthcare_prices, mean)
nasdaq_energy_prices_mean <- aggregate(adjusted ~ date, nasdaq_energy_prices, mean)
nasdaq_technology_prices_mean <- aggregate(adjusted ~ date, nasdaq_technology_prices, mean)
nasdaq_transportation_prices_mean <- aggregate(adjusted ~ date, nasdaq_transportation_prices, mean)

#Creates additional variable labeled sector and populates the correct sector for each data set
nasdaq_finance_prices_mean <- nasdaq_finance_prices_mean %>%
  mutate(sector = "finance")
nasdaq_healthcare_prices_mean <- nasdaq_healthcare_prices_mean %>%
  mutate(sector = "healthcare")
nasdaq_energy_prices_mean <- nasdaq_energy_prices_mean %>%
  mutate(sector = "energy")
nasdaq_technology_prices_mean <- nasdaq_technology_prices_mean %>%
  mutate(sector = "technology")
nasdaq_transportation_prices_mean <- nasdaq_transportation_prices_mean %>%
  mutate(sector = "transportation")


#Merges the mean data sets based off of sector into one data set
nasdaq_prices_mean_merged <- do.call("rbind", list(nasdaq_finance_prices_mean, nasdaq_healthcare_prices_mean, nasdaq_energy_prices_mean, nasdaq_technology_prices_mean, nasdaq_transportation_prices_mean))
first_confirmed_case <- subset(nasdaq_prices_mean_merged, date == "2020-01-22")
first_spike_start <- subset(nasdaq_prices_mean_merged, date == "2020-03-02")
first_spike_end <- subset(nasdaq_prices_mean_merged, date == "2020-04-09")
Second_spike_start <- subset(nasdaq_prices_mean_merged, date == "2020-06-08")
Second_spike_end <- subset(nasdaq_prices_mean_merged, date == "2020-07-24")
third_spike_start <- subset(nasdaq_prices_mean_merged, date == "2020-10-01")
current <- subset(nasdaq_prices_mean_merged, date == "2020-11-27")


#Creates subsets for the spikes
spike1 <- subset(nasdaq_prices_mean_merged, date >= "2020-03-01" & date <= "2020-04-20")
spike2 <- subset(nasdaq_prices_mean_merged, date >= "2020-06-01" & date < "2020-08-01")
spike3 <- subset(nasdaq_prices_mean_merged, date >= "2020-09-20")


spike1energy <- subset(spike1, sector == "energy")
spike1finance <- subset(spike1, sector == "finance")
spike1healthcare <- subset(spike1, sector == "healthcare")
spike1technology <- subset(spike1, sector == "technology")
spike1transportation <- subset(spike1, sector == "transportation")

spike2energy <- subset(spike2, sector == "energy")
spike2finance <- subset(spike2, sector == "finance")
spike2healthcare <- subset(spike2, sector == "healthcare")
spike2technology <- subset(spike2, sector == "technology")
spike2transportation <- subset(spike2, sector == "transportation")

spike3energy <- subset(spike3, sector == "energy")
spike3finance <- subset(spike3, sector == "finance")
spike3healthcare <- subset(spike3, sector == "healthcare")
spike3technology <- subset(spike3, sector == "technology")
spike3transportation <- subset(spike3, sector == "transportation")


#Plot all time-series means on one graph
nasdaq_prices_mean_merged %>%
  ggplot(aes(x = date, y = adjusted, color = sector)) +
  ggtitle("Mean Adjusted Price for Each Sector") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_date(date_labels = "%b %y", date_breaks = "3 months") +
  geom_line() +
  geom_vline(xintercept = as.numeric(nasdaq_prices_mean_merged$date[c(120, 157, 241, 296)]))


#Spike1 Plot
spike1 %>%
  ggplot(aes(x = date, y = adjusted, color = sector)) +
  ggtitle("Mean Adjusted Price for Each Sector - First Spike of COVID-19") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_date(date_labels = "%b %y", date_breaks = "1 week") +
  geom_line() +
  geom_vline(xintercept = as.numeric(spike1$date[c(11, 26)]), show.legend = T)
#Spike1 Standard Deviation
sd(spike1energy$adjusted)
sd(spike1finance$adjusted)
sd(spike1healthcare$adjusted)
sd(spike1technology$adjusted)
sd(spike1transportation$adjusted)


#Spike2 Plot
spike2 %>%
  ggplot(aes(x = date, y = adjusted, color = sector)) +
  ggtitle("Mean Adjusted Price for Each Sector - Second Spike of COVID-19") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_date(date_labels = "%b %y", date_breaks = "1 week") +
  geom_line() +
  geom_vline(xintercept = as.numeric(spike2$date[c(11, 33)]), show.legend = T)
#Spike2 Standard Deviation
sd(spike2energy$adjusted)
sd(spike2finance$adjusted)
sd(spike2healthcare$adjusted)
sd(spike2technology$adjusted)
sd(spike2transportation$adjusted)


#Spike3 Plot
spike3 %>%
  ggplot(aes(x = date, y = adjusted, color = sector)) +
  ggtitle("Mean Adjusted Price for Each Sector - Current Spike of COVID-19") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_date(date_labels = "%b %y", date_breaks = "1 week") +
  geom_line() +
  geom_vline(xintercept = as.numeric(spike3$date[c(9, 48)]), show.legend = T)
#Spike 3 Standard Deviation
sd(spike3energy$adjusted)
sd(spike3finance$adjusted)
sd(spike3healthcare$adjusted)
sd(spike3technology$adjusted)
sd(spike3transportation$adjusted)


#Plot of times series for all prices for each sector
nasdaq_finance_prices_mean %>%
  ggplot(aes(x = date, y = adjusted)) +
  geom_line()

nasdaq_healthcare_prices_mean %>%
  ggplot(aes(x = date, y = adjusted)) +
  geom_line()

nasdaq_energy_prices_mean %>%
  ggplot(aes(x = date, y = adjusted)) +
  geom_line()

nasdaq_technology_prices_mean %>%
  ggplot(aes(x = date, y = adjusted)) +
  geom_line()

nasdaq_transportation_prices_mean %>%
  ggplot(aes(x = date, y = adjusted)) +
  geom_line()


sd(spike1$adjusted)
sd(spike2$adjusted)
sd(spike3$adjusted)

#Standard Deviations
sd(nasdaq_finance_prices_mean$adjusted)
summary(nasdaq_prices_mean_merged)
summary(nasdaq_finance_prices_mean)

#Slice data to see first row of each symbol, not necessary to run
nasdaq_energy_prices %>%
  group_by(symbol) %>%
  slice(1)



