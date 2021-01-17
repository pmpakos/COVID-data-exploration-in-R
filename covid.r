library(data.table)
library(tidyr)
library(lubridate)
library(dplyr)

# 0 - read data from csv file
confirmed_url <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv"
confirmed_dt <- fread(confirmed_url)
deaths_url <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv"
deaths_dt <- fread(deaths_url)

dim(confirmed_dt)
colnames(confirmed_dt)

# 1 - get rid of columns Province/State, Lat, Long
confirmed_dt[, ":="("Province/State"=NULL,"Lat"=NULL, "Long"=NULL)]
deaths_dt[, ":="("Province/State"=NULL,"Lat"=NULL, "Long"=NULL)]

# 2 - convert data from wide to long format
# we need to get the column names from 2nd and beyond, which are the dates that
# will be used as "condition" in long format
coln_c <- colnames(confirmed_dt)
coln_d <- colnames(deaths_dt)
# gather command converts wide to long format, by setting key and value names (date, count respectively)
# http://www.cookbook-r.com/Manipulating_data/Converting_data_between_wide_and_long_format/
# I need to convert it back to data.table format, with 
confirmed_dt <- as.data.table(gather(confirmed_dt, date, count, coln_c[2:length(coln_c)]))
confirmed_dt
deaths_dt <- as.data.table(gather(deaths_dt, date, count, coln_d[2:length(coln_d)]))
deaths_dt

# 3 - change column name Country/Region to Country
setnames(confirmed_dt, "Country/Region", "Country")
setnames(deaths_dt, "Country/Region", "Country")

# 4 - change "count" variable that was set previously in step 2 to "confirmed" or "deaths"
setnames(confirmed_dt, "count", "confirmed")
setnames(deaths_dt, "count", "deaths")

# 5 - convert variable "date" from character to date object
#check datatype of date
str(confirmed_dt$date)
# indeed it is character of type "month/day/year"

# use mdy function from lubridate
# https://stackoverflow.com/questions/4310326/convert-character-to-date-in-r/4310474
confirmed_dt$date <- mdy(confirmed_dt$date)
deaths_dt$date <- mdy(deaths_dt$date)

str(confirmed_dt$date)
# now it is of type Date

# 6 - group by country and date
# firstly, we define the two columns that we will group by. then, we summarise confirmed or deaths, so that
# cases from all regions of this country at the specific date are accumulated
# https://uc-r.github.io/pipe
accum_confirmed_dt <- as.data.table(confirmed_dt %>% group_by(Country,date) %>% summarise(confirmed = sum(confirmed),.groups="keep"))
accum_deaths_dt <- as.data.table(deaths_dt %>% group_by(Country,date) %>% summarise(deaths = sum(deaths),.groups="keep"))

dim(confirmed_dt)
dim(accum_confirmed_dt)
# as a result a smaller data table occurs

# 7 - merge two datasets in one
confirmed_and_deaths_dt <- merge(accum_confirmed_dt, accum_deaths_dt)

# 8 - total count of confirmed and deaths
# since counts of confirmed and deaths are cumulative, we have to sum only
# counts of confirmed and deaths on the last date of data.table for all countries

# find last date in dataset (we have converted to date object, so max can be used)
last_date <- max(confirmed_and_deaths_dt$date)

# get rows of datatable of specific date (all columns)
last_date_rows <- confirmed_and_deaths_dt[confirmed_and_deaths_dt$date==last_date,]

confirmed_last_date <- sum(last_date_rows$confirmed)
confirmed_last_date
deaths_last_date <- sum(last_date_rows$deaths)
deaths_last_date

# 9 - sort by country and date 
# it was already sorted by the previous step (#6), but prsented here
# https://stackoverflow.com/questions/50282341/how-to-sort-a-data-table-using-vector-of-multiple-columns
setorderv(confirmed_and_deaths_dt, c("Country","date"))

# 10 - confirmed.ind and deaths.ind are the daily confirmed and deaths respectively (with lag function)
confirmed_and_deaths_dt <- confirmed_and_deaths_dt %>% group_by(Country) %>% mutate(confirmed.ind = confirmed - lag(confirmed, k=1))
confirmed_and_deaths_dt <- confirmed_and_deaths_dt %>% group_by(Country) %>% mutate(deaths.inc = deaths - lag(deaths, k=1))

day1 = min(confirmed_and_deaths_dt$date)
confirmed_and_deaths_dt[confirmed_and_deaths_dt$date==day1, 5] <- confirmed_and_deaths_dt[confirmed_and_deaths_dt$date==day1, 3]
confirmed_and_deaths_dt[confirmed_and_deaths_dt$date==day1, 6] <- confirmed_and_deaths_dt[confirmed_and_deaths_dt$date==day1, 4]
confirmed_and_deaths_dt <- as.data.table(confirmed_and_deaths_dt)

############################################################################################################################################################
############################################################################################################################################################

############################################################################################################################################################
# exploratory data analysis
############################################################################################################################################################
library(tidyverse)
library(countrycode)
library(scales)
library(hrbrthemes)
library(zoo)
library(ggrepel)

width = 30
height = 15

# for some days there are negative deaths.inc and confirmd.ind stats, which means that maybe a correction to stats of previous
# days was performed (wrongly reported data that were added and then subtracted from cumulative stats)
negative_confirmed.ind <- dim(confirmed_and_deaths_dt[confirmed_and_deaths_dt$confirmed.ind<0,]) # 51 cases
negative_deaths.inc <- dim(confirmed_and_deaths_dt[confirmed_and_deaths_dt$deaths.inc<0,]) # 72 cases

################################################################################
# first of all, get all countries that we have data of
countries <- unique(confirmed_and_deaths_dt$Country)
length(countries)
# we have data for 191 countries (not all are countries!!)

# Get continent name for each country in dataset
# https://stackoverflow.com/questions/47510141/get-continent-name-from-country-name-in-r
confirmed_and_deaths_dt$continent = countrycode(sourcevar = confirmed_and_deaths_dt$Country, origin = "country.name", destination = "continent")

# Gives warning that not all rows are assigned to continent.
# Princess Diamond and MS Zaandam are cruise ships that were hit from covid
# Kosovo is in Europe
confirmed_and_deaths_dt$continent[confirmed_and_deaths_dt$Country=="Diamond Princess"] = "overseas"
confirmed_and_deaths_dt$continent[confirmed_and_deaths_dt$Country=="MS Zaandam"] = "overseas"
confirmed_and_deaths_dt$continent[confirmed_and_deaths_dt$Country=="Kosovo"] = "Europe"

continent_stats <- as.data.table(confirmed_and_deaths_dt %>% group_by(continent))
continent_stats[, ":="("Country"=NULL)]

continent_confirmed <- as.data.table(continent_stats %>% group_by(continent,date) %>% summarise(confirmed = sum(confirmed),.groups="keep"))
continent_deaths <- as.data.table(continent_stats %>% group_by(continent,date) %>% summarise(deaths = sum(deaths),.groups="keep"))
continent_confirmed.ind <- as.data.table(continent_stats %>% group_by(continent,date) %>% summarise(confirmed.ind = sum(confirmed.ind),.groups="keep"))
continent_deaths.inc <- as.data.table(continent_stats %>% group_by(continent,date) %>% summarise(deaths.inc = sum(deaths.inc),.groups="keep"))
continent_confirmed_and_deaths_dt <- merge(merge(continent_confirmed, continent_deaths), merge(continent_confirmed.ind, continent_deaths.inc))

# let's get data for Greece and explore them
cont1 = "Asia"
cont2 = "Europe"
cont3 = "Africa"
cont4 = "Americas"
cont5 = "Oceania"

cont1_data <- continent_confirmed_and_deaths_dt[continent_confirmed_and_deaths_dt$continent==cont1,]
cont2_data <- continent_confirmed_and_deaths_dt[continent_confirmed_and_deaths_dt$continent==cont2,]
cont3_data <- continent_confirmed_and_deaths_dt[continent_confirmed_and_deaths_dt$continent==cont3,]
cont4_data <- continent_confirmed_and_deaths_dt[continent_confirmed_and_deaths_dt$continent==cont4,]
cont5_data <- continent_confirmed_and_deaths_dt[continent_confirmed_and_deaths_dt$continent==cont5,]

continents_deaths_plot <- ggplot(mapping = aes()) +
  geom_line(data = cont1_data, mapping = aes(x=cont1_data$date, y=cont1_data$deaths, color=cont1)) +
  geom_line(data = cont2_data, mapping = aes(x=cont1_data$date, y=cont2_data$deaths, color=cont2)) +
  geom_line(data = cont3_data, mapping = aes(x=cont1_data$date, y=cont3_data$deaths, color=cont3)) +
  geom_line(data = cont4_data, mapping = aes(x=cont1_data$date, y=cont4_data$deaths, color=cont4)) +
  geom_line(data = cont5_data, mapping = aes(x=cont1_data$date, y=cont5_data$deaths, color=cont5)) +
  geom_point() + 
  labs(x = "date (month/year)",
       y = "deaths",
       color = "Legend",
       title = "Per continent cumulative COVID Deaths") +
  scale_x_date(breaks = date_breaks("months"), labels = date_format("%m/%y")) +
  scale_y_continuous(breaks = seq(0,max(continent_confirmed_and_deaths_dt$deaths), by=1e5)) +
  theme_ipsum()

continents_deaths_plot
ggsave("continents_deaths.png", width = width, height = height, units="cm")

continents_confirmed_plot <- ggplot(mapping = aes()) +
  geom_line(data = cont1_data, mapping = aes(x=cont1_data$date, y=cont1_data$confirmed, color=cont1)) +
  geom_line(data = cont2_data, mapping = aes(x=cont1_data$date, y=cont2_data$confirmed, color=cont2)) +
  geom_line(data = cont3_data, mapping = aes(x=cont1_data$date, y=cont3_data$confirmed, color=cont3)) +
  geom_line(data = cont4_data, mapping = aes(x=cont1_data$date, y=cont4_data$confirmed, color=cont4)) +
  geom_line(data = cont5_data, mapping = aes(x=cont1_data$date, y=cont5_data$confirmed, color=cont5)) +
  geom_point() + 
  labs(x = "date (month/year)",
       y = "confirmed",
       color = "Legend",
       title = "Per continent cumulative Confirmed COVID cases") +
  scale_x_date(breaks = date_breaks("months"), labels = date_format("%m/%y")) +
  scale_y_continuous(breaks = seq(0,max(continent_confirmed_and_deaths_dt$confirmed), by=2.5e6)) +
  theme_ipsum()

continents_confirmed_plot
ggsave("continents_confirmed.png", width = width, height = height, units="cm")

continents_deaths_daily_plot <- ggplot(mapping = aes()) +
  geom_line(data = cont1_data, mapping = aes(x=cont1_data$date, y=cont1_data$deaths.inc, color=cont1)) +
  geom_line(data = cont2_data, mapping = aes(x=cont1_data$date, y=cont2_data$deaths.inc, color=cont2)) +
  geom_line(data = cont3_data, mapping = aes(x=cont1_data$date, y=cont3_data$deaths.inc, color=cont3)) +
  geom_line(data = cont4_data, mapping = aes(x=cont1_data$date, y=cont4_data$deaths.inc, color=cont4)) +
  geom_line(data = cont5_data, mapping = aes(x=cont1_data$date, y=cont5_data$deaths.inc, color=cont5)) +
  geom_point() + 
  labs(x = "date (month/year)",
       y = "deaths",
       color = "Legend",
       title = "Per continent daily COVID Deaths") +
  scale_x_date(breaks = date_breaks("months"), labels = date_format("%m/%y")) +
  scale_y_continuous(breaks = seq(0,max(continent_confirmed_and_deaths_dt$deaths.inc), by=1000)) +
  theme_ipsum()

continents_deaths_daily_plot
ggsave("continents_deaths_daily.png", width = width, height = height, units="cm")

continents_confirmed_daily_plot <- ggplot(mapping = aes()) +
  geom_line(data = cont1_data, mapping = aes(x=cont1_data$date, y=cont1_data$confirmed.ind, color=cont1)) +
  geom_line(data = cont2_data, mapping = aes(x=cont1_data$date, y=cont2_data$confirmed.ind, color=cont2)) +
  geom_line(data = cont3_data, mapping = aes(x=cont1_data$date, y=cont3_data$confirmed.ind, color=cont3)) +
  geom_line(data = cont4_data, mapping = aes(x=cont1_data$date, y=cont4_data$confirmed.ind, color=cont4)) +
  geom_line(data = cont5_data, mapping = aes(x=cont1_data$date, y=cont5_data$confirmed.ind, color=cont5)) +
  geom_point() + 
  labs(x = "date (month/year)",
       y = "confirmed",
       color = "Legend",
       title = "Per continent daily Confirmed COVID cases") +
  scale_x_date(breaks = date_breaks("months"), labels = date_format("%m/%y")) +
  scale_y_continuous(breaks = seq(0,max(continent_confirmed_and_deaths_dt$confirmed.ind), by=1e5)) +
  theme_ipsum()

continents_confirmed_daily_plot
ggsave("continents_confirmed_daily.png", width = width, height = height, units="cm")

################################################################################
# Data for Greece only
greece_data <- confirmed_and_deaths_dt[Country=="Greece"]
# dates for lockdown to plot later
lockdown1_s = mdy("3/22/20")
lockdown1_f = mdy("5/4/20")
lockdown2_s = mdy("11/5/20")

scale_value <- max(greece_data$confirmed) / max(greece_data$confirmed.ind)
greece_confirmed_plot <- ggplot() +
  geom_bar(data = greece_data, size=0.5, mapping = aes(x=greece_data$date, y=scale_value * greece_data$confirmed.ind, color="Daily cases"), stat="identity", show.legend = F) +
  geom_line(data = greece_data, size=1.5, mapping = aes(x=greece_data$date, y=greece_data$confirmed, color="Cumulative cases")) +
  geom_vline(size=0.8, mapping = aes(xintercept=as.numeric(lockdown1_s), color="Lockdown 1 begins"), show.legend = F) +
  geom_vline(size=0.8, mapping = aes(xintercept=as.numeric(lockdown1_f), color="Lockdown 1 ends"), show.legend = F) +
  geom_vline(size=0.8, mapping = aes(xintercept=as.numeric(lockdown2_s), color="Lockdown 2 begins"), show.legend = F) +
  scale_color_manual(name = "Legend", values = c("Cumulative cases" = "red", "Daily cases" = "dodgerblue4", "Lockdown 1 begins" = "snow4", "Lockdown 1 ends" = "darkorange", "Lockdown 2 begins" = "gray10")) +   
  geom_point() + 
  labs(x = "date (month/year)",
       y = "Cumulative cases",
       color = "Legend",
       title = "Greece Confirmed COVID cases") +
  scale_x_date(breaks = date_breaks("months"), labels = date_format("%m/%y")) +
  scale_y_continuous(breaks = seq(0,max(greece_data$confirmed), by=1e4), sec.axis = sec_axis(~./scale_value, name="Daily cases", breaks = seq(0,max(greece_data$confirmed.ind), by=.25e3))) +
  theme_ipsum()

greece_confirmed_plot
ggsave("greece_confirmed.png", width = width, height = height, units="cm")

scale_value <- max(greece_data$deaths) / max(greece_data$deaths.inc)
greece_deaths_plot <- ggplot() +
  geom_bar(data = greece_data, size=0.5, mapping = aes(x=greece_data$date, y=scale_value * greece_data$deaths.inc, color="Daily cases"), stat="identity", show.legend = F) +
  geom_line(data = greece_data, size=1.5, mapping = aes(x=greece_data$date, y=greece_data$deaths, color="Cumulative cases")) +
  geom_vline(size=0.8, mapping = aes(xintercept=as.numeric(lockdown1_s), color="Lockdown 1 begins"), show.legend = F) +
  geom_vline(size=0.8, mapping = aes(xintercept=as.numeric(lockdown1_f), color="Lockdown 1 ends"), show.legend = F) +
  geom_vline(size=0.8, mapping = aes(xintercept=as.numeric(lockdown2_s), color="Lockdown 2 begins"), show.legend = F) +
  scale_color_manual(name = "Legend", values = c("Cumulative cases" = "red", "Daily cases" = "darkorchid4", "Lockdown 1 begins" = "snow4", "Lockdown 1 ends" = "darkorange", "Lockdown 2 begins" = "gray10")) +   
  geom_point() + 
  labs(x = "date (month/year)",
       y = "Cumulative cases",
       color = "Legend",
       title = "Greece COVID deaths") +
  scale_x_date(breaks = date_breaks("months"), labels = date_format("%m/%y")) +
  scale_y_continuous(breaks = seq(0,max(greece_data$deaths), by=.5e3), sec.axis = sec_axis(~./scale_value, name="Daily cases", breaks = seq(0,max(greece_data$deaths.inc), by=.25e2))) + 
  theme_ipsum()

greece_deaths_plot
ggsave("greece_deaths.png", width = width, height = height, units="cm")

################################################################################
# Time for World population now
################################################################################
# get world population for 2020 (estimate) https://bayespop.shinyapps.io/wpp2019explorer
world_population <- fread("tpop_2020.csv")
world_population$value <- world_population$value*1e3 # population stored in 1000s, scale it
world_population[, ":="("id"=NULL,"UN"=NULL, "rank"=NULL)]
# set proper names for merging with covid dataset later
setnames(world_population, "value", "Population")
setnames(world_population, "name", "Country")
# fix some European names that are different from covid dataset
world_population[world_population$Country=="Macedonia, the Former Yugoslav Republic Of"]$Country = "North Macedonia"
world_population[world_population$Country=="Moldova, Republic of"]$Country = "Moldova"
world_population[world_population$Country=="Russian Federation"]$Country = "Russia"

################################################################################
# let's get data for european countries with same population as Greece and explore them
europe_population <- world_population[countrycode(sourcevar = world_population$Country, origin = "country.name", destination = "continent") == "Europe"]
gr_pop <- europe_population[Country=="Greece"]$Population
europe_same_pop <- europe_population[europe_population$Population>0.92*gr_pop & europe_population$Population<1.15*gr_pop,]$Country
europe_same_pop

country1 = "Czechia"
country2 = "Greece"
country3 = "Hungary"
country4 = "Portugal"
country5 = "Sweden"
country6 = "Belgium"

country1_data <- confirmed_and_deaths_dt[confirmed_and_deaths_dt$Country==country1,]
country2_data <- confirmed_and_deaths_dt[confirmed_and_deaths_dt$Country==country2,]
country3_data <- confirmed_and_deaths_dt[confirmed_and_deaths_dt$Country==country3,]
country4_data <- confirmed_and_deaths_dt[confirmed_and_deaths_dt$Country==country4,]
country5_data <- confirmed_and_deaths_dt[confirmed_and_deaths_dt$Country==country5,]
country6_data <- confirmed_and_deaths_dt[confirmed_and_deaths_dt$Country==country6,]

countries_comparison_deaths_plot <- ggplot(mapping = aes()) +
  geom_line(data = country1_data, mapping = aes(x=country1_data$date, y=country1_data$deaths, color=country1)) +
  geom_line(data = country2_data, mapping = aes(x=country1_data$date, y=country2_data$deaths, color=country2)) +
  geom_line(data = country3_data, mapping = aes(x=country1_data$date, y=country3_data$deaths, color=country3)) +
  geom_line(data = country4_data, mapping = aes(x=country1_data$date, y=country4_data$deaths, color=country4)) +
  geom_line(data = country5_data, mapping = aes(x=country1_data$date, y=country5_data$deaths, color=country5)) +
  geom_line(data = country6_data, mapping = aes(x=country1_data$date, y=country6_data$deaths, color=country6)) +
  geom_point() + 
  labs(x = "date (month/year)",
       y = "deaths",
       color = "Legend",
       title = "Cumulative COVID Deaths") +
  scale_x_date(breaks = date_breaks("months"), labels = date_format("%m/%y"))  +
  scale_y_continuous(breaks = seq(0,max(confirmed_and_deaths_dt$deaths), by=.25e4))+
  theme_ipsum()

countries_comparison_deaths_plot
ggsave("europe_same_pop_deaths.png", width = width, height = height, units="cm")

countries_comparison_confirmed_plot <- ggplot(mapping = aes()) +
  geom_line(data = country1_data, mapping = aes(x=country1_data$date, y=country1_data$confirmed, color=country1)) +
  geom_line(data = country2_data, mapping = aes(x=country1_data$date, y=country2_data$confirmed, color=country2)) +
  geom_line(data = country3_data, mapping = aes(x=country1_data$date, y=country3_data$confirmed, color=country3)) +
  geom_line(data = country4_data, mapping = aes(x=country1_data$date, y=country4_data$confirmed, color=country4)) +
  geom_line(data = country5_data, mapping = aes(x=country1_data$date, y=country5_data$confirmed, color=country5)) +
  geom_line(data = country6_data, mapping = aes(x=country1_data$date, y=country6_data$confirmed, color=country6)) +
  geom_point() +
  labs(x = "date (month/year)",
       y = "confirmed",
       color = "Legend",
       title = "Cumulative Confirmed COVID cases") +
  scale_x_date(breaks = date_breaks("months"), labels = date_format("%m/%y")) +
  scale_y_continuous(breaks = seq(0,max(confirmed_and_deaths_dt$confirmed), by=.5e5))+
  theme_ipsum()

countries_comparison_confirmed_plot
ggsave("europe_same_pop_confirmed.png", width = width, height = height, units="cm")

confirmed_and_deaths_dt <- confirmed_and_deaths_dt %>% mutate(fatality_rate = deaths/confirmed*100)
confirmed_and_deaths_dt[is.na(confirmed_and_deaths_dt)] <- 0

countries_comparison_fatality_plot <- ggplot(mapping = aes()) +
  geom_line(data = country1_data, mapping = aes(x=country1_data$date, y=country1_data$fatality_rate, color=country1)) +
  geom_line(data = country2_data, mapping = aes(x=country1_data$date, y=country2_data$fatality_rate, color=country2)) +
  geom_line(data = country3_data, mapping = aes(x=country1_data$date, y=country3_data$fatality_rate, color=country3)) +
  geom_line(data = country4_data, mapping = aes(x=country1_data$date, y=country4_data$fatality_rate, color=country4)) +
  geom_line(data = country5_data, mapping = aes(x=country1_data$date, y=country5_data$fatality_rate, color=country5)) +
  geom_line(data = country6_data, mapping = aes(x=country1_data$date, y=country6_data$fatality_rate, color=country6)) +
  geom_point() + 
  labs(x = "date (month/year)",
       y = "Fatality Rate (%)",
       color = "Legend",
       title = "Fatality Rate of Covid") +
  scale_x_date(breaks = date_breaks("months"), labels = date_format("%m/%y"))  +
  scale_y_continuous(breaks = seq(0,max(confirmed_and_deaths_dt$fatality_rate), by=2.5))+
  theme_ipsum()

countries_comparison_fatality_plot
ggsave("europe_same_pop_fatality_rate.png", width = width, height = height, units="cm")

################################################################################
# from our dataset, keep last date only (cumulative -> total stats) and only countries from Europe
europe_data <- confirmed_and_deaths_dt[confirmed_and_deaths_dt$continent=="Europe" & confirmed_and_deaths_dt$date==last_date, c("Country","deaths","confirmed","fatality_rate")]
europe_data <- merge(europe_data, world_population)
europe_data$deaths_per_million <- europe_data$deaths / europe_data$Population * 1e6 # per million
avg_dpm <- round(mean(sum(europe_data$deaths)/sum(europe_data$Population/1e6)),-2) # round to give better look in x axis later

europe_data <- europe_data[order(deaths_per_million),]
europe_data$Country <- factor(europe_data$Country, levels=europe_data$Country) # convert to factor to retain sorted order in plot

europe_dpm_plot <- ggplot(europe_data, aes(x=europe_data$Country, y=europe_data$deaths_per_million-avg_dpm, fill=europe_data$deaths_per_million-avg_dpm)) +
  geom_bar(stat='identity', width=.75) +
  scale_fill_gradient(name="Legend", low = "green2", high = "red2",
                      labels = function(x)x + avg_dpm, # move axis forward to show real deaths_per_million
                      breaks = seq(-1000,1000, by=200)) + 
  scale_y_continuous(labels = function(x)x + avg_dpm, # move axis forward to show real deaths_per_million
                     breaks = seq(-1000,1000, by=200)) +
  labs(title= "Deaths per million in Europe",
       x = "Country",
       y = "Deaths per million") +
  coord_flip() +
  theme_ipsum()

europe_dpm_plot
ggsave("europe_dpm.png", width = width, height = 0.6*width, units="cm")

europe_data_1M <- europe_data[europe_data$Population>.5e6]
europe_scatterplot <- ggplot(europe_data_1M, aes(x=europe_data_1M$confirmed, y=europe_data_1M$deaths)) +
  # https://stackoverflow.com/questions/12881553/add-a-logarithmic-regression-line-to-a-scatterplot-comparison-with-excel
  stat_smooth(method="lm",formula=y~log(x),fill="red") + 
  geom_point(color="dodgerblue4") +
  geom_text_repel(aes(label=europe_data_1M$Country), size=3.4, color="black") +
  scale_x_continuous(trans = 'log10', labels = format_format(scientific = FALSE)) +
  scale_y_continuous(trans = 'log10', labels = format_format(scientific = FALSE)) +
  labs(title= "Deaths vs Confirmed cases in Europe",
       x = "Confirmed Cases (log axis)",
       y = "Deaths (log axis)") +
  theme_ipsum()

europe_scatterplot
ggsave("europe_scatterplot.png", width = width, height = height, units="cm")

################################################################################
# Top 5 (worst) countries from Europe, Asia and Americas
cnt <- 5
europe_confirmed_top <- confirmed_and_deaths_dt[date==last_date & continent=="Europe",c("Country","confirmed")] %>% arrange(desc(confirmed)) %>% slice(1:cnt) 
asia_confirmed_top <- confirmed_and_deaths_dt[date==last_date & continent=="Asia",c("Country","confirmed")] %>% arrange(desc(confirmed)) %>% slice(1:cnt) 
americas_confirmed_top <- confirmed_and_deaths_dt[date==last_date & continent=="Americas",c("Country","confirmed")] %>% arrange(desc(confirmed)) %>% slice(1:cnt)

europe_deaths_top <- confirmed_and_deaths_dt[date==last_date & continent=="Europe",c("Country","deaths")] %>% arrange(desc(deaths)) %>% slice(1:cnt)
asia_deaths_top <- confirmed_and_deaths_dt[date==last_date & continent=="Asia",c("Country","deaths")] %>% arrange(desc(deaths)) %>% slice(1:cnt)
americas_deaths_top <- confirmed_and_deaths_dt[date==last_date & continent=="Americas",c("Country","deaths")] %>% arrange(desc(deaths)) %>% slice(1:cnt)

################################################################################
europe_top_data <- confirmed_and_deaths_dt[, c("Country","date","deaths.inc")] %>% filter(Country %in% europe_deaths_top$Country)

europe_top_data <- merge(europe_top_data, world_population)
europe_top_data$dpm_1day <- europe_top_data$deaths.inc / europe_top_data$Population * 1e6

# https://www.storybench.org/how-to-calculate-a-rolling-average-in-r/
europe_top_data <- europe_top_data %>% group_by(Country) %>% mutate(dpm_7days = rollmean(dpm_1day, k = 7, fill = NA))

country1 = "United Kingdom"
country2 = "Italy"
country3 = "France"
country4 = "Russia"
country5 = "Spain"

country1_data <- europe_top_data[europe_top_data$Country==country1,]
country2_data <- europe_top_data[europe_top_data$Country==country2,]
country3_data <- europe_top_data[europe_top_data$Country==country3,]
country4_data <- europe_top_data[europe_top_data$Country==country4,]
country5_data <- europe_top_data[europe_top_data$Country==country5,]
country5_data[country5_data$dpm_7days<0 & !(is.na(country5_data$dpm_7days)),]$dpm_7days = 0

dpm7days_comparison_plot <- ggplot(mapping = aes()) +
  geom_line(data = country1_data, mapping = aes(x=country1_data$date, y=country1_data$dpm_7days, color=country1)) +
  geom_line(data = country2_data, mapping = aes(x=country1_data$date, y=country2_data$dpm_7days, color=country2)) +
  geom_line(data = country3_data, mapping = aes(x=country1_data$date, y=country3_data$dpm_7days, color=country3)) +
  geom_line(data = country4_data, mapping = aes(x=country1_data$date, y=country4_data$dpm_7days, color=country4)) +
  geom_line(data = country5_data, mapping = aes(x=country1_data$date, y=country5_data$dpm_7days, color=country5)) +
  geom_point() + 
  labs(x = "date (month/year)",
       y = "deaths/1M",
       color = "Legend",
       title = "Daily Deaths per million (averaged over 1 week period)") +
  scale_x_date(breaks = date_breaks("months"), labels = date_format("%m/%y"))  +
  scale_y_continuous(limits = c(0,20), breaks = seq(0,20, by=5))+
  theme_ipsum()

dpm7days_comparison_plot
ggsave("europe_dpm7days.png", width = width, height = height, units="cm")

################################################################################