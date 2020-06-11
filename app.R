#Data bankruptcies
#https://www.abi.org/newsroom/bankruptcy-statistics
#install.packages("shinyWidgets", repos="https://cloud.r-project.org") 

#https://blog.cpsievert.me/2018/01/30/learning-improving-ggplotly-geom-sf/

###Data to aggreagate
#https://www.bea.gov/data/gdp/gross-domestic-product



require(tidyverse)
library(dplyr)
library(lubridate)
library(data.table)
library(readr)
library(purrr)
library(plotly)
library(shiny)
library(slider)
require(httr)
require(httpcache)
library(shinydashboard)
library(shinyWidgets)
library(readxl)
library(gapminder)
#data arbetslÃ¶shet
##https://www.bls.gov/web/laus/ststdsadata.txt

#gdp.. halvtaskig sida
#https://www.bea.gov/data/gdp/gdp-state

#Source: US Department of Labor
#Source: US Bureau of Labor Statistics
#Source: US Bureau of Economic Analysis

##
####BerÃ¤kning arbetslÃ¶shet blir konstigt. se Ã¶ver. Nu rÃ¤tt? Se Ã¶ver om man ska kÃ¶ra sÃ¤songsrensat eller inte.
###

####### IMPORT DATA

###https://www.bls.gov/web/laus.supp.toc.htm
##Obs jag kollar pÃ¥ sÃ¤songsjusterade siffror fÃ¶r arbetslÃ¶shet.
#https://www.bls.gov/opub/ted/2020/employment-up-in-21-metro-areas-unchanged-in-367-from-march-2019-to-march-2020.htm

url_jobless_claims="https://oui.doleta.gov/unemploy/csv/ar539.csv"
url_state <- ("https://raw.githubusercontent.com/cnordenlow/static_data/master/us_states_abbreviation.csv")
url_data <- ("https://www.bls.gov/web/laus/ststdsadata.txt")   #seasonally adjusted
url_data2 <- ("https://www.bls.gov/web/laus/ststdnsadata.txt")  #not seasonally adjusted
url_data_gdp <- ("https://www.bea.gov/system/files/2020-04/qgdpstate0420.xlsx")
url_population ="https://raw.githubusercontent.com/JoshData/historical-state-population-csv/primary/historical_state_population_by_year.csv"


us_states <-read_csv2(url(url_state))
download.file(url=url_data_gdp, destfile="localcopy.xlsx", mode="wb")
data_employment_main <- read.table(url_data, sep = "", skip=20, header = FALSE, fill=TRUE, na.strings ="", stringsAsFactors= F,strip.white=TRUE)

get_data <- function(url) {
  httpcache::GET(url) %>%
    httr::content()
}
data_jobless_claims <- get_data(url_jobless_claims)
data_pop<-read_csv(url(url_population), col_names =  c("state_short", "year", "pop"))



#data_jobless_claims <- fread(url_jobless_claims)
#url_jobless_claims="https://oui.doleta.gov/unemploy/csv/ar539.csv"
#data_jobless_claims<-read_csv(url(url_jobless_claims))


##Functions
######### funktion fÃ¶r att ta ut right
right = function(text, num_char) {
  substr(text, nchar(text) - (num_char-1), nchar(text))
}
left = function(text, num_char) {
  substr(text, 1, num_char)
}




#behÃ¶ver lÃ¤gga till 2020 om det saknas. sen 2021 etc.
#
data_pop_2020 <- data_pop %>%
  filter(year == 2019) %>%
  mutate(year = ifelse(year == 2019, 2020, 2020))

data_pop <- rbind(data_pop, data_pop_2020)
#names(data_pop)[names(data_pop) == "state"] <- "state_short"



###############
###############
##Labor Data
###############
###############

data_employment <- data_employment_main%>%
  unite(V1V2, V1, V2, sep=" ", remove = FALSE)%>%
  
  mutate(state_long = case_when(
    str_sub(V2, 1, 1) == '.'  ~ V1,
    str_sub(V3, 1, 1) == '.'  ~ V1V2))%>%
  #str_sub(V3, 1, 1) == '.'  ~ unite(V1:V2)))%>%
  #   str_sub(V4, 1, 1) == '.'  ~ concated_column = paste(V1, V2, V3, sep = '_'))))%>%
  
  mutate(civ_population = case_when(
    str_sub(V2, 1, 1) == '.'  ~ V3,
    str_sub(V3, 1, 1) == '.'  ~ V4,
    str_sub(V4, 1, 1) == '.'  ~ V5))%>%
  
  mutate(participation = case_when(
    str_sub(V2, 1, 1) == '.'  ~ V4,
    str_sub(V3, 1, 1) == '.'  ~ V5,
    str_sub(V4, 1, 1) == '.'  ~ V6))%>%
  
  mutate(participation_rate = case_when(
    str_sub(V2, 1, 1) == '.'  ~ V5,
    str_sub(V3, 1, 1) == '.'  ~ V6,
    str_sub(V4, 1, 1) == '.'  ~ V7))%>%
  
  mutate(employed_population = case_when(
    str_sub(V2, 1, 1) == '.'  ~ V6,
    str_sub(V3, 1, 1) == '.'  ~ V7,
    str_sub(V4, 1, 1) == '.'  ~ V8))%>%
  
  mutate(employed_to_pop_rate = case_when(
    str_sub(V2, 1, 1) == '.'  ~ V7,
    str_sub(V3, 1, 1) == '.'  ~ V8,
    str_sub(V4, 1, 1) == '.'  ~ V9))%>%
  
  
  mutate(unemployed_population = case_when(
    str_sub(V2, 1, 1) == '.'  ~ V8,
    str_sub(V3, 1, 1) == '.'  ~ V9,
    str_sub(V4, 1, 1) == '.'  ~ V10))%>%
  
  mutate(unemployment = case_when(
    str_sub(V2, 1, 1) == '.'  ~ V9,
    str_sub(V3, 1, 1) == '.'  ~ V10))%>%
  
  #year
  mutate(year = case_when(
    str_sub(V2, 1, 1) == 1 ~ V2,
    str_sub(V2, 1, 1) == 2  ~ V2))%>%
  
  mutate(month = case_when(
    str_sub(V2, 1, 1) == 1  ~ V1,
    str_sub(V2, 1, 1) == 2  ~ V1))%>%
  
  #fill year
  fill(year, .direction = "down")%>%
  fill(month, .direction = "down")%>%
  
  select(year, month, state_long, civ_population, participation, participation_rate,employed_population, employed_to_pop_rate,unemployed_population,unemployment)

#merge with short name and remove NA
data_employment<-merge(x = data_employment, y = us_states, by.x = "state_long", by.y = "state_long", all.x = TRUE)
data_employment <- na.omit(data_employment)




###
#Convert to dates
###
data_employment <- data_employment%>%
  mutate(rptdate = case_when(
    data_employment$month =="January" ~ paste(data_employment$year,"01-31", sep="-"),
    data_employment$month =="February" ~ paste(data_employment$year,"02-28", sep="-"),
    data_employment$month =="March" ~ paste(data_employment$year,"03-31", sep="-"),
    data_employment$month =="April" ~ paste(data_employment$year,"04-30", sep="-"),
    data_employment$month =="May" ~ paste(data_employment$year,"05-31", sep="-"),
    data_employment$month =="June" ~ paste(data_employment$year,"06-30", sep="-"),
    data_employment$month =="July" ~ paste(data_employment$year,"07-31", sep="-"),
    data_employment$month =="August" ~ paste(data_employment$year,"08-31", sep="-"),
    data_employment$month =="September" ~ paste(data_employment$year,"09-30", sep="-"),
    data_employment$month =="October" ~ paste(data_employment$year,"10-31", sep="-"),
    data_employment$month =="November" ~ paste(data_employment$year,"11-30", sep="-"),
    data_employment$month =="December" ~ paste(data_employment$year,"12-31", sep="-")
  ))%>%
  select(-month)%>%
  select(-year)

data_employment$rptdate <- as.Date(data_employment$rptdate)
data_employment <-arrange(data_employment, rptdate)%>%
  filter(rptdate > "2010-01-01")

data_employment <- data_employment%>%
  mutate(unemployment = as.numeric(unemployment))%>%
  mutate(employed_to_pop_rate = as.numeric(employed_to_pop_rate))%>%
  mutate(participation_rate = as.numeric(participation_rate))


###Sum for US and add to data.
data_employment <- data_employment%>%
  mutate(year = year(rptdate))
data_employment2 <- data_employment



data_employment<-merge(x = data_employment, y = data_pop, by = c("state_short", "year"), all.x = TRUE)
#data_employment <- merge(x = data_employment, y = data_pop, by.x = c("state_short", "year"), by.y = c("state_short", "year"))
data_employment <- unique(data_employment)


temp_data <- data_employment%>%
  mutate(unemployed_population = as.numeric(gsub(",","",unemployed_population,fixed=TRUE)))%>%
  mutate(participation = as.numeric(gsub(",","",participation,fixed=TRUE)))%>%
  mutate(employed_population = as.numeric(gsub(",","",employed_population,fixed=TRUE)))%>%
  mutate(civ_population = as.numeric(gsub(",","",civ_population,fixed=TRUE)))%>%
#  mutate(employed_population = as.numeric(gsub(",","",employed_population,fixed=TRUE)))%>%
  group_by(rptdate)%>%
  summarise(unemployed_population = sum(unemployed_population),
            participation = sum(participation),
            employed_population = sum(employed_population),
            unemployment= round(sum(unemployed_population) / sum(participation) *100,1),
            employed_to_pop_rate= round(sum(employed_population) / sum(civ_population) *100,1),
            participation_rate = round(sum(participation) / sum(civ_population)*100,1),
            share_civ_pop = sum(civ_population) / sum(pop),
            civ_population = sum(civ_population),
            pop = sum(pop)
  )%>%
 # mutate(year = year(rptdate))%>%
  mutate(state_short = "US")%>%
  mutate(state_long = "United States")%>%
  mutate(state_group = "United States")%>%
  mutate(year = year(rptdate))


data_employment <- merge(data_employment, temp_data, all = TRUE)
data_employment2 <- merge(data_employment2, temp_data, all = TRUE)

data_employment <- arrange(data_employment,rptdate)
data_employment2 <- arrange(data_employment2,rptdate)


###############
###############
##Jobless Claims
###############
###############
data_jobless_claims2 <- data_jobless_claims



########################Data manipulation
names(data_jobless_claims2)[names(data_jobless_claims2) == "st"] <- "state_short"
names(data_jobless_claims2)[names(data_jobless_claims2) == "c1"] <- "week"
names(data_jobless_claims2)[names(data_jobless_claims2) == "c3"] <- "jobless_claims"
names(data_jobless_claims2)[names(data_jobless_claims2) == "c8"] <- "continuing_claims"

data_jobless_claims2$rptdate <- as.Date(data_jobless_claims2$rptdate,
                                        format = "%m/%d/%Y")

#Filter dates after 2000
data_jobless_claims2 <- data_jobless_claims2 %>%
  filter(rptdate > "2010-04-01")%>%
  select(state_short, week, rptdate, jobless_claims, continuing_claims)%>%
  mutate(year = year(rptdate))


###lyft in full name o group
data_jobless_claims2<-merge(x = data_jobless_claims2, y = us_states, by.x = "state_short", by.y = "state_short", all.x = TRUE)

####
####Addera civilian population to data jobless claims
####
data_temp <- data_employment %>%
  select(state_short, rptdate, civ_population)
data_temp <- data_temp%>%
  mutate(civ_population = as.numeric(gsub(",","",civ_population,fixed=TRUE)))

data_temp <- data_temp%>%
mutate(year = case_when(
  month(data_temp$rptdate) == 12 ~ year(data_temp$rptdate)))
data_temp <- na.omit(data_temp)

data_temp_2020 <- data_temp %>%
  filter(year == 2019) %>%
  mutate(year = ifelse(year == 2019, 2020, 2020))

data_temp <- rbind(data_temp, data_temp_2020)

data_temp <- data_temp%>%
  select(-rptdate)

#detta tar bort dÃ¥ de som inte matchar. rÃ¤tt eller fel? TÃ¤nk till hÃ¤r. bÃ¤ttre lÃ¤gga en Ã¥rlig befolkning dÃ¥ det inte gÃ¥r att matcha datumen dÃ¥ jobless claims ej Ã¤r sista per mÃ¥nad. givet man inte fixar en hjÃ¤lp rad.
data_jobless_claims2<-merge(x = data_jobless_claims2, y = data_temp, by.x = c("state_short", "year"), by.y = c("state_short", "year"))
#data_jobless_claims2 <- arrange(data_jobless_claims2, rptdate)

#lyft in population till data_jobbless_claims med faktor state och year som composite

data_jobless_claims2<-merge(x = data_jobless_claims2, y = data_pop, by = c("state_short", "year"), all.x = TRUE)
#saknas Ã¥rtal sÃ¥ tar den fÃ¶regÃ¥ende

#tar bort alla rader med NA
data_jobless_claims2 <- na.omit(data_jobless_claims2)


#berÃ¤kna relativt befolkning
data_jobless_claims2 <- data_jobless_claims2%>%
  mutate(initial_claims_relative_pop = round(jobless_claims / civ_population*100,2))%>%
  mutate(continuing_claims_relative_pop = round(continuing_claims / civ_population*100,2))

#data_jobless_claims2 <- data_jobless_claims2%>%
 # mutate(days_since_last = max(rptdate) - rptdate)


#funktion slide
fun <- function(x) {
  y <- tail(na.omit(x), 1)
  ifelse(length(y)>0, y, NA)
}

data_jobless_claims3 <- data_jobless_claims2


###Sum for US and add to data.
temp_data <- data_jobless_claims2%>%
  group_by(rptdate)%>%
  summarise(jobless_claims = sum(jobless_claims),
            continuing_claims = sum(continuing_claims),
            initial_claims_relative_pop = round(sum(jobless_claims) / sum(civ_population)*100,2),
            continuing_claims_relative_pop = round(sum(continuing_claims) / sum(civ_population)*100,2),
            civ_population = sum(civ_population),
            pop = sum(pop)
  )%>%
  mutate(year = year(rptdate))%>%
  mutate(state_short = "US")%>%
  mutate(state_long = "United States")%>%
  mutate(state_group = "United States")%>%
  mutate(year = year(rptdate))


data_jobless_claims2 <- merge(data_jobless_claims2, temp_data, all = TRUE)
data_jobless_claims3 <- merge(data_jobless_claims3, temp_data, all = TRUE)


data_jobless_claims2 <- data_jobless_claims2 %>%
  mutate(country = ifelse(state_short == "US",1,0))
data_jobless_claims3 <- data_jobless_claims3 %>%
  mutate(country = ifelse(state_short == "US",1,0))


###fÃ¶rsÃ¶k fÃ¥ ner datan
#n_days_before = 30
data_jobless_claims2 <- data_jobless_claims2 %>%
  filter(rptdate >"2020-01-01")%>%
  arrange(rptdate, country)
#  group_by(state_short)%>%
#  mutate(initial_1m = (jobless_claims-slide_index_int(jobless_claims, rptdate, fun, .before=Inf, .after=-30,.complete=T))/civ_population*100)%>%
 # mutate(initial_3m = (jobless_claims-slide_index_int(jobless_claims, rptdate, fun, .before=Inf, .after=-30*3,.complete=T))/civ_population*100)%>%
#  mutate(initial_6m = (jobless_claims-slide_index_int(jobless_claims, rptdate, fun, .before=Inf, .after=-30*6,.complete=T))/civ_population*100)%>%
#  mutate(initial_12m = (jobless_claims-slide_index_int(jobless_claims, rptdate, fun, .before=Inf, .after=-30*12,.complete=T))/civ_population*100)%>%
#  
#  mutate(continuing_1m = (continuing_claims - slide_index_int(continuing_claims, rptdate, fun, .before=Inf, .after=-30,.complete=T))/civ_population*100)%>%
#  mutate(continuing_3m = (continuing_claims - slide_index_int(continuing_claims, rptdate, fun, .before=Inf, .after=-30*3,.complete=T))/civ_population*100)%>%
#  mutate(continuing_6m = (continuing_claims - slide_index_int(continuing_claims, rptdate, fun, .before=Inf, .after=-30*6,.complete=T))/civ_population*100)%>%
  #  mutate(continuing_12m =(continuing_claims - slide_index_int(continuing_claims, rptdate, fun, .before=Inf, .after=-30*12,.complete=T))/civ_population)%>%
  
  # filter(rptdate == max(rptdate))
 # mutate(continuing_12m =(continuing_claims - slide_index_int(continuing_claims, rptdate, fun, .before=Inf, .after=-30*12,.complete=T))/civ_population*100)
#





############################Labor market and GSP





##############Denna laddar in frÃ¥n en stÃ¶kig excelfil
#Import GDP-data

#url_data_gdp <- ("https://www.bea.gov/system/files/2020-04/qgdpstate0420.xlsx")
#download.file(url=url_data_gdp, destfile="localcopy.xlsx", mode="wb")


#table 1: Table 1. Percent Change in Real Gross Domestic Product (GDP) by State and state
table_gdp_1_change <- read_excel('localcopy.xlsx', sheet = 1, skip =4, col_names = FALSE)

#import header
data_gdp_header <- read_excel('localcopy.xlsx', sheet = 1, skip =0, col_names = TRUE, range = cell_rows(c(2, 4)))

#Basically, I transposed df, filled every column downward, then transposed it back to the original orientation
data_gdp_header <- data.frame(t(data_gdp_header)) %>%
  fill(., names(.)) %>%
  t()

new_names <- paste0(as.character(data_gdp_header[1,]), as.character(data_gdp_header[2,]))
colnames(table_gdp_1_change) <- new_names


table_gdp_1_change <- na.omit(table_gdp_1_change)

#merge with state short name
colnames(table_gdp_1_change)[1] <- 'state_long'
#
#table_gdp_1_change <- na.omit(table_gdp_1_change)

##############200421
#ta bort kolumnen "rank"
table_gdp_1_change <- table_gdp_1_change%>%
  select(-grep("Rank", names(table_gdp_1_change)))


# kÃ¶r gather. ej fÃ¶rsta kolumnen men Ã¶vriga.
table_gdp_1_change <- table_gdp_1_change %>%
  gather("date", "gdp_qoq", c(-1))

#merge with state short name
table_gdp_1_change<-merge(x = table_gdp_1_change, y = us_states, by.x = "state_long", by.y = "state_long", all.x = TRUE)
table_gdp_1_change <- na.omit(table_gdp_1_change)
###ovanstÃ¥ende klart, ej datum.



table_gdp_1_change <- table_gdp_1_change%>%
  mutate(rptdate = case_when(
    right(table_gdp_1_change$date, 1) == 1 ~ paste(left(table_gdp_1_change$date,4),"03-31", sep="-"),
    right(table_gdp_1_change$date, 1) == 2 ~ paste(left(table_gdp_1_change$date,4),"06-30", sep="-"),
    right(table_gdp_1_change$date, 1) == 3 ~ paste(left(table_gdp_1_change$date,4),"09-30", sep="-"),
    right(table_gdp_1_change$date, 1) == 4 ~ paste(left(table_gdp_1_change$date,4),"12-31", sep="-")
  ))%>%
  select(-date)

table_gdp_1_change$rptdate <- as.Date(table_gdp_1_change$rptdate)



#table_gdp_1_change%>%
# str_split(table_gdp_1_change$date, "Q") 






#Import table 2
table_gdp_2_contributions_qoq <- read_excel('localcopy.xlsx', sheet = 2, skip =3, col_names = TRUE)
colnames(table_gdp_2_contributions_qoq)[1] <- 'state_long'
colnames(table_gdp_2_contributions_qoq)[2] <- 'change_in_real_gdp'

table_gdp_2_contributions_b <- read_excel('localcopy.xlsx', sheet = 3, skip =3, col_names = TRUE)
colnames(table_gdp_2_contributions_b)[1] <- 'state_long'
table_gdp_2_contributions_b <- table_gdp_2_contributions_b %>%
  select(-state_long)


table_gdp_2_contributions_qoq <- cbind(table_gdp_2_contributions_qoq,table_gdp_2_contributions_b)
table_gdp_2_contributions_qoq <- na.omit(table_gdp_2_contributions_qoq)

#merge with state short name
table_gdp_2_contributions_qoq<-merge(x = table_gdp_2_contributions_qoq, y = us_states, by.x = "state_long", by.y = "state_long", all.x = TRUE)
table_gdp_2_contributions_qoq <- na.omit(table_gdp_2_contributions_qoq)





#Import table 3
#import part of percentage of us
table_gdp_3_percent_of_us <- read_excel('localcopy.xlsx', sheet = 4, skip =4, col_names = FALSE)

#only select those as percent part of us
table_gdp_3_percent_of_us <- table_gdp_3_percent_of_us%>%
  select(1, 10:17)

#import header
data_gdp_header <- read_excel('localcopy.xlsx', sheet = 4, skip =1, col_names = TRUE, range = cell_rows(c(3,5)))
data_gdp_header <- data_gdp_header%>%
  select(1, 10:17)

#Basically, I transposed df, filled every column downward, then transposed it back to the original orientation
data_gdp_header <- data.frame(t(data_gdp_header)) %>%
  fill(., names(.)) %>%
  t()

new_names <- paste0(as.character(data_gdp_header[1,]), as.character(data_gdp_header[2,]))
colnames(table_gdp_3_percent_of_us) <- new_names
table_gdp_3_percent_of_us <- na.omit(table_gdp_3_percent_of_us)

#merge with state short name
colnames(table_gdp_3_percent_of_us)[1] <- 'state_long'

#Blir ovan rÃ¤tt?

#gather
table_gdp_3_percent_of_us <- table_gdp_3_percent_of_us %>%
  gather("date", "gdp_share", c(-1))

table_gdp_3_percent_of_us<-merge(x = table_gdp_3_percent_of_us, y = us_states, by.x = "state_long", by.y = "state_long", all.x = TRUE)
table_gdp_3_percent_of_us <- na.omit(table_gdp_3_percent_of_us)


###
#Convert to dates
###
table_gdp_3_percent_of_us <- table_gdp_3_percent_of_us%>%
  mutate(rptdate = case_when(
    right(table_gdp_3_percent_of_us$date, 1) == 1 ~ paste(left(table_gdp_3_percent_of_us$date,4),"03-31", sep="-"),
    right(table_gdp_3_percent_of_us$date, 1) == 2 ~ paste(left(table_gdp_3_percent_of_us$date,4),"06-30", sep="-"),
    right(table_gdp_3_percent_of_us$date, 1) == 3 ~ paste(left(table_gdp_3_percent_of_us$date,4),"09-30", sep="-"),
    right(table_gdp_3_percent_of_us$date, 1) == 4 ~ paste(left(table_gdp_3_percent_of_us$date,4),"12-31", sep="-")
  ))%>%
  mutate(gdp_share = as.numeric(gdp_share))%>%
  select(-date)

table_gdp_3_percent_of_us$rptdate <- as.Date(table_gdp_3_percent_of_us$rptdate)

##lÃ¤gg 0 till us as country
table_gdp_3_percent_of_us <- table_gdp_3_percent_of_us%>%
  mutate(gdp_share = ifelse(state_long =="United States", 0, gdp_share))


temp_table <- table_gdp_3_percent_of_us %>%
  select(state_long, rptdate, gdp_share)

#merge table 1 and table 3
table_gdp_1_change<-merge(x = table_gdp_1_change, y = temp_table, by.x = c("state_long", "rptdate"), by.y = c("state_long", "rptdate"), all.x = TRUE)
table_gdp_1_change <- arrange(table_gdp_1_change, rptdate)

#detta behÃ¶vs fÃ¶r att ha alla datum kvar fÃ¶r graf
table_gdp_1_change2 <- table_gdp_1_change




#import table 4 years compared
table_gdp_4_multiple_yr <- read_excel('localcopy.xlsx', sheet = 5, skip =1, col_names = TRUE)
colnames(table_gdp_4_multiple_yr)[1] <- 'state_long'

#ta bort rank
table_gdp_4_multiple_yr <- table_gdp_4_multiple_yr%>%
  select(-grep("Rank", names(table_gdp_4_multiple_yr)))

table_gdp_4_multiple_yr <- table_gdp_4_multiple_yr %>%
  gather("date", "gdp_yoy", c(-1))

table_gdp_4_multiple_yr <- table_gdp_4_multiple_yr%>%
  mutate(rptdate = paste(date, "12-31", sep = "-"))%>%
  select(-date)

table_gdp_4_multiple_yr<-merge(x = table_gdp_4_multiple_yr, y = us_states, by.x = "state_long", by.y = "state_long", all.x = TRUE)
table_gdp_4_multiple_yr <- na.omit(table_gdp_4_multiple_yr)


#Addera gdp_yoy till table 1
temp_data <- table_gdp_4_multiple_yr %>%
  select(state_long, rptdate, gdp_yoy)

table_gdp_1_change<-merge(x = table_gdp_1_change, y = temp_data, by.x = c("state_long", "rptdate"), by.y = c("state_long", "rptdate"), all.x = TRUE)
table_gdp_1_change <- fill(table_gdp_1_change,gdp_yoy,.direction = "up")
table_gdp_1_change <- arrange(table_gdp_1_change,rptdate)


###############Cross table

temp_table <- data_jobless_claims3 %>%
  filter(rptdate >"2018-01-01")%>%
  arrange(rptdate)%>%
  group_by(state_short)%>%
  mutate(initial_latest = (slide_index_int(jobless_claims, rptdate, fun, .before=Inf, .after=-0,.complete=T))/civ_population*100)%>%
  mutate(continuing_latest = (slide_index_int(continuing_claims, rptdate, fun, .before=Inf, .after=-0,.complete=T))/civ_population*100)%>%
  #mutate(initial_3m = (slide_index_int(jobless_claims, rptdate, fun, .before=Inf, .after=-30*3,.complete=T))/civ_population*100)%>%
  # mutate(initial_6m = (jobless_claims-slide_index_int(jobless_claims, rptdate, fun, .before=Inf, .after=-30*6,.complete=T))/civ_population*100)%>%
  #  mutate(initial_12m = (slide_index_int(jobless_claims, rptdate, fun, .before=Inf, .after=-30*12,.complete=T))/civ_population*100)%>%
  filter(rptdate == max(data_jobless_claims3$rptdate))%>%
  mutate(rptdate_claims = rptdate)
#  

joint_table <- temp_table %>%
  select(state_long, state_short, rptdate_claims, state_group, jobless_claims, continuing_claims, initial_latest, continuing_latest)


#gdp qoq
temp_table <- table_gdp_1_change%>%
  filter(rptdate == max(table_gdp_1_change$rptdate))%>%
  mutate(gdp_share = as.numeric(gdp_share))%>%
  mutate(gdp_qoq = gdp_qoq)%>%
  mutate(rptdate_gdp = rptdate)%>%
  select(state_long, rptdate_gdp, gdp_qoq, gdp_yoy, gdp_share)

joint_table<-merge(x = joint_table, y = temp_table, by.x = c("state_long"), by.y = c("state_long"), all.x = TRUE)

#gdp yoy

#gdp qoq
#temp_table <- table_gdp_4_multiple_yr%>%
#  filter(rptdate == max(table_gdp_4_multiple_yr$rptdate))%>%
#  mutate(gdp_yoy = gdp_yoy)%>%
#  select(state_long, rptdate, gdp_yoy)
#joint_table<-merge(x = joint_table, y = temp_table, by.x = c("state_long"), by.y = c("state_long"), all.x = TRUE)

#labor
temp_table <- data_employment%>%
  filter(rptdate == max(data_employment$rptdate))%>%
  mutate(rptdate_labor = rptdate)%>%
  select(state_long, rptdate_labor, unemployment, participation_rate, employed_to_pop_rate)


joint_table<-merge(x = joint_table, y = temp_table, by.x = c("state_long"), by.y = c("state_long"), all.x = TRUE)
















#html
#https://www.w3schools.com/html/html_formatting.asp


#https://stackoverflow.com/questions/48210709/show-content-for-menuitem-when-menusubitems-exist-in-shiny-dashboard


ui <- dashboardPage(skin = "black",
                    dashboardHeader(title = "US Regional Data",tags$li(class = "dropdown", actionButton(inputId='show', label="Learn More", icon = icon("th")))),
                  #  tags$li(class = "dropdown", actionButton("home", "Home")))
                    
                    
                    dashboardSidebar(
                      sidebarMenu(id = "sidebarmenu",
                                  menuItem("Dashboard", tabName = "dashboard",  icon = icon("group", lib="font-awesome")),
                                  conditionalPanel("input.sidebarmenu === 'dashboard'"),
                                  
                                  
                                  menuItem("Jobless Claims", tabName = "claims", icon = icon("check-circle", lib = "font-awesome")),
                                  conditionalPanel("input.sidebarmenu === 'claims'"
                                         #          radioButtons("type", "Select Data",
                                          #                      c("Initial Jobless Claims" = "initial_claims",
                                           #                       "Continuing Claims" = "continuing_claims")),
                                               #    radioButtons("when", "Select Data",
                                                #                c("Latest" = "latest",
                                                 #                 "1m change" = "1m_chg",
                                                #                  "3m change" = "3m_chg",
                                                 #                 "6m change" = "6m_chg")),
                                                 #  sliderTextInput("range_us", "Select Mapping Date", 
                                                  #                choices = as.Date(data_jobless_claims2$rptdate),
                                                   #                selected = as.Date(max(data_jobless_claims2$rptdate)),
                                                    #               animate =
                                                     #               animationOptions(interval = 15, loop = FALSE))  
                                  ),
                                  
                                  
                                  menuItem("Labor Market", tabName = "labor_market", icon = icon("check-circle", lib = "font-awesome")),
                                  
                                  conditionalPanel("input.sidebarmenu === 'labor_market'"
                                              #     radioButtons("labor_type", "Select Data",
                                               #                 c("Unemployment" = "unemployment",
                                                #                  "Participation Rate" = "participation",
                                                 #                 "Employed-to-Population Ratio" = "employed_ratio")),
                                                #   
                                                 #  sliderTextInput("range_labor", "Select Mapping Date", 
                                                  #                 choices = as.Date(data_employment$rptdate),
                                                   #                selected = as.Date(max(data_employment$rptdate)),
                                                    #               animate =
                                                     #                animationOptions(interval = 10, loop = FALSE))
                                  ),
                                  
                                                   
                                  menuItem("GDP", tabName = "gdp", icon = icon("check-circle", lib = "font-awesome")),
                                                   
                                          conditionalPanel("input.sidebarmenu === 'gdp'"
#                                                             radioButtons("gdp_type", "Select GDP input",
 #                                                                         c("GDP QoQ" = "gdp_qoq",
  #                                                                          "GDP YoY" = "gdp_yoy",
   #                                                                        "GDP Share of Economy" = "gdp_share")),
    #                                                                
     #                                     sliderTextInput("range_gdp", "Select Mapping Date", 
      #                                    choices = as.Date(table_gdp_1_change$rptdate),
       #                                    selected = as.Date(max(table_gdp_1_change$rptdate)),
        #                                  animate =
         #                                   animationOptions(interval = 15, loop = FALSE))                                       
                                                   
                                  )
                      )
                      # sliderInput("x", "Outside of menu", 1, 100, 50)
                    ),
                    
                    dashboardBody(
                      
                      tabItems(
                        
                        tabItem(
                          tabName = "dashboard", class='active',
                          fluidRow(
                            
                            
                            
                           # box(title="Summary US. Data based on release date of regional data. There may be a later print on aggregate level.",
                           box(title="This page aims to give a deeper knowledge of US Regional Economy.",
                               
                                status="primary", solidHeader = TRUE,width=12,
                               
                                
                            #nedan styr hÃ¶
                            tags$head(tags$style(HTML(".small-box {height: 120px}"))),
                            valueBoxOutput(outputId = "dash_initial"),
                            
                           # column(width=4, height =5, valueBoxOutput("dash_initial", width=NULL)),
                                   
#                            valueBoxOutput("dash_initial", width = 2),
                            valueBoxOutput("dash_continuing"),
                            valueBoxOutput("dash_claims_percent"),
                            valueBoxOutput("dash_unemployment"),
                            valueBoxOutput("dash_participation"),
                            valueBoxOutput("dash_employed_to_pop_rate"),
                            valueBoxOutput("dash_gdp_qoq"),
                            valueBoxOutput("dash_gdp_yoy")),

                          #  actionButton(inputId='show', label="Learn More", icon = icon("th"))),

                            


                            box(title="Regional Dashboard - Plot latest available regional data",
                                status="primary", solidHeader = TRUE,width=12,
                            
                            box(#collapsible = TRUE,
                            radioButtons("type1", "Select X-axis",
                                         c("Jobless Claims" = "initial_claims",
                                           "Conituing Claims" = "continuing_claims",
                                           "Unemployment" = "unemployment_rate",
                                           "Participation Rate" = "participation_rate",
                                           "GDP QoQ" = "gdp_qoq",
                                           "GDP YoY" = "gdp_yoy",
                                           "GDP Share of Economy" = "gdp_share")),width=6),
                            box(#collapsible = TRUE,
                            radioButtons("type2", "Select Y-axis",
                                         c("Jobless Claims" = "initial_claims",
                                           "Conituing Claims" = "continuing_claims",
                                           "Unemployment" = "unemployment_rate",
                                           "Participation Rate" = "participation_rate",
                                           "GDP QoQ" = "gdp_qoq",
                                           "GDP YoY" = "gdp_yoy",
                                           "GDP Share of Economy" = "gdp_share")),width=6),
                                      
                            
                            box(#title="Plot latest data",
                                #status="primary",solidHeader = TRUE,
                                
                                
                                
                                
                                plotlyOutput(outputId = "scatter_all"), height=500, width=12))
                            


                            #  box(title="Dynamic Flow",
                            #status="primary",solidHeader = TRUE,plotlyOutput(outputId = "flow"), height=500, width=12)
  
                          )),
                        
                        
                        
                        tabItem(
                          tabName = "claims",
                          fluidRow(
                            
                            shinydashboard::valueBoxOutput("initial"),
                            shinydashboard::valueBoxOutput("continuing"),
                            

                            
                          box(title="Select input parameters",
                              status="primary", solidHeader = TRUE,width=12,
                              
                              box(#collapsible = TRUE,
                                radioButtons("type", "Select Data",
                                             c("Initial Jobless Claims" = "initial_claims",
                                               "Continuing Claims" = "continuing_claims")),width=6),
                                box(#collapsible = TRUE,

                                sliderTextInput("range_us", "Select Mapping Date", 
                                                choices = as.Date(data_jobless_claims2$rptdate),
                                                selected = as.Date(last(data_jobless_claims2$rptdate)),
                                                animate =
                                                  animationOptions(interval = 15, loop = FALSE)),width=6)), 
                                
                                
                          
                            box(title="Initial Jobless Claims and Continuing Claims in percent of civ population",
                                status="primary",solidHeader = TRUE,plotlyOutput(outputId = "map_us"), height=500, width=12),
                            
                            box(title="Initial Jobless Claims and Continuing Claims in percent of civ population",
                                status="primary",solidHeader = TRUE,plotlyOutput(outputId = "scatter_claims"), height=500, width=12))),
                        
                        
                        tabItem(
                          tabName = "labor_market", 
                          fluidRow(
                            
                            valueBoxOutput("unemployment"),
                            valueBoxOutput("participation"),
                            valueBoxOutput("employed_to_pop_rate"),
                            
                            
                            box(title="Select input parameters",
                                status="primary", solidHeader = TRUE,width=12,
                                
                                box(#collapsible = TRUE,
                                  radioButtons("labor_type", "Select Data",
                                               c("Unemployment" = "unemployment",
                                                 "Participation Rate" = "participation",
                                                 "Employed-to-Population Ratio" = "employed_ratio")),width=6),
                                box(#collapsible = TRUE,
                                  
                                  sliderTextInput("range_labor", "Select Mapping Date", 
                                                  choices = as.Date(data_employment$rptdate),
                                                  selected = as.Date(last(data_employment$rptdate)),
                                                  animate =
                                                    animationOptions(interval = 3, loop = FALSE)),width=6)), 
                            
                            box(title="US Labor Market",
                                status="primary",solidHeader = TRUE,plotlyOutput(outputId = "map_labor_market"), height=500, width=12),
                            box(title="US Labor Market",
                                status="primary",solidHeader = TRUE,plotlyOutput(outputId = "scatter_labor"), height=500, width=12))),
                          
                          
                        tabItem(
                          tabName = "gdp", 
                          fluidRow(
                            
                            valueBoxOutput("gdp_qoq"),
                            valueBoxOutput("gdp_yoy"),
                            
                            box(title="Select input parameters",
                                status="primary", solidHeader = TRUE,width=12,
                                
                                box(#collapsible = TRUE,
                                  radioButtons("gdp_type", "Select GDP input",
                                               c("GDP QoQ" = "gdp_qoq",
                                                 "GDP YoY" = "gdp_yoy",
                                                 "GDP Share of Economy" = "gdp_share")),width=6),
                                box(#collapsible = TRUE,
                                  
                                  sliderTextInput("range_gdp", "Select Mapping Date", 
                                                  choices = as.Date(table_gdp_1_change$rptdate),
                                                  selected = as.Date(last(table_gdp_1_change$rptdate)),
                                                  animate =
                                                    animationOptions(interval = 15, loop = FALSE)) ,width=6)), 
          # 

          

          
          
          
                          box(title= "US GDP",
                                status="primary",solidHeader = TRUE,plotlyOutput(outputId = "map_gdp"), height=500, width=12),
                            box(title="US GDP",
                                status="primary",solidHeader = TRUE,plotlyOutput(outputId = "scatter_gdp"), height=500, width=12)))
                        
                        

                      )
                      
                    ))
#https://github.com/mcpasin/PlayingGoogleAnalyticsDataViz/blob/master/app.R









server <- shinyServer(
  
  
  function(input,output, session){
    
    datasetInput <- reactive({
      
      data_jobless_claims2%>%
        filter(rptdate==input$range_us)
    })
    
    #unemployment
    datasetInput_2 <- reactive({
      
      data_employment%>%
        filter(rptdate==input$range_labor)
    })
    
    #gdp
    datasetInput_3 <- reactive({
      
      table_gdp_1_change%>%
        filter(rptdate==input$range_gdp)
    })
    
    
    #gdp
    datasetInput_4 <- reactive({
      
      joint_table
      })
    
    
    
    output$map_us <- renderPlotly({
      
      dataset <- datasetInput()
      
      claims_diff <- dataset$initial_claims_relative_pop
     # title_ = paste("US Initial Jobless Claims in percentage of population",'<br>', as.Date(max(dataset$rptdate)))
      min_legend <- min(data_jobless_claims3$initial_claims_relative_pop)
      max_legend <- max(data_jobless_claims3$initial_claims_relative_pop)
      title_ =  as.Date(max(dataset$rptdate))
      #claims_diff <- dataset$initial_claims / dataset$pop
      if (input$type == "initial_claims"){
        claims_diff <- dataset$initial_claims_relative_pop
        title_ =  as.Date(max(dataset$rptdate))
        
      #  if (input$when == "latest") {
      #    claims_diff <- dataset$initial_claims_relative_pop
      #    title_ = "US Initial Jobless Claims in percentage of population"
      #  }
      #  else if (input$when == "1m_chg") {
      #    claims_diff <- dataset$initial_1m
      #    title_ = "US Initial Jobless Claims in percentage of population. 1m change"
      #  }
      #  else if (input$when == "3m_chg") {
      #    claims_diff <- dataset$initial_3m
      #    title_ = "US Initial Jobless Claims in percentage of population. 3m change"
      #  }
      #  else if (input$when == "6m_chg") {
      #    claims_diff <- dataset$initial_6m
      #    title_ = "US Initial Jobless Claims in percentage of population. 6m change"
      #  }      
      #  else if (input$when == "12m_chg") {
    #      claims_diff <- dataset$initial_12m
    #      title_ = "US Initial Jobless Claims in percentage of population. 12m change"
     #   }  
        dataset$hover <- with(dataset, paste(state_long, '<br>', "Date", rptdate, "Initial Jobless Claims", jobless_claims, "<br>"
        ))
        text_hover <- with(dataset, paste(state_long, '<br>', "Initial Jobless Claims:", jobless_claims, "<br>","Initial Jobless Claims / Pop:", initial_claims_relative_pop))
        
      }
      
      else if (input$type == "continuing_claims"){
              claims_diff <- dataset$continuing_claims_relative_pop
              title_ =  as.Date(max(dataset$rptdate))-7
        
     #   if (input$when == "latest") {
    #      claims_diff <- dataset$continuing_claims_relative_pop
    #      title_ = "US Continuing Claims in percentage of population"
    #    }
    #    else if (input$when == "1m_chg") {
    #      claims_diff <- dataset$continuing_1m
    #      title_ = "US Continuing Claims in percentage of population. 1m change"
    #    }
    #    else if (input$when == "3m_chg") {
    #      claims_diff <- dataset$continuing_3m
    #      title_ = "US Continuing Claims in percentage of population. 3m change"
    #    }
    #    else if (input$when == "6m_chg") {
    #      claims_diff <- dataset$continuing_6m
    #      title_ = "US Continuing Claims in percentage of population. 6m change"        
    #    }      
    #    else if (input$when == "12m_chg") {
    #      claims_diff <- dataset$continuing_12m
    #      title_ = "US Continuing Claims in percentage of population. 12m change"
    #    }  
        
        dataset$hover <- with(dataset, paste(state_long, '<br>', "Date", rptdate, "Continuing Claims", continuing_claims, "<br>"))
        text_hover <- with(dataset, paste(state_long, '<br>', "Continuing Claims:", continuing_claims, "<br>","Continuing Claims / Pop:", continuing_claims_relative_pop))
        min_legend <- min(data_jobless_claims3$continuing_claims_relative_pop)
        max_legend <- max(data_jobless_claims3$continuing_claims_relative_pop)
        
    } 
      
      state_codes = dataset$state_short
      
      
   

      
      plot_ly(
        dataset,
        z=claims_diff,
        locations=dataset$state_short,
#        text=paste0(dataset$state_long, '<br>Initial Claims: ', dataset$claims_diff),
        text = text_hover,
        type="choropleth",
        locationmode="USA-states",
        colors = 'Purples',
zmin=min_legend,
zmax=max_legend,
        # autocolorscale=FALSE, zmin=0, zmax=100,
        filename="stackoverflow/simple-choropleth"
      ) %>%
        
        layout(title=paste('<br>', title_), geo = list(scope="usa"))%>%
        hide_colorbar() %>%
        plotly::config(displayModeBar = F)%>%
        layout(annotations = 
                 list(x = 1, y = -0.1, text = "Source: US Department of Labor", 
                      showarrow = F, xref='paper', yref='paper', 
                      xanchor='right', yanchor='auto', xshift=0, yshift=0,
                      font=list(size=10, color="black")))
      
      
      
      ###https://stackoverflow.com/questions/41731818/how-can-i-apply-the-same-colorscale-to-2-maps-in-plotly-using-r
      ###https://plotly.com/r/choropleth-maps/
      
    })
    
    
    output$scatter_claims <- renderPlotly({
      
      dataset <- datasetInput()
      

      
     # title_ <- as.Date(input$range_us)
      
      fig <- plot_ly(data = dataset, x = ~continuing_claims_relative_pop, y = ~initial_claims_relative_pop, color = ~state_group, symbol = ~state_group,
                     colors = 'Purple',
                                  
                     text = ~paste(state_long,'<br>',"Initial Claims: ", initial_claims_relative_pop, '<br>Continuing Claims:', continuing_claims_relative_pop))%>%
        layout(
               xaxis = list(title = "Continuing Claims", range=c(0, max(data_jobless_claims3$continuing_claims_relative_pop))),
               yaxis = list(title = "Initial Jobless Claims", range=c(0, max(data_jobless_claims3$initial_claims_relative_pop))))%>%
        layout(legend = list(orientation = "h",xanchor = "center", x = 0.4, y = -0.2))%>%
        hide_colorbar() %>%
        plotly::config(displayModeBar = F)%>%
        layout(annotations = 
                 list(x = 1, y = -0.1, text = "Source: US Department of Labor", 
                      showarrow = F, xref='paper', yref='paper', 
                      xanchor='right', yanchor='auto', xshift=0, yshift=0,
                      font=list(size=10, color="black")))
        
      
      
      
    })
    
    ###
    ###
    ###
    ###
    ###
    
    output$map_labor_market <- renderPlotly({
      
      dataset2 <- datasetInput_2()
      labor <- data_employment$unemployment
      min_legend <- min(data_employment2$unemployment)
      max_legend <- max(data_employment2$unemployment)
      if (input$labor_type == "unemployment"){
        labor <- dataset2$unemployment
        title_ = "Unemployment per state"
        text_hover <- with(dataset2, paste(state_long, '<br>', "Date", rptdate, "Unemployment", unemployment, "<br>"))
        min_legend <- min(data_employment2$unemployment)
        max_legend <- max(data_employment2$unemployment)
        
      }
      else if (input$labor_type == "participation"){
        labor <- dataset2$participation_rate
        title_ = "Participation Rate per state"
        text_hover <- with(dataset2, paste(state_long, '<br>', "Date", rptdate, "Participation Rate", participation_rate, "<br>"))
        min_legend <- min(data_employment2$participation_rate)
        max_legend <- max(data_employment2$participation_rate)
      }
      
    
    else if (input$labor_type == "employed_ratio"){
      labor <- dataset2$employed_to_pop_rate
      title_ = "Employed-to-Population Ratio per state"
      text_hover <- with(dataset2, paste(state_long, '<br>', "Date", rptdate, "Employed-to-Population Ratio", employed_to_pop_rate, "<br>"))
      min_legend <- min(data_employment2$employed_to_pop_rate)
      max_legend <- max(data_employment2$employed_to_pop_rate)
    }
      
      
      
      title_ <- as.Date(input$range_labor)
      
      #title_ <- as.Date(input$range_labor)
      
      plot_ly(
        dataset2,
        z=labor,
        locations=dataset2$state_short,
        text = text_hover,
#        text=paste0(dataset2$state_long, '<br>Labor Market: ', dataset2$labor),
        type="choropleth",
        locationmode="USA-states",
        colors = 'Purples',
zmin=min_legend,
zmax=max_legend,
        # autocolorscale=FALSE, zmin=0, zmax=100,
        filename="stackoverflow/simple-choropleth2"
      ) %>%
        
        layout(title=paste('<br>', title_), geo = list(scope="usa"))%>%
        hide_colorbar() %>%
        plotly::config(displayModeBar = F)%>%
        layout(annotations = 
                 list(x = 1, y = -0.1, text = "Source: US Bureau of Labor Statistics", 
                      showarrow = F, xref='paper', yref='paper', 
                      xanchor='right', yanchor='auto', xshift=0, yshift=0,
                      font=list(size=10, color="black")))
      
       })
    
    
    output$scatter_labor <- renderPlotly({
      
      dataset2 <- datasetInput_2()
      
      
      
      
      
      title_ <- as.Date(input$range_labor)
      
      fig <- plot_ly(data = dataset2, x = ~participation_rate, y = ~unemployment, color = ~state_group, symbol = ~state_group, colors = 'Purple',
                     text = ~paste(state_long,'<br>',"Unemployment: ", unemployment, '<br>Participation Rate:', participation_rate))%>%
        layout(title=paste('<br>', title_),
               xaxis = list(title = "Participation Rate", range=c(min(data_employment2$participation_rate)-1, max(data_employment2$participation_rate)+1)),
               yaxis = list(title = "Unemployment Rate", range=c(min(data_employment2$unemployment)-1, max(data_employment2$unemployment)+1)))%>%
       # layout(legend = list(orientation = "h",   # show entries horizontally
        #                     xanchor = "center",  # use center of legend as anchor
         #                    x = 1)) 
        layout(legend = list(orientation = "h",xanchor = "center", x = 0.4, y = -0.2))%>%
        hide_colorbar() %>%
        plotly::config(displayModeBar = F)%>%
        layout(annotations = 
                 list(x = 1, y = -0.1, text = "Source: US Bureau of Labor Statistics", 
                      showarrow = F, xref='paper', yref='paper', 
                      xanchor='right', yanchor='auto', xshift=0, yshift=0,
                      font=list(size=10, color="black")))
      
      
    })

    
    ####
    ####
    ####
    ####
    ####
    
  #######GDP  
    output$map_gdp <- renderPlotly({
      
      dataset3 <- datasetInput_3()
      value <- table_gdp_1_change$gdp_qoq
      min_legend <- min(table_gdp_1_change$gdp_qoq)
      max_legend <- max(table_gdp_1_change$gdp_qoq)
      
      if (input$gdp_type == "gdp_qoq"){
        value <- dataset3$gdp_qoq
        title_ = "GDP QoQ"
        text_hover <- with(dataset3, paste(state_long, '<br>', "Date", rptdate, "GDP QoQ", gdp_qoq, "<br>"))
        min_legend <- min(table_gdp_1_change$gdp_qoq)
        max_legend <- max(table_gdp_1_change$gdp_qoq)
      }
      
      else if (input$gdp_type == "gdp_yoy"){
        value <- dataset3$gdp_yoy
        title_ = "GDP  YoY"
        text_hover <- with(dataset3, paste(state_long, '<br>', "Date", rptdate, "GDP YoY", gdp_yoy, "<br>"))
        min_legend <- min(table_gdp_1_change$gdp_yoy)
        max_legend <- max(table_gdp_1_change$gdp_yoy)
      }
      
      else if (input$gdp_type == "gdp_share"){
        value <- dataset3$gdp_share
        title_ = "GDP Share of Economy"
        text_hover <- with(dataset3, paste(state_long, '<br>', "Date", rptdate, "GDP share of Economy", gdp_share, "<br>"))
        min_legend <- min(table_gdp_1_change$gdp_share)
        max_legend <- max(table_gdp_1_change$gdp_share)
      }
      
      title_ <- as.Date(input$range_gdp)
      
     # title_ <- as.Date(input$range_labor)
      
      plot_ly(
        dataset3,
        z=value,
        locations=dataset3$state_short,
        #text=paste0(dataset3$state_long, '<br>Labor Market: ', dataset3$value),
        #text = ~paste(state_long,'<br>',"GDP QoQ: ", gdp_qoq, '<br>GDP Share of Economy:', gdp_share),
       text = text_hover,
         type="choropleth",
        locationmode="USA-states",
        colors = 'Purples',
       zmin=min_legend,
       zmax=max_legend,
        # autocolorscale=FALSE, zmin=0, zmax=100,
        filename="stackoverflow/simple-choropleth2"
      ) %>%
        
        layout(title=paste('<br>', title_), geo = list(scope="usa"))%>%
        hide_colorbar() %>%
        plotly::config(displayModeBar = F)%>%
        layout(annotations = 
                 list(x = 1, y = -0.1, text = "Source: US Bureau of Economic Analysis", 
                      showarrow = F, xref='paper', yref='paper', 
                      xanchor='right', yanchor='auto', xshift=0, yshift=0,
                      font=list(size=10, color="black")))
      
    })
    
    output$scatter_gdp <- renderPlotly({
      
      dataset3 <- datasetInput_3()
      
      
      value <- table_gdp_1_change$gdp_qoq
      if (input$gdp_type == "gdp_qoq"){
        value <- dataset3$gdp_qoq
        title_ = "GDP QoQ"
        text_hover <- with(dataset3, paste(state_long,'<br>',"GDP QoQ: ", gdp_qoq, '<br>GDP Share of Economy:', gdp_share))
      }
      else if (input$gdp_type == "gdp_yoy"){
        value <- dataset3$gdp_yoy
        title_ = "GDP  YoY"
        text_hover <- with(dataset3, paste(state_long,'<br>',"GDP YoY: ", gdp_yoy, '<br>GDP Share of Economy:', gdp_share))
      }
      
      #behÃ¶ver ha en ifall det blir gdp_share fÃ¶r den andra grafen.
      if (input$gdp_type == "gdp_share"){
        value <- dataset3$gdp_qoq
        title_ = "GDP QoQ"
        text_hover <- with(dataset3, paste(state_long,'<br>',"GDP QoQ: ", gdp_qoq, '<br>GDP Share of Economy:', gdp_share))
      }
      
      
      
      

      title_ <- as.Date(input$range_gdp)
      
      fig <- plot_ly(data = dataset3, x = ~gdp_share, y = ~value, color = ~state_group, symbol = ~state_group, colors = 'Purple',
                    text =text_hover)%>%
                    #text = ~paste(state_long,'<br>',"GDP QoQ: ", gdp_qoq, '<br>GDP Share of Economy:', gdp_share))%>%
        layout(title=paste('<br>', title_),
               xaxis = list(title = "GDP Share of Economy", range=c(min(table_gdp_1_change2$gdp_share)-1, max(table_gdp_1_change2$gdp_share)+1)),
               yaxis = list(title = "GDP QoQ", range=c(min(table_gdp_1_change2$gdp_qoq)-1, max(table_gdp_1_change2$gdp_qoq)+1)))%>%
        layout(legend = list(orientation = "h",xanchor = "center", x = 0.4, y = -0.2))%>%
        hide_colorbar() %>%
        plotly::config(displayModeBar = F)%>%
        layout(annotations = 
                 list(x = 1, y = -0.1, text = "Source: US Bureau of Economic Analysis", 
                      showarrow = F, xref='paper', yref='paper', 
                      xanchor='right', yanchor='auto', xshift=0, yshift=0,
                      font=list(size=10, color="black")))
      
      
    })
    
    ##Scatetr all
    
    
    output$scatter_all <- renderPlotly({
      
      dataset4 <- datasetInput_4()
      
      
      value1 <- joint_table$initial_latest
      value2 <- joint_table$gdp_share
      
      x_name <- "Initial Jobless Claims relative pop"
      y_name <- "GDP Share of Economy"
      
      if (input$type1 == "initial_claims"){
        value1 <- dataset4$initial_latest
        #  title_ = "GDP QoQ"
        #  dataset3$hover <- with(dataset3, paste(state_long, '<br>', "Date", rptdate, "GDP", gdp, "<br>"))
        x_name <- "Initial Jobless Claims relative pop"
      }
      else if (input$type1 == "continuing_claims"){
        value1 <- dataset4$continuing_latest
        #   title_ = "GDP Share of Economy"
        # dataset3$hover <- with(dataset3, paste(state_long, '<br>', "Date", rptdate, "GDP share of Economy", gdp_share, "<br>"))
        x_name <- "Continuing Claims relative pop"
        }
      else if (input$type1 == "unemployment_rate"){
        value1 <- dataset4$unemployment
        x_name <- "Unemployment"
        }
      else if (input$type1 == "participation_rate"){
        value1 <- dataset4$participation_rate
        x_name <- "Participation Rate"
      }
      else if (input$type1 == "gdp_qoq"){
        value1 <- dataset4$gdp_qoq
        x_name <- "GDP QoQ"
      }
      else if (input$type1 == "gdp_yoy"){
        value1 <- dataset4$gdp_yoy
        x_name <- "GDP YoY"
      }
      else if (input$type1 == "gdp_share"){
        value1 <- dataset4$gdp_share
        x_name <- "GDP Share of Economy"
      }
      
      
      if (input$type2 == "initial_claims"){
        value2 <- dataset4$initial_latest
     #  title_ = "GDP QoQ"
      #  dataset3$hover <- with(dataset3, paste(state_long, '<br>', "Date", rptdate, "GDP", gdp, "<br>"))
        y_name <- "Initial Jobless Claims relative pop"
      }
      else if (input$type2 == "continuing_claims"){
        value2 <- dataset4$continuing_latest
     #   title_ = "GDP Share of Economy"
        y_name <- "Continuing Claims relative pop"
       # dataset3$hover <- with(dataset3, paste(state_long, '<br>', "Date", rptdate, "GDP share of Economy", gdp_share, "<br>"))
      }
      else if (input$type2 == "unemployment_rate"){
        value2 <- dataset4$unemployment
        y_name <- "Unemployment Rate"
             }
      else if (input$type2 == "participation_rate"){
        value2 <- dataset4$participation_rate
        y_name <- "Participation Rate"
      }
      else if (input$type2 == "gdp_qoq"){
        value2 <- dataset4$gdp_qoq
        y_name <- "GDP QoQ"
      }
      else if (input$type2 == "gdp_yoy"){
        value2 <- dataset4$gdp_yoy
        y_name <- "GDP YoY"
      }
      else if (input$type2 == "gdp_share"){
        value2 <- dataset4$gdp_share
        y_name <- "GDP Share of Economy"
      }
      

        
      title_ <- "Latest Data"
      
      fig <- plot_ly(data = dataset4, x = ~value1, y = ~value2, color = ~state_group, symbol = ~state_group, colors = 'Purple',
                     text = ~paste(state_long,'<br>', x_name,round(value1,1), '<br>', y_name, round(value2,1)))%>%
        layout(title=paste('<br>', title_),
            xaxis = list(title = x_name),
            yaxis = list(title = y_name))%>%
                            #  xaxis = list(title = "GDP Share of Economy", range=c(min(dataset4$gdp_share)-1, max(table_gdp_1_change2$gdp_share)+1)),
            #   yaxis = list(title = "GDP QoQ", range=c(min(dataset4$gdp)-1, max(table_gdp_1_change2$gdp)+1)))%>%
        layout(legend = list(orientation = "h",xanchor = "center", x = 0.4, y = -0.2))%>%
        hide_colorbar() %>%
        plotly::config(displayModeBar = F)
      
      
      
    })
    
    
    
    
    
    
    #####
    #creating the valueBoxOutput content
    output$initial <- renderValueBox({
      
      
      temp_data <- data_jobless_claims2%>%
        filter(rptdate == input$range_us)%>%
        filter(state_short == "US")%>%
        group_by(rptdate)%>%
        summarise(total_claims = sum(jobless_claims))
        total <- temp_data$total_claims
      
      #valueBox("Initial Jobless Claims", total)
      valueBox(
        formatC(total, format="d", big.mark=',')
        ,paste('Initial Jobless Claims')
        ,color = "light-blue")  
    })
    output$continuing <- renderValueBox({ 
      
      temp_data <- data_jobless_claims2%>%
        filter(rptdate == input$range_us)%>%
        filter(state_short == "US")%>%
        group_by(rptdate)%>%
        summarise(total_claims = sum(continuing_claims))
      total <- temp_data$total_claims
      
      valueBox(
        formatC(total, format="d", big.mark=',')
        ,paste('Continuing Jobless Claims')
        ,color = "light-blue")   
    })
    
    
    #creating the valueBoxOutput content: Labor
    output$unemployment <- renderValueBox({
      
      
      temp_data <- data_employment%>%
        filter(rptdate == (input$range_labor))%>%
        filter(state_short == "US")%>%
        mutate(unemployed_population = as.numeric(gsub(",","",unemployed_population,fixed=TRUE)))%>%
        mutate(participation = as.numeric(gsub(",","",participation,fixed=TRUE)))%>%
        group_by(rptdate)%>%
        summarise(country_unemployment = sum(unemployed_population)/sum(participation) * 100)
      total <- paste(round(temp_data$country_unemployment,1),"%", sep="")
      
      #valueBox("Initial Jobless Claims", total)
      valueBox(
        formatC(total, format="d", big.mark=',')
        ,paste('Unemployment')
        ,color = "light-blue")  
    })
    output$participation <- renderValueBox({ 
      
      temp_data <- data_employment%>%
        filter(rptdate == (input$range_labor))%>%
        filter(state_short == "US")%>%
        mutate(civ_population = as.numeric(gsub(",","",civ_population,fixed=TRUE)))%>%
        mutate(participation = as.numeric(gsub(",","",participation,fixed=TRUE)))%>%
        group_by(rptdate)%>%
        summarise(country_participation = sum(participation)/sum(civ_population)*100)
      
      total <- paste(round(temp_data$country_participation,1),"%", sep="")
      
      valueBox(
        formatC(total, format="d", big.mark=',')
        ,paste('Participation Rate')
        ,color = "light-blue")   
    })
    
    output$employed_to_pop_rate <- renderValueBox({ 
      
      temp_data <- data_employment%>%
        filter(rptdate == (input$range_labor))%>%
        filter(state_short == "US")%>%
        mutate(civ_population = as.numeric(gsub(",","",civ_population,fixed=TRUE)))%>%
        mutate(employed_population = as.numeric(gsub(",","",employed_population,fixed=TRUE)))%>%
        group_by(rptdate)%>%
        summarise(country_employment = sum(employed_population)/sum(civ_population)*100)
      
      total <- paste(round(temp_data$country_employment,1),"%", sep="")
      
      valueBox(
        formatC(total, format="d", big.mark=',')
        ,paste('Employed-to-Population')
        ,color = "light-blue")   
    })
    
    
    
    
    #creating the valueBoxOutput content: GDP
    output$gdp_qoq <- renderValueBox({
      
      
      temp_data <- table_gdp_1_change%>%
        filter(rptdate == (input$range_gdp))%>%
        filter(state_short == "US")
       # mutate(unemployed_population = as.numeric(gsub(",","",unemployed_population,fixed=TRUE)))%>%
      #  mutate(participation = as.numeric(gsub(",","",participation,fixed=TRUE)))%>%
  #      group_by(rptdate)%>%
        #ummarise(country_unemployment = sum(unemployed_population)/sum(participation) * 100)
      total <- paste(round(temp_data$gdp_qoq,1),"%", sep="")
      
      #valueBox("Initial Jobless Claims", total)
      valueBox(
        formatC(total, format="d", big.mark=',')
        ,paste('GDP QoQ, ',input$range_gdp)
        ,color = "light-blue")  
    })
    output$gdp_yoy <- renderValueBox({ 
      
      temp_data <- table_gdp_1_change%>%
        filter(rptdate == (input$range_gdp))%>%
        filter(state_short == "US")
     #   mutate(civ_population = as.numeric(gsub(",","",civ_population,fixed=TRUE)))%>%
      #  mutate(participation = as.numeric(gsub(",","",participation,fixed=TRUE)))%>%
      #  group_by(rptdate)%>%
       # summarise(country_participation = sum(participation)/sum(civ_population)*100)
      
      total <- paste(round(temp_data$gdp_yoy,1),"%", sep="")
      
      valueBox(
        formatC(total, format="d", big.mark=',')
        ,paste('GDP YoY, ', input$range_gdp)
        ,color = "light-blue")   
    })
    
    
    
    
    ###########
    ####Valuebox to dashboard
    ###########
    
    output$dash_initial <- renderValueBox({ 
        temp_data <- joint_table%>%
       # filter(rptdate == (input$range_labor))%>%
        filter(state_short == "US")
#        total <- paste(round(temp_data$jobless_claims,1),"%", sep="")
        total <- temp_data$jobless_claims
        date_ <- temp_data$rptdate_claims

        valueBox(
          formatC(total, format="d", big.mark=',')
          ,paste('Initial Claims, ', date_)
          ,color = "light-blue")   
            })
    
    output$dash_continuing <- renderValueBox({ 
      temp_data <- joint_table%>%
        # filter(rptdate == (input$range_labor))%>%
        filter(state_short == "US")
      #        total <- paste(round(temp_data$jobless_claims,1),"%", sep="")
      total <- temp_data$continuing_claims
      date_ <- temp_data$rptdate_claims - 7
      
      valueBox(
        formatC(total, format="d", big.mark=',')
        ,paste('Continuing Claims, ', date_)
        ,color = "light-blue")   
    })
    
    
    output$dash_claims_percent <- renderValueBox({ 
      temp_data <- joint_table%>%
        # filter(rptdate == (input$range_labor))%>%
        filter(state_short == "US")
      total <- paste(round(temp_data$initial_latest,1),"% / ",round(temp_data$continuing_latest,1), "%", sep="")
 #     total <- temp_data$continuing_claims
      date_ <- temp_data$rptdate_claims - 7
      
      valueBox(
        formatC(total, format="d", big.mark=',')
        ,paste('Inital /  Continuing relative population')
        ,color = "light-blue")   
    })
    
    output$dash_unemployment <- renderValueBox({ 
      temp_data <- joint_table%>%
        # filter(rptdate == (input$range_labor))%>%
        filter(state_short == "US")
      total <- paste(round(temp_data$unemployment,1),"%", sep="")
      date_ <- temp_data$rptdate_labor
      
      valueBox(
        formatC(total, format="d", big.mark=',')
        ,paste('Unemployment, ', date_)
        ,color = "light-blue")   
    })
    
    output$dash_participation <- renderValueBox({ 
      temp_data <- joint_table%>%
        # filter(rptdate == (input$range_labor))%>%
        filter(state_short == "US")
      total <- paste(round(temp_data$participation_rate,1),"%", sep="")
      date_ <- temp_data$rptdate_labor
      
      valueBox(
        formatC(total, format="d", big.mark=',')
        ,paste('Participation Rate, ', date_)
        ,color = "light-blue")   
    })
    
    output$dash_employed_to_pop_rate <- renderValueBox({ 
      temp_data <- joint_table%>%
        # filter(rptdate == (input$range_labor))%>%
        filter(state_short == "US")
      total <- paste(round(temp_data$employed_to_pop_rate,1),"%", sep="")
      date_ <- temp_data$rptdate_labor
      
      valueBox(
        formatC(total, format="d", big.mark=',')
        ,paste('Employment-to-Population, ', date_)
        ,color = "light-blue")   
    })
    
    
    output$dash_gdp_qoq <- renderValueBox({ 
      temp_data <- joint_table%>%
        # filter(rptdate == (input$range_labor))%>%
        filter(state_short == "US")
      total <- paste(round(temp_data$gdp_qoq,1),"%", sep="")
      date_ <- temp_data$rptdate_gdp
      
      valueBox(
        formatC(total, format="d", big.mark=',')
        ,paste('GDP QoQ, ', date_)
        ,color = "light-blue")   
    })
    
    output$dash_gdp_yoy <- renderValueBox({ 
      temp_data <- joint_table%>%
        # filter(rptdate == (input$range_labor))%>%
        filter(state_short == "US")
      total <- paste(round(temp_data$gdp_yoy,1),"%", sep="")
      date_ <- temp_data$rptdate_gdp
      
      valueBox(
        formatC(total, format="d", big.mark=',')
        ,paste('GDP YoY, ', date_)
        ,color = "light-blue")   
    })
    

    
    observeEvent(input$show, {
      showModal(modalDialog(
        title = "More information",
        
        HTML("Data based on release date of regional data. There may be a later print on aggregate level. <br><br>
        <b>Initial Jobless Claims:</b> People filing to receive unenpmoyment insurance benefits for the first time. Reliable number. Moves close to general economy, good indicator. <br><br>
        <b>Continuing Jobless Claims:</b> People who are continuing filing to receive unenpmoyment insurance. <br><br>
        <b>Unemployment Rate:</b> The unemployment rate measured as the number of persons unemployed divided by the civilian labor force.<br><br>
        <b>Participation Rate:</b> The participation rate refers to the total number of people or individuals who are currently employed or in search of a job.<br><br>
        <b>Employment-to-population rate:</b> The employment-to-population ratio is equal to the number of persons employed divided by the working-age population.<br><br>
        <b>Civilian Population:</b> In the United States, the civilian noninstitutional population refers to people 16 years of age and older residing in the 50 States and the District of Columbia who are not inmates of institutions (penal, mental facilities, homes for the aged), and who are not on active duty in the Armed Forces.<br><br>
             
         "
        
        ),
        
        easyClose = TRUE
      ))
    })
    
  
    output$flow <- renderPlotly({
      gg <- ggplot(data_employment, aes(participation_rate, unemployment, color = state_group)) +
        geom_point(aes(size = unemployment, frame = year, ids = state_short))+
        theme_classic()
      
      
      ggplotly(gg)%>%
        layout(legend = list(orientation = "h",xanchor = "center", x = 0.4, y = -0.2))%>%
        hide_colorbar() %>%
        plotly::config(displayModeBar = F)%>%
        layout(annotations = 
                 list(x = 1, y = -0.1, text = "Source: US Bureau of Labor Statistics", 
                      showarrow = F, xref='paper', yref='paper', 
                      xanchor='right', yanchor='auto', xshift=0, yshift=0,
                      font=list(size=8, color="black")))
      
    })  
  })


#https://www.r-graph-gallery.com/stacked-area-chart-plotly.html


shiny::shinyApp(ui = ui, server = server)
