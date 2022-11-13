rm(list = ls())
library(shiny)
library(fullPage)
library(readxl)
library(usmap)
library(readr)
library(lubridate)
library(dplyr)
library(shinyBS)
library(shinyWidgets)



options(digits=16)

#read data
product <- readxl::read_xlsx("data/AB_Data.xlsx", sheet = 1)
customer <- readxl::read_xlsx("data/AB_Data.xlsx", sheet = 2)
sale <- readxl::read_xlsx("data/AB_Data.xlsx", sheet = 3)

# merge ab data
ab_data <- merge(sale, customer, all.x = TRUE, by = "cust_id")
ab_data <- merge(ab_data, product, all.x = TRUE, by = "sku_id")

#Processing data
ab_data$age <- trunc(as.numeric(difftime(Sys.Date(), ab_data$dob, units = "days")) / 365.25)
ab_data$product_total_usd <- ab_data$unit_price * ab_data$qty

# read nfl data
nfl_schedule  <- readxl::read_xlsx("data/NFL data collection_2022112.xlsx", sheet = 2)
nfl_team  <- readxl::read_xlsx("data/NFL data collection_2022112.xlsx", sheet = 1)
nfl_team <- nfl_team[1:32,]
nfl_team$team_logo <- paste0("nfl_team_logo/",nfl_team$`Team Name`,".webp")

nfl_schedule$id <- c(1:nrow(nfl_schedule))
nfl_schedule$game_week <- paste0("WEEK ",nfl_schedule$`Round Number`)

nfl_schedule$Date1 <- lubridate::dmy_hms(nfl_schedule$Date ,tz = "America/New_York")
nfl_schedule$Date2 <- janitor::excel_numeric_to_date(as.numeric(as.character(nfl_schedule$Date)),include_time = TRUE, tz = "EST")
nfl_schedule$Date1[which(is.na(as.POSIXct(nfl_schedule$Date1)))] <- nfl_schedule$Date2[which(is.na(as.POSIXct(nfl_schedule$Date1)))] 
nfl_schedule$Date <- nfl_schedule$Date1
nfl_schedule <- nfl_schedule[, !colnames(nfl_schedule) %in% c("Date1","Date2")]

nfl_schedule$time <- paste0(substr(as.character(nfl_schedule$Date), 12, 16), " EST")
nfl_schedule$weekdays <- lubridate::wday(nfl_schedule$Date , label = T, abbr = F)
nfl_schedule$month <- month(nfl_schedule$Date, label=TRUE, abbr = F)
nfl_schedule$day <-  ifelse(day(nfl_schedule$Date)==1, "1ST",
                            ifelse(day(nfl_schedule$Date)==2, "2ND",
                                   ifelse(day(nfl_schedule$Date)==3, "3RD",paste0(day(nfl_schedule$Date), "TH"))))
nfl_schedule$date_sche <- paste0(toupper(nfl_schedule$weekdays),", ",toupper(nfl_schedule$month ), " ",nfl_schedule$day)

nfl_schedule <- merge(nfl_schedule, nfl_team[,c("team_id","team_logo", "Team Name")], all.x = T, by.x = "Home Team", by.y = "Team Name")
names(nfl_schedule)[names(nfl_schedule) == "team_id"] <- "home_team_id"
names(nfl_schedule)[names(nfl_schedule) == "team_logo"] <- "home_team_logo"

nfl_schedule <- merge(nfl_schedule, nfl_team[,c("team_id","team_logo", "Team Name")], all.x = T, by.x = "Away Team", by.y = "Team Name")
names(nfl_schedule)[names(nfl_schedule) == "team_id"] <- "away_team_id"
names(nfl_schedule)[names(nfl_schedule) == "team_logo"] <- "away_team_logo"

nfl_schedule  <- nfl_schedule[order(nfl_schedule$Date),]



nfl_schedule$label <- paste0('
                             <div class="row" style="horizontal-align:middle;color:black;vertical-align:middle;" >
                             
                             <div class="col-md-2" style="text-align:left;padding-top:10px;">
                             <p style = "font-size:18px;">',nfl_schedule$time,'</p>
                             </div>
                             
                             <div class="col-md-3" style="text-align:left;padding-top:10px;">
                             <p style = "font-size:18px;">',nfl_schedule$`Home Team`,'</p>
                             </div>
                             
                             <div class="col-md-1" style="text-align:right;">
                             <img src="',nfl_schedule$home_team_logo,'" width="50" height="50"/>
                             </div>
                             
                             <div class="col-md-1" style="text-align:center;padding-top:10px;">
                             <img src="team_logo/vs_logo.png" width="20" height="20"/>
                             </div>
                             
                             <div class="col-md-1" style="text-align:left;">
                             <img src="',nfl_schedule$away_team_logo,'" width="50" height="50"/>
                             </div>
                             
                              <div class="col-md-3" style="text-align:left;padding-top:10px;">
                             <p style = "font-size:18px;">',nfl_schedule$`Away Team`,'</p>
                             </div>
                             
                             </div>
                             ')


# nfl_schedule1 <- nfl_schedule[nfl_schedule$game_week == "WEEK 13",]
# nfl_schedule1$id <- c(1:nrow(nfl_schedule1))
# 
# 
# schedule <- unique(nfl_schedule1$date_sche)
# i = 1
# for(i in 1:length(schedule)){
#   dt <- nfl_schedule1[nfl_schedule1$date_sche == schedule[i],]
# 
# }








  
