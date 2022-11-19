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
library(dplyr)
library(plotly)



library(leaflet)
#devtools::install_github('bhaskarvk/leaflet.extras')
library(leaflet.extras)# to add addSearchFeatures #http://leaflet-extras.github.io/leaflet-providers/preview/index.html
library(sp)
library(raster)
library(rgeos)
library(sf)
library(maps)
library(tools)



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
ab_data$age_group[ab_data$age <= 30 & ab_data$age >= 21] <- "22 - 30"
ab_data$age_group[ab_data$age <= 40 & ab_data$age >= 31] <- "31 - 40"
ab_data$age_group[ab_data$age <= 50 & ab_data$age >= 41] <- "41 - 50"
ab_data$age_group[ab_data$age <= 60 & ab_data$age >= 51] <- "51 - 60"
ab_data$age_group[ab_data$age >= 61] <- "Above 60"


ab_data$income_text[ab_data$income == "A) 0-49999" ] <- "Below 50K"
ab_data$income_text[ab_data$income == "B) 50000-99999"] <- "50K - 100K"
ab_data$income_text[ab_data$income == "C) 100000-149999" ] <- "100K - 150K"
ab_data$income_text[ab_data$income == "D) 150000-249999" ] <- "150K - 250K"
ab_data$income_text[ab_data$income == "E) 250000-499999" ] <- "250K - 500K"
ab_data$income_text[ab_data$income == "F) 500000-999999" ] <- "500K - 1M"
ab_data$income_text[ab_data$income == "G) 1000000+"  ] <- "Above 1M"

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


#============================================
# Tab 2

# working directory: Documents

product = read.csv('data/products.csv')
#glimpse(product)

sales = read.csv('data/sales.csv')
#glimpse(sales)

customer = read.csv('data/customer.csv')
#glimpse(customer)


# Data join
rfm_data = merge(x = sales, y = product, by = "sku_id", all.x=T, all.y=F)
# Selecting required columns
rfm_data = dplyr::select(rfm_data,c(sales_id,sku_id,cust_id,order_date,qty,ab,unit_price))


# Getting the Analysis Date
rfm_data$order_date <- as.Date(rfm_data$order_date, "%Y-%m-%d")
analysis_date = max(rfm_data$order_date)

# Adding Total Amount
rfm_data$total_amount <- rfm_data$qty * rfm_data$unit_price

# Getting customers who purchased ONLY AB branding historic sales

Data_tranform = rfm_data %>% group_by(cust_id,ab) %>% summarise(n())
AB_Brand = Data_tranform %>% group_by(cust_id) %>% summarise(AB_Brands = n())
customer = merge(x = Data_tranform, y = AB_Brand, by = "cust_id", all.x=F, all.y=F)
customer_AB_only = customer %>% filter(ab == 1 & AB_Brands ==1)
nrow(customer_AB_only)

# Customer sales with only AB brand
rfm_AB = merge(x = rfm_data, y = customer_AB_only, by = "cust_id", all.x=F, all.y=F)

# Creating data frame only for AB brand
rfm_AB =rfm_AB %>%
  group_by(cust_id) %>%
  summarise(Recency = as.numeric(analysis_date - max(order_date)),                Frequency = sum(qty),
            Monetary = sum(total_amount))

# Creating data frame only for non AB brand
rfm_nonAB = rfm_data %>%
  filter(ab==0) %>%
  group_by(cust_id) %>%
  summarise(Recency = as.numeric(analysis_date - max(order_date)),                Frequency = sum(qty),
            Monetary = sum(total_amount))


# RFM score for AB brand

quantile(rfm_AB$Recency, c(.333, .666))
rfm_AB$R <- ifelse(rfm_AB$Recency < 12.00, 3,ifelse(rfm_AB$Recency>103.32,1,2))

quantile(rfm_AB$Frequency, c(.333, .666))
rfm_AB$F <- ifelse(rfm_AB$Frequency < 3, 1,ifelse(rfm_AB$Frequency>29,3,2))

quantile(rfm_AB$Monetary, c(.333, .666))
rfm_AB$M <- ifelse(rfm_AB$Monetary < 44.3376, 1,ifelse(rfm_AB$Monetary>369.0064,3,2))

rfm_AB <- rfm_AB %>%
  mutate(RFM_score = 100 *R +10 * F + M)

# RFM score for nonAB brand

quantile(rfm_nonAB$Recency, c(.333, .666))
rfm_nonAB$R <- ifelse(rfm_nonAB$Recency < 17, 3,ifelse(rfm_nonAB$Recency>216.348,1,2))

quantile(rfm_nonAB$Frequency, c(.333, .666))
rfm_nonAB$F <- ifelse(rfm_nonAB$Frequency < 2, 1,ifelse(rfm_nonAB$Frequency>17,3,2))

quantile(rfm_nonAB$Monetary, c(.333, .666))
rfm_nonAB$M <- ifelse(rfm_nonAB$Monetary < 27.62, 1,ifelse(rfm_nonAB$Monetary>164.85,3,2))

rfm_nonAB <- rfm_nonAB %>%
  mutate(RFM_score = 100 *R +10 * F + M)


# rfm_AB segment
rfm_AB$Segment = "0"
rfm_AB$Segment[which(rfm_AB$RFM_score %in% c(122,123,132,133))] = "Cannot Lose Them (Used to be loyal, but have not purchased in a while)"
rfm_AB$Segment[which(rfm_AB$RFM_score %in% c(233,323,332,333))] = "Champions (Best Customers)"
rfm_AB$Segment[which(rfm_AB$RFM_score %in% c(111,112,121))] = "Lost (Tried it, didn’t come back)"
rfm_AB$Segment[which(rfm_AB$RFM_score %in% c(223,232,322))] = "Loyalist (Consistent Customers)"
rfm_AB$Segment[which(rfm_AB$RFM_score %in% c(211,311,321))] = "New Customers (Shopped recently, but not a lot)"
rfm_AB$Segment[which(rfm_AB$RFM_score %in% c(212,213,221,222,312))] = "On The Brink (Shown Interest, but not Committed)"
rfm_AB$seg_name <- stringr::str_trim(gsub("\\(.*","",as.character(rfm_AB$Segment)))
rfm_AB$seg_def <- stringr::str_trim(gsub("\\).*","",gsub(".*\\(","",as.character(rfm_AB$Segment))))



head(rfm_AB)

# rfm_nonAB segment
rfm_nonAB$Segment = "0"
rfm_nonAB$Segment[which(rfm_nonAB$RFM_score %in% c(122,123,132,133))] = "Cannot Lose Them (Used to be loyal, but have not purchased in a while)"
rfm_nonAB$Segment[which(rfm_nonAB$RFM_score %in% c(233,323,332,333))] = "Champions (Best Customers)"
rfm_nonAB$Segment[which(rfm_nonAB$RFM_score %in% c(111,112,121))] = "Lost (Tried it, didn’t come back)"
rfm_nonAB$Segment[which(rfm_nonAB$RFM_score %in% c(223,232,322))] = "Loyalist (Consistent Customers)"
rfm_nonAB$Segment[which(rfm_nonAB$RFM_score %in% c(211,311,321))] = "New Customers (Shopped recently, but not a lot)"
rfm_nonAB$Segment[which(rfm_nonAB$RFM_score %in% c(212,213,221,222,312))] = "On The Brink (Shown Interest, but not Committed)"
rfm_nonAB$seg_name <- stringr::str_trim(gsub("\\(.*","",as.character(rfm_nonAB$Segment)))
rfm_nonAB$seg_def <- stringr::str_trim(gsub("\\).*","",gsub(".*\\(","",as.character(rfm_nonAB$Segment))))



plot_rfm_AB <- data.frame(table(rfm_AB$Segment))
names(plot_rfm_AB) <- c("seg","count")
plot_rfm_AB$per <- round(100*(plot_rfm_AB$count/sum(plot_rfm_AB$count)),2)

plot_rfm_AB$seg_name <- stringr::str_trim(gsub("\\(.*","",as.character(plot_rfm_AB$seg)))
plot_rfm_AB$seg_def <- stringr::str_trim(gsub("\\).*","",gsub(".*\\(","",as.character(plot_rfm_AB$seg))))

plot_rfm_AB <- plot_rfm_AB[order(plot_rfm_AB$count, decreasing = T),]
plot_rfm_AB$seg_name <- factor(plot_rfm_AB$seg_name, levels = c(plot_rfm_AB$seg_name))
plot_rfm_AB$hovertext <- paste0("<b><i>",plot_rfm_AB$seg_name , "</i></b>", "<br>",
                              "<b><i>",plot_rfm_AB$seg_def , "</i></b>", "<br>",
                              "<b><i>",formatC(plot_rfm_AB$count, format="f", big.mark=",", digits=0), 
                              " (", sprintf("%.2f", plot_rfm_AB$per), "%)", "<br>")
#============================================
plot_rfm_nonAB <- data.frame(table(rfm_nonAB$Segment))
names(plot_rfm_nonAB) <- c("seg","count")
plot_rfm_nonAB$per <- round(100*(plot_rfm_nonAB$count/sum(plot_rfm_nonAB$count)),2)

plot_rfm_nonAB$seg_name <- stringr::str_trim(gsub("\\(.*","",as.character(plot_rfm_nonAB$seg)))
plot_rfm_nonAB$seg_def <- stringr::str_trim(gsub("\\).*","",gsub(".*\\(","",as.character(plot_rfm_nonAB$seg))))

plot_rfm_nonAB <- plot_rfm_nonAB[order(plot_rfm_nonAB$count, decreasing = T),]
plot_rfm_nonAB$seg_name <- factor(plot_rfm_nonAB$seg_name, levels = c(plot_rfm_nonAB$seg_name))
plot_rfm_nonAB$hovertext <- paste0("<b><i>",plot_rfm_nonAB$seg_name , "</i></b>", "<br>",
                                "<b><i>",plot_rfm_nonAB$seg_def , "</i></b>", "<br>",
                                "<b><i>",formatC(plot_rfm_nonAB$count, format="f", big.mark=",", digits=0), 
                                " (", sprintf("%.2f", plot_rfm_nonAB$per), "%)", "<br>")

#=========================================================


# library(ggplot2)
# 
# # AB Brand Customer Segment
# table(rfm_AB$Segment)
# 
# ggplot(rfm_AB) + geom_bar(aes(x = Segment, fill = Segment))+ theme(axis.text.x=element_text(angle=90,hjust=1))+
#   labs(title = "Barplot for Customer Segement for AB Brand")
# 
# # Non AB Brand Customer Segment
# table(rfm_nonAB$Segment)
# 
# ggplot(rfm_nonAB) + geom_bar(aes(x = Segment, fill = Segment))+ theme(axis.text.x=element_text(angle=90,hjust=1))+
#   labs(title = "Barplot for Customer Segement for Non AB Brand")


#====================================
# AB Brand Customer Segment 
ABcust_segment = dplyr::select(rfm_AB, c(cust_id,seg_name))
#nrow(ABcust_segment)
#write.csv(ABcust_segment,'ABCust_Segment.csv')

# Non AB Brand Customer Segment
NonABcust_segment = dplyr::select(rfm_nonAB, c(cust_id,seg_name))
#nrow(NonABcust_segment)
#write.csv(NonABcust_segment,'NonABcust_segment.csv')

#=======================================
get_plot_dt <- function(dt,var = "gender"){
  data <- data.frame(table(dt[[var]]))
  names(data) <- c(var,"count")
  data$per <- round(100*(data$count/sum(data$count)),2)
  data <- data[order(data$count, decreasing = T),]
  data[[var]] <- factor(data[[var]], levels = c(data[[var]]))
  data$hovertext <- paste0("<b><i>",data[[var]] , "</i></b>", "<br>",
                            "<b><i>",formatC(data$count, format="f", big.mark=",", digits=0), 
                            " (", sprintf("%.0f", data$per), "%)", "<br>")
  return(data)
}


# dt_gender <- get_plot_dt(ab_data, "gender")
# dt_race <- get_plot_dt(ab_data, "race")
# dt_income <- get_plot_dt(ab_data, "income_text")

get_map_dt <- function(dt){
  statepop$state <- statepop$abbr
  statepop$location <- statepop$full
  sale_map_dt <- merge(dt,statepop[,c("state","location")],all.y = TRUE, by.y = "state", by.x = "st")
  
  states <- read_rds("data/states.rds")
  states$ID <- tools::toTitleCase(states$ID)
  sale_map_dt <- sale_map_dt[!is.na(sale_map_dt$location), ]
  sale_map_dt <- sale_map_dt[order(sale_map_dt$location,match(sale_map_dt$location,states$ID)),]
  #https://rdrr.io/cran/leaflet/man/addLegend.html
  sale_map_dt$labels <- sprintf("<strong style='color: red;font-size:14px;'>State: </strong><em style='font-size:14px;'>%s</em>
                               <br/><strong style='color: red;font-size:14px;'>City: </strong><em style='font-size:14px;'>%s</em>
                               <br/><strong style='color: red;font-size:14px;'>Customer ID: </strong><em style='font-size:14px;'>%g</em>
                               <br/><strong style='color: #3EACA8;font-size:14px;'>Product: </strong><em style='font-size:14px;'>%s</em>
                               <br/><strong style='color: #00d084;font-size:14px;'>Purchase: </strong><em style='font-size:14px;'>$%s</em>
                               ",
                                sale_map_dt$location,
                                sale_map_dt$city,
                                sale_map_dt$cust_id,
                                sale_map_dt$prod_name,
                                sale_map_dt$product_total_usd)%>% lapply(htmltools::HTML)
  return(sale_map_dt)
}


pie_plot <- function(dt, label, value, hovertext, colors = c("#636466","#e3af32")){
  
  trimmer <- function(x,break_limit){ sapply(strwrap(stringr::str_to_title(x), break_limit, simplify=FALSE), paste, collapse="\n")}
  dt[,label] <- trimmer(as.character(dt[,label]), 30)

  if(length(100*( dt[,value] /sum( dt[,value] ))<8) >= 3){
    margin =list( l=10,r=10,b=80,t=10)
  }else{
    margin =list( l=30,r=30,b=30,t=30)
  }
  p <- plotly::plot_ly(dt, 
                       labels = ~get(label), 
                       values = ~get(value), 
                       type = 'pie',
                       textposition = ifelse(100*(dt[,value]/sum(dt[,value]))<8,"outside","inside"),
                       #textposition = 'inside',
                       sort = FALSE,
                       textinfo = 'label+value+percent',
                       #texttemplate = '<b>%{label}</br></br>%{percent}</b>', 
                       insidetextfont = list(color = '#FFFFFF'),
                       hoverinfo = 'text',
                       text = ~get(value),
                       marker = list(colors = colors,
                                     line = list(color = '#FFFFFF', width = 1)),
                       #The 'pull' attribute can also be used to create space between the sectors
                       showlegend = FALSE)
  p <- p%>% layout(title = '',
                   margin =margin ,
                   showlegend = FALSE,separators = ',.',
                   xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                   yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE)) 
  return(p)
}

#pie_plot(dt = dt_gender, label = "gender", value = "count", hovertext = "hovertext",colors = c("#636466","#e3af32"))


#===========================================

bar_plot <- function(plot_data,
                     y_var = "count",
                     x_var = "seg_name",
                     color_var = "seg_name",
                     text = "count",
                     legend = "bottom",
                     type_var="bar",
                     color_fill_out = c("#e3af32","#636466"),
                     source = "summary_out",
                     y_var_label = "",
                     x_var_label = "",
                     title = "",
                     hovertext = "hovertext"
                     ){
  p <- plotly::plot_ly(plot_data, 
                       y = ~ get(y_var), 
                       color = ~ get(color_var),
                       x = ~ get(x_var),
                       customdata = ~ get(color_var),
                       text = ~ formatC(get(text), format="f", big.mark=",", digits=0),
                       hoverinfo = "text",
                       hovertext = ~hovertext,
                       textposition = c('outside'),
                       textfont = list(size = 11, color = "black"),
                       type = type_var,
                       alpha = 1,
                       colors = color_fill_out,
                       source = source)
  plot <- p %>% plotly::layout(
    font = list(color = 'gray',size = 10),
    hoverlabel = list(font=list(size=13)),
    showlegend =  FALSE,
    title = list(text = title,font = list(size = 15,color = "#485E89")),
    margin =list( l=30,r=10,b=10,t=40),
    xaxis = list(
      tickfont = list(
        size = 11,
        color = "#485E89"
      ),
      titlefont = list(
        size =  13,
        color = "#485E89"
      ),
      title = x_var_label,
      zeroline = FALSE,
      tickmode = "array",
      color = "#485E89"
    ),
    yaxis = list(
      range = c(0, max(plot_data[,y_var]) + max(plot_data[,y_var])/10 ),
      fixedrange = TRUE,
      tickfont = list(
        size = 11,
        color = "#485E89"),
      titlefont = list(
        size =  13,
        color = "#485E89"),
      title = y_var_label,
      zeroline = FALSE,
      color = "#485E89"),
    legend =  list(itemwidth = 29,
                   orientation = "h",
                   xanchor = "center",
                   size = 12,y =-0.05,x = 0.5,
                   title=list(size = 12,color = "#485E89"),
                   font = list(size = 12,color = "#485E89")))
  return(plot)

}
  

# bar_plot(plot_data = dt_race,
#          y_var = "count",
#          x_var = "race",
#          color_var = "race",
#          text = "count",
#          legend = "bottom",
#          type_var="bar",
#          color_fill_out = c("#e3af32","#636466"),
#          source = "summary_out",
#          y_var_label = "",
#          x_var_label = "",
#          title = "",
#          hovertext = "hovertext"
# )
# 
# bar_plot(plot_data = dt_income,
#          y_var = "count",
#          x_var = "income_text",
#          color_var = "income_text",
#          text = "count",
#          legend = "bottom",
#          type_var="bar",
#          color_fill_out = c("#e3af32","#636466"),
#          source = "summary_out",
#          y_var_label = "",
#          x_var_label = "",
#          title = "",
#          hovertext = "hovertext"
# )
