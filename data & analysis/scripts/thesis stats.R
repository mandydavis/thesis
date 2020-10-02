library(readxl)
library(ggplot2)
library(psych)
library(psychTools)
library(reshape2)
library(janitor)
library(tidyverse)
# install.packages("rlist")
library(ggrepel)
library(directlabels)
library(ggforce)
library(gridExtra)
library(gtable)

# set theme for maps
# set defaults
default_font_family <- 'Tahoma'
default_font_color <- '#22211d'
default_background_color <- '#f5f5f2'
theme_map <- function(...) {
  theme_minimal() +
    theme(
      text = element_text(family = default_font_family,
                          color = default_font_color),
      # remove all axes
      axis.line = element_blank(),
      # axis.text.x = element_blank(),
      # axis.text.y = element_blank(),
      axis.ticks = element_blank(),
      # add a subtle grid
      # panel.grid.major = element_line(color = "#dbdbd9", size = 0.2),
      # panel.grid.minor = element_blank(),
      # background colors
      plot.background = element_rect(fill = default_background_color,
                                     color = NA),
      panel.background = element_rect(fill = default_background_color,
                                      color = NA),
      legend.background = element_rect(fill = default_background_color,
                                       color = NA),
      # borders and margins
      plot.margin = unit(c(.5, .5, .2, .5), "cm"),
      panel.border = element_blank(),
      panel.spacing = unit(c(-.1, 0.2, .2, 0.2), "cm"),
      # titles
      legend.title = element_text(size = 9, margin = margin(-.2,0,.2,0, unit = 'cm')),
      legend.text = element_text(size = 9, hjust = 0,
                                 color = default_font_color),
      legend.position="bottom", 
      legend.box = "horizontal" ,
      plot.title = element_text(size = 14, hjust = 0.5,
                                color = default_font_color),
      plot.subtitle = element_text(size = 9, hjust = 0.5,
                                   color = default_font_color,
                                   margin = margin(b = .5,
                                                   t = 0,
                                                   l = 2,
                                                   unit = "cm"),
                                   debug = F),
      # captions
      plot.caption = element_text(size = 7,
                                  hjust = .5,
                                  margin = margin(t = 0.2,
                                                  b = 0,
                                                  unit = "cm"),
                                  color = "#939184"),
      ...
    )
}

theme_pie <- function(...) {
  theme_minimal() +
    theme(
      text = element_text(family = default_font_family,
                          color = default_font_color),
      # remove all axes
      axis.line = element_blank(),
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks = element_blank(),
      # add a subtle grid
      # panel.grid.major = element_line(color = "#dbdbd9", size = 0.2),
      panel.grid.minor = element_blank(),
      # background colors
      plot.background = element_rect(fill = default_background_color,
                                     color = NA),
      panel.background = element_rect(fill = default_background_color,
                                      color = NA),
      legend.background = element_rect(fill = default_background_color,
                                       color = NA),
      # borders and margins
      plot.margin = unit(c(.5, .5, .2, .5), "cm"),
      panel.border = element_blank(),
      panel.spacing = unit(c(-.1, 0.2, .2, 0.2), "cm"),
      # titles
      legend.title = element_blank(),
      legend.text = element_text(size = 9, hjust = 0,
                                 color = default_font_color),
      legend.position="top", 
      legend.box = "horizontal" ,
      plot.title = element_text(size = 9, hjust = 0.5,
                                color = default_font_color),
      plot.subtitle = element_text(size = 9, hjust = 0.5,
                                   color = default_font_color,
                                   margin = margin(b = .5,
                                                   t = 0,
                                                   l = 2,
                                                   unit = "cm"),
                                   debug = F),
      # captions
      plot.caption = element_text(size = 7,
                                  hjust = .5,
                                  margin = margin(t = 0.2,
                                                  b = 0,
                                                  unit = "cm"),
                                  color = "#939184"),
      ...
    )
}

internet_usage <- read_excel("Internet Usage downloaded 08.24.20 (World Bank)/internet_usage.xls")

# useful way to rename one specific column
colnames(internet_usage)[1] <- "country_name"

# countries of interest
countries <- cs(Morocco, "United Kingdom", France, Estonia, Qatar, Singapore, "New Zealand")

internet_usage_selection <- as.data.frame(t(subset(internet_usage, country_name %in% countries)[, -c(2, 3, 4)]))
# make country names the header
internet_usage_selection <- row_to_names(internet_usage_selection, row_number = 1) 
# make years (currently as rownames) to their own column
internet_usage_selection$year <- as.numeric(row.names(internet_usage_selection))
# we don't care about the years before the internet existed
internet_usage_selection <- subset(internet_usage_selection, subset = year > 1989)

# plot internet usage for my 7 countries from 1990-2020:
internet <- ggplot(internet_usage_selection, aes(x = year)) +
  # ylim(0, 100) +
  geom_line(aes(y = as.numeric(as.character(Morocco))), group = 1, color = 'red') + 
  geom_line(aes(y = as.numeric(as.character(Estonia))), group = 1, color = 'blue') +
  geom_line(aes(y = as.numeric(as.character(France))), group = 1, color = 'pink') +
  geom_line(aes(y = as.numeric(as.character(Qatar))), group = 1, color = 'green') +
  geom_line(aes(y = as.numeric(as.character(`United Kingdom`))), group = 1, color = 'gray') +
  geom_line(aes(y = as.numeric(as.character(`New Zealand`))), group = 1, color = 'orange') +
  geom_line(aes(y = as.numeric(as.character(Singapore))), group = 1, color = 'black') +
  labs(title = "Individuals using the Internet 1990-2018 (% of population)", subtitle = "Internet users are individuals who have used the Internet (from any location) in the last 3 months.\n The Internet can be used via a computer, mobile phone, personal digital assistant, games machine, digital TV etc.", y = "Individuals using the Internet (% of population)", x = "Year", caption = "Source: World Bank 2020, Data: International Telecommunication Union, World Telecommunication/ICT Development Report and database") +
  geom_text(x= 2019, y = 82.04319, label = "France", size = 3, color= "pink") +
  geom_text(x= 2019, y = 64.80387, label = "Morocco", size = 3, color= "red") +
  geom_text(x= 2019, y = 88.16564, label = "Singapore", size = 3, color= "black") +
  geom_text(x= 2019, y = 94.89674, label = "UK", size = 3, color= "gray") +
  geom_text(x= 2018, y = 90.81109, label = "New Zealand", size = 3, color= "orange") +
  geom_text(x= 2019, y = 89.35701, label = "Estonia", size = 3, color= "blue") +
  geom_text(x= 2019, y = 99.65285, label = "Qatar", size = 3, color= "green") +
  theme_map()



####### visualize degree popularity (what % of all women and men tertiary graduates studied ICT?):
percent_of_women <- read.csv("./data & analysis/data/percent_of_women.csv")
percent_of_men <- read.csv("./data & analysis/data/percent_of_men.csv")

# munge data [** keep in mind this is averaging across years **]
percent_of_women <- subset(percent_of_women, Indicator == 'Percentage of female graduates from tertiary education graduating from Information and Communication Technologies programmes, female (%)')
percent_of_women <- aggregate(percent_of_women[,'Value'], list(percent_of_women$LOCATION), mean)
colnames(percent_of_women) <- c("iso_a3", "percent_of_women")

# do the same for the men
percent_of_men <- subset(percent_of_men, Indicator == 'Percentage of male graduates from tertiary education graduating from Information and Communication Technologies programmes, male (%)')
percent_of_men <- aggregate(percent_of_men[,'Value'], list(percent_of_men$LOCATION), mean)
colnames(percent_of_men) <- c("iso_a3", "percent_of_men")

# combine the percent_of_women and percent_of_men tables
percent_of_gender <- inner_join(percent_of_women, percent_of_men, by = "iso_a3")

# a function that creates a ICT vs Other Degree pie chart for any country in the dataset
# TO DO !!!!

# Morocco Gender Ratio of Tertiary ICT Degrees Awarded

# load ICT data
ict_raw <- read.csv("./data & analysis/data/ict.csv")
# the distinct country codes in case you need them
ict_countries <- levels(ict_raw[,'LOCATION'])
# get average across all years that each country has data for (2013-2019)
ict_averages <- aggregate(ict_raw[,'Value'], list(ict_raw$LOCATION), mean)
# rename columns
colnames(ict_averages) <- c('iso_a3', 'average_ICT_w')
average_ICT_w <- subset(ict_averages, subset = ict_averages["iso_a3"] == "QAT")$average_ICT_w
QAT_percent_ICT <- data.frame("gender" = c("Women", "Men"), "percent" = c(average_ICT_w, 100 - average_ICT_w))

# (load percent_of_gender data) create tables that will work well to construct percent_of_m and percent_of_w pie charts
QAT_percent_w <- subset(percent_of_gender, subset = percent_of_gender["iso_a3"] == "QAT")$percent_of_women
QAT_percent_of_w = data.frame("degree" = c("ICT", "Other Degree"), "percent" = c(QAT_percent_w, 100 - QAT_percent_w))
QAT_percent_m <- subset(percent_of_gender, subset = percent_of_gender["iso_a3"] == "QAT")$percent_of_men
QAT_percent_of_m = data.frame("degree" = c("ICT", "Other Degree"), "percent" = c(QAT_percent_m, 100 - QAT_percent_m))

# create pie chart (% of ICT graduates who are women)
QAT_p_of_ICT <- ggplot(QAT_percent_ICT, aes(x = "", y = percent, fill = gender)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start = 0) + 
  geom_label(aes(label = paste((round(percent, digits = 1)), "%")), position = position_stack(vjust = 0.5), show.legend = FALSE, size = 5) +
  scale_fill_manual(values = c("#1FBFC3", "#F57670")) +
  labs(x= NULL, y = NULL, title = "% of ICT tertiary graduates who are women") +
  theme_pie()

# Qatar percent of women
QAT_p_of_women_plot <- ggplot(QAT_percent_of_w, aes(x = "", y = percent, fill = degree)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start = 0) + 
  geom_label(aes(label = paste((round(percent, digits = 1)), "%")), position = position_stack(vjust = 0.5), show.legend = FALSE, size = 5) +
  scale_fill_manual(values = c("#6874E8", "#9F9FED")) +
  labs(x= NULL, y = NULL, title = "% of women who pursued...") +
  theme_pie()

# Qatar percent of men
QAT_p_of_men_plot <- ggplot(QAT_percent_of_m, aes(x = "", y = percent, fill = degree)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start = 0) + 
  geom_label(aes(label = paste((round(percent, digits = 1)), "%")), position = position_stack(vjust = 0.5), show.legend = FALSE, size = 5) +
  scale_fill_manual(values = c("#6874E8", "#9F9FED")) +
  labs(x= NULL, y = NULL, title = "% of men who pursued...") +
  theme_pie() 

# combine the two charts
qatar_pie <- grid.arrange(QAT_p_of_ICT, QAT_p_of_women_plot, QAT_p_of_men_plot, nrow = 1)
qatar_pie <- ggplotGrob(qatar_pie)

# function to do this for the rest:
country_pie_charts <- function(country_codes) {
  
  pie_names_list <- list("morocco_pie")
  
  for (country in country_codes) {
    
    average_ICT_w <- subset(ict_averages, subset = ict_averages["iso_a3"] == country)$average_ICT_w
    country_percent_ICT <- data.frame("gender" = c("Women", "Men"), "percent" = c(average_ICT_w, 100 - average_ICT_w))
    
    # (load percent_of_gender data) create tables that will work well to construct percent_of_m and percent_of_w pie charts
    country_percent_w <- subset(percent_of_gender, subset = percent_of_gender["iso_a3"] == country)$percent_of_women
    country_percent_of_w = data.frame("degree" = c("ICT", "Other Degree"), "percent" = c(country_percent_w, 100 - country_percent_w))
    country_percent_m <- subset(percent_of_gender, subset = percent_of_gender["iso_a3"] == country)$percent_of_men
    country_percent_of_m = data.frame("degree" = c("ICT", "Other Degree"), "percent" = c(country_percent_m, 100 - country_percent_m))
    
    # create pie chart (% of ICT graduates who are women)
    country_p_of_ICT <- ggplot(country_percent_ICT, aes(x = "", y = percent, fill = gender)) +
      geom_bar(width = 1, stat = "identity") +
      coord_polar("y", start = 0) + 
      geom_label(aes(label = paste((round(percent, digits = 1)), "%")), position = position_stack(vjust = 0.5), show.legend = FALSE, size = 5) +
      scale_fill_manual(values = c("#1FBFC3", "#F57670")) +
      labs(x= NULL, y = NULL, title = NULL) +
      theme_pie() + theme(legend.position = "none")
    
    # percent of women
    country_p_of_women_plot <- ggplot(country_percent_of_w, aes(x = "", y = percent, fill = degree)) +
      geom_bar(width = 1, stat = "identity") +
      coord_polar("y", start = 0) + 
      geom_label(aes(label = paste((round(percent, digits = 1)), "%")), position = position_stack(vjust = 0.5), show.legend = FALSE, size = 5) +
      scale_fill_manual(values = c("#6874E8", "#9F9FED")) +
      labs(x= NULL, y = NULL, title = NULL) +
      theme_pie() + theme(legend.position = "none")
    
    # percent of men
      country_p_of_men_plot <- ggplot(country_percent_of_m, aes(x = "", y = percent, fill = degree)) +
      geom_bar(width = 1, stat = "identity") +
      coord_polar("y", start = 0) + 
      geom_label(aes(label = paste((round(percent, digits = 1)), "%")), position = position_stack(vjust = 0.5), show.legend = FALSE, size = 5) +
      scale_fill_manual(values = c("#6874E8", "#9F9FED")) +
      labs(x= NULL, y = NULL, title = NULL) +
      theme_pie() + theme(legend.position = "none")
      
      country <<- grid.arrange(country_p_of_ICT, country_p_of_women_plot, country_p_of_men_plot, nrow = 1)
    
    pie_name <- paste(country,"_pie", sep = "")
    pie_names_list <- append(pie_names_list, pie_name)
    
  }
  
  # return the list of the pie chart names
  return(country)
}

country_pie_charts("MAR")
country_pies <- grid.arrange(qatar_pie, country, nrow = 2)
country_pie_charts("SGP")
country_pies <- grid.arrange(country_pies, country, nrow = 3)
country_pie_charts("EST")
country_pie_charts("NZL")
country_pie_charts("GBR")
country_pie_charts("FRA")


###    change over time    ###
country_codes <- c("QAT","MAR","SGP","EST","NZL","GBR","FRA")
ICT_circ <- subset(ict_raw, subset = ict_raw$LOCATION %in% country_codes)
# ICT_circ_df <- data.frame(row.names = c("QAT","MAR","SGP","EST","NZL","GBR","FRA"))
# ICT_circ_df <- t(ICT_circ_df)
ICT_circ_df <- data.frame(row.names = c(2013:2018))

ICT_circ_df$QAT <- NA
ICT_circ_df$MAR <- NA
ICT_circ_df$SGP <- NA
ICT_circ_df$EST <- NA
ICT_circ_df$NZL <- NA
ICT_circ_df$GBR <- NA
ICT_circ_df$FRA <- NA

for (country_code in colnames(ICT_circ_df)) {
  country_years <- subset(ICT_circ$TIME, ICT_circ$LOCATION == country_code)
  for (year in strtoi(rownames(ICT_circ_df))) {
    if(year %in% country_years) {
      country_rows <- subset(ICT_circ, subset = ICT_circ$LOCATION==country_code)
      ICT_circ_df[year,country_code] <- subset(country_rows, subset = country_rows$TIME==year)$Value
    }
  }
}

# chop off the rest of the rows
ICT_circ_df <- ICT_circ_df[2013:2018,]
rownames(ICT_circ_df) <- c(2013:2018)

ICT_circ_df$year <- as.numeric(row.names(ICT_circ_df))



# plot ICT (women) graduation rates usage for my 7 countries using the years available:
ICT_change <- ggplot(ICT_circ_df, aes(x = year)) +
  # ylim(0, 100) +
  geom_line(aes(y = as.numeric(as.character(MAR))), group = 1, color = 'red') + 
  geom_line(aes(y = as.numeric(as.character(EST))), group = 1, color = 'blue') +
  geom_line(aes(y = as.numeric(as.character(FRA))), group = 1, color = 'pink') +
  geom_line(aes(y = as.numeric(as.character(QAT))), group = 1, color = 'green') +
  geom_line(aes(y = as.numeric(as.character(GBR))), group = 1, color = 'gray') +
  geom_line(aes(y = as.numeric(as.character(NZL))), group = 1, color = 'orange') +
  geom_line(aes(y = as.numeric(as.character(SGP))), group = 1, color = 'black') +
  labs(title = "Change in the percent of ICT tertiary graduates who are women over time", subtitle = "subtitle goes here", y= "percent of ICT tertiary graduates who are women") +
  geom_text(x= 2016.25, y = 16.45824, label = "France", size = 3, color= "pink") +
  geom_text(x= 2017.25, y = 41.28349, label = "Morocco", size = 3, color= "red") +
  geom_text(x= 2017.3, y = 32.22160, label = "Singapore", size = 3, color= "black") +
  geom_text(x= 2016.15, y = 19.38466, label = "UK", size = 3, color= "gray") +
  geom_text(x= 2017.35, y = 23.10945, label = "New Zealand", size = 3, color= "orange") +
  geom_text(x= 2017.25, y = 28.89201, label = "Estonia", size = 3, color= "blue") +
  geom_text(x= 2018, y = 54.2, label = "Qatar", size = 3, color= "green") +
  theme_map()
  


# TO DO : 

## in-chart text doesn't seem to be taking on theme_pie format
## flip the colors in the first graph
## change the colors of the second two charts completely
## create function to do this for the rest
# go in order of percent of ict who are women: QAT, MAR, SGP, EST, NZL, GBR, FRA
