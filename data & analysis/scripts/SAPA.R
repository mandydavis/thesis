##########################
##### load packages ######
##########################
library(tidyverse) # ggplot2, dplyr, tidyr, readr, purrr, tibble
library(dplyr)
library(forcats)
require(devtools)
# install_version("psych", version = "2.0.7")
# install.packages("psych")
library(psych)
library(psychTools)

# necessary mapping packages
library("ggplot2")
theme_set(theme_bw())
library("sf")
library("rnaturalearth")
library("rnaturalearthdata")
library("rgeos")
library(magrittr) # pipes
library(lintr) # code linting
library(raster) # raster handling (needed for relief)
library(viridis) # viridis color scale
library('dichromat')

# # only need to do this once
# # font package
# library(extrafont)
# font_import()

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
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
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

##########################
##### load SAPA data #####
##########################
load("./data & analysis/data/SAPA/SAPA.rdata")
# rename the monster
SAPA <- SAPAdata18aug2010thru7feb2017
rm(SAPAdata18aug2010thru7feb2017)
# reset ItemLists with the file Bill provided (running this line will replace the existing one from the earlier load)
load("./data & analysis/data/SAPA/ItemLists.rda")



# gender, age, country, education, jobfield, major, discipline, occupation, p1occ, p2occ
# thinking about the Zheng et al paper: p1occIncomeEst, p2occIncomeEst
# unsure: ethdiv?

### overview of variables: ###
# country: 221 distinct country codes. Assuming these are ISOa3?
# education: CurrentInUniv, HSgrad, less12yrs, <NA>, CollegeDegree, SomeCollege, InGradOrProSchool, GradOrProDegree
# jobfield: 23 distinct (relevant: LifePhysicalSocialScience, InstallMntnceRepair, ComputerMath, EngineeringArchitecture)
# major: 147 distinct
# discipline: 14 distinct (relevant: Engineering and Technology, Computer and Information Sciences, Natural Sciences, Medicine and Allied Health, Mathematics)
# occupation: 992 distinct
# ethnic: 19 distinct
# gender: male, female, or <NA>
# *<NA> likely accounts for one distinct value per category

### overview of other measures: ###
# International Cognitive Ability Resource (Condon & Revelle, 2014)
# Oregon Vocational Interest Scales (Pozzebon, Visser, Ashton, Lee & Goldberg, 2010): https://www.ncbi.nlm.nih.gov/pmc/articles/PMC2822996/
# Oregon Avocational Interest Scales (Goldberg, 2009): https://books.google.com/books?id=QtvT-xbUKCoC&pg=PA205&lpg=PA205&dq=Personality,+demographics+and+self-reported+behavioral+acts&source=bl&ots=TDjZYKjdM4&sig=RgoHenYb9YD883RUu0cvvMcw9tw&hl=en&sa=X&ei=Ot-cUfbkO-2hyAHT6YCgDw#v=onepage&q=Personality%2C%20demographics%20and%20self-reported%20behavioral%20acts&f=false

# Interest would be an interesting parallel to PISA



# creates a table of the majors with a second column 'N' showing how many participants reported that major
majors_tibble <- SAPA %>%
  mutate(major = forcats::fct_explicit_na(major)) %>%
  group_by(major) %>%
  summarize(N = n())

# check if countries are ISOa3:
# store the world (except Antarctica)! 
world <- ne_countries(scale = "medium", returnclass = "sf")
world <- subset(world, name_long != 'Antarctica')

# country/participant count
countries_tibble <- SAPA %>%
  mutate(country = forcats::fct_explicit_na(country)) %>%
  group_by(country) %>%
  summarize(N = n()) %>%
  arrange(desc(N))

# country/participant count [LOG TRANSFORMED]
countries_tibble_log <- SAPA %>%
  mutate(country = forcats::fct_explicit_na(country)) %>%
  group_by(country) %>%
  summarize(N = log(n())) %>%
  arrange(desc(N))

# countries with >= 500 participants
countries_tibble_500 <- filter(countries_tibble, N >= 500)

# a new version of the SAPA data, which includes only those participants who are from countries that have >= 500 total participants
SAPA_500 <- subset(SAPA, SAPA$country %in% countries_tibble_500$country)

# merge SAPA_500 with GGGI data (we have SAPA data for 2010-2017, so select same years for GGGI)
# update: there is no 2017 in the GGGI data (?), so the last year I include is 2016, as the SAPA data only goes through Feb 2017.
ggi_raw <- read.csv("./data & analysis/data/ggi.csv", header = TRUE)
# reduces the data frame to contain only the relevant data, aka the ggi index value
ggi_index <- subset(ggi_raw, Indicator=='Overall Global Gender Gap Index' & Subindicator.Type== 'Index')
# find average GGI from 2013-2018
ggi_index$avg_ggi <- rowMeans(ggi_index[,10:16], na.rm = TRUE)

# map of participant location, log transformed 
countries_tibble <- merge(world, countries_tibble, by.x='iso_a3', by.y='country', all.x = TRUE)

ggplot(data = countries_tibble) +
  # it is essential to have the empty 'colour' string here so that we can include the 'no data' legend
  geom_sf(aes(fill = N, colour="")) +
  scale_color_continuous(name = "number of participants (log transformed)",
                       guide = guide_colorbar(title.position = 'top', title.hjust = 0.5,
                                              label.hjust = 0.5, ticks = FALSE)) +
  scale_colour_manual(values=NA) +
  guides(colour=guide_legend("\nNo data", override.aes=list(colour="#d6d6d2"), title.position = 'top')) +
  labs(x = NULL,
       y = NULL,
       title = "SAPA participant count (log transformed)",
       subtitle = "",
       caption = "Data: SAPA Aug. 18, 2010 - Feb. 7, 2017") +
  theme_map()



##########################
##### ORVIS items ########
##########################

# glossary:
# keys.list	: A list of scoring keys suitable to use for make.keys
# make.keys(): [NOT NEEDED DUE TO UPDATES TO scoreItems] Creates a keys matrix for use by score.items or cluster.cor. 
# items: Matrix or dataframe of raw item scores
# keys: A list of scoring keys or a matrix or dataframe of -1, 0, or 1 weights for each item on 
# each scale which may be created by hand, or by using make.keys. Just using a list of scoring keys (see example) is probably more convenient.
# scoreItems():

###   stepping stones: don't need to re-run   ###
# rownames of ORVIS_items are the q #s of the ORVIS items (q_3102 to q_3193)
ORVIS_items <- lookupItems("ORVIS", ItemInfo, search="ORVIS") # search for the columns labeled ORVIS
# checks to make sure ORVIS items are in the SAPA dataset
rownames(ORVIS_items) %in% colnames(SAPA_500) # all TRUE
# find which columns of the SAPA data correspond to the ORVIS items
which(colnames(SAPA_500) %in% rownames(ORVIS_items)) # 802 to 893
which(rownames(ORVIS_items) %in% keys.list[1])
# Variable names in keys are incorrectly specified?

###   now can score these scales (all scored in the same direction)   ###
# 451:458 are the ORVIS scale keys
ORVIS_scores <- char2numeric(scoreItems(keys = ItemLists[451:458], items = SAPA_500, impute = "none")) # need the char2numeric?
# ORVIS_scores <- scoreItems(keys = ItemLists[451:458], items = SAPA_500, impute = "none") # char2numeric?


# check some things
# dim(ORVIS_scores$scores)
# describe(ORVIS_scores$scores)

ORVIS_gender <- data.frame(ORVIS_scores$scores, SAPA_500$gender)
ORVIS_country <- data.frame(ORVIS_scores$scores, SAPA_500$country)

ORVIS_gen_country <- data.frame(ORVIS_gender, country=SAPA_500$country)
# put gender as first row to makes things easier w statsby
ORVIS_gen_country <- ORVIS_gen_country[c(9,1:8,10)] # switches the order -- come back to this
colnames(ORVIS_gen_country)[1] <- "gender"
# spits out all the countries with their participant count (given the data we're using, SAPA_500):
table(ORVIS_gen_country$country)

# statsBy() function?
sb <- statsBy(ORVIS_gen_country, ORVIS_gen_country$country, cors = TRUE)
# this found how much the countries differ
names(sb)
# rbg: r between groups
# rwg: r within groups (pooled w in group corr)
sb$r
# ^ this gives us every correlation with ORVIS by each country

# takes each of these correlations ^ as a vector
# i care about orvis_p with gender
head(sb$within)

# this is working but I don't know why. Not working for having country in the mix 
sb <- statsBy(ORVIS_gender, "SAPA_500.gender", cors = TRUE)
sb <- statsBy(ORVIS_country, "SAPA_500.country", cors = TRUE)
sb <- statsBy(ORVIS_gen_country, "country", cors = TRUE)
# get rid of the cors that aren't relevant
head(sb$within[,1:9])
ORVIS_gen_by_country <- round(sb$within[,1:9], 2)
# then clean up by sample size, plot outliers?

# add gggi column 
ORVIS_gggi <- data.frame(ORVIS_gen_by_country, ggi_index$Country.ISO3)

pairs.panels(ORVIS_gen_by_country)

# make sure this is okay -- got things like "ORVIS_P-ORVIS_Ad" instead of just ORVIS_P













