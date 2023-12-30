
# Libraries and data ------------------------------------------------------

library(tidyverse)

# 2016 Data
df_2016 <- read.table("Data_USA_CB_2016_Q2.txt", sep = ";", header = T)

# 2017 Data
df_2017 <- read.table("Data_USA_CB_2017_Q2.txt", sep = ";", header = T)


# Wrangling ---------------------------------------------------------------

# How many airlines?
plyr::count(df_2016$AIRLINE) # 12 airlines AA, AS, B6, DL, F9, G4, NH, NK, SY, UA, VX, WN
plyr::count(df_2017$AIRLINE) # 12 airlines AA, AS, B6, DL, F9, G4, NK, OO, SY, UA, VX, WN

# Get the two end points
df_2016 <- df_2016 %>%
  separate(MARKET, c("point1", "point2"), "-", remove = F)

# Are AERODEP and VILLE_DEP identical?
identical(df_2016[['point1']],df_2016[['AERODEP']]) # FALSE
identical(df_2016[['point1']],df_2016[['VILLE_DEP']]) # TRUE
identical(df_2016[['point2']],df_2016[['AEROARR']]) # FALSE
identical(df_2016[['point2']],df_2016[['VILLE_ARR']]) # TRUE

df_2017 <- df_2017 %>%
  separate(MARKET, c("point1", "point2"), "-", remove = F)

# Different markets between 2016 and 2017
setdiff(df_2016$MARKET, df_2017$MARKET)


# Generate variables ------------------------------------------------------

# City2
df_2016 <- df_2016 %>%
  group_by(AIRLINE) %>%
  mutate(city2 = case_when(point2 %in% point1 ~ 1, T ~ 0), .after = point2)

df_2017 <- left_join(df_2017, select(df_2016, MARKET, AIRLINE, DIRECT, city2),
                     by = c("MARKET", "AIRLINE", "DIRECT"))

df_2017 <- df_2017 %>% mutate(city2 = case_when(is.na(city2) ~ 0, T ~ city2))

# Entry threats
df_2016 <- df_2016 %>%
  group_by(point1, point2) %>%
  mutate(market_count = n(), .after = MARKET)


# Stack -------------------------------------------------------------------

df_2017 <- subset(df_2017, select = c(YEAR, MARKET, AIRLINE, DIRECT))

dput(head(df_2017))
