
# Libraries and data ------------------------------------------------------

library(tidyverse)
library(janitor)
library(data.table)

# 2016 Data
df_2016 <- read.table("Data_USA_CB_2016_Q2.txt", sep = ";", header = T)

# 2017 Data
df_2017 <- read.table("Data_USA_CB_2017_Q2.txt", sep = ";", header = T)


# Wrangling ---------------------------------------------------------------

# How many airlines?
plyr::count(df_2016$AIRLINE) # 12 airlines AA, AS, B6, DL, F9, G4, NH, NK, SY, UA, VX, WN
plyr::count(df_2017$AIRLINE) # 12 airlines AA, AS, B6, DL, F9, G4, NK, OO, SY, UA, VX, WN

# Different markets between 2016 and 2017
setdiff(df_2016$MARKET, df_2017$MARKET) # "LVL-MEM", "MEM-STL", "RAL-RIC", "RAL-VIR"


# Generate variables ------------------------------------------------------

# city2
df_2016 <- df_2016 %>%
  group_by(AIRLINE) %>%
  mutate(city2 = case_when(VILLE_ARR %in% VILLE_DEP ~ 1, T ~ 0), .after = MARKET)

df_2017 <- left_join(df_2017, select(df_2016, MARKET, AIRLINE, DIRECT, city2),
                     by = c("MARKET", "AIRLINE", "DIRECT"))

df_2017 <- df_2017 %>% mutate(city2 = case_when(is.na(city2) ~ 0, T ~ city2))


# Alec's code -------------------------------------------------------------

# Data
data2016 <- read.table("Data_USA_CB_2016_Q2.txt", sep = ";", header = T) %>%
  clean_names() %>%
  mutate(pax = pax * 10)

data2017 <- read.table("Data_USA_CB_2017_Q2.txt", sep = ";", header = T) %>%
  clean_names() %>%
  mutate(pax = pax * 10)


## Cleaning ----
# Goal: Dataset at market-level with # of potential entrants + # of actual entrants + market chars

# (1) Take the list of airlines and check whether or not they operate directly in each market.

# (1a) Get lists of all cities, all airlines, all markets served directly
all_cities17 <- c(data2017$ville_dep, data2017$ville_arr) %>% unique()

all_airlines17 <- data2017$airline %>% unique()

direct_markets17 <- data2017 %>%
  filter(direct == 1) %>%
  filter(pax >= 90) %>%
  select(airline, market) %>%
  mutate(id = paste(airline, market, sep = "-")) %>%
  pull(id)

# (1b) Construct all possible markets
all_mkts17 <- expand.grid(dep = all_cities17, dest = all_cities17) %>%
  filter(dep != dest) %>%
  arrange(dep)
all_mkts17 <- all_mkts17[!duplicated(t(apply(all_mkts17[1:2], 1, sort))), ] # De-duplicate (i.e. keep A-B, drop B-A)
all_mkts17 <- all_mkts17 %>%
  mutate(market = paste(dep, dest, sep = "-"))

# (1c) Construct list of airline-market combinations (1176*12 = 14,112 combos)
airlinesXmkts17 <- expand.grid(airline = all_airlines17, market = all_mkts17$market) %>%
  separate(market, c("end1", "end2"), sep = "-", remove = F) %>%
  mutate(end1_id = paste(airline, end1, sep = "-"),
         end2_id = paste(airline, end2, sep = "-"),
         mkt_id = paste(airline, market, sep = "-"))

# (1d) Check whether airline directly operates in each market
airlinesXmkts17 <- airlinesXmkts17 %>%
  mutate(incumbent = mkt_id %in% direct_markets17)


# (2) Hub and slot: https://en.wikipedia.org/wiki/List_of_hub_airports#United_States
hubs <- c("AS-LAX", "AS-SFO", "AS-POR", "AS-SEA", "AA-PHX", "AA-LAX", "AA-MIA",
          "AA-CHI", "AA-NYC", "AA-CHA", "AA-PHI", "AA-DAL", "AA-WSH", "DL-LAX",
          "DL-ATL", "DL-BOS", "DL-DET", "DL-MIN", "DL-NYC", "DL-SLC", "DL-SEA",
          "DL-CIN", "F9-DNV", "SY-MIN", "UA-LAX", "UA-SFO", "UA-DNV", "UA-CHI",
          "UA-NYC", "UA-HOU", "UA-WSH")
slots <- c("NYC", "CHI", "WSH") # Market-level, so using cities rather than airports
airlinesXmkts17 <- airlinesXmkts17 %>%
  mutate(HUB_dummy = (end1_id %in% hubs | end2_id %in% hubs),
         Slot_dummy = (end1 %in% slots | end2 %in% slots))


# (3) Define entry threats based on airline operations in 2016

# (3a) Construct list of endpoints in which airlines operated
airlinesXcities16 <- data2016 %>%
  filter(pax >= 90) %>%
  select(airline, ville_dep, ville_arr) %>%
  mutate(id1 = paste(airline, ville_dep, sep = "-"),
         id2 = paste(airline, ville_arr, sep = "-"))
airlinesXcities16 <- c(airlinesXcities16$id1, airlinesXcities16$id2) %>% unique()

# (3b) Check whether airline operates at endpoints in 2016
airlinesXmkts17 <- airlinesXmkts17 %>%
  mutate(presence_end1_16 = end1_id %in% airlinesXcities16,
         presence_end2_16 = end2_id %in% airlinesXcities16,
         presence_any_16 = presence_end1_16 == T | presence_end2_16 == T,
         City1 = (presence_end1_16 == T & presence_end2_16 == F) |
           (presence_end1_16 == F & presence_end2_16 == T),
         City2 = (presence_end1_16 == T & presence_end2_16 == T),
         pot_entrant = presence_any_16 == T | incumbent == T)

# Check against Table 2
pot_entrants <- airlinesXmkts17 %>%
  filter(pot_entrant == T) %>%
  mutate(type = case_when(City1 == T ~ "City 1",
                          City2 == T ~ "City 2",
                          T ~ "City 0")) %>%
  group_by(type) %>%
  summarize(no = n(), entry = sum(incumbent)) %>%
  mutate(perc_entry = entry / no)

data.table(pot_entrants)
# Results closely match those from paper: 100% City 0 enter; 0.25% of City 1 enter; 27.5% of City 2 enter.
# (Compare to 100%, 0.84%, 22.13% from 2007 data in the paper)


# (3c) Number of entry threats = # of airlines with City2 == 1 but do not enter market

agg_entrants <- airlinesXmkts17 %>%
  filter(pot_entrant == T) %>%
  group_by(market) %>%
  summarize(nentrants = sum(incumbent),
            pentrants = sum(pot_entrant),
            nentrythreats = sum(City2 == T & incumbent == F), # present at both endpoints
            nentrythreats_noHub = sum(City2 == T & incumbent == F & HUB_dummy == F), # present at both endpoints but neither is Hub
            nentrythreats_Hub = sum(City2 == T & incumbent == F & HUB_dummy == T), # present at both endpoints and one or both are hubs
            nentrythreats_one = sum(City1 == T & incumbent == F)) # present at one endpoint only


# (4) Put all data together for final entry model dataset

# (4a) Append agg entrants info
entry_data <- left_join(airlinesXmkts17, agg_entrants, by="market")

# (4b) Add market characteristics and city characteristics
direct_dist <- bind_rows(data2016, data2017) %>%
  select(market, direct_distance) %>%
  group_by(market) %>%
  filter(direct_distance == min(direct_distance)) %>%
  distinct()
entry_data <- left_join(entry_data, direct_dist, by="market")
missing_dist <- entry_data %>%
  filter(is.na(direct_distance)) %>%
  select(market) %>%
  distinct()
# 17 markets for which no airlines fly direct and thus missing direct distance.
# All are between airports generally close together (about <2 hrs driving)
# Will use https://www.airmilescalculator.com/distance/aus-to-bhm/ to fill these in manually

entry_data <- entry_data %>%
  mutate(direct_distance = case_when(market == "AUS-SAT" ~ 66,
                                     market == "BOS-HRT" ~ 91,
                                     market == "BUF-PIT" ~ 186,
                                     market == "BUF-ROC" ~ 55,
                                     market == "CIN-COL" ~ 115,
                                     market == "CIN-IND" ~ 99,
                                     market == "CIN-LVL" ~ 84,
                                     market == "CLE-PIT" ~ 106,
                                     market == "COL-LVL" ~ 198,
                                     market == "COL-PIT" ~ 145,
                                     market == "IND-LVL" ~ 111,
                                     market == "JCK-ORL" ~ 144,
                                     market == "JCK-TMP" ~ 181,
                                     market == "LVL-NSH" ~ 151,
                                     market == "MEM-NSH" ~ 200,
                                     market == "ORL-TMP" ~ 81,
                                     market == "RIC-VIR" ~ 75,
                                     T ~ direct_distance))

dep_chars <- data2017 %>%
  select(ville_dep, popdep, gdpdep, gdppercapdep) %>%
  distinct()

arr_chars <- data2017 %>%
  select(ville_arr, poparr, gdparr, gdppercaparr) %>%
  distinct()

entry_data <- left_join(entry_data, dep_chars, by = c("end1" = "ville_dep"))
entry_data <- left_join(entry_data, arr_chars, by = c("end2" = "ville_arr"))
