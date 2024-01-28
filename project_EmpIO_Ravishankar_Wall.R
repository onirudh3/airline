rm(list=ls())

##############################################################################
# Title: Empirical IO - Part II Project
# Authors: Anirudh Ravishankar and Alec Wall
# Course: TSE - M2 EEE - Empirical Industrial Organization

# This do-file produces all results and figures for the Empirical Project
# submitted for the course. The project seeks to replicate results from 
# Gayle & Wu (2013) using more recent data.

# This do-file entails
# 1) The data cleaning process
# 2) Simulated Maxmum Likelihood (SML) procedures
##############################################################################

#-------------------------- Set WD & Load Packages --------------------------#

if (Sys.info()["user"] == "alecr") {
  projdir <- "C:/Users/alecr/OneDrive/Documents/TSE_2023-24/Empirical IO/Project/"
}

library("easypackages")
libraries("dplyr", "tidyr", "data.table", "janitor")

#-------------------------------- Load Data ---------------------------------#

data2016 <- read.csv(paste0(projdir, "Data_USA_CB_2016_Q2.txt"), sep=";") %>% 
  clean_names() %>%
  mutate(pax = pax*10) # adjust # of passengers
data2017 <- read.csv(paste0(projdir, "Data_USA_CB_2017_Q2.txt"), sep=";") %>% 
  clean_names() %>% 
  mutate(pax = pax*10) # adjust # of passengers

#------------------------- Initial Data Exploration -------------------------#

data2016 %>% group_by(airline) %>% summarize(n=n()) # 12 airlines
data2017 %>% group_by(airline) %>% summarize(n=n()) %>% arrange(desc(n)) # still 12 airlines 
# NH only in 2016 and only 1 market -- will drop
# OO only in 2017 and only 3 markets -- will drop

# WN = Southwest Airlines, AA = American Airlines, DL = Delta Airlines
# UA = United Airlines, F9 = Frontier Airlines, NK = Spirit Airlines, 
# B6 = JetBlue, AS = Alaska Airlines, G4 = Allegiant Air
# VX = Virgin America, SY = Sun Country Airlines

# Could collapse LCC per Wiki: Allegiant, Frontier, JetBlue, Southwest, Spirit, Sun Country, Virgin
# May want to separate out Southwest as Ciliberto & Tamer.

choose(49,2) # With 49 cities, 1176 possible non-directional markets
n_distinct(data2016$market) # 1159 markets in '16
n_distinct(data2017$market) # 1155 markets in '17

#------------------------------ Data Cleaning -------------------------------#
#
# (1) Take the list of airlines and check whether or not they operate directly in each market. 

# (1a) Get lists of all cities, all airlines, all markets served directly

# Drop Nippon & SkyWest

data2016 <- data2016 %>% filter(airline != "NH")
data2017 <- data2017 %>% filter(airline != "OO")

# Aggregate LCCs (except Southwest)

LCC = TRUE

if (LCC == TRUE) {

  data2016 <- data2016 %>%
    mutate(airline = case_when(airline %in% c("F9", "B6", "NK", "G4", "VX", "SY") ~ "LCC",
                               TRUE ~ airline)) %>%
    group_by(airline, market, ville_dep, ville_arr, popdep, poparr, gdpdep, gdparr,
             gdppercapdep, gdppercaparr) %>%
    summarize(direct = max(direct),
              direct_distance = min(direct_distance),
              pax = mean(pax),
              price = mean(price))

  data2017 <- data2017 %>%
    mutate(airline = case_when(airline %in% c("F9", "B6", "NK", "G4", "VX", "SY") ~ "LCC",
                               TRUE ~ airline)) %>%
    group_by(airline, market, ville_dep, ville_arr, popdep, poparr, gdpdep, gdparr,
             gdppercapdep, gdppercaparr) %>%
    summarize(direct = max(direct),
              direct_distance = min(direct_distance),
              pax = mean(pax),
              price = mean(price))

}

all_cities17 <- c(data2017$ville_dep, data2017$ville_arr) %>% unique()
all_airlines17 <- data2017$airline %>% unique()

direct_markets17 <- data2017 %>% 
  filter(direct==1) %>%
  filter(pax >= 90) %>% #AW: could see what happens if get rid of this criterion
  select(airline, market) %>% 
  mutate(id = paste(airline, market, sep="-")) %>% 
  pull(id)

# (1b) Construct all possible markets

all_mkts17 <- expand.grid(dep=all_cities17,dest=all_cities17) %>% 
  filter(dep != dest) %>% 
  arrange(dep)

all_mkts17 <- all_mkts17[!duplicated(t(apply(all_mkts17[1:2], 1, sort))),] # Deduplicate (i.e. keep A-B, drop B-A)

all_mkts17 <- all_mkts17 %>% 
  mutate(market = paste(dep, dest, sep="-"))

# (1c) Construct list of airline-market combinations (1176*11 = 12,936 combos if all airlines; 7056 if collapsing LCCs)
airlinesXmkts17 <- expand.grid(airline = all_airlines17, market = all_mkts17$market) %>% 
  separate(market, c("end1", "end2"), sep = "-", remove=FALSE) %>% 
  mutate(end1_id = paste(airline, end1, sep="-"),
         end2_id = paste(airline, end2, sep="-"),
         mkt_id = paste(airline, market, sep="-"))

# (1d) Check whether airline directly operates in each market
airlinesXmkts17 <- airlinesXmkts17 %>% 
  mutate(incumbent = mkt_id %in% direct_markets17)

# (2) Add Hub airports: https://en.wikipedia.org/wiki/List_of_hub_airports#United_States
# and slot airports
# FOR HUBS: have added "Focus cities" since LCCs mostly don't have hubs, but may not use

hubs <- c("AS-LAX", "AS-SFO", "AS-POR", "AS-SEA", 
          "AA-PHX", "AA-LAX", "AA-MIA", "AA-CHI", "AA-NYC", "AA-CHA", "AA-PHI", "AA-DAL", "AA-WSH",
          "DL-LAX", "DL-ATL", "DL-BOS", "DL-DET", "DL-MIN", "DL-NYC", "DL-SLC", "DL-SEA", "DL-CIN",  # Delta had Cincinnati hub until 2020 
          "F9-DNV",
          "SY-MIN", 
          "UA-LAX", "UA-SFO", "UA-DNV", "UA-CHI", "UA-NYC", "UA-HOU", "UA-WSH",
          "LCC-DNV", "LCC-MIN")

focus <- c("G4-PHX", "G4-LAX", "G4-MIA", "G4-TMP", "G4-IND", "G4-CIN", "G4-GRK", "G4-VEG", "G4-PIT", "G4-NSH", "G4-AUS", 
           "AS-SDG",
           "DL-RAL",
           "F9-PHX", "F9-MIA", "F9-ORL", "F9-TMP", "F9-ATL", "F9-CHI", "F9-VEG", "F9-CLE", "F9-PHL",
           "B6-LAX", "B6-MIA", "B6-ORL", "B6-BOS", "B6-NYC",
           "WN-PHX", "WN-LAX", "WN-SFO", "WN-DNV", "WN-ORL", "WN-ATL", "WN-CHI", "WN-WSH", "WN-VEG", "WN-DAL", "WN-HOU",
           "SY-VEG", "SY-DAL",
           "NK-MIA", "NK-ORL", "NK-ATL", "NK-CHI", "NK-DET", "NK-VEG", "NK-DAL")

slots <- c("NYC", "CHI", "WSH") # Market-level, so using cities rather than airports

airlinesXmkts17 <- airlinesXmkts17 %>% 
  mutate(HUB_dummy = (end1_id %in% hubs | end2_id %in% hubs),
         Slot_dummy = (end1 %in% slots | end2 %in% slots))

use_focus = FALSE

if (use_focus == TRUE) {
  
  airlinesXmkts17 <- airlinesXmkts17 %>% 
    mutate(HUB_dummy = (end1_id %in% focus | end2_id %in% focus))
}

# (3) Define entry threats based on airline operations in 2016

# (3a) Construct list of endpoints in which airlines operated (direct or indirect)
# Gayle & Wu: "Unlike the 3rd quarter data, the 1st quarter data are less restricted by not solely focusing on nonstop itineraries.""
airlinesXcities16 <- data2016 %>% 
  filter(pax >= 90) %>%
  select(airline, ville_dep, ville_arr) %>% 
  mutate(id1 = paste(airline, ville_dep, sep="-"),
         id2 = paste(airline, ville_arr, sep="-"))

airlinesXcities16 <- c(airlinesXcities16$id1, airlinesXcities16$id2) %>% unique()

# (3b) Check whether airline operates at endpoints in 2016 Q2
airlinesXmkts17 <- airlinesXmkts17 %>% 
  mutate(presence_end1_16 = end1_id %in% airlinesXcities16,
         presence_end2_16 = end2_id %in% airlinesXcities16,
         presence_any_16 = presence_end1_16 == TRUE | presence_end2_16 == TRUE,
         City1 = (presence_end1_16 == TRUE & presence_end2_16 == FALSE) | 
           (presence_end1_16 == FALSE & presence_end2_16 == TRUE), # City1 = only operates in 1 endpoint
         City2 = (presence_end1_16 == TRUE & presence_end2_16 == TRUE), # City2 = operates in both endpoints
         pot_entrant = presence_any_16 == TRUE | incumbent == TRUE) # Potential Entrant = Incumbent in 2017 or has presence in 2016

# (3c) Number of entry threats = # of airlines with City2 == 1 but do not enter market

agg_entrants <- airlinesXmkts17 %>% 
  filter(pot_entrant == TRUE) %>% 
  group_by(market) %>% 
  summarize(nentrants = sum(incumbent), 
            pentrants = sum(pot_entrant),
            nentrythreats = sum(City2 == TRUE & incumbent == FALSE), # present at both endpoints
            nentrythreats_noHub = sum(City2 == TRUE & incumbent == FALSE & HUB_dummy == FALSE), # present at both endpoints but neither is Hub
            nentrythreats_Hub = sum(City2 == TRUE & incumbent == FALSE & HUB_dummy == TRUE), # present at both endpoints and one or both are hubs
            nentrythreats_one = sum(City1 == TRUE & incumbent == FALSE)) # present at one endpoint only

# (4) Put all data together for final entry model dataset

# (4a) Append agg entrants info
entry_data <- left_join(airlinesXmkts17, agg_entrants, by="market")

# (4b) Add market characteristics and city characteristics

direct_dist <- bind_rows(data2016, data2017) %>%
  ungroup() %>% 
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
# All are between airports generally close together (about <3 hrs driving)
# Will use https://www.airmilescalculator.com/distance/ to fill these in manually

entry_data <- entry_data %>% 
  mutate(direct_distance = case_when(market == "BUF-ROC" ~ 55,
                                     market == "AUS-SAT" ~ 66,
                                     market == "RIC-VIR" ~ 75,
                                     market == "ORL-TMP" ~ 81,
                                     market == "CIN-LVL" ~ 84,
                                     market == "CIN-IND" ~ 99,
                                     market == "BOS-HRT" ~ 91,
                                     market == "CLE-PIT" ~ 106,
                                     market == "IND-LVL" ~ 111,
                                     market == "CIN-COL" ~ 115,
                                     market == "JCK-ORL" ~ 144,
                                     market == "COL-PIT" ~ 145,
                                     market == "LVL-NSH" ~ 151,
                                     market == "JCK-TMP" ~ 181,
                                     market == "BUF-PIT" ~ 186,
                                     market == "COL-LVL" ~ 198,
                                     market == "MEM-NSH" ~ 200,
                                     TRUE ~ direct_distance)) # Consider dropping these

# Drop markets with very short distances (per CB advice)
entry_data <- entry_data %>% 
  filter(direct_distance > 99)
  
dep_chars <- data2017 %>% 
  ungroup() %>% 
  select(ville_dep, popdep, gdpdep, gdppercapdep) %>% 
  distinct()

arr_chars <- data2017 %>% 
  ungroup() %>% 
  select(ville_arr, poparr, gdparr, gdppercaparr) %>% 
  distinct()

entry_data <- left_join(entry_data, dep_chars, by = c("end1" = "ville_dep"))
entry_data <- left_join(entry_data, arr_chars, by = c("end2" = "ville_arr"))

# Add pop and income product vars for SML
sml_data <- entry_data %>% 
  # filter(nentrants > 0) %>% 
  mutate(pop = popdep/1e14 * poparr, # Check against paper to make sure scaling correctly
         income = gdppercapdep*gdppercaparr/1e4, 
         dist = direct_distance/1e4,
         dist2 = direct_distance^2/1e4,
         slot = as.numeric(Slot_dummy),
         City1 = as.numeric(City1),
         City2 = as.numeric(City2),
         hub = as.numeric(HUB_dummy),
         I = as.numeric(incumbent),
         pot_entrant = as.numeric(pot_entrant)) %>% 
  select(-end1, -end2, -end1_id, -end2_id, -presence_end1_16, -presence_end2_16, 
         -presence_any_16, -direct_distance, -popdep, -gdpdep, -gdppercapdep, 
         -poparr, -gdparr, -gdppercaparr)

# Save Data
saveRDS(sml_data, paste0(projdir, "sml_data.rds"))

# Collapse to market-level for price regression

mkt_data <- sml_data %>% 
  # filter(nentrants > 0) %>% # Obviously can only have prices for markets that 1+ airline enters
  group_by(market) %>% 
  summarize(hub = sum(hub),
            City2 = sum(City2),
            City1 = sum(City1),
            nentrants = mean(nentrants),
            nentrythreats = mean(nentrythreats),
            nentrythreats_Hub = mean(nentrythreats_Hub),
            nentrythreats_noHub = mean(nentrythreats_noHub),
            nentrythreats_one = mean(nentrythreats_one),
            slot = mean(slot),
            pop = mean(pop),
            income = mean(income),
            dist = mean(dist),
            dist2 = mean(dist2))

price_data <- data2017 %>% 
  # filter(direct == 1) %>% # Only want direct prices in market
  group_by(market) %>% 
  summarize(mprice = mean(price),
            myield = mean(price/direct_distance))

price_data <- mkt_data %>% 
  left_join(price_data, by="market")

saveRDS(price_data, paste0(projdir, "price_data.rds"))

#-------------------------- Descriptive Statistics ---------------------------#

# AW: should clean these up and present better tables (e.g., stargazer)

# Check against Table 2 of paper
pot_entrants <- sml_data %>% 
  filter(pot_entrant == TRUE) %>% 
  mutate(type = case_when(City1 == TRUE ~ "City 1",
                          City2 == TRUE ~ "City 2",
                          TRUE ~ "City 0")) %>% 
  group_by(type) %>% 
  summarize(no = n(), entry = sum(incumbent)) %>% 
  mutate(perc_entry = entry/no)

data.table(pot_entrants)
# Results closely match those from paper: no City0, 0.25% of City 1 enter; 27.7% of City 2 enter.
# (Compare to 100% (from 3 City0 pot entrants), 0.84%, 22.13% from 2007 data in the paper)

# Table 3

summary(sml_data)

summary(price_data)

# Table 3 price summary
data2017 %>% 
  filter(direct == 1) %>% 
  group_by(market) %>% 
  summarize(price50 = median(price),
            price25 = quantile(price,  probs=.25),
            price75 = quantile(price, probs=.75)) %>% 
  summary()

## Numbers much higher than 2007, inflation expected but seems to go beyond...?
## Could it also be that we have ROUND TRIP PRICE rather than one-way?
