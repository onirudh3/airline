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
popdata <- read.csv(paste0(projdir, "pop_msa_2010-2022.csv"), sep=";") %>% clean_names() # Do we need this?

#------------------------- Initial Data Exploration -------------------------#

unique(data2016$airline) # 12 airlines
unique(data2017$airline) # still 12 airlines, but NH (Nippon) replaced with OO (SkyWest) --> will drop

# AA = American Airlines
# UA = United Airlines
# DL = Delta Airlines
# WN = Southwest Airlines
# F9 = Frontier Airlines
# B6 = JetBlue 
# NK = Spirit Airlines
# AS = Alaska Airlines
# G4 = Allegiant Air
# VX = Virgin America
# SY = Sun Country Airlines
# OO = SkyWest Airlines

# Could collapse LCC per Wiki: Allegiant, Frontier, JetBlue, Southwest, Spirit, Sun Country, Virgin
# May want to separate out Southwest as Ciliberto & Tamer.

# Per CB: "You may want to limit the number of airlines and aggregate some into a 
# ﬁctitious airline. Propose solutions (see for example Ciliberto and Tamer, Eca 2009)."

n_distinct(data2016$market) # 1159 markets in '16
n_distinct(data2017$market) # 1155 markets in '17

#------------------------------ Data Cleaning -------------------------------#
#
# Goal: Dataset at market-level with # of potential entrants + # of actual entrants + market chars

# (1) Take the list of airlines and check whether or not they operate directly in each market. 

# (1a) Get lists of all cities, all airlines, all markets served directly

# Drop Nippon & SkyWest + aggregate LCC

data2016 <- data2016 %>% filter(airline != "NH")
data2017 <- data2017 %>% filter(airline != "OO")

# If we want to collapse airlines:
# data2016 <- data2016 %>% 
#   mutate(airline = case_when(airline %in% c("WN", "F9", "B6", "NK", "G4", "VX", "SY") ~ "LCC",
#                              TRUE ~ airline)) %>% 
#   group_by(airline, market, ville_dep, ville_arr, popdep, poparr, gdpdep, gdparr, 
#            gdppercapdep, gdppercaparr) %>% 
#   summarize(direct = max(direct),
#             direct_distance = min(direct_distance),
#             pax = mean(pax),
#             price = mean(price))
# 
# data2017 <- data2017 %>% 
#   mutate(airline = case_when(airline %in% c("WN", "F9", "B6", "NK", "G4", "VX", "SY") ~ "LCC",
#                              TRUE ~ airline)) %>% 
#   group_by(airline, market, ville_dep, ville_arr, popdep, poparr, gdpdep, gdparr, 
#            gdppercapdep, gdppercaparr) %>% 
#   summarize(direct = max(direct),
#             direct_distance = min(direct_distance),
#             pax = mean(pax),
#             price = mean(price))

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

# 49C2 = 1176, so this should be correct.

all_mkts17 <- all_mkts17 %>% 
  mutate(market = paste(dep, dest, sep="-"))

# (1c) Construct list of airline-market combinations (1176*12 = 14,112 combos)
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
# FOR HUBS: have added "Focus cities" since LCCs mostly don't have hubs

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
  mutate(HUB_dummy = (end1_id %in% hubs | end2_id %in% hubs), # | end1_id %in% focus | end2_id %in% focus),
         Slot_dummy = (end1 %in% slots | end2 %in% slots))

# (3) Define entry threats based on airline operations in 2016

# (3a) Construct list of endpoints in which airlines operated 
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
           (presence_end1_16 == FALSE & presence_end2_16 == TRUE),
         City2 = (presence_end1_16 == TRUE & presence_end2_16 == TRUE),
         pot_entrant = presence_any_16 == TRUE | incumbent == TRUE)

# Check against Table 2

pot_entrants <- airlinesXmkts17 %>% 
  filter(pot_entrant == TRUE) %>% 
  mutate(type = case_when(City1 == TRUE ~ "City 1",
                          City2 == TRUE ~ "City 2",
                          TRUE ~ "City 0")) %>% 
  group_by(type) %>% 
  summarize(no = n(), entry = sum(incumbent)) %>% 
  mutate(perc_entry = entry/no)

data.table(pot_entrants)
# Results closely match those from paper: 100% City 0 enter; 0.25% of City 1 enter; 27.5% of City 2 enter.
# (Compare to 100%, 0.84%, 22.13% from 2007 data in the paper)

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
# All are between airports generally close together (about <2 hrs driving)
# Will use https://www.airmilescalculator.com/distance/aus-to-bhm/ to fill these in manually

entry_data <- entry_data %>% 
  filter(!is.na(direct_distance))
  
  # mutate(direct_distance = case_when(market == "AUS-SAT" ~ 66,
  #                                    market == "BOS-HRT" ~ 91,
  #                                    market == "BUF-PIT" ~ 186,
  #                                    market == "BUF-ROC" ~ 55,
  #                                    market == "CIN-COL" ~ 115,
  #                                    market == "CIN-IND" ~ 99,
  #                                    market == "CIN-LVL" ~ 84,
  #                                    market == "CLE-PIT" ~ 106,
  #                                    market == "COL-LVL" ~ 198,
  #                                    market == "COL-PIT" ~ 145,
  #                                    market == "IND-LVL" ~ 111,
  #                                    market == "JCK-ORL" ~ 144,
  #                                    market == "JCK-TMP" ~ 181,
  #                                    market == "LVL-NSH" ~ 151,
  #                                    market == "MEM-NSH" ~ 200,
  #                                    market == "ORL-TMP" ~ 81,
  #                                    market == "RIC-VIR" ~ 75,
  #                                    TRUE ~ direct_distance)) # COnsider dropping these
  
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
  mutate(pop = popdep/1e14 * poparr,
         income = gdppercapdep*gdppercaparr/1e4,
         dist = direct_distance/1e4,
         dist2 = dist^2,
         slot = as.numeric(Slot_dummy),
         City1 = as.numeric(City1),
         City2 = as.numeric(City2),
         hub = as.numeric(HUB_dummy),
         I = as.numeric(incumbent),
         pot_entrant = as.numeric(pot_entrant)) %>% 
  select(-end1, -end2, -end1_id, -end2_id, -presence_end1_16, -presence_end2_16, 
         -presence_any_16, -direct_distance, -popdep, -gdpdep, -gdppercapdep, 
         -poparr, -gdparr, -gdppercaparr)

# Table 3

summary(sml_data)

# Table 3 price summary
data2017 %>% group_by(market) %>% summarize(price50 = median(price),
                                            price25 = quantile(price,  probs=.25),
                                            price75 = quantile(price, probs=.75)) %>% 
  summary()

## Numbers much higher than 2007, inflation expected but seems to go beyond...?
## Could it also be that we have ROUND TRIP PRICE rather than one-way?

# Save Data
saveRDS(sml_data, paste0(projdir, "sml_data.rds"))
