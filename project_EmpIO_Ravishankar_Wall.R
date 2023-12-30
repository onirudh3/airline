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
unique(data2017$airline) # still 12 airlines, but NH (Nippon) replaced with OO (SkyWest)

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

# AW: should definitely address the change above, possibly by dropping or collapsing some airlines?
# Per CB: "You may want to limit the number of airlines and aggregate some into a 
# ﬁctitious airline. Propose solutions (see for example Ciliberto and Tamer, Eca 2009)."

n_distinct(data2016$market) # 1159 markets in '16
n_distinct(data2017$market) # 1155 markets in '17

#------------------------------ Da"ta Cleaning -------------------------------#
#
# Goal: Dataset at market-level with # of potential entrants + # of actual entrants + market chars

# (1) Take the list of airlines and check whether or not they operate directly in each market. 

# (1a) Get lists of all cities, all airlines, all markets served directly

all_cities17 <- c(data2017$ville_dep, data2017$ville_arr) %>% unique()
all_airlines17 <- data2017$airline %>% unique()

direct_markets17 <- data2017 %>% 
  filter(direct==1) %>% 
  filter(pax >= 90) %>% 
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

hubs <- c("AS-LAX", "AS-SFO", "AS-POR", "AS-SEA", 
          "AA-PHX", "AA-LAX", "AA-MIA", "AA-CHI", "AA-NYC", "AA-CHA", "AA-PHI", "AA-DAL", "AA-WSH",
          "DL-LAX", "DL-ATL", "DL-BOS", "DL-DET", "DL-MIN", "DL-NYC", "DL-SLC", "DL-SEA", "DL-CIN", # Delta had Cincinnatti hub until 2020 
          "F9-DNV", 
          "SY-MIN",
          "UA-LAX", "UA-SFO", "UA-DNV", "UA-CHI", "UA-NYC", "UA-HOU", "UA-WSH")

slots <- c("NYC", "CHI", "WSH") # Market-level, so using cities rather than airports

airlinesXmkts17 <- airlinesXmkts17 %>% 
  mutate(HUB_dummy = (end1_id %in% hubs | end2_id %in% hubs),
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
                                     TRUE ~ direct_distance))
  
dep_chars <- data2017 %>% 
  select(ville_dep, popdep, gdpdep, gdppercapdep) %>% 
  distinct()

arr_chars <- data2017 %>% 
  select(ville_arr, poparr, gdparr, gdppercaparr) %>% 
  distinct()

entry_data <- left_join(entry_data, dep_chars, by = c("end1" = "ville_dep"))
entry_data <- left_join(entry_data, arr_chars, by = c("end2" = "ville_arr"))

# You need to create the variables City2, Nentrythreats, Hub and 
# Slot. Because markets are non directional, you need to slightly adapt some of 
# the deﬁnitions used in Gayle and Wu. For example, you may modify Nentrythreats 
# and consider only competitors which are present at BOTH endpoints (the year 
# before). Alternatively, you can build TWO variables, the number of competitors 
# present at both end points and the number of competitors present at only one 
# end-point. Don’t forget to add the ”non-existent” markets, i.e. those for 
# which no airline proposes a direct ﬂight. Suggest a descriptive analysis, i.e. 
# tables 1 to 3 of Gayle and Wu (2013). Note that I only ask you to consider the 
# average price in a given market.

#------------------------------ SML Procedure -------------------------------#