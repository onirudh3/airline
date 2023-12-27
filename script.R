
# Libraries and data ------------------------------------------------------

library(tidyverse)

# 2016 Data
df_2016 <- read.table("Data_USA_CB_2016_Q2.txt", sep = ";", header = T)

df_2017 <- read.table("Data_USA_CB_2017_Q2.txt", sep = ";", header = T)

df_pop <- read.csv("pop_msa_2010-2022.csv", sep = ";")
