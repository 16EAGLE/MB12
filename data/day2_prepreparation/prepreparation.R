library(dplyr)
library(tidyr)
library(RCurl)

# Initial df
df <- read.csv("day2_data_energy_prod_EU_2020-08-03_2020-08-09.csv")
old_count <- nrow(df)

# remove unrealistic outliers
df <- df[!df$ActualGenerationOutput > df$InstalledGenCapacity*4,]
new_count <- nrow(df)
difference <- old_count - new_count

# remove almost empty rows
df <- df[rowSums(is.na(df))<10, ]
new_count_1 <- nrow(df)
difference_1 <- new_count - new_count_1

# date col
df$DATE <- substr(df$DateTime,1,10)
df$DATE <- as.POSIXct(df$DATE, format = "%Y-%m-%d")

# DE & Northern Ireland adjustments
df$COUNTRY <- df$MapCode
df$COUNTRY[grep("DE_", df$MapCode)] <- "DE" # uniform DE
df$COUNTRY <- gsub("NIE", "GB", df$COUNTRY) # Northern Ireland to GB

# Add column that adds up daily entries per plant
df <- df %>%
  group_by(PowerSystemResourceName, DATE) %>%
  mutate(entries_this_day = n())
df <- ungroup(df)
View(df) # Entries per day different! Hungary: 96, GB: 48, Spain: 24

# Delete 2020-08-09 lines because this day is not fully recorded as there is always only one entry (00:00)
df <- df %>% 
  filter(DATE != "2020-08-09")




# Sum up daily Energy generation
df_grouped <- df %>% 
  group_by(PowerSystemResourceName, DATE, COUNTRY, ProductionTypeName, entries_this_day) %>% 
  summarise(sum_generation = sum(ActualGenerationOutput)) %>% 
  spread(DATE, sum_generation, fill = 0)
View(df_grouped)

# Divide sum through daily entry count (W = J/s)
df_grouped_1 <- df_grouped %>%
  mutate(across(starts_with("2020"), ~./entries_this_day, .names = "{.col}"))
View(df_grouped_1)

# Aggregate per country & sort
df_aggr <- df_grouped_1[,!colnames(df_grouped_1) %in% "ProductionTypeName"] %>% 
  group_by(COUNTRY) %>% 
  summarise(across(`2020-08-03`:`2020-08-08`, sum, .names = "{.col}"))
df_aggr <- df_aggr %>% arrange(desc(`2020-08-03`))
df_aggr <- ungroup(df_aggr)
df_aggr <- df_aggr %>% 
  mutate(AVERAGE_GENERATION = (`2020-08-03` + `2020-08-04` + `2020-08-05` + `2020-08-06` + `2020-08-07` +`2020-08-08`) / 6)
View(df_aggr)
# We could use this df as a basis for our targeted spatial country plot (day 5, slide 7)

# Aggregate per production type per country
df_type <- df_grouped_1 %>% 
  group_by(COUNTRY, ProductionTypeName) %>% 
  summarise(across(`2020-08-03`:`2020-08-08`, sum, .names = "{.col}"))
df_type <- df_type %>% arrange(desc(COUNTRY))
df_type <- ungroup(df_type)
View(df_type)




# Sum up daily Installed Capacity
df_grouped_IC <- df %>% 
  group_by(PowerSystemResourceName, DATE, COUNTRY, ProductionTypeName, entries_this_day) %>% 
  summarise(sum_generation = sum(InstalledGenCapacity)) %>% 
  spread(DATE, sum_generation, fill = 0)
View(df_grouped_IC)

# Divide sum through daily entry count (W = J/s)
df_grouped_1_IC <- df_grouped_IC %>%
  mutate(across(starts_with("2020"), ~./entries_this_day, .names = "{.col}"))
View(df_grouped_1_IC) # Installed Capacity for single plants fits, e.g. Isar 2 1410 MW, see https://de.wikipedia.org/wiki/Kernkraftwerk_Isar

# Aggregate per country & sort
df_aggr_IC <- df_grouped_1_IC[,!colnames(df_grouped_1_IC) %in% "ProductionTypeName"] %>% 
  group_by(COUNTRY) %>% 
  summarise(across(`2020-08-03`:`2020-08-08`, sum, .names = "{.col}"))
df_aggr_IC <- df_aggr_IC %>% arrange(desc(`2020-08-03`))
df_aggr_IC <- ungroup(df_aggr_IC)
View(df_aggr_IC) # It seems like there is quite a lot missing, e.g. DE, https://www.smard.de/page/en/wiki-article/5884/6038 says 232,000 MW (Nov 23) vs. 73,500 MW here

# Aggregate per production type per country
df_type_IC <- df_grouped_1_IC %>% 
  group_by(COUNTRY, ProductionTypeName) %>% 
  summarise(across(`2020-08-03`:`2020-08-08`, sum, .names = "{.col}"))
df_type_IC <- df_type %>% arrange(desc(COUNTRY))
df_type_IC <- ungroup(df_type_IC)
View(df_type_IC) # Wind, Photovoltaics, Biomass are missing completely and other numbers like Fossil Gas don´t match at all
