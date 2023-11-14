df <- read.csv("data/day2_data_energy_prod_EU_2020-08-03_2020-08-09.csv")

# explore: column names
names(df)
# first rows
head(df)

# number of countries or energy grids
length(unique(df$MapCode)) # clean countries
# number of records
dim(df)
nrow(df)
# what type/strucutre/class is this?
class(df$DateTime) # make this sortable (= not character)
# transform into POSIXct
df$DateTimePOS <- as.POSIXct(df$DateTime)

# let's make a first plot: scatter generated energy against installed capacity
plot(
  x = df$InstalledGenCapacity,
  y = df$ActualGenerationOutput
)

# we see that we have some outliers, so lets introduce a filter based on this assumption:
# generated energy cannot be very much higher than installed capacity (maybe only a bit due
# to reading errors or something like that)
# so: keep only those records of which the generated energy is smaller then the installed capacity times 4
nrwos_df_not_cleand <- nrow(df)
df <- df[df$ActualGenerationOutput < df$InstalledGenCapacity*4,]
nrow(df)

# run the plot again
plot(
  x = df$InstalledGenCapacity,
  y = df$ActualGenerationOutput
)
# we still see some outlieres and we already see how the data is distributed

# TASK: Create plots using plot and ggplot syntax. Find out more information
# about our dataset!
# for example: 
# - plot the number of power plants per production type, or
# - production capacity per type


