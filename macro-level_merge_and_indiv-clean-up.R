# set working directory
setwd("/Users/cynthia/Sync/Dokumente/Studium/Master/Semester 7/Strategies of Social Science Inquiry/Gruppenarbeit/DatenR")

# Load the dplyr package
library(dplyr)

library(tidyverse)


############################################################
#### Government Expenditure on Environmental Protection ####

IMFdata <- read.csv('./IMF-Government Expenditure on Environmental Protection/Environmental_Protection_Expenditures.csv', sep = ",")

# delete rows with domestic currency data, keep rows with percent of GDP
IMFdata <- subset(IMFdata, Unit == "Percent of GDP")

# Delete a column from a data set
IMFdata <- IMFdata %>% select(-Source)
IMFdata <- IMFdata %>% select(-CTS_Code)
IMFdata <- IMFdata %>% select(-CTS_Name)
IMFdata <- IMFdata %>% select(-CTS_Full_Descriptor)
IMFdata <- IMFdata %>% select(-F1995)
IMFdata <- IMFdata %>% select(-F2017)
IMFdata <- IMFdata %>% select(-F2018)
IMFdata <- IMFdata %>% select(-F2019)
IMFdata <- IMFdata %>% select(-F2020)
IMFdata <- IMFdata %>% select(-F2021)

# Get the unique values of the ISO3 column
unique_values <- unique(IMFdata$ISO3)

# Loop over the unique values
for (value in unique_values) {

  # Filter the data set and calculate the sums
  sums <- IMFdata %>%
    filter(ISO3 == value) %>%
    summarise(F1996 = sum(F1996), F1997 = sum(F1997), F1998 = sum(F1998),
              F1999 = sum(F1999), F2000 = sum(F2000), F2001 = sum(F2001), F2002 = sum(F2002),
              F2003 = sum(F2003), F2004 = sum(F2004), F2005 = sum(F2005), F2006 = sum(F2006),
              F2007 = sum(F2007), F2008 = sum(F2008), F2009 = sum(F2009), F2010 = sum(F2010),
              F2011 = sum(F2011), F2012 = sum(F2012), F2013 = sum(F2013), F2014 = sum(F2014),
              F2015 = sum(F2015), F2016 = sum(F2016))

  # Create a new row with the sums and the correct values for Country, ISO2, ISO3, and Unit
  new_row <- data.frame(ObjectId = "S", Country = first(IMFdata$Country[IMFdata$ISO3 == value]),
                        ISO2 = first(IMFdata$ISO2[IMFdata$ISO3 == value]),
                        ISO3 = first(IMFdata$ISO3[IMFdata$ISO3 == value]),
                        Indicator = "Sum", Unit = first(IMFdata$Unit[IMFdata$ISO3 == value]), sums)

  # Add the new row to the data set
  IMFdata <- rbind(IMFdata, new_row)
}

# delete no longer needed rows
IMFdata <- subset(IMFdata, ObjectId == "S")


#############################################################
#### World Bank Final consumption expenditure (% of GDP) ####

WBdata <- read.csv('./World Bank Final consumption expenditure/API_NE.CON.TOTL.ZS_DS2_en_csv_v2_4771651.csv',
                   sep = ",", skip = 4)

# Delete the specified columns from the data set
WBdata <- WBdata %>%
  select(-40:-4)

WBdata <- WBdata %>%
  select(-25:-30)


#################################
#### RSF Press Freedom Index ####

PFI2002 <- read.csv('./PressFreedomIndex/2002.csv', sep = ";", dec = ",")
PFI2003 <- read.csv('./PressFreedomIndex/2003.csv', sep = ";", dec = ",")
PFI2004 <- read.csv('./PressFreedomIndex/2004.csv', sep = ";", dec = ",")
PFI2005 <- read.csv('./PressFreedomIndex/2005.csv', sep = ";", dec = ",")
PFI2006 <- read.csv('./PressFreedomIndex/2006.csv', sep = ";", dec = ",")
PFI2007 <- read.csv('./PressFreedomIndex/2007.csv', sep = ";", dec = ",")
PFI2008 <- read.csv('./PressFreedomIndex/2008.csv', sep = ";", dec = ",")
PFI2009 <- read.csv('./PressFreedomIndex/2009.csv', sep = ";", dec = ",")
PFI2010 <- read.csv('./PressFreedomIndex/2010.csv', sep = ";", dec = ",")
PFI2011 <- read.csv('./PressFreedomIndex/2011-2012.csv', sep = ";", dec = ",")
PFI2012 <- read.csv('./PressFreedomIndex/2011-2012.csv', sep = ";", dec = ",")
PFI2013 <- read.csv('./PressFreedomIndex/2013.csv', sep = ";", dec = ",")
PFI2014 <- read.csv('./PressFreedomIndex/2014.csv', sep = ";", dec = ",")
PFI2015 <- read.csv('./PressFreedomIndex/2015.csv', sep = ";", dec = ",")
PFI2016 <- read.csv('./PressFreedomIndex/2016.csv', sep = ";", dec = ",")


PFIfinal <- PFI2002 %>%
  select(-3, -5:-11, -13:-16)

# Rename the column
names(PFIfinal)[names(PFIfinal) == "Score.N"] <- "2002"

PFIfinal <- PFIfinal %>% select(-Year..N.)

# Rearrange the columns in the data set
PFIfinal <- PFIfinal[, c("ISO", "EN_country", "2002")]

# Add the "Score.N" column from PFI2003 to PFIfinal, using the "ISO" column as the key
PFIfinal <- merge(PFIfinal, PFI2003[, c("ISO", "Score.N")], by = "ISO")

# Rename the column
names(PFIfinal)[names(PFIfinal) == "Score.N"] <- "2003"

# Create a list of the data sets
data_sets <- list(PFI2004, PFI2005, PFI2006, PFI2007, PFI2008, PFI2009, PFI2010,
                  PFI2011, PFI2012, PFI2013, PFI2014, PFI2015, PFI2016)

# Iterate over the data sets
for (i in 1:length(data_sets)) {
  # Add the "Score.N" column to the PFIfinal data set, using the "ISO" column as the key
  PFIfinal <- merge(PFIfinal, data_sets[[i]][, c("ISO", "Score.N")], by = "ISO")

  # Rename the column
  names(PFIfinal)[names(PFIfinal) == "Score.N"] <- paste0(i + 2003)
}

# Convert the columns "2002" to "2016" from character to numeric
PFIfinal <- PFIfinal %>%
  mutate_at(vars(starts_with("2")), as.numeric)

# different methods used with different result and different categories

# 2002 - 2005
# 1: 53.00 - 109 points (Very serious situation)
# 2: 32.01 - 52.99 points (Difficult situation)
# 3: 13.51 - 32.00 points (Problematic situation)
# 4: 5.50 - 13.50 points (Satisfactory situation)
# 5: 0.00 to 5.49 points (Good situation)

# Define the columns to loop through
columns <- c("2002", "2003", "2004", "2005")

# Loop through the columns
for (column in columns) {
  # Change the values of the column based on specific conditions
  PFIfinal <- PFIfinal %>%
    mutate(!!sym(column) := ifelse(!!sym(column) >= 0 & !!sym(column) <= 5.49, 5,
                                   ifelse(!!sym(column) >= 5.5 & !!sym(column) <= 13.5, 4,
                                          ifelse(!!sym(column) >= 13.51 & !!sym(column) <= 32, 3,
                                                 ifelse(!!sym(column) >= 32.01 & !!sym(column) <= 52.99, 2,
                                                        ifelse(!!sym(column) >= 53 & !!sym(column) <= 109, 1, NA))))))
}

# 2006
# 1: 54.01 - 109 points (Very serious situation)
# 2: 36.01 - 54.00 points (Difficult situation)
# 3: 16.01 - 36.00 points (Problematic situation)
# 4: 8.01 - 16.00 points (Satisfactory situation)
# 5: 0.00 to 8.00 points (Good situation)

# Change the values of the "2006" column based on specific conditions
PFIfinal <- PFIfinal %>%
  mutate(`2006` = ifelse(`2006` >= 0 & `2006` <= 8, 5,
                         ifelse(`2006` >= 8.01 & `2006` <= 16, 4,
                                ifelse(`2006` >= 16.01 & `2006` <= 36, 3,
                                       ifelse(`2006` >= 36.01 & `2006` <= 54, 2,
                                              ifelse(`2006` >= 54.01 & `2006` <= 109, 1, NA))))))

# 2007 - 2009
# 1: 67.51 - 116 points (Very serious situation)
# 2: 33.51 - 67.50 points (Difficult situation)
# 3: 20.01 - 33.50 points (Problematic situation)
# 4: 8.71 - 20.00 points (Satisfactory situation)
# 5: 0.00 to 8.70 points (Good situation)

# Define the columns to loop through
columns <- c("2007", "2008", "2009")

# Loop through the columns
for (column in columns) {
  # Change the values of the column based on specific conditions
  PFIfinal <- PFIfinal %>%
    mutate(!!sym(column) := ifelse(!!sym(column) >= 0 & !!sym(column) <= 8.7, 5,
                                   ifelse(!!sym(column) >= 8.71 & !!sym(column) <= 20, 4,
                                          ifelse(!!sym(column) >= 20.01 & !!sym(column) <= 33.5, 3,
                                                 ifelse(!!sym(column) >= 33.51 & !!sym(column) <= 67.5, 2,
                                                        ifelse(!!sym(column) >= 67.51 & !!sym(column) <= 116, 1, NA))))))
}

# 2010
# 1: 61 - 105 points (Very serious situation)
# 2: 39.00 - 60.00 points (Difficult situation)
# 3: 13.00 - 38.99 points (Problematic situation)
# 4: 7.00 - 12.99 points (Satisfactory situation)
# 5: 0.00 to 6.99 points (Good situation)

# Change the values of the "2010" column based on specific conditions
PFIfinal <- PFIfinal %>%
  mutate(`2010` = ifelse(`2010` >= 0 & `2010` <= 6.99, 5,
                         ifelse(`2010` >= 7 & `2010` <= 12.99, 4,
                                ifelse(`2010` >= 13 & `2010` <= 38.99, 3,
                                       ifelse(`2010` >= 39 & `2010` <= 60, 2,
                                              ifelse(`2010` >= 61 & `2010` <= 105, 1, NA))))))

# 2011 - 2012
# 1: 78 - 142 points (Very serious situation)
# 2: 52.00 - 77.99 points (Difficult situation)
# 3: 16.00 - 51.99 points (Problematic situation)
# 4: -5.00 - 15.99 points (Satisfactory situation)
# 5: -10.00 to -5.01 points (Good situation)

# Define the columns to loop through
columns <- c("2011", "2012")

# Loop through the columns
for (column in columns) {
  # Change the values of the column based on specific conditions
  PFIfinal <- PFIfinal %>%
    mutate(!!sym(column) := ifelse(!!sym(column) >= -10 & !!sym(column) <= -5.01, 5,
                                   ifelse(!!sym(column) >= -5 & !!sym(column) <= 15.99, 4,
                                          ifelse(!!sym(column) >= 16 & !!sym(column) <= 51.99, 3,
                                                 ifelse(!!sym(column) >= 52 & !!sym(column) <= 77.99, 2,
                                                        ifelse(!!sym(column) >= 78 & !!sym(column) <= 142, 1, NA))))))
}

# 2013 onward
# 1: 0 - 44 points (Very serious situation)
# 2: 45 - 64 points (Difficult situation)
# 3: 65 - 74 points (Problematic situation)
# 4: 75 - 84 points (Satisfactory situation)
# 5: 85 - 100 points (Good situation)

# Define the columns to loop through
columns <- c("2013", "2014", "2015", "2016")

# Loop through the columns
for (column in columns) {
  # Change the values of the column based on specific conditions
  PFIfinal <- PFIfinal %>%
    mutate(!!sym(column) := ifelse(!!sym(column) >= 0 & !!sym(column) <= 44.99, 1,
                                   ifelse(!!sym(column) >= 45 & !!sym(column) <= 64.99, 2,
                                          ifelse(!!sym(column) >= 65 & !!sym(column) <= 74.99, 3,
                                                 ifelse(!!sym(column) >= 75 & !!sym(column) <= 84.99, 4,
                                                        ifelse(!!sym(column) >= 85 & !!sym(column) <= 100, 5, NA))))))
}

# add a empty column
PFIfinal <- PFIfinal %>%
  mutate("1996" = NA)
PFIfinal <- PFIfinal %>%
  mutate("1997" = NA)
PFIfinal <- PFIfinal %>%
  mutate("1998" = NA)
PFIfinal <- PFIfinal %>%
  mutate("1999" = NA)
PFIfinal <- PFIfinal %>%
  mutate("2000" = NA)
PFIfinal <- PFIfinal %>%
  mutate("2001" = NA)

# Rearrange the columns of the data set
PFIfinal <- PFIfinal %>%
  select(ISO, EN_country, `1996`, `1997`, `1998`, `1999`, `2000`, `2001`, `2002`, `2003`, `2004`, `2005`, `2006`,
         `2007`, `2008`, `2009`, `2010`, `2011`, `2012`, `2013`, `2014`, `2015`, `2016`)

PFIfinal$`1996` <- as.numeric(PFIfinal$`1996`)
PFIfinal$`1997` <- as.numeric(PFIfinal$`1997`)
PFIfinal$`1998` <- as.numeric(PFIfinal$`1998`)
PFIfinal$`1999` <- as.numeric(PFIfinal$`1999`)
PFIfinal$`2000` <- as.numeric(PFIfinal$`2000`)
PFIfinal$`2001` <- as.numeric(PFIfinal$`2001`)


#####################################
#### Clean up CSES from Danielle ####

# load file "cses_imd.rdata" into RStudio
load("cses_imd.rdata")

indiv_data <- cses_imd

## Renaming variables
indiv_data <- indiv_data %>% rename(age = IMD2001_1,
                                    turnout = IMD3001,
                                    education = IMD2003,
                                    income = IMD2006,
                                    region = IMD2007,
                                    sestatus = IMD2016,
                                    unionmember = IMD2019_1,
                                    beamember = IMD2025,
                                    farmermember = IMD2026,
                                    profmember = IMD2027,
                                    prevvote = IMD3003_PR_1,
                                    party_id = IMD3005_1,
                                    vote_effect = IMD3012,
                                    employment = IMD2014)

# IMD2001_1 Age

# delete rows with missing values
indiv_data <- subset(indiv_data, age < 116)

indiv_data <- indiv_data %>% mutate(young = ifelse(age < 31, 1, 0),
                                    young_2 = ifelse(age < 25, 1, 0),
                                    young_3 = ifelse(age < 20, 1, 0))

# We could also just use the variable age in its continuous form in the model, have to see about that

# IMD2003 Education

# Change specific values in the "education" column to NA
indiv_data <- indiv_data %>%
  mutate(education = ifelse(education > 4, NA, education))

# indiv_data <- indiv_data %>% filter(education < 6)

# Now the variable runs from 0 (no education) to 4 (uni education) in steps from lower to higher education
# I think we can keep it like so? Otherwise maybe use categories like Andreina proposed:
# data_2 <- data_2 %>% mutate(university_ed = ifelse(education == 4, 1, 0),
#                            postsec_university = ifelse(education %in% 3:4, 1, 0),
#                            no_education = ifelse(education == 0, 1, 0),
#                            none_primary = ifelse(education <= 1, 1, 0),
#                            primary = ifelse(education == 1, 1, 0))

# IMD2006 Income

# Change specific values in the "income" column to NA
indiv_data <- indiv_data %>%
  mutate(income = ifelse(income > 5, NA, income))

# indiv_data <- indiv_data %>% filter(income < 6)

# Again, categories can be added here. Otherwise, 1 (lowest) to 5 (highest)
# data_2 <- data_2 %>% mutate(low_income = ifelse(income <= 2, 1, 0),
#                            high_income = ifelse(income >= 4, 1, 0))

# IMD2007 Region

# Change specific values in the "region" column to NA
indiv_data <- indiv_data %>%
  mutate(region = ifelse(region > 4, NA, region))

# indiv_data <- indiv_data %>% filter(region < 6)

indiv_data <- indiv_data %>% rename(urban_degree = region)
# Renamed to more accurately represent what this variable shows
# I kept the variables as-is, since now it is simply the higher the number,
# the more urban/large city the resp lives
# If we want categories (like for age) we need to discuss what counts as rural and urban

# IMD2014 Employment

# Change specific values in the "employment" column to NA
indiv_data <- indiv_data %>%
  mutate(employment = ifelse(employment > 96, NA, employment))

# indiv_data <- indiv_data %>% filter(employment < 96)

# These data are kind of all over the place, so categorisation is a must here
indiv_data <- indiv_data %>% mutate(unemployed = ifelse(employment == 5, 1, 0))

# Alternatively, since the previous categorisation labels those outside the labour force as employed:
indiv_data <- indiv_data %>% mutate(employed = ifelse(employment <= 3 |
                                                        employment == 11 |
                                                        employment == 12, 1, 0))

# IMD2016 Socio-economic status

# Change specific values in the "sestatus" column to NA
indiv_data <- indiv_data %>%
  mutate(sestatus = ifelse(sestatus > 5, NA, sestatus))

# indiv_data <- indiv_data %>% filter(sestatus < 6)

# Categorisation is a must here, but I am not sure how. Examples:
indiv_data <- indiv_data %>% mutate(white_collar = ifelse(sestatus == 1, 1, 0))
indiv_data <- indiv_data %>% mutate(self_employed = ifelse(sestatus == 4, 1, 0))

# IMD2019_1 Union membership

# Change specific values in the "unionmember" column to NA
indiv_data <- indiv_data %>%
  mutate(unionmember = ifelse(unionmember > 1, NA, unionmember))

# indiv_data <- indiv_data %>% filter(unionmember < 6)

# 0 = no member, 1 = member

# IMD2025 Business or employers' association membership

# Change specific values in the "beamember" column to NA
indiv_data <- indiv_data %>%
  mutate(beamember = ifelse(beamember > 1, NA, beamember))

# indiv_data <- indiv_data %>% filter(beamember < 6)

# IMD2026 Farmers' association membership

# Change specific values in the "farmermember" column to NA
indiv_data <- indiv_data %>%
  mutate(farmermember = ifelse(farmermember > 1, NA, farmermember))

# indiv_data <- indiv_data %>% filter(farmermember < 6)

# IMD2027 Professional association membership

# Change specific values in the "profmember" column to NA
indiv_data <- indiv_data %>%
  mutate(profmember = ifelse(profmember > 1, NA, profmember))

# indiv_data <- indiv_data %>% filter(profmember < 6)


# Memberships
indiv_data$member <- ifelse(rowSums(replace(indiv_data[, c("unionmember", "beamember", "farmermember", "profmember")], is.na(indiv_data[, c("unionmember", "beamember", "farmermember", "profmember")]), 0)) > 0, 1, 0)

# indiv_data$member <- ifelse(rowSums(indiv_data[, c("unionmember", "beamember", "farmermember", "profmember")]) > 0, 1, 0)


# IMD3001 Turnout
indiv_data <- indiv_data %>% filter(turnout < 2)

# IMD3003_PR_1 Vote in previous election (Presidential Round 1)
# I picked only one, picking four felt like a bit much? If we do want that I'll add the others too

# Change specific values in the "prevvote" column to NA
indiv_data <- indiv_data %>%
  mutate(prevvote = ifelse(prevvote > 1, NA, prevvote))

# indiv_data <- indiv_data %>% filter(prevvote < 3)

# IMD3005_1 Party identification

# Change specific values in the "party_id" column to NA
indiv_data <- indiv_data %>%
  mutate(party_id = ifelse(party_id > 1, NA, party_id))

# indiv_data <- indiv_data %>% filter(party_id < 3)
# Close to party? 0 = no, 1 = yes

# IMD3012 Voting makes a difference

# Change specific values in the "vote_effect" column to NA
indiv_data <- indiv_data %>%
  mutate(vote_effect = ifelse(vote_effect > 5, NA, vote_effect))

# indiv_data <- indiv_data %>% filter(vote_effect < 6)

# This runs from 0 (voting makes no difference) to 5 (voting makes a big difference)
# and thus is sort of continuous
# Not sure if we should keep this as-is or categorise and if the latter, how

# Delete multiple columns using the subset() function
# indiv_data <- subset(indiv_data, select = -c(IMD1001,IMD1002_VER,IMD1002_DOI,IMD1003,IMD1004,
#                                        IMD1007,IMD1008_MOD_1,IMD1008_MOD_2,IMD1008_MOD_3,IMD1008_MOD_4,
#                                        IMD1008_RES,IMD1016_1,IMD1016_2,IMD1016_3,
#                                        IMD5100_A,IMD5100_B,IMD5100_C,IMD5100_D,IMD5100_E,
#                                        IMD5100_F,IMD5100_G,IMD5100_H,IMD5100_I,IMD5101_A,
#                                        IMD5101_B,IMD5101_C,IMD5101_D,IMD5101_E,IMD5101_F,
#                                        IMD5101_G,IMD5101_H,IMD5101_I,IMD5102_A,IMD5102_B,
#                                        IMD5102_C,IMD5102_D,IMD5102_E,IMD5102_F,IMD5102_G,
#                                        IMD5102_H,IMD5102_I,IMD5103_A,IMD5103_B,IMD5103_C,
#                                        IMD5103_D,IMD5103_E,IMD5103_F,IMD5103_G,IMD5103_H,
#                                        IMD5103_I))

# Create a new dataset with just the columns used for the regression
indiv_data <- subset(indiv_data, select = c(IMD1006_UNAlpha2, IMD1008_YEAR, age, young, young_2, young_3,
                                            turnout, education, income, urban_degree, sestatus, unionmember,
                                            beamember, farmermember, profmember, prevvote, party_id,
                                            vote_effect, employment, unemployed, employed, white_collar, self_employed,member))


################################
#### Merge to CSES data set ####

bigdata <- indiv_data

library(countrycode)

# Convert two-digit ISO country codes to three-digit ISO country codes
indiv_data$ISO3 <- countrycode(indiv_data$IMD1006_UNAlpha2, "iso2c", "iso3c")
bigdata$ISO3 <- countrycode(bigdata$IMD1006_UNAlpha2, "iso2c", "iso3c")


## IMFdata ##

# add a empty column
bigdata <- indiv_data %>%
  mutate(IMF = NA)

bigdata$IMF <- as.numeric(bigdata$IMF)

getYearValueIMF <- function(year, country) {
  columnName <- paste0("F", year)
  row <- IMFdata %>%
    filter(ISO3 == country)
  return(row[1, columnName])
}

## WBdata ##

# add a empty column
bigdata <- bigdata %>%
  mutate(WB = NA)

bigdata$WB <- as.numeric(bigdata$WB)

getYearValueWB <- function(year, country) {
  columnName <- paste0("X", year)
  row <- WBdata %>%
    filter(Country.Code == country)
  return(row[1, columnName])
}


## PFIfinal ##

# add a empty column
bigdata <- bigdata %>%
  mutate(PFI = NA)

bigdata$PFI <- as.numeric(bigdata$PFI)

getYearValuePFI <- function(year, country) {
  columnName <- paste0(year)
  row <- PFIfinal %>%
    filter(ISO == country)
  return(row[1, columnName])
}

## add all values ##

imfValue <- 0
wbValue <- 0
pfiValue <- 0
globalYear <- 0
globalCountry <- 0

for (i in 1:nrow(indiv_data)) {
  if (i %% 10000 == 0) {
    print(i)
  }

  row <- indiv_data[i,]
  country <- row$ISO3[[1]]
  year <- row$IMD1008_YEAR[[1]]

  if (country != globalCountry || year != globalYear) {
    globalCountry <- country
    globalYear <- year
    imfValue <- getYearValueIMF(year, country)
    wbValue <- getYearValueWB(year, country)
    pfiValue <- getYearValuePFI(year, country)
  }

  bigdata[i, "IMF"] <- imfValue
  bigdata[i, "WB"] <- wbValue
  bigdata[i, "PFI"] <- pfiValue
}


# export
write.csv(bigdata, "/Users/cynthia/Sync/Dokumente/Studium/Master/Semester 7/Strategies of Social Science Inquiry/Gruppenarbeit/DatenR/bigdata.csv")
