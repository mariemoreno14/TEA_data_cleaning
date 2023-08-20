library(dplyr)
library(tidyr)
library(readxl)
library(writexl)
library(stringr)  

# set the directory to the folder where the files being read in are. For this project, we are copying the required data and posting into one folder.
setwd("/Users/heidimariemoreno/portfolio/8th_Grade_Math_2013-2022")

# Read in Data files
data_2013 <- read_excel("2012-2013 8th Math DISTSTAAR1.xlsx")
data_2014 <- read_excel("2013-2014 8th Math DISTSTAAR1.xlsx")
data_2016 <- read_excel("2015-2016 8th Math DISTSTAAR1.xlsx")
data_2017 <- read_excel("2016-2017 8th Math DISTSTAAR1.xlsx")
data_2018 <- read_excel("2017-2018 8th Math DISTSTAAR1.xlsx")
data_2019 <- read_excel("2018-2019 8th Math DISTSTAAR1.xlsx")
data_2021 <- read_excel("2020-2021 8th Math DISTSTAAR1.xlsx")
data_2022 <- read_excel("2021-2022 8th Math DISTSTAAR1.xlsx")


# Read in Codebook files
codebook_2013 <- read_excel("8th Grade Math 2012-2013 Code Book.xlsx")
codebook_2014 <- read_excel("8th Grade Math 2013-2014 Code Book.xlsx")
codebook_2016 <- read_excel("8th Grade Math 2015-2016 Code Book.xlsx") 
codebook_2017 <- read_excel("8th Grade Math 2016-2017 Code Book.xlsx") 
codebook_2018 <- read_excel("8th Grade Math 2017-2018 Code Book.xlsx") 
codebook_2019 <- read_excel("8th Grade Math 2018-2019 Code Book.xlsx")
codebook_2021 <- read_excel("8th Grade Math 2020-2021 Code Book.xlsx")
codebook_2022 <- read_excel("8th Grade Math 2021-2022 Code Book.xlsx") 


# Decode column names using 
# match() function to match the column names in the data file with the Name column in the codebook file, and then using the colnames() 
# function to set the column names of the data file to the corresponding Label values in the codebook file.
colnames(data_2013) <- codebook_2013$LABEL[match(colnames(data_2013), codebook_2013$NAME)]
colnames(data_2014) <- codebook_2014$LABEL[match(colnames(data_2014), codebook_2014$NAME)]
colnames(data_2016) <- codebook_2017$LABEL[match(colnames(data_2016), codebook_2017$NAME)]
colnames(data_2017) <- codebook_2017$LABEL[match(colnames(data_2017), codebook_2017$NAME)] 
colnames(data_2018) <- codebook_2018$LABEL[match(colnames(data_2018), codebook_2018$NAME)]
colnames(data_2019) <- codebook_2019$LABEL[match(colnames(data_2019), codebook_2019$NAME)]
colnames(data_2021) <- codebook_2021$LABEL[match(colnames(data_2021), codebook_2021$NAME)]
colnames(data_2022) <- codebook_2022$LABEL[match(colnames(data_2022), codebook_2022$NAME)]


# Keep columns that match this pattern: "District Number" OR  start with "District YEAR" AND "Grade 8" AND end with "STAAR Mathematics Numerator/Denominator" 

# Check column names
cols_to_keep_2013 <- c("District Number", grep("Grade 8.*2013.*Mathematics.*(Numerator|Denominator)", colnames(data_2013), value = TRUE, perl = TRUE))
data_2013 <- data_2013[, cols_to_keep_2013]
colnames(data_2013)

cols_to_keep_2014 <- c("District Number", grep("2014.*Grade 8.*Mathematics.*(Numerator|Denominator)", colnames(data_2014), value = TRUE))
data_2014 <- data_2014[, cols_to_keep_2014]
colnames(data_2014)

cols_to_keep_2016 <- c("District Number", grep("2016.*Grade 8.*Mathematics.*(Numerator|Denominator)", colnames(data_2016), value = TRUE))
data_2016 <- data_2016[, cols_to_keep_2016]
colnames(data_2016)

cols_to_keep_2017 <- c("District Number", grep("2017.*Grade 8.*Mathematics.*(Numerator|Denominator)", colnames(data_2017), value = TRUE))
data_2017 <- data_2017[, cols_to_keep_2017]
colnames(data_2017)

cols_to_keep_2018 <- c("District Number", grep("2018.*Grade 8.*Mathematics.*(Numerator|Denominator)", colnames(data_2018), value = TRUE))
data_2018 <- data_2018[, cols_to_keep_2018]
colnames(data_2018)

cols_to_keep_2019 <- c("District Number", grep("2019.*Grade 8.*Mathematics.*(Numerator|Denominator)", colnames(data_2019), value = TRUE))
data_2019 <- data_2019[, cols_to_keep_2019]
colnames(data_2019)

cols_to_keep_2021 <- c("District Number", grep("2021.*Grade 8.*Mathematics.*(Numerator|Denominator)", colnames(data_2021), value = TRUE))
data_2021 <- data_2021[, cols_to_keep_2021]
colnames(data_2021)

cols_to_keep_2022 <- c("District Number", grep("2022.*Grade 8.*Mathematics.*(Numerator|Denominator)", colnames(data_2022), value = TRUE))
data_2022 <- data_2022[, cols_to_keep_2022]
colnames(data_2022)

# Steps to simplify column names--drop extraneous/repetitive info from name
# create vectors of column names to be used in altering the names
col_names_2013 <- colnames(data_2013)
col_names_2014 <- colnames(data_2014)
col_names_2016 <- colnames(data_2016)
col_names_2017 <- colnames(data_2017)
col_names_2018 <- colnames(data_2018)
col_names_2019 <- colnames(data_2019)
col_names_2021 <- colnames(data_2021)
col_names_2022 <- colnames(data_2022)


# extract the year and group identifier from each column name and numerator/denominator
# this regex extracts the year and group from the original column name and creates a new column name
# the first capture group extracts the district number as a string
# the second capture group extracts the year and group identifier and combines them with a colon
# the third capture group matches the rest of the original column name (which is not used)
new_col_names_2013 <- gsub(".*?2013 (.+?) Mathematics (.+)$", "2013: \\1: \\2", col_names_2013)
new_col_names_2014 <- gsub(".* (\\d{4}) .*, Grade 8, (.+), STAAR Mathematics (Numerator|Denominator)", "\\1: \\2: \\3", col_names_2014)
new_col_names_2016 <- gsub(".* (\\d{4}) .*, Grade 8, (.+), STAAR Mathematics (Numerator|Denominator)", "\\1: \\2: \\3", col_names_2016)
new_col_names_2017 <- gsub(".* (\\d{4}) .*, Grade 8, (.+), STAAR Mathematics (Numerator|Denominator)", "\\1: \\2: \\3", col_names_2017)
new_col_names_2018 <- sub("District (\\d+) Domain 1A: (.+), (Grade 8|Grade Level STD, Grade 8), (.+),.*STAAR Mathematics (Numerator|Denominator)", "\\1: \\2: \\4: \\5", col_names_2018)
new_col_names_2019 <- sub("District (\\d+) Domain 1A: (.+), (Grade 8|Grade Level STD, Grade 8), (.+),.*STAAR Mathematics (Numerator|Denominator)", "\\1: \\2: \\4: \\5", col_names_2019)
new_col_names_2021 <- sub("District (\\d+) Domain 1A: (.+), (Grade 8|Grade Level STD, Grade 8), (.+),.*STAAR Mathematics (Numerator|Denominator)", "\\1: \\2: \\4: \\5", col_names_2021)
new_col_names_2022 <- sub("DISTRICT (\\d+) Domain 1A: (.+), (Grade 8|Grade Level STD, Grade 8), (.+),.*STAAR Mathematics (Numerator|Denominator)", "\\1: \\2: \\4: \\5", col_names_2022)

# replace the original column names with the new column names
# if the column name is "District Number", leave it unchanged
colnames(data_2013) <- ifelse(colnames(data_2013) == "District Number", "District Number", new_col_names_2013)
colnames(data_2014) <- ifelse(colnames(data_2014) == "District Number", "District Number", new_col_names_2014)
colnames(data_2016) <- ifelse(colnames(data_2016) == "District Number", "District Number", new_col_names_2016)
colnames(data_2017) <- ifelse(colnames(data_2017) == "District Number", "District Number", new_col_names_2017)
colnames(data_2018) <- ifelse(colnames(data_2018) == "District Number", "District Number", new_col_names_2018)
colnames(data_2019) <- ifelse(colnames(data_2019) == "District Number", "District Number", new_col_names_2019)
colnames(data_2021) <- ifelse(colnames(data_2021) == "District Number", "District Number", new_col_names_2021)
colnames(data_2022) <- ifelse(colnames(data_2022) == "District Number", "District Number", new_col_names_2022)

# Define a function to remove " Grade Level STD" from column names
remove_grade_level_std <- function(df) {
  colnames(df) <- gsub(" Grade Level STD", "", colnames(df))
  return(df)
}

# Apply the function to each data frame
data_2018 <- remove_grade_level_std(data_2018)
data_2019 <- remove_grade_level_std(data_2019)
data_2021 <- remove_grade_level_std(data_2021)
data_2022 <- remove_grade_level_std(data_2022)




# reshape the data using pivot_longer by creating new columns Year, Group, and Rate
# `cols = -District Number` specifies that all columns except "District Number" should be pivoted
# `names_to = "Year_Group"` specifies that the new columns should be named "Year_Group" and filled with column names
# `values_to = "Rate"` specifies that the column values should be pivoted to a new column named "Rate
# separate the column "Year_Group" to "Year" and "Group" columns and separate values with ":" as the delimiter
data_reshaped_2013 <- data_2013 %>%
  pivot_longer(cols = -`District Number`,
               names_to = "Year_Group",
               values_to = "Rate") %>%
  separate(Year_Group, into = c("Year", "Group"), sep = ": ", remove = FALSE) %>%
  mutate(NumeratorDenominator = ifelse(grepl("Numerator", Year_Group), "Numerator", "Denominator"))

data_reshaped_2014 <- data_2014 %>%
  pivot_longer(cols = -`District Number`,
               names_to = "Year_Group",
               values_to = "Rate") %>%
  separate(Year_Group, into = c("Year", "Group"), sep = ": ", remove = FALSE) %>%
  mutate(NumeratorDenominator = ifelse(grepl("Numerator", Year_Group), "Numerator", "Denominator"))

data_reshaped_2016 <- data_2016 %>%
  pivot_longer(cols = -`District Number`,
               names_to = "Year_Group",
               values_to = "Rate") %>%
  separate(Year_Group, into = c("Year", "Group"), sep = ": ", remove = FALSE) %>%
  mutate(NumeratorDenominator = ifelse(grepl("Numerator", Year_Group), "Numerator", "Denominator"))

data_reshaped_2017 <- data_2017 %>%
  pivot_longer(cols = -`District Number`,
               names_to = "Year_Group",
               values_to = "Rate") %>%
  separate(Year_Group, into = c("Year", "Group"), sep = ": ", remove = FALSE) %>%
  mutate(NumeratorDenominator = ifelse(grepl("Numerator", Year_Group), "Numerator", "Denominator"))

data_reshaped_2018 <- data_2018 %>%
  pivot_longer(cols = -`District Number`,
               names_to = "Year_Group",
               values_to = "Rate") %>%
  separate(Year_Group, into = c("Year", "Score", "Group", "NumeratorDenominator"), sep = ":\\s*", remove = FALSE) %>%
  select(`District Number`, Year, Score, Group, Rate, NumeratorDenominator)

data_reshaped_2019 <- data_2019 %>%
  pivot_longer(cols = -`District Number`,
               names_to = "Year_Group",
               values_to = "Rate") %>%
  separate(Year_Group, into = c("Year", "Score", "Group", "NumeratorDenominator"), sep = ":\\s*", remove = FALSE) %>%
  select(`District Number`, Year, Score, Group, Rate, NumeratorDenominator)

data_reshaped_2021 <- data_2021 %>%
  pivot_longer(cols = -`District Number`,
               names_to = "Year_Group",
               values_to = "Rate") %>%
  separate(Year_Group, into = c("Year", "Score", "Group", "NumeratorDenominator"), sep = ":\\s*", remove = FALSE) %>%
  select(`District Number`, Year, Score, Group, Rate, NumeratorDenominator)

data_reshaped_2022 <- data_2022 %>%
  pivot_longer(cols = -`District Number`,
               names_to = "Year_Group",
               values_to = "Rate") %>%
  separate(Year_Group, into = c("Year", "Score", "Group", "NumeratorDenominator"), sep = ":\\s*", remove = FALSE) %>%
  select(`District Number`, Year, Score, Group, Rate, NumeratorDenominator)

# reorder the columns as desired. Any column not named will be dropped
data_reshaped_2013 <- data_reshaped_2013[, c("District Number", "Year", "Group", "Rate", "NumeratorDenominator")]
data_reshaped_2014 <- data_reshaped_2014[, c("District Number", "Year", "Group", "Rate", "NumeratorDenominator")]
data_reshaped_2016 <- data_reshaped_2016[, c("District Number", "Year", "Group", "Rate", "NumeratorDenominator")]
data_reshaped_2017 <- data_reshaped_2017[, c("District Number", "Year", "Group", "Rate", "NumeratorDenominator")]
data_reshaped_2018 <- data_reshaped_2018[, c("District Number", "Year", "Group", "Score", "Rate", "NumeratorDenominator")]
data_reshaped_2019 <- data_reshaped_2019[, c("District Number", "Year", "Group", "Score", "Rate", "NumeratorDenominator")]
data_reshaped_2021 <- data_reshaped_2021[, c("District Number", "Year", "Group", "Score", "Rate", "NumeratorDenominator")]
data_reshaped_2022 <- data_reshaped_2022[, c("District Number", "Year", "Group", "Score", "Rate", "NumeratorDenominator")]



# Find "'0158" and "0159" then drop the "'0"
data_reshaped_2021 <- data_reshaped_2021 %>%
  filter(grepl("^'0158|^'0159", `District Number`))
data_reshaped_2021$`District Number` <- gsub("'", "", data_reshaped_2021$`District Number`)
data_reshaped_2021$`District Number` <- gsub("^0", "", data_reshaped_2021$`District Number`)

# Drop '0 from values in District Number
data_reshaped_2022 <- data_reshaped_2022 %>%
  filter(grepl("^'0158|^'0159", `District Number`))
data_reshaped_2022$`District Number` <- gsub("'", "", data_reshaped_2022$`District Number`)
data_reshaped_2022$`District Number` <- gsub("^0", "", data_reshaped_2022$`District Number`)


# Keep only District Number values that begin with 158 or 159
data_reshaped_2013 <- data_reshaped_2013 %>%
  filter(grepl("^158|^159", `District Number`))
data_reshaped_2014 <- data_reshaped_2014 %>%
  filter(grepl("^158|^159", `District Number`))
data_reshaped_2016 <- data_reshaped_2016 %>%
  filter(grepl("^158|^159", `District Number`))
data_reshaped_2017 <- data_reshaped_2017 %>%
  filter(grepl("^158|^159", `District Number`))
data_reshaped_2018 <- data_reshaped_2018 %>%
  filter(grepl("^158|^159", `District Number`))
data_reshaped_2019 <- data_reshaped_2019 %>%
  filter(grepl("^158|^159", `District Number`))


# Read in District Name Codebook--will be used to turn District Numbers to District Names
districts <- read_excel("District Code Book New.xlsx")

# Ensure the District Number column in the data_reshaped data (currently a string) is
# numeric so it can be matched to the districts dataframe
data_reshaped_2013$"District Number" <- as.numeric(data_reshaped_2013$"District Number")
data_reshaped_2014$"District Number" <- as.numeric(data_reshaped_2014$"District Number")
data_reshaped_2016$"District Number" <- as.numeric(data_reshaped_2016$"District Number")
data_reshaped_2017$"District Number" <- as.numeric(data_reshaped_2017$"District Number")
data_reshaped_2018$"District Number" <- as.numeric(data_reshaped_2018$"District Number")
data_reshaped_2019$"District Number" <- as.numeric(data_reshaped_2019$"District Number")
data_reshaped_2021$"District Number" <- as.numeric(data_reshaped_2021$"District Number")
data_reshaped_2022$"District Number" <- as.numeric(data_reshaped_2022$"District Number")


# Merge using left_join() the two dataframes on the "District Number" and "District_N" columns
data_reshaped_2013 <- data_reshaped_2013 %>%
  left_join(districts, by = c("District Number" = "DISTRICT")) 
data_reshaped_2014 <- data_reshaped_2014 %>%
  left_join(districts, by = c("District Number" = "DISTRICT")) 
data_reshaped_2016 <- data_reshaped_2016 %>%
  left_join(districts, by = c("District Number" = "DISTRICT")) 
data_reshaped_2017 <- data_reshaped_2017 %>%
  left_join(districts, by = c("District Number" = "DISTRICT")) 
data_reshaped_2018 <- data_reshaped_2018 %>%
  left_join(districts, by = c("District Number" = "DISTRICT")) 
data_reshaped_2019 <- data_reshaped_2019 %>%
  left_join(districts, by = c("District Number" = "DISTRICT")) 
data_reshaped_2021 <- data_reshaped_2021 %>%
  left_join(districts, by = c("District Number" = "DISTRICT")) 
data_reshaped_2022 <- data_reshaped_2022 %>%
  left_join(districts, by = c("District Number" = "DISTRICT")) 

# Specify columns to keep and use. The other columns will be dropped.
# Order the columns are written in will determine the order of the columns in the dataframe
data_reshaped_2013 <- data_reshaped_2013[, c("District Name", "Year", "Group", "Rate", "NumeratorDenominator")]
data_reshaped_2014 <- data_reshaped_2014[, c("District Name", "Year", "Group", "Rate", "NumeratorDenominator")]
data_reshaped_2016 <- data_reshaped_2016[, c("District Name", "Year", "Group", "Rate", "NumeratorDenominator")]
data_reshaped_2017 <- data_reshaped_2017[, c("District Name", "Year", "Group", "Rate", "NumeratorDenominator")]
data_reshaped_2018 <- data_reshaped_2018[, c("District Name", "Year", "Group", "Score", "Rate", "NumeratorDenominator")]
data_reshaped_2019 <- data_reshaped_2019[, c("District Name", "Year", "Group", "Score", "Rate", "NumeratorDenominator")]
data_reshaped_2021 <- data_reshaped_2021[, c("District Name", "Year", "Group", "Score", "Rate", "NumeratorDenominator")]
data_reshaped_2022 <- data_reshaped_2022[, c("District Name", "Year", "Group", "Score", "Rate", "NumeratorDenominator")]


# Dropping rows with NA values in "District Name" column
data_reshaped_2013 <- subset(data_reshaped_2013, !is.na(data_reshaped_2013$'District Name'))
data_reshaped_2014 <- subset(data_reshaped_2014, !is.na(data_reshaped_2014$'District Name'))
data_reshaped_2016 <- subset(data_reshaped_2016, !is.na(data_reshaped_2016$'District Name'))
data_reshaped_2017 <- subset(data_reshaped_2017, !is.na(data_reshaped_2017$'District Name'))
data_reshaped_2018 <- subset(data_reshaped_2018, !is.na(data_reshaped_2018$'District Name'))
data_reshaped_2019 <- subset(data_reshaped_2019, !is.na(data_reshaped_2019$'District Name'))
data_reshaped_2021 <- subset(data_reshaped_2021, !is.na(data_reshaped_2021$'District Name'))
data_reshaped_2022 <- subset(data_reshaped_2022, !is.na(data_reshaped_2022$'District Name'))

# Change Rate column to numeric values (currently string). Any column with a character (., *, etc.) will become NA
data_reshaped_2013$Rate <- as.numeric(data_reshaped_2013$Rate)
data_reshaped_2014$Rate <- as.numeric(data_reshaped_2014$Rate)
data_reshaped_2016$Rate <- as.numeric(data_reshaped_2016$Rate)
data_reshaped_2017$Rate <- as.numeric(data_reshaped_2017$Rate)
data_reshaped_2018$Rate <- as.numeric(data_reshaped_2018$Rate)
data_reshaped_2019$Rate <- as.numeric(data_reshaped_2019$Rate)
data_reshaped_2021$Rate <- as.numeric(data_reshaped_2021$Rate)
data_reshaped_2022$Rate <- as.numeric(data_reshaped_2022$Rate)


# Calculate the aggregated rates for each Year and Group within Bexar County
# make value column numeric and group by Year and Group
# add all the numerator values (above 0) within the groups and all the denominator values
# divide the numerator by denominator total (based on values above 0)
# Make all district names be Bexar County
# specify columns to keep
data_reshaped_2013 <- data_reshaped_2013 %>%
  group_by(Year, Group) %>%
  summarize(
    Numerator_Total = sum(ifelse(Rate[NumeratorDenominator == "Numerator"] > 0, Rate[NumeratorDenominator == "Numerator"], 0), na.rm = TRUE),
    Denominator_Total = sum(ifelse(Rate[NumeratorDenominator == "Denominator"] > 0, Rate[NumeratorDenominator == "Denominator"], 0), na.rm = TRUE),
    Rate = if_else(is.na(Denominator_Total), 0, Numerator_Total / Denominator_Total),
    `District Name` = "Bexar County"
  ) %>%
  select(`District Name`, Year, Group, Rate)

# Data frame: data_reshaped_2014
data_reshaped_2014 <- data_reshaped_2014 %>%
  group_by(Year, Group) %>%
  summarize(
    Numerator_Total = sum(ifelse(Rate[NumeratorDenominator == "Numerator"] > 0, Rate[NumeratorDenominator == "Numerator"], 0), na.rm = TRUE),
    Denominator_Total = sum(ifelse(Rate[NumeratorDenominator == "Denominator"] > 0, Rate[NumeratorDenominator == "Denominator"], 0), na.rm = TRUE),
    Rate = if_else(is.na(Denominator_Total), 0, Numerator_Total / Denominator_Total),
    `District Name` = "Bexar County"
  ) %>%
  select(`District Name`, Year, Group, Rate)

# Data frame: data_reshaped_2016
data_reshaped_2016 <- data_reshaped_2016 %>%
  group_by(Year, Group) %>%
  summarize(
    Numerator_Total = sum(ifelse(Rate[NumeratorDenominator == "Numerator"] > 0, Rate[NumeratorDenominator == "Numerator"], 0), na.rm = TRUE),
    Denominator_Total = sum(ifelse(Rate[NumeratorDenominator == "Denominator"] > 0, Rate[NumeratorDenominator == "Denominator"], 0), na.rm = TRUE),
    Rate = if_else(is.na(Denominator_Total), 0, Numerator_Total / Denominator_Total),
    `District Name` = "Bexar County"
  ) %>%
  select(`District Name`, Year, Group, Rate)

# Data frame: data_reshpaed_2017
data_reshaped_2017 <- data_reshaped_2017 %>%
  group_by(Year, Group) %>%
  summarize(
    Numerator_Total = sum(ifelse(Rate[NumeratorDenominator == "Numerator"] > 0, Rate[NumeratorDenominator == "Numerator"], 0), na.rm = TRUE),
    Denominator_Total = sum(ifelse(Rate[NumeratorDenominator == "Denominator"] > 0, Rate[NumeratorDenominator == "Denominator"], 0), na.rm = TRUE),
    Rate = if_else(is.na(Denominator_Total), 0, Numerator_Total / Denominator_Total),
    `District Name` = "Bexar County"
  ) %>%
  select(`District Name`, Year, Group, Rate)


# Only keep rows with a Score value of "Meets"
data_reshaped_2018 <- data_reshaped_2018 %>%
  filter(Score %in% c("Performance", "Meets"))
data_reshaped_2019 <- data_reshaped_2019 %>%
  filter(Score %in% c("Performance", "Meets"))
data_reshaped_2021 <- data_reshaped_2021 %>%
  filter(Score %in% c("Performance", "Meets"))
data_reshaped_2022 <- data_reshaped_2022 %>%
  filter(Score %in% c("Performance", "Meets"))


data_reshaped_2018 <- data_reshaped_2018 %>%
  group_by(Year, Group) %>%
  summarize(
    Numerator_Total = sum(ifelse(Rate[NumeratorDenominator == "Numerator"] > 0, Rate[NumeratorDenominator == "Numerator"], 0), na.rm = TRUE),
    Denominator_Total = sum(ifelse(Rate[NumeratorDenominator == "Denominator"] > 0, Rate[NumeratorDenominator == "Denominator"], 0), na.rm = TRUE),
    Rate = if_else(is.na(Denominator_Total), 0, Numerator_Total / Denominator_Total),
    `District Name` = "Bexar County"
  ) %>%
  select(`District Name`, Year, Group, Rate)

data_reshaped_2019 <- data_reshaped_2019 %>%
  group_by(Year, Group) %>%
  summarize(
    Numerator_Total = sum(ifelse(Rate[NumeratorDenominator == "Numerator"] > 0, Rate[NumeratorDenominator == "Numerator"], 0), na.rm = TRUE),
    Denominator_Total = sum(ifelse(Rate[NumeratorDenominator == "Denominator"] > 0, Rate[NumeratorDenominator == "Denominator"], 0), na.rm = TRUE),
    Rate = if_else(is.na(Denominator_Total), 0, Numerator_Total / Denominator_Total),
    `District Name` = "Bexar County"
  ) %>%
  select(`District Name`, Year, Group, Rate)

data_reshaped_2021 <- data_reshaped_2021 %>%
  group_by(Year, Group) %>%
  summarize(
    Numerator_Total = sum(ifelse(Rate[NumeratorDenominator == "Numerator"] > 0, Rate[NumeratorDenominator == "Numerator"], 0), na.rm = TRUE),
    Denominator_Total = sum(ifelse(Rate[NumeratorDenominator == "Denominator"] > 0, Rate[NumeratorDenominator == "Denominator"], 0), na.rm = TRUE),
    Rate = if_else(is.na(Denominator_Total), 0, Numerator_Total / Denominator_Total),
    `District Name` = "Bexar County"
  ) %>%
  select(`District Name`, Year, Group, Rate)

data_reshaped_2022 <- data_reshaped_2022 %>%
  group_by(Year, Group) %>%
  summarize(
    Numerator_Total = sum(ifelse(Rate[NumeratorDenominator == "Numerator"] > 0, Rate[NumeratorDenominator == "Numerator"], 0), na.rm = TRUE),
    Denominator_Total = sum(ifelse(Rate[NumeratorDenominator == "Denominator"] > 0, Rate[NumeratorDenominator == "Denominator"], 0), na.rm = TRUE),
    Rate = if_else(is.na(Denominator_Total), 0, Numerator_Total / Denominator_Total),
    `District Name` = "Bexar County"
  ) %>%
  select(`District Name`, Year, Group, Rate)

# For 2019, drop any rows with Group value of "Current-&-Monitored-Y1toY4-EL"
# Using logical indexing
data_reshaped_2019 <- data_reshaped_2019[data_reshaped_2019$Group != "Current-&-Monitored-Y1toY4-EL", ]

# For 2021, drop any rows with Group value of  "Current & Monitored EL"
# Using logical indexing
data_reshaped_2021 <- data_reshaped_2021[data_reshaped_2021$Group != "Current & Monitored EL", ]


# For 2022, drop any rows with Group value of  "Current & Monitored EB/EL" 
# Using logical indexing
data_reshaped_2022 <- data_reshaped_2022[data_reshaped_2022$Group != "Current & Monitored EB/EL" , ]

# Read in file with district 8th Grade math
math_districts <- read_excel("districts_8th_Grade_Math_2013-2022.xlsx")

#colnames(math_districts)
#colnames(data_reshaped_2022)

# Drop "Outcome" column from reading districts
math_districts <- subset(math_districts, select = -Outcome)

stacked_data <- rbind(math_districts, data_reshaped_2013, data_reshaped_2014, data_reshaped_2016, data_reshaped_2017, data_reshaped_2018, data_reshaped_2019, data_reshaped_2021, data_reshaped_2022)

sort(unique(stacked_data$Group))

stacked_data <- stacked_data %>%
  mutate(
    Group = recode(
      Group,
      "EL"                  = "Emergent Bilingual",
      "ELL"                  = "Emergent Bilingual",
      "EB/EL"               = "Emergent Bilingual",
      "Current & Monitored EB/EL" = "Emergent Bilingual",
      "Current & Monitored EL" = "Emergent Bilingual",
      "Current-&-Monitored-Y1toY4-EL" = "Emergent Bilingual",
      "African American"    = "Black/African American",
      "Hispanic"            = "Hispanic/Latinx",
      "Econ Disadv"         = "Economically Disadvantaged",
      "Special Ed"          = "Special Education",
      "American Indian"     = "American Indian/Native American"
    )
  )

View(stacked_data)


# write the updated data to a new excel file--type file path to Tableau Ready folder!!
write_xlsx(stacked_data, "/Users/heidimariemoreno/portfolio/8th_Grade_Math_2013-2022/RScript_Output/Bexar_County_Complete.xlsx")


