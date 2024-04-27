library(haven)
library(dplyr)

# Process each year, using a prefix to avoid naming confusion
df_2005 <- read_sav("ISSP_2005.sav") %>%
  filter(C_ALPHAN == "FR") %>%
  select(TOPBOT, URBRURAL) %>%
  mutate(year = 2005)

df_2007 <- read_sav("ISSP_2007.sav") %>%
  filter(V4 == 250) %>%
  select(TOPBOT, URBRURAL) %>%
  mutate(year = 2007)

df_2009 <- read_sav("ISSP_2009.sav") %>%
  filter(V4 == 250) %>%
  select(TOPBOT, URBRURAL) %>%
  mutate(year = 2009)

df_2011 <- read_sav("ISSP_2011.sav") %>%
  filter(V4 == 250) %>%
  select(TOPBOT, URBRURAL) %>%
  mutate(year = 2011)

df_2013 <- read_sav("ISSP_2013.sav") %>%
  filter(V4 == 250) %>%
  select(TOPBOT, URBRURAL) %>%
  mutate(year = 2013)

df_2015 <- read_sav("ISSP_2015.sav") %>%
  filter(c_alphan == "FR") %>%
  select(TOPBOT, URBRURAL) %>%
  mutate(year = 2015)

df_2017 <- read_sav("ISSP_2017.sav") %>%
  filter(c_alphan == "FR") %>%
  select(TOPBOT, URBRURAL) %>%
  mutate(year = 2017)

df_2019 <- read_sav("ISSP_2019.sav") %>%
  filter(c_alphan == "FR") %>%
  select(TOPBOT, URBRURAL) %>%
  mutate(year = 2019)

# Combine all datasets
data_total <- bind_rows(df_2005, df_2007, df_2009, df_2011, df_2013, df_2015, df_2017, df_2019)

data_total <-data_total %>% mutate(TOPBOT10 = ifelse(TOPBOT %in% c(-9, -8, 97, 98, 99, NA, "a", "d", "n", "r"), NA, TOPBOT))

# Remove NA values
data_total <- data_total %>%
  filter(!is.na(TOPBOT))

# Assign year groups
data_total <- data_total %>%
  mutate(year_group = case_when(
    year %in% c(2005, 2007) ~ "2005-2007",
    year %in% c(2009, 2011) ~ "2009-2011",
    year %in% c(2013, 2015) ~ "2013-2015",
    year %in% c(2017, 2019) ~ "2017-2019"
  )) %>%
  select(-year) %>%
  rename(year = year_group)

#years are in correct wave-format already

data_total$TOPBOT <- as.numeric(data_total$TOPBOT)
data_total$URBRURAL <- as.numeric(data_total$URBRURAL)


# creating place dummies ===== 
data_total <- data_total %>%
  mutate(PlaceSuburb = if_else(URBRURAL == 2, 1, 0),
         PlaceSmallTown = if_else(URBRURAL == 3, 1, 0),
         PlaceRural = if_else(URBRURAL == 4 | URBRURAL == 5, 1, 0))

data_total <- data_total %>%
  mutate(Year_f2005_2007 = if_else(year == "2005-2007", 1, 0),
         Year_f2009_2011 = if_else(year == "2009-2011", 1, 0),
         Year_f2013_2015 = if_else(year == "2013-2015", 1, 0),
         Year_f2017_2019 = if_else(year == "2017-2019", 1, 0))


## MODEL BUILDING #################

# Model 1: Only years
model_1 <- lm(TOPBOT ~ Year_f2009_2011 + Year_f2013_2015 + Year_f2017_2019, data = data_total)

# Model 2: Years and locations
model_2 <- lm(TOPBOT ~ Year_f2009_2011 + Year_f2013_2015 + Year_f2017_2019 + PlaceSuburb + PlaceSmallTown + PlaceRural, data = data_total)

# Model 3: Years, locations, and their interactions
model_3 <- lm(TOPBOT ~ (Year_f2009_2011 + Year_f2013_2015 + Year_f2017_2019) * (PlaceSuburb + PlaceSmallTown + PlaceRural), data = data_total)

summary_model_1 <- summary(model_1)
print(summary_model_1)

#______________________________________________________________________________________
install.packages('stargazer')
library(stargazer)


# Use stargazer to generate the table
stargazer(model_1, model_2, model_3, type = "text",
          title = "Comparative Regression Analysis",
          out = "models_comparison.txt",
          keep = c("Year_f2009_2011", "Year_f2013_2015", "Year_f2017_2019",
                   "PlaceSuburb", "PlaceSmallTown", "PlaceRural",
                   "Year_f2009_2011:PlaceSuburb", "Year_f2013_2015:PlaceSuburb", "Year_f2017_2019:PlaceSuburb",
                   "Year_f2009_2011:PlaceSmallTown", "Year_f2013_2015:PlaceSmallTown", "Year_f2017_2019:PlaceSmallTown",
                   "Year_f2009_2011:PlaceRural", "Year_f2013_2015:PlaceRural", "Year_f2017_2019:PlaceRural"),
          covariate.labels = c("Year 2009-2011", "Year 2013-2015", "Year 2017-2019",
                               "Suburban Place", "Small Town Place", "Rural Place",
                               "Year 2009-2011 * Suburban", "Year 2013-2015 * Suburban", "Year 2017-2019 * Suburban",
                               "Year 2009-2011 * Small Town", "Year 2013-2015 * Small Town", "Year 2017-2019 * Small Town",
                               "Year 2009-2011 * Rural", "Year 2013-2015 * Rural", "Year 2017-2019 * Rural"),
          add.lines = list(c("Number of Observations", nrow(data_total), nrow(data_total), nrow(data_total)),
                           c("R-squared", summary(model_1)$r.squared, summary(model_2)$r.squared, summary(model_3)$r.squared),
                           c("Adjusted R-squared", summary(model_1)$adj.r.squared, summary(model_2)$adj.r.squared, summary(model_3)$adj.r.squared),
                           c("Residual Std. Error", summary(model_1)$sigma, summary(model_2)$sigma, summary(model_3)$sigma),
                           c("F Statistic", paste(summary(model_1)$fstatistic[1], "df =", summary(model_1)$fstatistic[2], "and", summary(model_1)$fstatistic[3]),
                             paste(summary(model_2)$fstatistic[1], "df =", summary(model_2)$fstatistic[2], "and", summary(model_2)$fstatistic[3]),
                             paste(summary(model_3)$fstatistic[1], "df =", summary(model_3)$fstatistic[2], "and", summary(model_3)$fstatistic[3]))))




stargazer(model_1, model_2, model_3, type = "html", 
          out = "models_comparison.html")

browseURL("models_comparison.html")

