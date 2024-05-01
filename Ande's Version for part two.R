setwd("C:/Users/Dell/Desktop/Sciences Po/Second Semester/QUANTI MIDTERM/Midterm II_neu/scpo-midterm-olm")
library(haven)
library(dplyr)
library(ggplot2)

# Processing each year (some variables I do not end up using)

usa_2005 <- read_sav("ISSP_2005.sav") %>%
  filter(C_ALPHAN == "US") %>%
  select(TOPBOT, URBRURAL, AGE, SEX, MARITAL, COHAB, WRKST, SPWRKST, US_DEGR, US_RINC, US_INC, HOMPOP) %>%
  mutate(year = 2005)

usa_2007 <- read_sav("ISSP_2007.sav") %>%
  filter(V4 == 840) %>%
  select(TOPBOT, URBRURAL, AGE, SEX, MARITAL, COHAB, WRKST, SPWRKST, US_DEGR, US_RINC, US_INC, HOMPOP) %>%
  mutate(year = 2007)

usa_2009 <- read_sav("ISSP_2009.sav") %>%
  filter(V4 == 840) %>%
  select(TOPBOT, URBRURAL, AGE, SEX, MARITAL, COHAB, WRKST, SPWRKST, US_DEGR, US_RINC, US_INC, HOMPOP) %>%
  mutate(year = 2009)

usa_2011 <- read_sav("ISSP_2011.sav") %>%
  filter(V4 == 840) %>%
  select(TOPBOT, URBRURAL, AGE, SEX, MARITAL, PARTLIV, MAINSTAT, SPMAINST, US_DEGR, US_RINC, US_INC, HOMPOP) %>%
  mutate(year = 2011)

usa_2013 <- read_sav("ISSP_2013.sav") %>%
  filter(V4 == 840) %>%
  select(TOPBOT, URBRURAL, AGE, SEX, MARITAL, PARTLIV, MAINSTAT, SPMAINST, US_DEGR, US_RINC, US_INC, HOMPOP) %>%
  mutate(year = 2013)

usa_2015 <- read_sav("ISSP_2015.sav") %>%
  filter(c_alphan == "US") %>%
  select(TOPBOT, URBRURAL, AGE, SEX, MARITAL, PARTLIV, MAINSTAT, SPMAINST, US_DEGR, US_RINC, US_INC, HOMPOP) %>%
  mutate(year = 2015)

usa_2017 <- read_sav("ISSP_2017.sav") %>%
  filter(c_alphan == "US") %>%
  select(TOPBOT, URBRURAL, AGE, SEX, MARITAL, PARTLIV, MAINSTAT, SPMAINST, US_DEGR, US_RINC, US_INC, HOMPOP) %>%
  mutate(year = 2017)

usa_2019 <- read_sav("ISSP_2019.sav") %>%
  filter(c_alphan == "US") %>%
  select(TOPBOT, URBRURAL, AGE, SEX, MARITAL, PARTLIV, MAINSTAT, SPMAINST, US_DEGR, US_RINC, US_INC, HOMPOP) %>%
  mutate(year = 2019)

#####
#Unifying the variable MARITAL
# 1 = married, in a legal relationship, living together
# 2 = widowed
# 3 = single (being divorced or never having married, living alone)

usa_2005 <- usa_2005 %>%
  filter(!MARITAL %in% c(4, 9)) %>%
  mutate(MARITAL = ifelse(MARITAL == 5, 3, MARITAL))

usa_2007 <- usa_2007 %>%
  filter(!MARITAL %in% c(4, 9)) %>%
  mutate(MARITAL = ifelse(MARITAL == 5, 3, MARITAL))

usa_2009 <- usa_2009 %>%
  filter(!MARITAL %in% c(4, 9)) %>%
  mutate(MARITAL = ifelse(MARITAL == 5, 3, MARITAL))

usa_2011 <- usa_2011 %>%
  filter(!MARITAL %in% c(3, 7, 8, 9)) %>%
  mutate(MARITAL = case_when(
    MARITAL == 2 ~ 1,
    MARITAL == 5 ~ 2,
    MARITAL %in% c(4, 6) ~ 3,
    TRUE ~ MARITAL
  ))

usa_2013 <- usa_2013 %>%
  filter(!MARITAL %in% c(3, 7, 8, 9)) %>%
  mutate(MARITAL = case_when(
    MARITAL == 2 ~ 1,
    MARITAL == 5 ~ 2,
    MARITAL %in% c(4, 6) ~ 3,
    TRUE ~ MARITAL
  ))

usa_2015 <- usa_2015 %>%
  filter(!MARITAL %in% c(3, 7, 8, 9)) %>%
  mutate(MARITAL = case_when(
    MARITAL == 2 ~ 1,
    MARITAL == 5 ~ 2,
    MARITAL %in% c(4, 6) ~ 3,
    TRUE ~ MARITAL
  ))

usa_2017 <- usa_2017 %>%
  filter(!MARITAL %in% c(3, 7, 8, 9)) %>%
  mutate(MARITAL = case_when(
    MARITAL == 2 ~ 1,
    MARITAL == 5 ~ 2,
    MARITAL %in% c(4, 6) ~ 3,
    TRUE ~ MARITAL
  ))

usa_2019 <- usa_2019 %>%
  filter(!MARITAL %in% c(3, 7, 8, 9)) %>%
  mutate(MARITAL = case_when(
    MARITAL == 2 ~ 1,
    MARITAL == 5 ~ 2,
    MARITAL %in% c(4, 6) ~ 3,
    TRUE ~ MARITAL
  ))

###Unifying WRKST / MAINST
# 1 = employed, 5 = unemployed/not in workforce (all reasons), 
# 6 = student/vocational training, 7 = retired, 8 = all kinds of domestic work

usa_2005 <- usa_2005 %>%
  filter(!WRKST %in% c(2, 3, 4, 97, 98, 99)) %>%
  mutate(WRKST = case_when(
    WRKST %in% c(9, 10) ~ 5,
    TRUE ~ WRKST
  ))

usa_2007 <- usa_2007 %>%
  filter(!WRKST %in% c(2, 3, 4, 97, 98, 99)) %>%
  mutate(WRKST = case_when(
    WRKST %in% c(9, 10) ~ 5,
    TRUE ~ WRKST
  ))

usa_2009 <- usa_2009 %>%
  filter(!WRKST %in% c(2, 3, 4, 97, 98, 99)) %>%
  mutate(WRKST = case_when(
    WRKST %in% c(9, 10) ~ 5,
    TRUE ~ WRKST
  ))

usa_2011 <- usa_2011 %>%
  filter(!MAINSTAT %in% c(8, 9, 99)) %>%
  mutate(WRKST = case_when(
    MAINSTAT == 2 ~ 5,
    MAINSTAT %in% c(3, 4) ~ 6,
    MAINSTAT == 6 ~ 7,
    MAINSTAT == 7 ~ 8,
    TRUE ~ MAINSTAT
  ))

usa_2013 <- usa_2013 %>%
  filter(!MAINSTAT %in% c(8, 9, 99)) %>%
  mutate(WRKST = case_when(
    MAINSTAT == 2 ~ 5,
    MAINSTAT %in% c(3, 4) ~ 6,
    MAINSTAT == 6 ~ 7,
    MAINSTAT == 7 ~ 8,
    TRUE ~ MAINSTAT
  ))

usa_2015 <- usa_2015 %>%
  filter(!MAINSTAT %in% c(8, 9, 99)) %>%
  mutate(WRKST = case_when(
    MAINSTAT == 2 ~ 5,
    MAINSTAT %in% c(3, 4) ~ 6,
    MAINSTAT == 6 ~ 7,
    MAINSTAT == 7 ~ 8,
    TRUE ~ MAINSTAT
  ))

usa_2017 <- usa_2017 %>%
  filter(!MAINSTAT %in% c(8, 9, 99)) %>%
  mutate(WRKST = case_when(
    MAINSTAT == 2 ~ 5,
    MAINSTAT %in% c(3, 4) ~ 6,
    MAINSTAT == 6 ~ 7,
    MAINSTAT == 7 ~ 8,
    TRUE ~ MAINSTAT
  ))

usa_2019 <- usa_2019 %>%
  filter(!MAINSTAT %in% c(8, 9, 99)) %>%
  mutate(WRKST = case_when(
    MAINSTAT == 2 ~ 5,
    MAINSTAT %in% c(3, 4) ~ 6,
    MAINSTAT == 6 ~ 7,
    MAINSTAT == 7 ~ 8,
    TRUE ~ MAINSTAT
  ))

#this is redundant, but it feels easier than changing my previous code:

usa_2011 <- usa_2011 %>% select(-MAINSTAT)
usa_2013 <- usa_2013 %>% select(-MAINSTAT)
usa_2015 <- usa_2015 %>% select(-MAINSTAT)
usa_2017 <- usa_2017 %>% select(-MAINSTAT)
usa_2019 <- usa_2019 %>% select(-MAINSTAT)

# Combine all datasets into one merged file
usa_total <- bind_rows(usa_2005, usa_2007, usa_2009, usa_2011, usa_2013, usa_2015, usa_2017, usa_2019)

#both US_INC and US_RINC follow the same labelling:

# <1000 USD = 1, 1000-10.000 USD = 2, 10.000 - 20.000 USD = 3, 
# 20.000 - 50.000 USD = 4, 
# 50.000 - 100k USD = 5, 100k-150k+ = 6

# Cleaning US_INC in the merged dataset
usa_total <- usa_total %>%
  filter(!US_INC %in% c(999990, 999997, 999998)) %>%  
  mutate(US_INC = case_when(
    US_INC == 500 ~ 1,
    US_INC %in% c(2000, 3500, 4500, 5500, 6500, 7500, 9000) ~ 2,
    US_INC %in% c(11250, 13750, 16750, 18750) ~ 3,
    US_INC %in% c(21750, 23750, 27500, 32500, 37500, 45000) ~ 4,
    US_INC %in% c(55000, 67500, 82500, 100000, 120000, 140000) ~ 5,
    US_INC == 160000 ~ 6,
    TRUE ~ US_INC  
  ))

# Cleaning US_RINC in the merged dataset
usa_total <- usa_total %>%
  filter(!US_RINC %in% c(999990, 999997, 999998)) %>%  
  mutate(US_RINC = case_when(
    US_RINC == 500 ~ 1,
    US_RINC %in% c(2000, 3500, 4500, 5500, 6500, 7500, 9000) ~ 2,
    US_RINC %in% c(11250, 13750, 16750, 18750) ~ 3,
    US_RINC %in% c(21750, 23750, 27500, 32500, 37500, 45000) ~ 4,
    US_RINC %in% c(55000, 67500, 82500, 100000, 120000, 140000) ~ 5,
    US_RINC == 160000 ~ 6,
    TRUE ~ US_RINC  
  ))

## Continue cleaning in the merged file
usa_total <-usa_total %>% mutate(TOPBOT10 = ifelse(TOPBOT %in% c(-9, -8, 97, 98, 99, NA, "a", "d", "n", "r"), NA, TOPBOT))

# Remove NA values
usa_total <- usa_total %>%
  filter(!is.na(TOPBOT))

usa_total <- usa_total %>%
  filter(!is.na(AGE))

usa_total <- usa_total %>%
  filter(!is.na(SEX))

usa_total <- usa_total %>%
  filter(!is.na(US_DEGR))

# Assign year groups
usa_total <- usa_total %>%
  mutate(year_group = case_when(
    year %in% c(2005, 2007) ~ "2005-2007",
    year %in% c(2009, 2011) ~ "2009-2011",
    year %in% c(2013, 2015) ~ "2013-2015",
    year %in% c(2017, 2019) ~ "2017-2019"
  )) %>%
  select(-year) %>%
  rename(year = year_group)

#making sure certain variables are numeric 
#(maybe my R is acting up but I had some trouble with this?)

usa_total$TOPBOT <- as.numeric(usa_total$TOPBOT)
usa_total$URBRURAL <- as.numeric(usa_total$URBRURAL)
usa_total$AGE <- as.numeric(usa_total$AGE)

# creating place dummies 
usa_total <- usa_total %>%
  mutate(PlaceSuburb = if_else(URBRURAL == 2, 1, 0),
         PlaceSmallTown = if_else(URBRURAL == 3, 1, 0),
         PlaceRural = if_else(URBRURAL == 4 | URBRURAL == 5, 1, 0))

usa_total <- usa_total %>%
  mutate(Year_f2005_2007 = if_else(year == "2005-2007", 1, 0),
         Year_f2009_2011 = if_else(year == "2009-2011", 1, 0),
         Year_f2013_2015 = if_else(year == "2013-2015", 1, 0),
         Year_f2017_2019 = if_else(year == "2017-2019", 1, 0))

usa_total$post67 <- ifelse(usa_total$AGE >= 67, 1, 0)

#making other variables are factors 

usa_total <- usa_total %>%
  mutate(SEX = factor(SEX, levels = c(1, 2), labels = c("Male", "Female")))

usa_total <- usa_total %>%
  mutate(
    SEX = factor(SEX),
    MARITAL = factor(MARITAL),
    WRKST = factor(WRKST),
    US_DEGR = factor(US_DEGR),
    US_INC = factor(US_INC),
    US_RINC = factor(US_RINC)
  )

####--------------------------------------------------------
#creating some MODELSSSSS

#Biggest model: SSS and age, gender, etc.
model1 <- lm(TOPBOT ~ AGE + SEX + US_DEGR + WRKST + US_INC + US_RINC + 
               PlaceSuburb + PlaceSmallTown + PlaceRural,
             data = usa_total)

#Second, adding the variable older or younger than 67, checking for gendered effects
model2 <- lm(TOPBOT ~ AGE + SEX * post67 + US_DEGR + WRKST + US_INC + US_RINC +
               PlaceSuburb + PlaceSmallTown + PlaceRural,
             data = usa_total)

#Third, exploring the effects of marital status on SSS for women as they age

model3_marital <- lm(TOPBOT ~ MARITAL + AGE + US_DEGR + WRKST + US_INC + US_RINC +
                       PlaceSuburb + PlaceSmallTown + PlaceRural,
                     data = subset(usa_total, SEX == "Female"))


#I then had quite some other models in here, trying to find better fits and working with more interaction terms
#but they became overfitted and did not offer more explanatory power, so I cut them

#Printing the model summaries 
summary_model1 <- summary(model1)
print(summary_model1)

summary_model2 <- summary(model2)
print(summary_model2)

summary_modelmarital <- summary(model3_marital)
print(summary_modelmarital)

### VISUALIZATIONS (some exploratory, some after having the regression models)
# Plotting Work Status for women
plot_work_status <- ggplot(subset(usa_total, SEX == "Female"), aes(x = AGE, y = TOPBOT, color = factor(WRKST))) +
  geom_smooth(method = "lm", se = FALSE, formula = y ~ x, aes(group = WRKST)) +  
  scale_color_manual(values = c("1" = "gray",    # Employed
                                "5" = "orange",  # Unemployed
                                "6" = "yellow",  # Student
                                "7" = "purple",  # Retired
                                "8" = "red"),    # Domestic Work
                     labels = c("Employed", "Unemployed", "Student/Vocational Training", "Retired", "Domestic Work"),
                     name = "Work Status") +
  labs(title = "Development of SSS by Work Status for Women",
       x = "Age",
       y = "Subjective Social Status") +
  theme_minimal() +
  theme(legend.position = "bottom")

print(plot_work_status)

# Plotting location differences
usa_total$Location <- with(usa_total,
                           ifelse(PlaceSuburb == 1, "Suburb",
                                  ifelse(PlaceSmallTown == 1, "Small Town",
                                         ifelse(PlaceRural == 1, "Rural", NA))))

usa_total$Location <- factor(usa_total$Location)

plot_location <- ggplot(subset(usa_total, SEX == "Female"), aes(x = AGE, y = TOPBOT, color = Location)) +
  geom_smooth(method = "lm", se = FALSE, formula = y ~ x) +  # Add linear regression lines without confidence intervals
  scale_color_manual(values = c("Suburb" = "green", "Small Town" = "blue", "Rural" = "red"),
                     labels = c("Suburb", "Small Town", "Rural"),
                     name = "Location") +
  labs(title = "Development of SSS by Location for Women",
       x = "Age",
       y = "Subjective Social Status") +
  theme_minimal() +
  theme(legend.position = "right")  # Ensure the legend is positioned correctly

print(plot_location)

# Plotting the differences between educational attainment levels
# this code sometimes need to run twice in order to produce the plot??

plot_education <- ggplot(subset(usa_total, SEX == "Female"), aes(x = AGE, y = TOPBOT, color = factor(US_DEGR))) +
  geom_smooth(method = "lm", se = FALSE, formula = y ~ x) +  
  scale_color_manual(values = c("1" = "red",    
                                "2" = "orange", 
                                "3" = "yellow", 
                                "4" = "green", 
                                "5" = "blue"),
                     labels = c("1" = "< HS", "2" = "HS Grad", "3" = "Junior College", "4" = "Bachelor's", "5" = "Graduate"),
                     name = "Education Level") +
  labs(title = "SSS by Education Level for Women",
       x = "Age",
       y = "Subjective Social Status") +
  theme_minimal() +
  theme(legend.position = "right")

print(plot_education)


# Plotting personal income for women 

usa_total$US_RINC <- factor(usa_total$US_RINC,
                            levels = c(1, 2, 3, 4, 5),
                            labels = c("Less than $1K","$1k-10k", "$10K-$20K", "$20K-$50K",
                                       "$50K-$100K"))

plot_income <- ggplot(subset(usa_total, SEX == "Female"), aes(x = AGE, y = TOPBOT, color = US_RINC)) +
  geom_smooth(method = "lm", se = FALSE, formula = y ~ x) +  
  scale_color_brewer(palette = "Set1", name = "Personal Income") +  
  labs(title = "SSS by Personal Income for Women",
       x = "Age",
       y = "Subjective Social Status") +
  theme_minimal() +
  theme(legend.position = "right")

print(plot_income)

# Plotting for Model 1
plot_model1 <- ggplot(usa_total, aes(x = AGE, y = TOPBOT, color = SEX)) +
  geom_smooth(method = "lm", se = FALSE) +
  scale_color_manual(values = c("blue", "red"), labels = c("Male", "Female")) +  
  labs(title = "Model 1: SSS vs. Age by Gender",
       x = "Age",
       y = "Subjective Social Status") +
  theme_minimal() +
  theme(legend.position = "right")

plot_model1

# Plotting for Model 2 
plot_model2 <- ggplot(usa_total, aes(x = AGE, y = TOPBOT, color = SEX, linetype = factor(post67))) +
  geom_smooth(method = "lm", se = FALSE) +
  scale_color_manual(values = c("blue", "red"), labels = c("Male", "Female")) +  
  scale_linetype_manual(values = c("solid", "dashed"), labels = c("Younger than 67", "Older than 67")) +  
  labs(title = "Model 2: SSS vs. Age by Gender and Post-67 Status",
       x = "Age",
       y = "Subjective Social Status",
       color = "Gender",
       linetype = "Post-67 Status") +
  theme_minimal() +
  theme(legend.position = "right")  

plot_model2

plot_model3 <- ggplot(subset(usa_total, SEX == "Female"), aes(x = AGE, y = TOPBOT, color = MARITAL)) +
  geom_smooth(method = "lm", se = FALSE) +
  scale_color_manual(values = c("blue", "red", "green"), labels = c("Married", "Divorced", "Single")) +  
  labs(title = "Model 3: SSS vs. Age for Females by Marital Status",
       x = "Age",
       y = "Subjective Social Status",
       color = "Marital Status") +
  theme_minimal() +
  theme(legend.position = "right")  

plot_model3
