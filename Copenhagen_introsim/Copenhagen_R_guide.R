## ----load_pacman--------------------------------------------------------------

# Check if the 'pacman' package is installed, if not install it:
if (!requireNamespace("pacman", quietly = TRUE)) install.packages("pacman")



## ----load_libraries-----------------------------------------------------------

# Load the required libraries into the current R session:
pacman::p_load(rio, 
               here, 
               tidyverse, 
               skimr, 
               janitor,
               lubridate,
               gtsummary, 
               flextable,
               officer,
               EpiStats,
               epikit, 
               apyramid, 
               scales)


## ----set_locale, eval=FALSE---------------------------------------------------
#  
#  # Check current system settings:
#  Sys.getlocale()
#  
#  # Set time in English:
#  Sys.setlocale("LC_TIME", "English")
#  

## ----working_directory, eval=FALSE--------------------------------------------
#  
#  here::here()
#  

## ----import_raw_data----------------------------------------------------------

# Import the raw data set:
linelist <- rio::import(here::here("data", "Copenhagen_raw.csv"))


## ----check_data---------------------------------------------------------------

# Skim the data to get a summary of the column types and values:
skimr::skim(linelist)


## ----check_date_format--------------------------------------------------------

# Check date element order:
head(linelist$dayonset)


## ----correct_date_format------------------------------------------------------

# Update linelist:
linelist <- linelist %>% 
  
  # Change column to date class:
  dplyr::mutate(dayonset = lubridate::dmy(dayonset))

# Check class of updated column:
class(linelist$dayonset)

# Check that updated format is correct:
head(linelist$dayonset)


## ----onset_histogram----------------------------------------------------------

# Check distribution of onset dates with a histogram:
hist(linelist$dayonset, breaks = "day")


## ----format_time_col----------------------------------------------------------

# Check format of time variable:
head(linelist$starthour)


## ----crosstab_dayonset_starthour----------------------------------------------

# Cross-tabulate dayonset with starthour:
janitor::tabyl(dat = linelist, 
               starthour, 
               dayonset)


## ----combine_date_time--------------------------------------------------------

linelist <- linelist %>% 
  # Combine dayonset and starthour in a new date time variable:
  mutate(onset_datetime = lubridate::ymd_h(paste(dayonset, starthour), 
                                           # Deal with missing starthour:
                                           truncated = 2))


## ----dayonset_format----------------------------------------------------------

head(linelist$dayonset)


## ----starthour_format---------------------------------------------------------

head(linelist$starthour)


## ----onset_datetime_format----------------------------------------------------

head(linelist$onset_datetime)


## ----skim_dose_cols-----------------------------------------------------------

drskim <- linelist %>% 
  # Select all columns with names that end in upper case 'D':
  select(ends_with("D", ignore.case = FALSE)) %>% 
  # Produce the skim summary table
  skimr::skim()


## ----maxrange_dose_cols-------------------------------------------------------

# Check range of maximum values across the selected columns:
range(drskim$numeric.p100)


## ----check_dose_cols----------------------------------------------------------

# Create summary table for dose response columns:
drtable <- linelist %>% 
  
  # Select all the columns with column names that end in upper case 'D':
  select(ends_with("D", ignore.case = FALSE)) %>% 
  
  # Create the summary table, excluding missing values:
  gtsummary::tbl_summary(missing = "no") %>% 
  
  # Convert to flextable:
  gtsummary::as_flex_table()

# Print the summary table:
drtable


## ----format_logical_cols------------------------------------------------------

# Convert cols to logical:
linelist <- linelist %>% 
  
  mutate(across(
    
    # Select columns that are numeric and where all values are 0, 1 or NA
    .cols = where(function(x) is.numeric(x) & all(x %in% c(0, 1, NA))), 
    
    # Convert columns matching these criteria to logical
    .fns = as.logical))



## ----hist_age-----------------------------------------------------------------

hist(linelist$age)


## ----factor_group-------------------------------------------------------------

linelist <- linelist %>% 
  
  # Convert group to a factor and label 0 as teacher, 1 as student:
  mutate(group = factor(group, labels = c("teacher", "student")))


## ----crosstab_group_age-------------------------------------------------------

# Create cross-tab:
janitor::tabyl(dat = linelist, age, group)


## ----age_recode---------------------------------------------------------------

# Update incorrect ages to the correct values with case_when:
linelist <- linelist %>% 
  
  mutate(age = 
           case_when(
             # Where the respondent is 16 and a teacher, change their age to 61:
             age == 16 & group == "teacher" ~ 61, 
             # where the respondent is 8 or 180 and a student, change their age to 18:
             age %in% c(8, 180) & group == "student" ~ 18, 
             # Keep remaining values as is: 
             TRUE ~ as.numeric(age)
             )
         )


## ----crosstab_group_age_corrected---------------------------------------------

# Create cross-tab:
janitor::tabyl(dat = linelist, age, group)


## ----factor_class_levels------------------------------------------------------

# Check variable sex:
janitor::tabyl(linelist, sex)

# Check variable class:
janitor::tabyl(linelist, class)


## ----factor_convert_class2----------------------------------------------------

# Get linelist and pipe it in:
linelist <- linelist %>% 
  
  # Now call 'mutate' to update the variables:
  mutate(
    sex = factor(sex, labels = c("female", "male")),
    class = factor(class)
    )


## ----skim_clean_final---------------------------------------------------------

# Final skim of the data before analysis:
skimr::skim(linelist)


## ----incubation_col-----------------------------------------------------------

# Start with linelist:
linelist <- linelist %>% 
  
  # Create new column for meal date and time:
  mutate(meal_datetime = lubridate::ymd_hm("2006-11-11 18:00"))

  

## ----define_gastrosymptoms----------------------------------------------------

# Define a list of gastro symptom columns:
gastro_cols <- c("diarrhoea", "bloody", "vomiting")


# Start with linelist:
linelist <- linelist %>% 
  
  # Create a new column called gastrosymptoms:
  mutate(gastrosymptoms = case_when(
    
    # gastrosymptoms = FALSE if none of the gastro columns are TRUE:
    if_all(.cols = all_of(gastro_cols), 
           .fns = ~ !. %in% c(TRUE)) ~ FALSE,
    
    # gastrosymptoms = TRUE if any of the gastro columns are TRUE:
    if_any(.cols = any_of(gastro_cols), 
           .fns = ~ . == TRUE) ~ TRUE
    
  ))



## ----no_gastrosymptoms--------------------------------------------------------
# How many people had no data for any of the symptoms of interest?
nosymptoms <- linelist %>% 
  filter(if_all(.cols = all_of(gastro_cols), .fns = ~ is.na(.))) %>% 
  count() %>% 
  pull()

# Print result:
nosymptoms


## ----no_gastrosymptoms_meal---------------------------------------------------

# Of these, how many also had a meal at the school dinner party?
nosymptoms_meal <- linelist %>% 
  filter(if_all(.cols = all_of(gastro_cols), 
                .fns = ~ is.na(.)) & meal == TRUE) %>% 
  count() %>% 
  pull()

# Print results:
nosymptoms_meal


## ----exposure_cols------------------------------------------------------------

# Get list of variable names in the data:
names(linelist)

# Choose the exposure variables:

# Create list of exposure variables to analyse:
exposure_cols <- c("tuna", 
                   "shrimps", 
                   "green", 
                   "veal", 
                   "pasta", 
                   "rocket", 
                   "sauce", 
                   "bread", 
                   "champagne", 
                   "beer", 
                   "redwine", 
                   "whitewine")


## ----define_ate_anything------------------------------------------------------

# Start with the linelist:
linelist <- linelist %>% 
  
  # Create ate_anything column:
  mutate(ate_anything = case_when(
    
    # ate_anything = FALSE if none of the exposure columns are TRUE:
    if_all(.cols = all_of(exposure_cols), 
           .fns = ~ !. %in% c(TRUE)) ~ FALSE,
    
    # ate_anything = TRUE if any of the exposure columns are TRUE:
    if_any(.cols = any_of(exposure_cols), 
           .fns = ~ . == TRUE) ~ TRUE

  ))


## ----check_meal---------------------------------------------------------------

# Start with linelist:
linelist %>% 
  
  # Cross-tabulate meal with ate_anything:
  tabyl(meal, ate_anything)


## ----recode_meal--------------------------------------------------------------

# Start with linelist:
linelist <- linelist %>% 
  
  # Change respondents who ate a food or drink item to meal = TRUE:
  mutate(meal = if_else(
    condition = meal == FALSE & ate_anything == TRUE, 
    true = TRUE, 
    false = meal
    ))


## ----define_casedef_nomaxtime-------------------------------------------------

# Start with linelist:
linelist <- linelist %>% 

  # Create case definition:
  mutate(
    case = case_when(
      
      # Cases have any gastro symptoms with onset after the meal:
      meal == TRUE & 
        gastrosymptoms == TRUE & 
        !is.na(onset_datetime) & 
        (onset_datetime >= meal_datetime) 
      ~ TRUE, 
      
      # Non cases have no gastro symptoms but ate a meal at the party:
      meal == TRUE & 
        (gastrosymptoms == FALSE | 
           # ... or have gastro symptoms but onset is before the meal:
           (gastrosymptoms == TRUE & (onset_datetime < meal_datetime))) 
      ~ FALSE,
      
      # Define excluded individuals as those with no record of a meal:
      meal == FALSE | is.na(meal) 
      ~ NA
      
      )
    )


## ----casedef_nomax_check------------------------------------------------------

# First check how many people ate a meal at the dinner party:
atemeal <- linelist %>% 
  filter(meal == TRUE) %>% 
  count(meal) %>% 
  pull()

# Print the result:
atemeal

# Next, check how many people ate a meal AND had onset after the meal:
atemealsick <- linelist %>% 
  filter(meal == TRUE & (onset_datetime >= meal_datetime)) %>% 
  count(meal) %>% 
  pull()

# Print the result:
atemealsick

# Finally check how many people ate a meal AND fell ill afterwards with 
# any of diarrhoea, bloody diarrhoea, or vomiting:
atemealsickcase <- linelist %>% 
  filter(meal == TRUE & 
           (diarrhoea == TRUE | bloody == TRUE | vomiting == TRUE) & 
           (onset_datetime >= meal_datetime)) %>% 
  count(meal) %>% 
  pull()

# Print the result:
atemealsickcase


## ----define_incubation--------------------------------------------------------

# First, we can makes sure that only case incubation times are examined:
linelist <- linelist %>% 
  
  # Update incubation to be NA if case is not TRUE:
  mutate(incubation = ifelse(test = case == TRUE, 
                             yes = onset_datetime - meal_datetime, 
                             no = NA))


## ----median_incubation--------------------------------------------------------

# Median incubation time:
med_incubation <- median(linelist$incubation, na.rm = TRUE)

# Print the result:
med_incubation


## ----check_case_before_update-------------------------------------------------

# Tabulate case:
janitor::tabyl(dat = linelist, case)


## ----update_case_def----------------------------------------------------------

# Update the case definition to limit to onset three days after meal:
linelist <- linelist %>% 
  
  mutate(case = case_when(
    
    # If respondent is a case but onset is more than two days after the meal
    # Change them to a non-case (FALSE)
    case == TRUE & (onset_datetime > (meal_datetime + days(2))) ~ FALSE, 
    
    # For everyone else, keep their current case status as is:
    TRUE ~ case
    
    )
  )


## ----check_case_after_update--------------------------------------------------

# Retabulate cases to see if anything has changed:
janitor::tabyl(dat = linelist, case)


## ----check_onset_after_update-------------------------------------------------

# Cross-tabulate onset date time with case status:
janitor::tabyl(dat = linelist, onset_datetime, case) %>% 
  adorn_totals()


## ----drop_nacase--------------------------------------------------------------

linelist <- linelist %>%

  # Remove rows where case is NA:
  drop_na(case)


## ----export_clean_data--------------------------------------------------------

rio::export(x = linelist, 
            file = here::here("data", "Copenhagen_clean.xlsx"))


## ----import_clean_data--------------------------------------------------------

# Import the cleaned data set using rio and here packages:
linelist <- rio::import(here::here("data", "Copenhagen_clean.xlsx")) %>% 
  
  # Convert character columns to factors:
  mutate(across(.cols = where(is.character), 
                .fns = as.factor))


