
# Output to R script:
knitr::knit_hooks$set(purl = knitr::hook_purl)


# Check if the 'pacman' package is installed, if not install it:
if (!requireNamespace("pacman", quietly = TRUE)) install.packages("pacman")

# Load the required libraries into the current R session:
pacman::p_load(rio, 
               here, 
               tidyverse, 
               skimr,
               plyr,
               janitor,
               lubridate,
               gtsummary, 
               flextable,
               officer,
               epikit, 
               apyramid, 
               scales,
               broom,
               EpiStats)



# Import the raw data set:
copdata <- rio::import(here::here("data", "Copenhagen_raw.csv"))


head(copdata)
dim(copdata)
str(copdata)
skimr::skim(copdata)
names(copdata)



# Have a look at the histogram  
hist(copdata$age)   
# Create cross-tab with the group variable:  
janitor::tabyl(dat = copdata, 
               var1 = age, 
               var2 = group)


# Convert group to a factor and label 0 as teacher, 1 as student:
copdata <- copdata %>% 
  mutate(group = factor(group, 
                        labels = c("teacher", "student")))




janitor::tabyl(dat = copdata, 
               var1 = age, 
               var2 = group) 



# Update incorrect ages to the correct values with case_when:  
copdata <- copdata %>%       
  mutate(age =                         
           case_when(                            
             # Where respondent is 16 and a teacher, change age to 61:
             age == 16 & group == "teacher" ~ 61,              
             # where respondent is 8 or 180 and a student, change age to 18:
             age == 8 & group == "student" ~ 18,              
             age == 180 & group == "student" ~ 18,              
             # Keep remaining values as is:              
             .default = as.numeric(age)   
             # if .default is not working, try:
             # TRUE ~ age
             )                    
         ) 



#| label: check_dose_cols
#| tbl-cap: no caption

# Create summary table for dose response columns: 
drtable <- copdata %>%       
  # Select all the columns with column names that end in upper case 'D':   
  select(ends_with("D", ignore.case = FALSE)) %>%       
  # Create the summary table, excluding missing values:   
  gtsummary::tbl_summary(missing = "no") 

  # Print the summary table: 
drtable


copdata <- copdata %>% 
  dplyr::mutate(
    sex = as.factor(sex),
    class = as.factor(class))
 


copdata <- copdata %>% 
  dplyr::mutate(
    # clinical symptoms
    across(.cols = c(diarrhoea, bloody, vomiting,
             abdo, nausea, fever,headache, jointpain), 
           .fns = ~ as.logical(.)
           )
    )


# Create a vector with all the food variables representing the amount of specific foods items eaten (those finishing with a capital "D")
# One way of doing it:
food_dose <- copdata %>% 
    dplyr::select(
      ends_with("D", ignore.case = FALSE)) %>% 
    names()

# Another way of doing it:
food_dose <- c("tunaD", "shrimpsD", "greenD", "vealD", 
                "pastaD", "rocketD", "sauceD", "breadD",
                "champagneD", "beerD", "redwineD", "whitewineD")


copdata <- copdata %>% 
  dplyr::mutate(
    # food dose variables
    across(.cols = all_of(food_dose), 
           .fns = ~as.factor(.))) 
  


# Have a look at how the data is stored
head(copdata$dayonset)
class(copdata$dayonset)

# Update copdata:
copdata <- copdata %>% 
  # Change column to date class:
  dplyr::mutate(
    dayonset = lubridate::dmy(dayonset))

# Check class of updated column:
class(copdata$dayonset)


# Cross-tabulate dayonset with starthour:
janitor::tabyl(dat = copdata, 
               var1 = starthour, 
               var2 = dayonset)


copdata <- copdata %>% 
  # Combine dayonset and starthour in a new date time variable:
  mutate(onset_datetime = 
           lubridate::ymd_h(
             str_glue("{dayonset}, {starthour}"), 
                                           # Deal with missing starthour:
                                           truncated = 2))



head(copdata$dayonset)
head(copdata$starthour)
head(copdata$onset_datetime)



rio::export(x = copdata, 
            file = here::here("data", "Copenhagen_clean1.rds"))



# Import the clean data set:
copdata <- rio::import(here::here("data", "Copenhagen_clean1.rds"))


copdata <- copdata %>% 
  filter(meal == TRUE)


copdata <- copdata %>% 
  mutate(gastrosymptoms = case_when(
    # Those had diarrhoea...
    diarrhoea == TRUE |
      #or bloody diarrhoea...
    bloody == TRUE |
      # or vomiting, are marked as TRUE (fell ill after the meal)
    vomiting == TRUE ~ TRUE,
    # The rest are FALSE. This includes those who ate a meal but had no symptoms (did not fell ill after the meal)
    .default = FALSE)
    )


# Start with copdata:
copdata <- copdata %>% 
  # Create new column for meal date and time:
  mutate(meal_datetime = lubridate::ymd_hm("2006-11-11 18:00"))



copdata <- copdata %>% 
  mutate(incubation = onset_datetime - meal_datetime,
         incubation = as.numeric (incubation))


median(as.numeric(copdata$incubation), na.rm = TRUE)




copdata <- copdata %>% 
  mutate(case = case_when(
    # Those who had symptoms <48h from the meal are cases (TRUE)
    gastrosymptoms == TRUE & 
      onset_datetime >= meal_datetime &
      onset_datetime <= (meal_datetime + days(2)) ~ TRUE,
    # Those who had symptoms >48h from the meal are non-cases (FALSE)
    gastrosymptoms == TRUE & 
      onset_datetime > (meal_datetime + days(2)) ~ FALSE,
    # The rest are considered non-cases. Including, those who had no symptoms at all, who have missing data on the onset_datetime variable, or who had symptoms before eating the meal 
    .default = FALSE)
  )


# Tabulate cases:
janitor::tabyl(dat = copdata, case)


copdata %>% 
  summarise(atemeal = sum(meal == TRUE),
            hadsympt = sum(gastrosymptoms == TRUE),
            nb_cases = sum(case == TRUE)
            )



rio::export(x = copdata, 
            file = here::here("data", "Copenhagen_clean2.rds"))



# Import the clean data set:
copdata <- rio::import(here::here("data", "Copenhagen_clean2.rds")) 




#| label: inc_time

# Create a dataset with only cases
cases <- copdata %>% 
  filter(case == TRUE)

incplot <- cases %>% 
  # Create an empty ggplot frame:
  ggplot() +
  # Add a histogram of incubation:
  geom_histogram(
    mapping = aes(x = incubation), 
    # Set bin widths to 6 hours:
    binwidth = 6) +
  # Adapt scale to better fit data
  scale_x_continuous(breaks = seq(0, 48, 6)) + 
  # Label x and y axes:
  labs(x = "Incubation period in 6-hour bins",
       y = "Number of cases")

# Print plot:
incplot


# Create a vector with sequences every 6h from the first to the last case
breaks_6h <- seq(from = min(cases$onset_datetime, na.rm = TRUE),
                 to = max(cases$onset_datetime, na.rm = TRUE),
                 by = "6 hours")

# Fetch cases data:
epicurve_datetime <- cases %>%  
  # Add factor onset_datetime to ggplot aesthetic:
  ggplot(
    mapping = aes(x = onset_datetime)) + 
  # Add geom_histogram:
  geom_histogram(
    # Apply the vector of requences created above
    breaks = breaks_6h) +
  # Adapt scale to data and adjust axis label angle:
  scale_x_datetime(
    date_breaks = "6 hours",
    labels = label_date_short()) +
  # Update x and y axis labels:
  labs(x = "Date and time of onset symptoms", 
       y = "Number of cases") +
  # Remove unnecessary grid lines:
  theme_bw()

# Print epicurve:
epicurve_datetime


epicurve_strata <- cases %>% 
  # Add factor onset_day to ggplot aesthetic:
  ggplot(
    mapping = aes(x = onset_datetime, fill = group)) + 
  # Add nicer fill colours:
  scale_fill_manual(values = c("darkred", "lightblue")) +
    # Add geom_histogram:
  geom_histogram(
    # Apply the vector of requences created above
    breaks = breaks_6h) +
  # Adjust x axis scales to a suitable unit:
  scale_x_datetime(
    date_breaks = "6 hours", 
    labels = label_date_short()) +
  # Update x and y axis labels:
  labs(x = "Date and time of onset", 
       y = "Number of cases", 
       fill = "Group", 
       title = "Epicurve of the outbreak, stratified by sex",
       subtitle = str_glue("Copenhagen, November 2006, N = {sum(copdata$case)}")) +
  # Stratify by sex:
  facet_wrap(facets = "sex",
             ncol = 2) +
  # Add theme:
  theme_bw()

# Print epicurve:
epicurve_strata 


copdata %>% 
  janitor::tabyl(case, group) %>% 
  adorn_totals() %>% 
  adorn_percentages() %>% 
  adorn_pct_formatting() 




copdata %>% 
  janitor::tabyl(case, sex) %>% 
  adorn_totals() %>% 
  adorn_percentages() %>% 
  adorn_pct_formatting() 




copdata <- copdata %>% 
  # Create age categories:
  mutate(age_cat = epikit::age_categories(
    # Name of age column:
    x = age, 
    # Define the age categories:
    breakers = c(0, 10, 16, 18, 20, 50, 70)
    )
  )


# Check age categories:
janitor::tabyl(copdata, age_cat)

# Pipe copdata:
agesex <- copdata %>% 
  # Filter for cases only:
  filter(case == TRUE) %>% 
  # Create age sex pyramid:
  apyramid::age_pyramid(
  # Specify column containing age categories:
    age_group = "age_cat",
    # Specify column containing sex:
    split_by = "sex", 
    # Don't show midpoint on the graph:
    show_midpoint = FALSE
    )

# Print plot:
agesex



# Create summary table:
tabsymptoms <- copdata %>% 
    # Select person characteristics to summarise:
  select(case, diarrhoea, bloody, vomiting,
             abdo, nausea, fever,headache, jointpain) %>% 
  # transform clinical symptoms to factors, so NA can be accounted properly in the table
  dplyr::mutate(
    across(.cols = c(diarrhoea, bloody, vomiting,
             abdo, nausea, fever,headache, jointpain), 
           .fns = ~as.factor(.))) %>%
  # Make NA a explicit level of factor variables
  dplyr::mutate(
    across(.cols = c(diarrhoea, bloody, vomiting,
           abdo, nausea, fever,headache, jointpain),
           .fns = ~forcats::fct_na_value_to_level(.))) %>% 
    
  # Create the summary table:
  gtsummary::tbl_summary(
    # Stratify by case:
    by = case, 
    # Calculate row percentages:
    percent = "column",
    # Create nice labels:
    label  = list(
      diarrhoea   ~ "Diarrhoea",                           
      bloody      ~ "Dysentary",
      vomiting    ~ "Vomiting",
      abdo        ~ "Abdominal pain",
      nausea      ~ "Nausea", 
      fever       ~ "Fever", 
      headache    ~ "Headache", 
      jointpain   ~ "Joint pain")
    
  ) %>% 
  
  # Add totals:
  add_overall() %>% 
  # Make variable names bold and italics:
  bold_labels() %>% 
  italicize_labels() %>% 
  # Modify header:
  modify_header(
    label = "**Characteristic**",
    stat_0 = "**Overall**\n **N** = {N}",
    stat_1 = "**Non-case**\n **N** = {n}",
    stat_2 = "**Case**\n **N** = {n}", 
    )

# Print the table:
tabsymptoms



# Create list of symptom variables:
symptoms <- c("diarrhoea", 
              "bloody", 
              "vomiting", 
              "abdo", 
              "nausea", 
              "fever", 
              "headache", 
              "jointpain")

# Create nice labels for case definition:
caselabs <- ggplot2::as_labeller(c(`FALSE` = "Non-case", 
                                   `TRUE` = "Case"))
# Select variables and cases:
symptom_bar <- copdata %>% 
  # Select symptom columns:
  select(case, c(all_of(symptoms))) %>%
  # Drop NAs:
  drop_na() %>% 
  # Reshape (pivot longer):
  pivot_longer(!case, 
               names_to = "Symptoms", 
               values_drop_na = TRUE) %>% 
  # Keep only TRUE values:
  filter(value == TRUE) %>% 
 
   # Group by symptoms and case:
  group_by(Symptoms, case) %>% 
  # Count for each symptom by case:
  dplyr::summarise(count = n()) %>% 
  # Create plot:
  ggplot(
    mapping = aes(
    # Order symptom bars so most common ones are ontop:
    x = reorder(Symptoms, desc(count), decreasing = TRUE), 
    y = count)) +
  # Display bars as proportions
  geom_bar(stat = "identity") +
  # Update x axis label:
  xlab("Symptoms") +
  # Update y axis label:
  ylab("Proportion of respondents") +
  # Flip plot on its side so symptom labels are clear:
  coord_flip() +
  # Facet the plot by (labelled) case:
  facet_wrap(facets = "case",
             labeller = caselabs,
             ncol = 2)

# Print plot:
symptom_bar



# Create table of case status:
total_ap <- tabyl(copdata, case) %>% 
 # Add row totals:
  adorn_totals(where = "row") %>% 
  # Add percentages with 1 digit after the decimal point:
  adorn_pct_formatting(digits = 1) %>% 
  # Filter to rows where case is TRUE:
  filter(case == TRUE) %>% 
  # Select the column percent:
  select(percent) %>% 
  # Extract (pull) the value from this cell:
  pull()

# Print result:
total_ap




# Table to calculate attack proportions:
attack_prop <- copdata %>% 
  # Select columns:
  select (case, class, group, sex) %>% 
  
  # Create table:
  tbl_summary(
    # Stratified by case
    by = case,
    # with row percentages
    percent = "row") %>%
  
  # Add totals:
  add_overall() %>%
  
  # Make variable names bold and italics:
  bold_labels() %>% 
  italicize_labels() %>% 
  
  # Modify header:
  modify_header(
    label = "**Characteristic**",
    stat_0 = "**Overall** **N** = {N}",
    stat_1 = "**Non-case** **N** = {n}",
    stat_2 = "**Case** **N** = {n}"
  )


# Print table:
attack_prop



# Import the raw data set:
copdata <- rio::import(here::here("data", "Copenhagen_clean2.rds"))


# Check if age overall follows a normal distribution:
shapiro.test(copdata$age)
# Can simply have a look at
hist(copdata$age)

# Looking only at the students:
students <- copdata %>% 
  filter(group == "student")
hist(students$age)


# Perform Wilcoxon rank sum test on age and sex:
wilcox.test(age ~ case, 
            data = copdata)


copdata %>% 
  select(sex, case) %>% 
  tbl_summary(by = case) %>% 
  add_p()



copdata %>% 
  select(class, case) %>% 
  tbl_summary(by = case) %>% 
  add_p()



copdata %>% 
  select(group, case) %>% 
  tbl_summary(by = case) %>% 
  add_p()



copdata %>% 
  select(sex, class, group, case) %>% 
  tbl_summary(by = case) %>% 
  add_p()



# You could use the EpiStats package for each food item
CS(copdata, "case", "tuna")
CS(copdata, "case", "shrimps")
CS(copdata, "case", "green")
CS(copdata, "case", "veal")
# And so one


# You can save time (and probably typos!) by creating a vector for food variables...
food_vars <- c("tuna", "shrimps", "green", "veal", 
                "pasta", "rocket", "sauce", "bread",
                "champagne", "beer", "redwine", "whitewine")

# ...and using EpiStats::CSTable() to run all variables together!
CSTable(copdata, "case", food_vars)


rr_tbl <- CSTable(copdata, "case", food_vars) %>% 
  as.data.frame() %>% 
  rownames_to_column() %>% 
  flextable() %>% 
   set_header_labels(
     values = c("Food Item",
                "Total exposed",     
               "Cases exposed", 
               "AR among exposed",    
               "Total unexposed",
               "Cases unexposed",
               "AR among unexposed",
               "RR",         
               "95% lower CI",             
               "95% upper CI",
               "p-value"))


# Binomial regression for RRs. 
# The outcome needs to be exponentiated so we can interpret it properly!
binom_pastaD <- glm(case ~ pastaD, data = copdata, 
             family = binomial(link = "log"))

# To get exponentiated:
binom_pastaD_exp <- glm(case ~ pastaD, data = copdata, 
                       family = binomial(link = "log")) %>% 
  tidy(exponentiate = TRUE, 
       conf.int = TRUE)

binom_pastaD_exp



# Let's get the results directly exponentiated
binom_vealD_exp <- glm(case ~ vealD, data = copdata, 
                       family = binomial(link = "log")) %>% 
  tidy(exponentiate = TRUE, 
       conf.int = TRUE)

binom_vealD_exp


# Let's get the results directly exponentiated
binom_champagneD_exp <- glm(case ~ champagneD, data = copdata, 
                       family = binomial(link = "log")) %>% 
  tidy(exponentiate = TRUE, 
       conf.int = TRUE)

binom_champagneD_exp


# Import the raw data set: 
copdata <- rio::import(here::here("data", "Copenhagen_clean2.rds"))


stratall <- copdata %>% 
  # Mutate across to convert cases to numeric:
  mutate(across(.cols = case, 
                .fns = ~ as.numeric(.)))

# Pass data to the csinter function:
pastastrata <- csinter(x = stratall, 
                       cases = "case", 
                       exposure = "veal", 
                       by = "pasta")

pastastrata


# Perform Wilcoxon rank sum test on pasta and veal:
wilcox.test(pasta ~ veal, 
            data = copdata)


# Pass data to the csinter function:
champstrata <- csinter(x = stratall, 
                       cases = "case", 
                       exposure = "champagne", 
                       by = "pasta")

