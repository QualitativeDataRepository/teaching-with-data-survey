library(readr)
library(dplyr)

# Read in the survey -- may need to adjust the filename
survey <- read_csv("data/survey data_final.csv")

# remove first two rows which have metadata
survey <- survey[-(1:2),]

# remove preview replies
survey <- survey %>% filter(!DistributionChannel == "preview")

# remove PII
# We might want to remove additional unnecessary columns here
survey <- survey %>% select(-c(Status, IPAddress, RecipientLastName, RecipientFirstName, RecipientEmail, LocationLatitude, LocationLongitude))

# Q9 should be Q09 for sorting
survey <- survey %>% rename(Q09 = Q9, Q01 = Q1)

# Sort columns (we may not want this -- looks like they're in the right order)
# survey <- survey[, order(names(survey))]

# Name variables
# to identify each fork, variables that are specific to a certain fork start
# fork id
survey <- survey %>% rename(
  consent = Q01,
  consent_datasharing = Q11,
  teach_qualitative = Q14,
  courses_taught = Q15,
  analytic_methods = Q16,
  analytic_methods_open = Q16_14_TEXT,
  used_data = Q17,
  #question used_data directs the respondent to either fork 1 or fork3. there are two "fork 3"s.
  #the first follows from the answer NO in the question used_data
  #first fork 3 (answered NO in used_data)
  fork3a_why_nodata = Q40,
  fork3a_why_nodata_open = Q40_5_TEXT,
  fork3a_data_future = Q41,
  fork3a_encourage_data = Q55,
  fork3a_encourage_data_open = Q55_5_TEXT,
  fork3a_thoughts_data = Q42,
  #fork 1 (answered YES in used_data)
  fork1_courses = Q09,
  fork1_methods = Q53,
  fork1_methods_open = Q53_4_TEXT,
  fork1_methods_open_2 = Q53_18_TEXT,
  fork1_type_data = Q11_1,
  fork1_type_data_open = Q11_5_TEXT,
  #question fork1_type_data can lead the respodent to forks 2 or the second fork 3
  #fork 2 (answered "data you yourself collected prior to the course", "data collected by another
  #resercher prior to the course" or "other" in question fork1_type_data and question fork1_type_data_open)
  fork2_first_time = Q12,
  fork2_motivation = Q13,
  fork2_frequency = Q14_1,
  fork2_examples = Q15_1,
  fork2_sources = Q16_1,
  fork2_sources_open = Q16_5_TEXT,
  fork2_challenges_obtain = Q17_1,
  fork2_challenges_obtain_open = Q17_4_TEXT,
  fork2_useful_methods = Q18,
  fork2_useful_methods_open = Q18_7_TEXT,
  fork2_useful_methods_open_a = Q18_21_TEXT,
  fork2_description_use = Q38,
  fork2_effect = Q20,
  fork2_useful = Q21,
  fork2_challenges = Q22,
  #second fork 3 (answered "data collected by students prior to the course" or "data collected
  #by stydents as part of the course" in question question fork1_type_data)
  fork3b_why_nodata = Q23,
  fork3b_why_nodata_2 = Q23_5_TEXT,
  fork3b_data_future = Q24,
  fork3b_encourage_data = Q54,
  fork3b_encourage_data_open = Q54_5_TEXT,
  fork3b_thoughts_data = Q25,
  #final questions answered by all respondents
  syllabi = Q27,
  permission_syllabi = Q29,
  age = Q49,
  gender = Q50,
  ethnicity = Q51,
  interview = Q30
)

# Turn strings into factors
survey$consent <- as.factor(survey$consent)
survey$consent_datasharing <- as.factor(survey$consent_datasharing)
survey$teach_qualitative <- as.factor(survey$teach_qualitative)
survey$courses_taught <- as.factor(survey$courses_taught)
survey$analytic_methods <- as.factor(survey$analytic_methods)
survey$used_data <- as.factor (survey$used_data)
survey$fork3a_why_nodata <- as.factor (survey$fork3a_why_nodata)
survey$fork3a_data_future <- as.factor (survey$fork3a_data_future)
survey$fork3a_encourage_data <- as.factor (survey$fork3a_encourage_data)
survey$fork3a_thoughts_data <- as.factor (survey$fork3a_thoughts_data)
survey$fork1_courses <- as.factor (survey$fork1_courses)
survey$fork1_methods <- as.factor (survey$fork1_methods)
survey$fork1_type_data <- as.factor (survey$fork1_type_data)
survey$fork2_first_time <- as.factor (survey$fork2_first_time)
survey$fork2_motivation <- as.factor (survey$fork2_motivation)
survey$fork2_frequency <- as.factor (survey$fork2_frequency)
survey$fork2_examples <- as.factor (survey$fork2_examples)
survey$fork2_sources <- as.factor (survey$fork2_sources)
survey$fork2_challenges_obtain <- as.factor (survey$fork2_challenges_obtain)
survey$fork2_usefulmethods <- as.factor (survey$fork2_useful_methods)
survey$fork2_description_use <- as.factor (survey$fork2_description_use)
survey$fork2_effect <- as.factor (survey$fork2_effect)
survey$fork2_useful <- as.factor (survey$fork2_useful)
survey$fork2_challenges <- as.factor (survey$fork2_challenges)
survey$fork3b_why_nodata <- as.factor(survey$fork3b_why_nodata)
survey$fork3b_data_future <- as.factor (survey$fork3b_data_future)
survey$fork3b_encourage_data <- as.factor (survey$fork3b_encourage_data)
survey$fork3b_thoughts_data <- as.factor (survey$fork3b_thoughts_data)
survey$syllabi <- as.factor (survey$syllabi)
survey$permission_syllabi <- as.factor (survey$permission_syllabi)
survey$age <- as.factor (survey$age)
survey$gender <- as.factor (survey$gender)
survey$ethnicity <- as.factor (survey$ethnicity)
survey$interview <- as.factor (survey$interview)

