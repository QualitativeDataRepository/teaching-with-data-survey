
# Load packages -----------------------------------------------------------

library(readr)
library(dplyr)
library(ggplot2)
# install.packages("remotes")
# remotes::install_github("ropenscilabs/gendercodeR")
library(gendercodeR)

# Read Data ---------------------------------------------------------------

# Read in the survey -- may need to adjust the filename
survey <- read_csv("data/survey data_final.csv")

# Clean Data --------------------------------------------------------------

# remove first two rows which have metadata
survey <- survey[-(1:2),]

# remove preview replies
survey <- survey %>% filter(!DistributionChannel == "preview")

# remove PII
# We might want to remove additional unnecessary columns here
survey <- survey %>% select(-c(Status, IPAddress, RecipientLastName, RecipientFirstName, RecipientEmail, LocationLatitude, LocationLongitude))

# recode courses
recode_coursename <- read_csv("data/recode_courses.csv")
for (row in 1:nrow(recode_coursename)){
  course <- as.character(recode_coursename[row, "original_courses"])
  recoded_course  <- as.character(recode_coursename[row, "recoded_courses"])
  print(course)
  print(recoded_course)
  survey$Q15 <- gsub(course, recoded_course, survey$Q15, fixed = TRUE, ignore.case = TRUE)
}

# Q9 should be Q09 for sorting
survey <- survey %>% rename(Q09 = Q9, Q01 = Q1)

# Sort columns (we may not want this -- looks like they're in the right order)
# survey <- survey[, order(names(survey))]


# Rename Variables --------------------------------------------------------

# Name variables
# to identify each fork, variables that are specific to a certain fork start
# fork id
survey <- survey %>% rename(
  consent = Q01,
  consent_datasharing = Q11,
  teach_qualitative = Q14,
  discipline = Discipline,
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
  #question fork1_type_data can lead the respondent to forks 2 or the second fork 3
  #fork 2 (answered "data you yourself collected prior to the course", "data collected by another
  #researcher prior to the course" or "other" in question fork1_type_data and question fork1_type_data_open)
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


# Turn Strings Into Factors -----------------------------------------------

# Turn strings into factors
survey$consent <- as.factor(survey$consent)
survey$consent_datasharing <- as.factor(survey$consent_datasharing)
survey$teach_qualitative <- as.factor(survey$teach_qualitative)
survey$courses_taught <- as.factor(survey$courses_taught)
survey$discipline <- as.factor(survey$discipline)
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


# Recode Factors ----------------------------------------------------------


#Recode the factors in all variables so they can be used as figure labels

levels(survey$teach_qualitative) <- gsub("Yes, qualitative methods are the focus of one or more courses that I teach", "Yes - Qualitative methods are the focus of my classes", levels(survey$teach_qualitative))
levels(survey$teach_qualitative) <- gsub("Yes, qualitative methods form part of one or more larger courses that I teach", "Yes - Qualitative methods are part of my classes", levels(survey$teach_qualitative))
levels(survey$teach_qualitative) <- gsub("No", "No - I do not teach qualitative methods", levels(survey$teach_qualitative))

levels(survey$analytic_methods) <- gsub("Qualitative Comparative Analysis","Qual. Comp. Analysis", levels(survey$analytic_methods))
levels(survey$analytic_methods) <- gsub("Comparative Historical Methods","Comp. Hist. Methods", levels(survey$analytic_methods))
levels(survey$analytic_methods) <- gsub('Other \\(Separate methods by semicolon\\)', 'Other', levels(survey$analytic_methods))

levels(survey$used_data) <- gsub("Yes", "Yes - I have used data", levels(survey$used_data))
levels(survey$used_data) <- gsub("No", "No - I have not used data", levels(survey$used_data))

levels(survey$fork3a_why_nodata) <- gsub("I don’t believe doing so would be effective for my course", "Not effective for my course", levels(survey$fork3a_why_nodata))
levels(survey$fork3a_why_nodata) <- gsub("I am not sure how to use shared data effectively in instruction", "Not sure how to use", levels(survey$fork3a_why_nodata))
levels(survey$fork3a_why_nodata) <- gsub("I do not think I could find suitable data for this purpose.", "Unlikely to find suitable data", levels(survey$fork3a_why_nodata))
levels(survey$fork3a_why_nodata) <- gsub("I have not been able to find suitable data for this purpose", "Could not find suitable data", levels(survey$fork3a_why_nodata))

levels(survey$fork3a_data_future) <- gsub("I am definitely planning to", "Plan to use it", levels (survey$fork3a_data_future))
levels(survey$fork3a_data_future) <- gsub("I would strongly consider it", "Strongly consider it", levels (survey$fork3a_data_future))
levels(survey$fork3a_data_future) <- gsub("I would consider it", "Consider it", levels (survey$fork3a_data_future))
levels(survey$fork3a_data_future) <- gsub("I would consider it but am skeptical", "Consider it, but skeptical", levels (survey$fork3a_data_future))
levels(survey$fork3a_data_future) <- gsub("I am almost certainly not going to", "Not going to", levels (survey$fork3a_data_future))

levels(survey$fork3a_encourage_data) <- gsub("Easily available data specifically prepared for teaching a specific method", "Easily available data for teaching method", levels(survey$fork3a_encourage_data))
levels(survey$fork3a_encourage_data) <- gsub("Prepared lessons/lesson plans for teaching a specific method based on shared data", "Prepared lessons for teaching method with shared data", levels(survey$fork3a_encourage_data))
levels(survey$fork3a_encourage_data) <- gsub('Other \\(Separate methods by semicolon\\)', 'Other',  levels(survey$fork3a_encourage_data))

levels(survey$fork1_methods) <- gsub("Qualitative Comparative Analysis","Qual. Comp. Analysis", levels(survey$fork1_methods))
levels(survey$fork1_methods) <- gsub("Comparative Historical Methods","Comp. Hist. Methods", levels(survey$fork1_methods))
levels(survey$fork1_methods) <- gsub("Other \\(Separate methods by semicolon\\)", "Other", levels(survey$fork1_methods))

levels(survey$fork1_type_data) <- gsub("Data that you yourself collected", "Data you yourself collected", levels(survey$fork1_type_data))
levels(survey$fork1_type_data) <- gsub("Data collected by another researcher prior to the course", "Data collected by other researcher", levels(survey$fork1_type_data))
levels(survey$fork1_type_data) <- gsub("Data collected by students prior to the course", "Data collected by students prior to course", levels(survey$fork1_type_data))
levels(survey$fork1_type_data) <- gsub("Data collected by students as part of the course", "Data collected by students for course", levels(survey$fork1_type_data))
levels(survey$fork1_type_data) <- gsub('Other \\(please specificy\\)', 'Other', levels(survey$fork1_type_data))

levels(survey$fork2_sources) <- gsub("I used data from a data repository (e.g., ICPSR, QDR)", "I used data from a data repository", levels(survey$fork2_sources))
levels(survey$fork2_sources) <- gsub("I used data from a textbook/e-resource page (e.g., SAGE Research Methods)", "I used data from a textbook/e-resource page", levels(survey$fork2_sources))
levels(survey$fork2_sources) <- gsub("Other (please specificy)", "Other", levels(survey$fork2_sources))

levels(survey$fork2_challenges_obtain) <- gsub("Other (please specificy)", "Other", levels(survey$fork2_challenges_obtain))

levels(survey$fork2_useful_methods) <- gsub("Qualitative Comparative Analysis","Qual. Comp. Analysis", levels(survey$fork2_useful_methods))
levels(survey$fork2_useful_methods) <- gsub("Comparative Historical Methods","Comp. Hist. Methods", levels(survey$fork2_useful_methods))
levels(survey$fork2_useful_methods <- gsub('Other \\(Separate methods by semicolon\\)', 'Other', survey$fork2_useful_methods))

levels(survey$fork3b_why_nodata) <- gsub("I don’t believe doing so would be effective for my course", "Not effective for my course", levels(survey$fork3b_why_nodata))
levels(survey$fork3b_why_nodata) <- gsub("I am not sure how to use pre-existing data effectively in instruction", "Not sure how to use", levels(survey$fork3b_why_nodata))
levels(survey$fork3b_why_nodata) <- gsub("I do not think I could find suitable data for this purpose.", "Unlikely to find suitable data", levels(survey$fork3b_why_nodata))
levels(survey$fork3b_why_nodata) <- gsub("I have not been able to find suitable data for this purpose", "Could not find suitable data", levels(survey$fork3b_why_nodata))

levels(survey$fork3b_data_future) <- gsub("I am definitely planning to", "Plan to use it", levels (survey$fork3b_data_future))
levels(survey$fork3b_data_future) <- gsub("I would strongly consider it", "Strongly consider it", levels (survey$fork3b_data_future))
levels(survey$fork3b_data_future) <- gsub("I would consider it", "Consider it", levels (survey$fork3b_data_future))
levels(survey$fork3b_data_future) <- gsub("I would consider it but am skeptical", "Consider it, but skeptical", levels (survey$fork3b_data_future))
levels(survey$fork3b_data_future) <- gsub("I am almost certainly not going to", "Not going to", levels (survey$fork3b_data_future))

levels(survey$fork3b_encourage_data) <- gsub("Easily available data specifically prepared for teaching a specific method", "Easily available data for teaching method", levels(survey$fork3b_encourage_data))
levels(survey$fork3b_encourage_data) <- gsub("Prepared lessons/lesson plans for teaching a specific method based on shared data", "Prepared lessons for teaching method with shared data", levels(survey$fork3b_encourage_data))
levels(survey$fork3b_encourage_data <- gsub('Other \\(Separate methods by semicolon\\)', 'Other', survey$fork3b_encourage_data))


# Combine Forks 3a and 3b -------------------------------------------------


#Combine Forks 3a and 3b
#Note: Choices in questions fork3a_why_nodata and fork3b_why_nodata are worded slightly different (one mentions
#shared data, the other pre-existing data), thus we need to adjust factors before merging the two factors

survey$fork3comb_why_nodata <- as.factor(ifelse(!is.na(survey$fork3a_why_nodata),as.character(survey$fork3a_why_nodata),as.character(survey$fork3b_why_nodata)))

survey$fork3comb_data_future <- as.factor(ifelse(!is.na(survey$fork3a_data_future),as.character(survey$fork3a_data_future),as.character(survey$fork3b_data_future)))

survey$fork3comb_encourage_data <- as.factor(ifelse(!is.na(survey$fork3a_encourage_data),as.character(survey$fork3a_encourage_data),as.character(survey$fork3b_encourage_data)))


# Turn Multiple Choice Variables Into Binary ------------------------------

#Turn multiple choice variables into multiple binaries

survey <- transform(survey, discourse_analysis = grepl("Discourse Analysis", survey$analytic_methods),
                            thematic_analysis = grepl("Thematic Analysis", survey$analytic_methods),
                            qual_com_analysis = grepl("Qual. Comp. Analysis", survey$analytic_methods),
                            process_tracing = grepl("ProcessTracing", survey$analytic_methods),
                            com_hist_met = grepl("Comp. Hist. Methods", survey$analytic_methods),
                            ground_theory = grepl("Grounded Theory", survey$analytic_methods),
                            case_study = grepl("Case Study Methods", survey$analytic_methods),
                            phenomenology = grepl("Phenomenology", survey$analytic_methods),
                            narrative_analysis = grepl("Narrative Analysis", survey$analytic_methods),
                            framework_analysis = grepl("Framework Analysis", survey$analytic_methods),
                            other_method = grepl("Other", survey$analytic_methods))

#fork1

survey <- transform(survey, fork1_discourse_analysis = grepl("Discourse Analysis", survey$fork1_methods),
                            fork1_thematic_analysis = grepl("Thematic Analysis", survey$fork1_methods),
                            fork1_qual_com_analysis = grepl("Qual. Comp. Analysis", survey$fork1_methods),
                            fork1_process_tracing = grepl("ProcessTracing", survey$fork1_methods),
                            fork1_com_hist_met = grepl("Comp. Hist. Methods", survey$fork1_methods),
                            fork1_ground_theory = grepl("Grounded Theory", survey$fork1_methods),
                            fork1_case_study = grepl("Case Study Methods", survey$fork1_methods),
                            fork1_phenomenology = grepl("Phenomenology", survey$fork1_methods),
                            fork1_narrative_analysis = grepl("Narrative Analysis", survey$fork1_methods),
                            fork1_framework_analysis = grepl("Framework Analysis", survey$fork1_methods),
                            fork1_other_method = grepl("Other", survey$fork1_methods))

survey <- transform(survey, fork1_data_yourself = grepl("Data you yourself collected", survey$fork1_type_data),
                            fork1_data_other_res = grepl("Data collected by other researcher", survey$fork1_type_data),
                            fork1_data_student_prior = grepl("Data collected by students prior to course", survey$fork1_type_data),
                            fork1_data_student_part = grepl("Data collected by students for course", survey$fork1_type_data),
                            fork1_other_source = grepl("Other", survey$fork1_type_data))

#fork 2

survey <- transform(survey, fork2_my_own = grepl("I used my own data", survey$fork2_sources),
                            fork2_colleague = grepl("I used data directly sent to me by a colleague", survey$fork2_sources),
                            fork2_repository = grepl("I used data from a data repository", survey$fork2_sources),
                            fork2_textbook = grepl("I used data from a textbook/e-resource page", survey$fork2_sources),
                            fork2_source_others = grepl("Others", survey$fork2_sources))

survey <- transform(survey, fork2_identify = grepl("Difficulty identifying useful data", survey$fork2_challenges_obtain),
                            fork2_access = grepl("Difficulty accessing the data", survey$fork2_challenges_obtain),
                            fork2_manage = grepl("Difficulty managing student access to data", survey$fork2_challenges_obtain),
                            fork2_obtain_other = grepl("Other", survey$fork2_challenges_obtain),
                            fork2_no_challenge = grepl("None", survey$fork2_challenges_obtain))

survey <- transform(survey, fork2_discourse_analysis = grepl("Discourse Analysis", survey$fork2_useful_methods),
                    fork2_casestudy = grepl("Case Study", survey$fork2_useful_methods),
                    fork2_thematic_analysis = grepl("Thematic Analysis", survey$fork2_useful_methods),
                    fork2_qual_com_analysis = grepl("Qual. Comp. Analysis", survey$fork2_useful_methods),
                    fork2_process_tracing = grepl("ProcessTracing", survey$fork2_useful_methods),
                    fork2_com_hist_met = grepl("Comp. Hist. Methods", survey$fork2_useful_methods),
                    fork2_ground_theory = grepl("Grounded Theory", survey$fork2_useful_methods),
                    fork2_phenomenology = grepl("Phenomenology", survey$fork2_useful_methods),
                    fork2_narrative_analysis = grepl("Narrative Analysis", survey$fork2_useful_methods),
                    fork2_framework_analysis = grepl("Framework Analysis", survey$fork2_useful_methods),
                    fork2_other_method = grepl("Other", survey$fork2_useful_methods))

#fork 3
survey <- transform(survey, fork3_not_effective = grepl("Not effective for my course", survey$fork3comb_why_nodata),
                            fork3_notsure_use = grepl("Not sure how to use", survey$fork3comb_why_nodata),
                            fork3_unlikely_find = grepl("Unlikely to find suitable data", survey$fork3comb_why_nodata),
                            fork3_couldnt_find = grepl("Could not find suitable data", survey$fork3comb_why_nodata))

survey <- transform(survey, fork3_someone_help = grepl("Someone teaching/helping me to do so", survey$fork3comb_encourage_data),
                            fork3_data_method = grepl("Easily available data for teaching method", survey$fork3comb_encourage_data),
                            fork3_data_topic = grepl("Easily available data on a specific topic", survey$fork3comb_encourage_data),
                            fork3_prep_lesson = grepl("Prepared lessons for teaching method with shared data", survey$fork3comb_encourage_data),
                            fork3_other_enc = grepl("Other", survey$fork3comb_encourage_data))
