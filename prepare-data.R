library(readr)
library(dplyr)
library(ggplot2)
# install.packages("remotes")
# remotes::install_github("ropenscilabs/gendercodeR")
library(gendercodeR)

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

#Recode the factors in all variables so they can be used as figure labels

levels(survey$teach_qualitative) <- gsub("Yes, qualitative methods are the focus of one or more courses that I teach", "Yes - Qualitative methods are the focus of my classes", levels(survey$teach_qualitative))
levels(survey$teach_qualitative) <- gsub("Yes, qualitative methods form part of one or more larger courses that I teach", "Yes - Qualitative methods are part of my classes", levels(survey$teach_qualitative))
levels(survey$teach_qualitative) <- gsub("No", "No - I do not teach qualitative methods", levels(survey$teach_qualitative))

levels(survey$analytic_methods) <- gsub("Qualitative Comparative Analysis","Qual. Comp. Analysis", levels(survey$analytic_methods))
levels(survey$analytic_methods) <- gsub("Comparative Historical Methods","Comp. Hist. Methods", levels(survey$analytic_methods))
survey$analytic_methods <- gsub('Other \\(Separate methods by semicolon\\)', 'Other', survey$analytic_methods)

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
survey$fork3a_encourage_data <- gsub('Other \\(Separate methods by semicolon\\)', 'Other', survey$fork3a_encourage_data)

levels(survey$fork1_methods) <- gsub("Qualitative Comparative Analysis","Qual. Comp. Analysis", levels(survey$fork1_methods))
levels(survey$fork1_methods) <- gsub("Comparative Historical Methods","Comp. Hist. Methods", levels(survey$fork1_methods))
survey$fork1_methods <- gsub('Other \\(Separate methods by semicolon\\)', 'Other', survey$fork1_methods)

levels(survey$fork1_type_data) <- gsub("Data collected by another researcher", "Data collected by other researcher", levels(survey$fork1_type_data))
levels(survey$fork1_type_data) <- gsub("Data collected by students prior to the course", "Data collected by students prior to course", levels(survey$fork1_type_data))
levels(survey$fork1_type_data) <- gsub("Data collected by students as part of the course", "Data collected by students for course", levels(survey$fork1_type_data))
levels(survey$fork1_type_data) <- gsub("Other (please specificy)", "Other", levels(survey$fork1_type_data))

levels(survey$fork2_sources) <- gsub("I used data from a data repository (e.g., ICPSR, QDR)", "I used data from a data repository", levels(survey$fork2_sources))
levels(survey$fork2_sources) <- gsub("I used data from a textbook/e-resource page (e.g., SAGE Research Methods)", "I used data from a textbook/e-resource page", levels(survey$fork2_sources))
levels(survey$fork2_sources) <- gsub("Other (please specificy)", "Other", levels(survey$fork2_sources))

levels(survey$fork2_challenges_obtain) <- gsub("Other (please specificy)", "Other", levels(survey$fork2_challenges_obtain))

levels(survey$fork2_useful_methods) <- gsub("Qualitative Comparative Analysis","Qual. Comp. Analysis", levels(survey$fork2_useful_methods))
levels(survey$fork2_useful_methods) <- gsub("Comparative Historical Methods","Comp. Hist. Methods", levels(survey$fork2_useful_methods))
survey$fork2_useful_methods <- gsub('Other \\(Separate methods by semicolon\\)', 'Other', survey$fork2_useful_methods)

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
survey$fork3b_encourage_data <- gsub('Other \\(Separate methods by semicolon\\)', 'Other', survey$fork3b_encourage_data)

#Combine Forks 3a and 3b
#Note: Choices in questions fork3a_why_nodata and fork3b_why_nodata are worded slightly different (one mentions
#shared data, the other pre-existing data), thus we need to adjust factors before merging the two factors

survey$fork3comb_why_nodata <- as.factor(ifelse(!is.na(survey$fork3a_why_nodata),as.character(survey$fork3a_why_nodata),as.character(survey$fork3b_why_nodata)))

survey$fork3comb_data_future <- as.factor(ifelse(!is.na(survey$fork3a_data_future),as.character(survey$fork3a_data_future),as.character(survey$fork3b_data_future)))

survey$fork3comb_encourage_data <- as.factor(ifelse(!is.na(survey$fork3a_encourage_data),as.character(survey$fork3a_encourage_data),as.character(survey$fork3b_encourage_data)))

#Turn multiple choice variables into multiple binaries

survey <- transform(survey, qual_methods_focus = grepl("Yes - Qualitative methods are the focus of my classes", survey$teach_qualitative),
                            qual_methods_part = grepl("Yes - Qualitative methods are part of my classes", survey$teach_qualitative),
                            not_qual_instructor = grepl("No - I do not teach qualitative methods", survey$teach_qualitative))

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

survey <- transform(survey, fork1_data_yourself = grepl("Data that yourself collected", survey$fork1_type_data),
                            fork1_data_other_res = grepl("Data collected by other researcher", survey$fork1_type_data),
                            fork1_data_student_prior = grepl("Data collected by students prior to course", survey$fork1_type_data),
                            fork1_data_student_part = grepl("Data collected by students for course", survey$fork1_type_data),
                            fork1_other_source = grepl("Other", survey$fork1_type_data))

#fork 2

survey <- transform(survey, fork2_firstime_0_4 = grepl("0-4 years ago", survey$fork2_first_time),
                            fork2_firstime_5_9 = grepl("5-9 years ago", survey$fork2_first_time),
                            fork2_firstime_10 = grepl("At least 10 years ago", survey$fork2_first_time))

survey <- transform(survey, fork2_decrease = grepl("Decreasingly over time", survey$fork2_frequency),
                            fork2_same = grepl("About the same", survey$fork2_frequency),
                            fork2_increase = grepl("Increasingly over time", survey$fork2_frequency))

survey <- transform(survey, fork2_my_own = grepl("I used my own data", survey$fork2_sources),
                            fork2_colleague = grepl("I used data directly sent to me by a colleague", survey$fork2_sources),
                            fork2_repository = grepl("I used data from a data repository", survey$fork2_sources),
                            fork2_textbook = grepl("I used data from a textbook/e-resource page", survey$fork2_sources),
                            fork2_source_others = grepl("Others", survey$fork2_sources))

survey <- transform(survey, fork2_identify = grepl("Difficulty identifying useful data", survey$fork2_challenges_obtain),
                            fork2_access = grepl("Difficulty accessing the data", survey$fork2_challenges_obtain),
                            fork2_manage = grepl("Difficulty managing student access to data", survey$fork2_challenges_obtain),
                            fork2_obtain_other = grepl("Other", survey$fork2_challenges_obtain))


survey <- transform(survey, fork2_discourse_analysis = grepl("Discourse Analysis", survey$fork2_useful_methods),
                    fork2_thematic_analysis = grepl("Thematic Analysis", survey$fork2_useful_methods),
                    fork2_qual_com_analysis = grepl("Qual. Comp. Analysis", survey$fork2_useful_methods),
                    fork2_process_tracing = grepl("ProcessTracing", survey$fork2_useful_methods),
                    fork2_com_hist_met = grepl("Comp. Hist. Methods", survey$fork2_useful_methods),
                    fork2_ground_theory = grepl("Grounded Theory", survey$fork2_useful_methods),
                    fork2_phenomenology = grepl("Phenomenology", survey$fork2_useful_methods),
                    fork2_narrative_analysis = grepl("Narrative Analysis", survey$fork2_useful_methods),
                    fork2_framework_analysis = grepl("Framework Analysis", survey$fork2_useful_methods),
                    fork2_other_method = grepl("Other", survey$fork2_useful_methods))

survey <- transform(survey, fork2_strong_improve = grepl("Strongly improved", survey$fork2_effect),
                            fork2_somewhat_improve = grepl("Somewhat improved", survey$fork2_effect),
                            fork2_neither = grepl("Neither improved nor made worse", survey$fork2_effect),
                            fork2_somewhat_worse = grepl("Made somewhat worse", survey$fork2_effect),
                            fork2_much_worse = grepl("made much worse", survey$fork2_effect))

#fork 3
survey <- transform(survey, fork3_not_effective = grepl("Not effective for my course", survey$fork3comb_why_nodata),
                            fork3_notsure_use = grepl("Not sure how to use", survey$fork3comb_why_nodata),
                            fork3_unlikely_find = grepl("Unlikely to find suitable data", survey$fork3comb_why_nodata),
                            fork3_couldnt_find = grepl("Could not find suitable data", survey$fork3comb_why_nodata))

survey <- transform(survey, fork3_plan_use = grepl("Plan to use it", survey$fork3comb_data_future),
                            fork3_strong_cons = grepl("Strongly consider it", survey$fork3comb_data_future),
                            fork3_cons = grepl("Consider it", survey$fork3comb_data_future),
                            fork3_cons_skep = grepl("Consider it, but skeptical", survey$fork3comb_data_future),
                            fork3_not_use = grepl("Not going to", survey$fork3comb_data_future))

survey <- transform(survey, fork3_someone_help = grepl("Someone teaching/helping me to do so", survey$fork3comb_encourage_data),
                            fork3_data_method = grepl("Easily available data for teaching method", survey$fork3comb_encourage_data),
                            fork3_data_topic = grepl("Easily available data on a specific topic", survey$fork3comb_encourage_data),
                            fork3_prep_lesson = grepl("Prepared lessons for teaching method with shared data", survey$fork3comb_encourage_data),
                            fork3_other_enc = grepl("Other", survey$fork3comb_encourage_data))

#final questions

survey <- transform(survey, age_25_34 = grepl("25-34 years old", survey$age),
                            age_35_44 = grepl("35-44 years old", survey$age),
                            age_45_54 = grepl("45-54 years old", survey$age),
                            age_55_64 = grepl("55-64 years old", survey$age),
                            age_65_older = grepl("65 and older", survey$age))

survey <- transform(survey, ethnicity_asian = grepl("Asian or Pacific Islander", survey$ethnicity),
                            ethnicity_black = grepl("Black or African American", survey$ethnicity),
                            ethnicity_hispanic = grepl("Hispanic or Latino",survey$ethnicity),
                            ethnicity_native = grepl("Native American or Alakan Native",survey$ethnicity),
                            ethnicity_white = grepl("White or Caucasian", survey$ethnicity),
                            ethnicity_multi = grepl("Multiracial or Biracial", survey$ethnicity),
                            ethnicity_not_listed = grepl("Not listed here", survey$ethnicity))

####################
#Summary Statistics#
####################


#General information: consent given by 258 respondents
summary(survey$consent)
#but only 240 gave consent for data sharing (13 no's and 5 n/a's)
summary(survey$consent_datasharing)

#The majority of respondents teach courses focused on qualitative methods (186);
#44 respondents teach courses with components of qualitative methods;
#18 respondens do not teach qualitative methods
#10 N/As
summary(survey$teach_qualitative)

#Getting an idea about which courses were taught
summary(survey$courses_taught)

#Analytic Methods
summary(survey$analytic_methods)

summary(survey$fork1_thematic_analysis)
summary(survey$fork1_qual_com_analysis)
summary(survey$fork1_process_tracing)
summary(survey$fork1_com_hist_met)
summary(survey$fork1_phenomenology)
summary(survey$fork1_narrative_analysis)
summary(survey$fork1_case_study)
summary(survey$fork1_ground_theory)
summary(survey$fork1_framework_analysis)
summary(survey$fork1_other_method)

#Question about whether respondents have used shared data to teach any of the above courses
summary(survey$used_data)
#32 NO's, 180 YES's, 46 N/A's

#Fork 1

#Analytic Methods
summary(survey$fork1_discourse_analysis)
summary(survey$fork1_thematic_analysis)
summary(survey$fork1_qual_com_analysis)
summary(survey$fork1_process_tracing)
summary(survey$fork1_com_hist_met)
summary(survey$fork1_ground_theory)
summary(survey$fork1_case_study)
summary(survey$fork1_phenomenology)
summary(survey$fork1_narrative_analysis)
summary(survey$fork1_framework_analysis)
summary(survey$fork1_other_method)

#Respondents who answered YES are directed to Fork 1
#The second question in Fork asks them  what type of data they used to teach qualitative analytic methods
summary(survey$fork1_data_yourself)
summary(survey$fork1_data_other_res)
summary(survey$fork1_data_student_prior)
summary(survey$fork1_data_student_part)
summary(survey$fork1_other_source)

#Fork2

#When was the first time you used “shared data”

summary(survey$fork2_firstime_0_4)
summary(survey$fork2_firstime_5_9)
summary(survey$fork2_firstime_10)

#Have you used shared data to teach qualitative analytic methods more or less over time”

summary(survey$fork2_decrease)
summary(survey$fork2_same)
summary(survey$fork2_increase)

#From where did you source the shared data that you used
summary (survey$fork2_my_own)
summary (survey$fork2_colleague)
summary (survey$fork2_repository)
summary (survey$fork2_textbook)
summary (survey$fork2_source_others)

#Did you experience any challenges in obtaining the data?
summary (survey$fork2_identify)
summary (survey$fork2_access)
summary (survey$fork2_manage)
summary (survey$fork2_obtain_other)

#Which qualitative analytic methods have you found it most useful to use shared data to teach?

summary(survey$fork2_discourse_analysis)
summary(survey$fork2_thematic_analysis)
summary(survey$fork2_qual_com_analysis)
summary(survey$fork2_process_tracing)
summary(survey$fork2_com_hist_met)
summary(survey$fork2_ground_theory)
summary(survey$fork2_phenomenology)
summary(survey$fork2_narrative_analysis)
summary(survey$fork2_framework_analysis)
summary(survey$fork2_other_method)

#What effect do you believe the integration of shared data had on student  learning in your course(s) on qualitative analytic methods?

summary(survey$fork2_strong_improve)
summary(survey$fork2_somewhat_improve)
summary(survey$fork2_neither)
summary(survey$fork2_somewhat_worse)
summary(survey$fork2_much_worse)

#Fork 3

#Why have you not used shared data to teach qualitative analytic methods?
summary (survey$fork3_not_effective)
summary (survey$fork3_notsure_use)
summary (survey$fork3_unlikely_find)
summary (survey$fork3_couldnt_find)


#Would you consider using shared data to teach qualitative analytic methods in the future
summary (survey$fork3_plan_use)
summary (survey$fork3_strong_cons)
summary (survey$fork3_cons)
summary (survey$fork3_cons_skep)
summary (survey$fork3_not_use)

#What would encourage and facilitate your use of shared data to teach qualitative analytic methods in the future

summary (survey$fork3_someone_help)
summary (survey$fork3_data_method)
summary (survey$fork3_data_topic)
summary (survey$fork3_prep_lesson)
summary (survey$fork3_other_enc)

#General Questions

#age
summary(survey$age_25_34)
summary(survey$age_35_44)
summary(survey$age_45_54)
summary(survey$age_55_64)
summary(survey$age_65_older)

#ethnicity

summary(survey$ethnicity_asian)
summary(survey$ethnicity_black)
summary(survey$ethnicity_hispanic)
summary(survey$ethnicity_native)
summary(survey$ethnicity_white)
summary(survey$ethnicity_multi)
summary(survey$ethnicity_not_listed)

#recode and summarize gender
survey <- survey %>% mutate(recoded_gender = recode_gender(gender = gender, dictionary = broad))
survey$recoded_gender <- as.factor (survey$recoded_gender)
levels(survey$recoded_gender) <- gsub("cis female", "female", levels(survey$recoded_gender))
levels(survey$recoded_gender) <- gsub("cis male", "male", levels(survey$recoded_gender))
levels(survey$recoded_gender) <- gsub("transgender", "Other responses", levels(survey$recoded_gender))
summary(survey$recoded_gender)

#Graphs

#Number of respondents who teach qualitative methods
ggplot(survey, aes(x = teach_qualitative)) +
  geom_bar() + scale_x_discrete(labels = c("Do Not Teach", "Major Focus", "Partial Focus", "N/A")) + ggtitle("Have you taught a graduate-level course focused on, or including material on, qualitative methods?") + ylab("Number of Respondents Teaching Qualitative Methods") + theme(axis.title.x=element_blank())

#Number of respondents who have used shared data
ggplot(survey, aes(x = used_data)) +
  geom_bar() + ggtitle("Have you used data to teach any of the qualitative analytic methods you just mentioned?") + ylab("Number of Respondents") + theme(axis.title.x=element_blank())

