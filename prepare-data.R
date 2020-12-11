library(readr)
library(dplyr)
library(ggplot2)

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
levels(survey$fork1_type_data) <- gsub("Other (please specificy", "Other", levels(survey$fork1_type_data))

levels(survey$fork2_sources) <- gsub("I used data from a data repository (e.g., ICPSR, QDR)", "I used data from a data repository", levels(survey$fork2_sources))
levels(survey$fork2_sources) <- gsub("I used data from a textbook/e-resource page (e.g., SAGE Research Methods)", "I used data from a textbook/e-resource page", levels(survey$fork2_sources))
levels(survey$fork2_sources) <- gsub("Other (please specificy", "Other", levels(survey$fork2_sources))

levels(survey$fork2_challenges_obtain) <- gsub("Other (please specificy", "Other", levels(survey$fork2_challenges_obtain))

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
#shared data, the other pre-existing data), thus we need to adjust factors before merginig the two factors

survey$fork3comb_why_nodata <- as.factor(ifelse(!is.na(survey$fork3a_why_nodata),as.character(survey$fork3a_why_nodata),as.character(survey$fork3b_why_nodata)))

#Turn multiple choice variables into multiple binaries

survey <- transform(survey, isDiscourseAnalysis = grepl("Discourse Analysis", survey$courses_taught),
                            isThematicAnalysis = grepl("Thematic Analysis", survey$courses_taught),
                            isQualitativeComparativeAnalysis = grepl("Qualitative Comparative Analysis", survey$courses_taught),
                            isProcessTracing = grepl("ProcessTracing", survey$courses_taught),
                            isComparativeHistoricalMethods = grepl("Comparative Historical Methods", survey$courses_taught),
                            isGroundTheory = grepl("Grounded Theory", survey$courses_taught),
                            isPhenomenology = grepl("Phenomenology", survey$courses_taught),
                            isNarrativeAnalysis = grepl("Narrative Analysis", survey$courses_taught),
                            isFrameworkAnalysis = grepl("Framework Analysis", survey$courses_taught))

survey <- transform(survey, isDatathatyourselfcollected = grepl("Data that yourself collected", survey$fork1_type_data),
                            isDatacollectedbyanotherresearcher = grepl("Data collected by another researcher", survey$fork1_type_data),
                            isDatacollectedbystudentspriortothecourse = grepl("Data collected by students prior to the course", survey$fork1_type_data),
                            isDatacollectedbystudentsaspartofthecourse = grepl("Data collected by students as part of the course", survey$fork1_type_data))


survey$isDatathatyourselfcollected <- as.factor (survey$isDatathatyourselfcollected)
summary(survey$isDatathatyourselfcollected)

survey$isDatacollectedbyanotherresearcher <- as.factor (survey$isDatacollectedbyanotherresearcher)
summary(survey$isDatacollectedbyanotherresearcher)


####################
#Summary Statistics#
####################

#Initial Questions#

#consent given by 258 respondents
summary(survey$consent)
#but only 240 gave consent for data sharing (13 no's and 5 n/a's)
summary(survey$consent_datasharing)
#the majority of respondents teach courses focused on qualitative methods (186);
#44 respondents teach courses with components of qualitative methods;
#18 respondens do not teach qualitative methods
summary(survey$teach_qualitative)
summary(survey$analytic_methods)
#most common answer here is "others", followed by "Thematic Analysis,Qualitative Comparative Analysis,Case Study Methods,Grounded Theory,Phenomenology,Narrative Analysis
summary(survey$analytic_methods_open)
summary(survey$used_data)
#32 NO's, 180 YES's, 45 N/A's

#fork 3a
summary(survey$fork3a_why_nodata)
#most respondents did not answer this question (226); 7 of them don't believe doing so would be effective for their course
summary(survey$fork3a_data_future)
#again, most respondents did not answer this question (227)
summary(survey$fork3a_encourage_data)
#again, most respondents did not answer this question (227)

#fork1
summary(survey$fork1_type_data)
#This is a bit of a mess as most respondents chose a combination of answers.

#fork2
summary(survey$fork2_first_time)
#again, 124 NAs here. Apart from that, most respondents used shared data for the first time 0-4 years ago
summary(survey$fork2_frequency)
#125 NAs here. Apart from that, 78 respondents answered "About the same" to whether they're using shared data more or less over time
summary(survey$fork2_sources)
#a combination of responses here. 125 NAs
summary(survey$fork2_challenges_obtain)
#82 respondents answered "none" to the challenges they faced to obtain shared data; 138 NAs.
summary(survey$fork2_effect)
#81 respondents said shared data strongly improved their courses (only 7 said it neither improved nor made it worse). 131 NAs.

#fork3b

summary(survey$fork3b_why_nodata)
#229 NAs. 13 respondents marked "other", and other answers are distributed across alternatives
summary(survey$fork3b_data_future)
#again, 230 NAs. Same pattern as above.
summary(survey$fork3b_encourage_data)
#232 NAs. As it happened with fork3a, most preferred answered after that is "easily available data prepared for teaching"

#final questions
summary(survey$syllabi)
#108 No, 78 Yes, 72 NAs
summary(survey$age)
#72NAs. Most respondents are within the 45-54 age range
summary(survey$ethnicity)
#143 white/caucasian, only 14 Black or African American. 70 NAs.
summary(survey$interview)
#73 No, 115 Yes, 70 NAs

#Graphs

#Number of respondents who teach qualitative methods
ggplot(survey, aes(x = teach_qualitative)) +
  geom_bar() + scale_x_discrete(labels = c("Do Not Teach", "Major Focus", "Partial Focus", "N/A")) + ggtitle("Have you taught a graduate-level course focused on, or including material on, qualitative methods?") + ylab("Number of Respondents Teaching Qualitative Methods") + theme(axis.title.x=element_blank())

#Number of respondents who have used shared data
ggplot(survey, aes(x = used_data)) +
  geom_bar() + ggtitle("Have you used data to teach any of the qualitative analytic methods you just mentioned?") + ylab("Number of Respondents") + theme(axis.title.x=element_blank())



