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
summary(survey$fork1_type_data)


#Fork2

#When was the first time you used “shared data”

summary(survey$fork2_first_time)

#Have you used shared data to teach qualitative analytic methods more or less over time”

summary(survey$fork2_frequency)

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
summary(survey$fork2_challenges_obtain)

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

summary(survey$fork2_effect)

#Fork 3

#Why have you not used shared data to teach qualitative analytic methods?
summary (survey$fork3_not_effective)
summary (survey$fork3_notsure_use)
summary (survey$fork3_unlikely_find)
summary (survey$fork3_couldnt_find)
summary (survey$fork3a_why_nodata)

#Would you consider using shared data to teach qualitative analytic methods in the future
summary (survey$fork3comb_data_future)

#What would encourage and facilitate your use of shared data to teach qualitative analytic methods in the future

summary (survey$fork3_someone_help)
summary (survey$fork3_data_method)
summary (survey$fork3_data_topic)
summary (survey$fork3_prep_lesson)
summary (survey$fork3_other_enc)

#General Questions

#age
summary(survey$age)

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

