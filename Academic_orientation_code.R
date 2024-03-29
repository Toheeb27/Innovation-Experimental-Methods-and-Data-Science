#Working Directory
getwd()
# "C:/Users/HP/OneDrive/Documents/RCODE"

#loading libraries

library("tidyverse")
library(dplyr)
library(ggplot2)

# Loading data into R

survey_data <- read_csv("Academic+Orientation_January.csv")

view(survey_data)


# Removing unnecessary columns

filtered_survey_data <- survey_data  %>%  select(-c(1:4, 6:19))

view(filtered_survey_data)

#Removing unnecessary rows

sliced_survey_data <- filtered_survey_data %>%  slice(-c(2,3))

view(sliced_survey_data)

#Filtering out incomplete data

completed_survey_data <- sliced_survey_data %>%  filter(Progress==100)
view(completed_survey_data)
nrow(completed_survey_data)


#Grouping Participants into 3 level of study

completed_survey_data <-  completed_survey_data %>% mutate(level= case_when(level_of_study=="First Year Undergraduate" |
                                                          level_of_study=="Second Year Undergraduate" | 
                                                          level_of_study=="Third Year Undergraduate"~ "Undergraduate", 
                                                          level_of_study=="First Year Master" | level_of_study== "Second Year Master" ~ "Masters",
                                                          level_of_study =="PhD" ~ "PHD" ))

#Factorizing data by level of study 

completed_survey_data$level <- factor(completed_survey_data$level, levels = c("Undergraduate","Masters","PHD"), ordered = TRUE)
completed_survey_data$confidence_to_succed <- factor(completed_survey_data$confidence_to_succed, levels = c("Not Confident","Less confident","Somewhat Confident","Very Confident","Extremely confident"), ordered = TRUE)

 
# Converting challenges questions response to Numbers


dat_wt_converted_likert <- completed_survey_data %>%
  mutate( openday_efectiveness_num = case_when(openday_efectiveness == "Excellent" ~ 5, openday_efectiveness == "Good" ~ 4 , openday_efectiveness == "Average" ~ 3, openday_efectiveness == "Fair" ~ 2 , openday_efectiveness == "Poor" ~ 1 ) ) %>% 
  mutate(adviser_efectiveness_num = case_when(adviser_efectiveness == "Very Effective" ~ 5, adviser_efectiveness == "Effective" ~ 4 , adviser_efectiveness == "Somewhat Effective" ~ 3, adviser_efectiveness == "Less Effective" ~ 2 , adviser_efectiveness == "Not Effective" ~ 1 )) %>%
  mutate(academic_resources_num = case_when(academic_resources == "Strongly Agree" ~ 5, academic_resources == "Agree" ~ 4 , academic_resources == "Neutral" ~ 3, academic_resources == "Disagree" ~ 2 , openday_efectiveness == "Strongly Disagree" ~ 1 ) ) %>%
  mutate(orientation_program_num = case_when(orient_program_efect == "Extremely Effective" ~ 5, orient_program_efect == "Very Effective" ~ 4 , orient_program_efect == "Somewhat Effective" ~ 3, orient_program_efect == "Less Effective" ~ 2 , orient_program_efect == "Not Effective" ~ 1 ) ) %>% 
  mutate(job_networking_num = case_when(job_networking == "Always" ~ 5, job_networking == "Occasionally" ~ 4 , job_networking == "Moderately" ~ 3, job_networking == "Rarely" ~ 2 , job_networking == "Never" ~ 1 )) %>% 
  mutate( social_inclusiveness_num = case_when(social_inclusiveness == "Always" ~ 5, social_inclusiveness == "Occasionally" ~ 4 , social_inclusiveness == "Moderately" ~ 3, social_inclusiveness == "Rarely" ~ 2 , social_inclusiveness == "Never" ~ 1 ) ) %>% 
  mutate(decide_career_path_num = case_when(decide_career_path == "Extremely Helpful" ~ 5, decide_career_path == "Very Helpful" ~ 4 , decide_career_path == "Somewhat Helpful" ~ 3, decide_career_path == "Slightly Helpful" ~ 2 , decide_career_path == "Not Helpful" ~ 1 ) )

#Converting measures of Overall orientation and Overall satisfaction to numbers

dat_wt_converted_likert <-   dat_wt_converted_likert %>%
  mutate( life_after_graduatio_num  = case_when(life_after_graduatio == "Excellently" ~ 5, life_after_graduatio == "Good" ~ 4 , life_after_graduatio == "Averagely" ~ 3, life_after_graduatio == "Fairly" ~ 2 , life_after_graduatio == "Poorly" ~ 1 )) %>% 
  mutate( satis_with_um6p_orie_num = case_when(satis_with_um6p_orie ==  "Extremely satisfied" ~ 5, satis_with_um6p_orie == "Very satisfied" ~ 4 , satis_with_um6p_orie == "Somewhat satisfied" ~ 3, satis_with_um6p_orie == "Less satisfied" ~ 2 , satis_with_um6p_orie == "Not satisfied" ~ 1) )
view(dat_wt_converted_likert)
# Creating a new data frame for Undergraduates

undegraduate_data <- dat_wt_converted_likert %>% filter(level =="Undergraduate")

# Getting the mean of all Undergraduates challenges


mean_cr_pt = mean(undegraduate_data$decide_career_path_num, na.rm = TRUE)
mean_sl_inc= mean(undegraduate_data$social_inclusiveness_num, na.rm = TRUE)
mean_jb_nt= mean(undegraduate_data$job_networking_num, na.rm = TRUE)
mean_or_prg= mean(undegraduate_data$orientation_program_num, na.rm = TRUE)
mean_acd_rs= mean(undegraduate_data$academic_resources_num, na.rm = TRUE)
mean_ad_eff= mean(undegraduate_data$adviser_efectiveness_num, na.rm = TRUE)
mean_op_eff= mean(undegraduate_data$openday_efectiveness_num, na.rm = TRUE)

#Summing up the means of undergraduate challenges

overall_challenges_u <- sum(mean_op_eff,mean_ad_eff , mean_acd_rs,mean_or_prg,mean_jb_nt,mean_sl_inc,mean_cr_pt )
overall_challenges_u

# Creating a new data frame for Masters students

Masters_data <- dat_wt_converted_likert %>% filter(level =="Masters")

# Getting the mean of all Undergraduates challenges

mean_cr_pt_m = mean(Masters_data$decide_career_path_num, na.rm = TRUE)
mean_sl_inc_m= mean(Masters_data$social_inclusiveness_num, na.rm = TRUE)
mean_jb_nt_m= mean(Masters_data$job_networking_num, na.rm = TRUE)
mean_or_prg_m= mean(Masters_data$orientation_program_num, na.rm = TRUE)
mean_acd_rs_m= mean(Masters_data$academic_resources_num, na.rm = TRUE)
mean_ad_eff_m= mean(Masters_data$adviser_efectiveness_num, na.rm = TRUE)
mean_op_eff_m= mean(Masters_data$openday_efectiveness_num, na.rm = TRUE)

#Summing up the means of Masters students challenges

overall_challenges_m <- sum(mean_op_eff_m,mean_ad_eff_m , mean_acd_rs_m,mean_or_prg_m,mean_jb_nt_m,mean_sl_inc_m,mean_cr_pt_m )
overall_challenges_m


# Creating a new data frame for PHD Students

phd_data <- dat_wt_converted_likert %>% filter(level =="PHD")

# Getting the mean of all PHD challenges

mean_cr_pt_p = mean(phd_data$decide_career_path_num, na.rm = TRUE)
mean_sl_inc_p= mean(phd_data$social_inclusiveness_num, na.rm = TRUE)
mean_jb_nt_p= mean(phd_data$job_networking_num, na.rm = TRUE)
mean_or_prg_p= mean(phd_data$orientation_program_num, na.rm = TRUE)
mean_acd_rs_p= mean(phd_data$academic_resources_num, na.rm = TRUE)
mean_ad_eff_p= mean(phd_data$adviser_efectiveness_num, na.rm = TRUE)
mean_op_eff_p= mean(phd_data$openday_efectiveness_num, na.rm = TRUE)

#Summing up the means of PHD Students challenges

overall_challenges_p <- sum(mean_op_eff_p,mean_ad_eff_p, mean_acd_rs_p,mean_or_prg_p,mean_jb_nt_p,mean_sl_inc_p,mean_cr_pt_p)
overall_challenges_p


########################
# Descriptive Statistics#
########################


#A bar chart that describes the number of participants by their level of study

ggplot(completed_survey_data, aes(level, fill = level)) + geom_bar() + 
  labs(title = "Participants By Level of Study", x="Level of Study",y="Number of Participants") + scale_fill_manual(values = c("#00008B", "#87CEEB","#051D41")) + guides(fill=guide_legend(title="Legend"))


#A bar chart that describes the number of participants awareness about the school open day program

ggplot(completed_survey_data, aes(Open_day, fill = Open_day)) + geom_bar() +
  labs(title = "Participants Awareness of Open Day Program", x="Open Day Awareness",y="Number of Participants") +
  facet_wrap(~level) + scale_fill_manual(values = c("#00008B", "#87CEEB")) + guides(fill=guide_legend(title="Legend"))

#A bar chart that describes the number of participants and their assess to an Academic Adviser

ggplot(completed_survey_data, aes(academic_adviser, fill = academic_adviser)) + geom_bar() +
  labs(title = "Participants by Academic Adviser", x="Level of Study",y="Number of Participants") + 
  scale_fill_manual(values = c("#00008B", "#87CEEB"))+ facet_wrap(~level) + guides(fill=guide_legend(title="Legend"))

#A bar chart that shows the number of participants and their awareness of UM6P Orientation programs

ggplot(completed_survey_data, aes(orientation_program, fill = orientation_program)) + geom_bar() +
  labs(title = "Awareness of Orientation Pograms offered by UM6P", x="Orientation Program Awareness",y="Number of Participants") +
  facet_wrap(~level) + scale_fill_manual(values = c("#00008B", "#87CEEB")) + guides(fill=guide_legend(title="Legend"))

#A plot of participants and their awareness of the Career Center

ggplot(completed_survey_data, aes(career_center, fill = career_center)) + geom_bar() +
  labs(title = "Participant's Awareness about the Career Center", x="Career Center Awareness",y="Number of Participants") + 
  facet_wrap(~level) + scale_fill_manual(values = c("#00008B", "#87CEEB")) + guides(fill=guide_legend(title="Legend"))

#A plot of participants about challenges on job facilitation
ggplot(completed_survey_data, aes(job_networking, fill = job_networking)) + geom_bar() +
  labs(title = "Satisfaction of Job Networking Opportunities", x="Satisfaction level",y="Number of Participants") + 
  facet_wrap(~level) +scale_fill_manual(values = c("#00008B", "#87CEEB","#333333", "#1560BD", "#014F86" )) + guides(fill=guide_legend(title="Legend"))

#A plot of participants about confidence to succeed
ggplot(completed_survey_data, aes(confidence_to_succed, fill = confidence_to_succed)) + geom_bar() +
  labs(title = "Confidence to succeed after Graduation", x="Confidence level",y="Frequency") + 
  facet_wrap(~level) +scale_fill_manual(values = c("#00008B", "#87CEEB","#333333", "#1560BD", "#014F86" )) + guides(fill=guide_legend(title="Legend"))+ theme(axis.text.x= element_text(angle= 90))


#########################
# Inferential Statistics#
#########################

###############################################################
#Using Linear regression to test Assumptions of our Hypothesis#
###############################################################

#1 Independence of observations

#The observations are independent of each other from the data observed

#2 Linearity

ggplot(dat_wt_converted_likert, aes(satis_with_um6p_orie_num ,life_after_graduatio_num )) + geom_point() + geom_abline() 



#Our independent and dependent variable are linearly related, this was checked with a scatter plot which shows a positive correlation between academic orientation and student satisfaction across all level of studies

#3 Checking for normality

qqplot(dat_wt_converted_likert$satis_with_um6p_orie_num,dat_wt_converted_likert$life_after_graduatio_num)

#The data is normally distributed because the normal probability plot of residuals approximately follows a straight line. 


#4 Checking for the homogeneity of variance 

bartlett.test(dat_wt_converted_likert$satis_with_um6p_orie_num,dat_wt_converted_likert$life_after_graduatio_num)

#The P value of 0.12 which is greater 0.05 shows there is equal homogeneity of variance

#Linear model

model <- lm(life_after_graduatio_num ~ satis_with_um6p_orie_num, data=dat_wt_converted_likert) 

summary(model)


#The results reveal that the model is significant at (p<0.05) and there is a strong relationship between the student's academic orientation 
#and Student's satisfaction. The p-value is 6.22e-09. Hence, we reject our null hypothesis and accept the alternative hypothesis.

#The F-statistic of 39.98 on 1 and 106 degrees of freedom suggests that the model is statistically significant. 
#the model is providing evidence that proper academic orientation is a good predictor of student satisfaction.
#The predictor is contributing significantly to explaining the variability in the dependent variable.

#The R-squared is showing that the proportion of variance in the student satisfaction explained by the academic orientation is 0.2738.
#This suggests that approximately 27.38% of the variability in the student satisfaction is explained by the model through academic orientation.

#The residual standard error is measuring the typical deviation of the observed values from the predicted values by the model.
#it is 0.7519 in our model, this indicates the model is a better fit and the model properly accounts for the variability in the data.


#Plot of Academic Orientation Vs Student Satisfaction

jittered_data <- dat_wt_converted_likert %>%
  mutate(satis_with_um6p_orie_num = jitter(satis_with_um6p_orie_num, amount = 0.1))  # Adjust the amount as needed

ggplot(jittered_data, aes(x = satis_with_um6p_orie_num, y = life_after_graduatio_num, color="red")) +
  geom_jitter() + geom_abline() + labs(title = "Linear Regression: Academic Orientation Vs Students's Satisfaction", x="Academic Orientation", y="Student's Satisfaction")



#Checking the direction of the relationship between the independent variable (academic orientation) and dependent variable (student satisfaction)
correlation_of_the_study <- cor(dat_wt_converted_likert$satis_with_um6p_orie_num,dat_wt_converted_likert$life_after_graduatio_num)
correlation_of_the_study

#The correlation result is 0.52, which shows their is positive relationship between academic orientation and student satisfaction.

############################################################################################
#Checking for level of study (Groups) that have the highest academic orientation challenges#
############################################################################################

#Collating all the means of each questions for each level of study categories
Undergraduate <- undergraduate_challenges_mean <- c(mean_cr_pt, mean_sl_inc, mean_jb_nt,mean_or_prg, mean_acd_rs, mean_ad_eff, mean_op_eff)
Masters <- masters_challenges_mean <- c(mean_cr_pt_m, mean_sl_inc_m, mean_jb_nt_m,mean_or_prg_m, mean_acd_rs_m, mean_ad_eff_m, mean_op_eff_m)
PHD <- phd_challenges_mean <- c(mean_cr_pt_p, mean_sl_inc_p, mean_jb_nt_p,mean_or_prg_p, mean_acd_rs_p, mean_ad_eff_p, mean_op_eff_p)

#collation all the level of study categories means into a single data.frame 
mean_of_all_the_group <- data.frame(Undergraduate, Masters, PHD)

#Splitting the data.frame into group and its associated values
original_challenges_data <- gather(mean_of_all_the_group, key = "Group", value = "Value")

#Factoring the level of study group into an ordered ordinal
original_challenges_data$Group <- factor(original_challenges_data$Group, levels = c("Undergraduate", "Masters", "PHD"), ordered = TRUE)


ggplot(original_challenges_data, aes(x= Group, y = Value, fill= Group)) + geom_boxplot() +
  scale_fill_manual(values = c("skyblue", "blue", "azure"))+
  labs(title = "Level of Study with the Highest Academic challenges", x="Level of Study",y="Means of the Academic Challenges") + guides(fill=guide_legend(title="Legend"))





