library (dplyr)
library (ggplot2)
library (fixest)
library (modelsummary)


df <- read_dta("teaching_evals.dta")

#2.1A

df %>% 
  summarize(
    unique_instructors = n_distinct(InstrID),
    unique_courses = n_distinct(CourseID),
    total_observations = n(),
    avg_obs_per_instr = n() / n_distinct(InstrID)
  )
#There are 48 unique instructors and 254 courses in the data.
#The average number of observations per instructor is is 9.27.
#this would be considered a short panel data set. 

#2.1B

ggplot(df, aes(x = Apct, y = Eval)) +
  geom_point() +                          
  geom_smooth(method = "lm") +           
  labs(
    x = "Percent of A Grades",
    y = "Average Course Evaluation",
    title = "Relationship Between Grades and Evaluations"
  )
#the scatter plot reveals a postive cross sectional relationship between grading generosity
#and evaluations. This is not surprising because the thoery suggests that students may give higher ratings
#to teachers that grade easier

#2.2A
m1 <- lm(Eval ~ Apct + Enrollment + Required, data = df)
modelsummary(m1, stars = TRUE, gof_omit = 'AIC|BIC|Log.Lik|F|RMSE')
#The coef. is 0.359 which is one % increase in share of A grades associated with the coef.
#The relationship is significant suggesting that instructors who give higher grades receive better evals.

#2.2b
#The OLS estimate of Apct might be biased because of unobserved hetergeneity, these are factors 
# to each instructor that are not included in the measurement yet influence how they grade and students
#rate them.
#There are two examples of unobservable characteristics first, could be sex of the teacher. Depending on the sex
# of the teacher could influence the way some students grade the teacher due to societal factors and the way teachers 
#also grade students based on their sex. Another example could be race of the student and teacher. There could be racial bias
#between either party that affects the perception of the grade and ratings. The bias is likely upward. 


#2.3a
m_instr <- feols(Eval ~ Apct + Enrollment + Required | InstrID, data = df)

m_twfe <- feols(Eval ~ Apct + Enrollment + Required | InstrID + Year, data = df)

#2.3b
modelsummary(
  list("Pooled OLS" = m1, "Instructor FE" = m_instr, "Two-Way FE" = m_twfe), 
  vcov = ~InstrID, 
  stars = TRUE, 
  gof_map = c("r.squared", "nobs"))

#2.3c
#The instructor fixed effect is controling for unobserved heterogeneity. It is controling for things
# that do not change over time such as sex, gender, race, personality etc.
#The FE coefficient on Apct is smaller than the OLS coef. This tells us that the direction of 
# OLS estimate was upwardly biased. Because the OLS estimate was higher it means that those that had higher grades
#are systematically better evaluators in unoberserved characteristics. Easy graders tend to have characteristics that
#students tend to like. 

#2.4a
install.packages ("plm")
library (plm)

pdata = pdata.frame(df, index = c("InstrID", "CourseID")) 
#2.4b
m_re = plm(Eval ~ Apct + Enrollment + Required, 
           data = pdata, model = "random")

m_fe_plm = plm(Eval ~ Apct + Enrollment + Required, 
               data = pdata, model = "within") 
phtest(m_fe_plm, m_re)
#2.4c
#The null hypothesis is testing the random effects model is consistent and that instructor level characteristics 
# are not related with the grades.Basedthe test and the substantive reasoning from the previous sub sections,
#we would prefer the fixed effects model to ensure these biases are fully removed. It is a more cautious approach 
# for making causal claims. This suggests that there is no statistically significant difference between the two models though.






















































