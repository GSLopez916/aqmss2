skool <- read.csv("star.csv")
library(dplyr)
library(tidyr)
library(ggplot2)
library(broom)
library(modelsummary)
#2.1
#b)
skool$classtype <- factor(skool$classtype, 
                         levels = c(1, 2, 3), 
                         labels = c("Small", "Regular", "Regular+Aide"))
#c
skool$race <- factor(skool$race, 
                     levels = c(1, 2, 3, 4, 5, 6), 
                     labels = c("White", "Black", "Asian", "Hispanic", "Native American", "Other"))
#d
skool$small <- ifelse(skool$classtype == "Small", 1, 0)
#e
total_obs <- nrow(skool)
valid_math <- sum(!is.na(skool$g4math))
valid_reading <- sum(!is.na(skool$g4reading))

print(paste("Total Observations:", total_obs))
print(paste("Valid Math Scores:", valid_math))
print(paste("Valid Reading Scores:", valid_reading))

#2.2
skool %>%
  group_by(classtype) %>%
  summarize(mean_reading = mean(g4reading, na.rm = TRUE))
#a) small scores the highest
#b
model_star_biv <- lm(g4reading ~ small, data = skool)
summary(model_star_biv)
#c
model_star_math <- lm(g4math ~ small, data = skool)
summary(model_star_math)
#d
# the students in the small class had better scores in math but the difference is small and not statistically significant

#2.3
#a
model_star_mult <- lm(g4math ~ small + race, data = skool)
summary(model_star_mult)
#b
#the small coefficient is 0.7195. Meaning the small classes are not statistically significant 
#c
#the small coefficient remains insignificant even after adding race. 

#2.4
#a
model_int <- lm(g4reading ~ small * race + yearssmall, data = skool)
#b
tidy(model_int)
#c
#To find the effect, I will add the small cof, with the yearsmall cof
#White students= -5.32+2.25=-3.07
#Black students= -3.07 + 6.97= 3.9
#d
#Is the interaction substantively meaningful? The p-value is 0.271, suggesting that no it is not significantly meaningful
# There is no significant difference in how class size affeccts black vs white students

#2.5
#a
model_read_mult <- lm(g4reading ~ small + race, data = skool)

reading_models <- list(
  "Bivariate"    = model_star_biv,
  "Multivariate" = model_read_mult,
  "Interaction"  = model_int)
#b
library(modelsummary)
table_results <- modelsummary(reading_models, 
                              vcov = "robust", 
                              stars = TRUE, 
                              title = "Table 3: Comparison of Reading Score Models")
table_results
library(ggplot2)

plot_results <- modelplot(reading_models, coef_omit = "Intercept") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Figure 1: Coefficient Plot (Reading Models)",
       x = "Estimated Effect Size",
       y = "Variable")

plot_results
modelsummary(reading_models, 
             vcov = "robust", 
             stars = TRUE, 
             output = "reading_results.html")
#c
ggsave("reading_coef_plot.png", plot = plot_results, width = 8, height = 6)

#2.6
#a
#The STAR data suggests that the effect of the small class sizes on student achievement 
# did not have a large impact on students. In the models for reading and math, the small variable was not
# statistical significant. Thus this means that any of the benefit from small classes in previous years
# is non existent in older years. When including for different races, small classes also did not make 
# significant differences. 
#b
#This study is more credible because it is a random experiment. 
# In other studies it may have students not assigned by chance.
# Families that are wealthier may choose to send their their children to schools
# with smaller class sizes. Project STAR uses a random assignment, which creates an
# equal opportunity from the start. This gives evidence that the class size causes
# the results rather than external forces.
#c
#There are limitations within the study. Looking at the data there are students that have missing scores 
#This could contribute to a lack of accuracy in the results. Also, the results for race can not
#be applied nation wide. Tennessee is not a racially diverse state like a California or New York
# there are far too few Hispanics and native Americans to truly understand have race is affected.
#The data only shows test scores and does not take into account the teacher quality within the classes
# and each life quality each individual student has. These could be far more important factors
#in the results of the test scores. 




