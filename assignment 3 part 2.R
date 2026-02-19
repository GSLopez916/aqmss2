library(tidyverse)

library(marginaleffects)
library(modelsummary)

df <- read.csv("C:/Users/samue/OneDrive/Documents/Quantitative class/assignment 3/star.csv")

#2.1 
df <- df %>%
  mutate(
    classtype = factor(classtype, levels = c(1, 2, 3), 
                       labels = c("Small", "Regular", "Regular+Aide")),
    race = factor(race, levels = c(1, 2, 3, 4, 5, 6),
                  labels = c("White", "Black", "Asian", "Hispanic", "Native American", "Other")),
    # b) Create binary 'small' indicator [cite: 74]
    small = ifelse(classtype == "Small", 1, 0)
  )

#c
df_clean <- df %>% drop_na(hsgrad)
nrow(df_clean)
# After dropping the values, 3,047 oberservations remain in the dataset
#d
mean(df_clean$hsgrad) # Overall rate
df_clean %>% group_by(classtype) %>% summarize(grad_rate = mean(hsgrad))
#The differences in the 3 different groups appear to be statiscally the same more or less
# though, the small and aide classtype out preform the regular classtype.

#2.2
#a
lpm1 <- lm(hsgrad ~ small, data = df_clean)
tidy(lpm1)

#b
logit1 <- glm(hsgrad ~ small, family = binomial, data = df_clean)
tidy(logit1)

#c
# The small coefficient for small is 0.0036.
#the estimated differnce in graduation probability between 
#small and non-small classes is not very significantly statically different
# but techinically, the small class has a higher probalbity than regular and aide
#d
avg_slopes(logit1)
#The average marginal effect from the logit model is 0.00375
#This is almost the same as the LPM. Though this effect is not statsically significant 

#2.3
#a
lpm2 <- lm(hsgrad ~ small + race + yearssmall, data = df_clean)
logit2 <- glm(hsgrad ~ small + race + yearssmall, family = binomial, data = df_clean)

tidy(lpm2)
tidy(logit2)
#b
#Comparing the 2 models, the small changed from 0.0036 to -0.0756
#and became statistically significant. The randomization tells us that 
# small and yearsmall is mathamatically related

avg_slopes(logit2, variables = "yearssmall")
# this shows us that every year that a student spends in a small class, the probality that that student
# graduates from high school increases 2.83%. This is highly statistical significant


#2.4
new_data <- datagrid(model = logit2, 
                     race = c("White", "Black"), 
                     small = c(1, 0), 
                     yearssmall = c(3, 0))

p_star <- plot_predictions(logit2, condition = c("yearssmall", "small"))

p_star + theme_minimal() + labs(title = "Graduation Probability by Years in Small Class")
ggsave("star_predictions_plot.png", width = 7, height = 5)


#2.5
logit3 <- glm(hsgrad ~ small * race + yearssmall, family = binomial, data = df_clean)

small_by_race <- avg_slopes(logit3, variables = "small", by = "race")
print(small_by_race)
# The effect of being assigned to a small class differs across racial groups
# this suggests that the small class has more pronouced negative association 
# for Black students than for White students.

#2.6
modelsummary(list("LPM (Biv)" = lpm1, "LPM (Ctrl)" = lpm2, 
                  "Logit (Biv)" = logit1, "Logit (Ctrl)" = logit2),
             vcov = list("robust", "robust", NULL, NULL),
             stars = TRUE)
modelplot(list("LPM (Ctrl)" = lpm2, "Logit (Ctrl)" = logit2))

#The star data suggest that the effect of the small class sizes on high school graduation
#in the bivariate models, the effect is near 0. In the controlled models, it becomes negative and
#significant. The variable yearssmall is high significant, for each year spent in a small class
# the proability of graduating increases 2.7%.
#when comparing to the LPM and Logit results, tells something simular. Despite the different underlying assumptions
# the concultions are robust.
# The experimental evidence is more credible than an obervational study because 
# it eliminates selection bias, and we can be more confidently claim the link
#between class duration and graduation.











