raw <- read.csv("anes_timeseries_2020.csv")


library(dplyr)      
library(ggplot2)     
library(broom)
library(modelsummary)
library(marginaleffects)

print(note: "note: mututate vs transmute")
print("note case_when and ifelse")

class(NA_character_)
class(NA_real_)
class(NA)



#1
df <- raw %>%
  transmute(
    voted = ifelse(V202109x < 0, NA, V202109x),
    age = ifelse(V201507x < 0, NA, V201507x), 
    female = case_when(   # Fixed: used '=' instead of '-' 
      V201600 == 2 ~ 1,
      V201600 == 1 ~ 0,
      TRUE ~ NA_real_     # Use NA_real_ to match numeric types [cite: 159, 161]
    ),
    education = case_when(
      V201511x == 1 ~ 10, V201511x == 2 ~ 12, V201511x == 3 ~ 14,
      V201511x == 4 ~ 16, V201511x == 5 ~ 20, TRUE ~ NA_real_ 
    ),
    income = ifelse(V201617x < 0, NA, V201617x),
    party_id = ifelse(V201231x < 0, NA, V201231x) # Fixed: typo 'felse' to 'ifelse' 
  )

#b
df <- na.omit(df)
nrow(df)

#c
mean(df$voted)
summary(df)

#2
turnout_by_edu <- df %>%
  group_by(education) %>%
  summarise(turnout = mean(voted))

ggplot(turnout_by_edu, aes(x = factor(education), y = turnout)) + # Added '+' 
  geom_col() +
  labs(x = "Years of education", y = "Turnout rate")
  
# 3 
lpm <- lm(voted ~ age + education + income + female, data = df) # Fixed: '=' instead of '-' 
tidy(lpm)



#d
preds_lpm <- predict(lpm)
sum(preds_lpm < 0)
sum(preds_lpm > 1)
range(preds_lpm)

#4
logit <- glm(voted ~ age + education + income + female,
             family = binomial, data = df)
tidy(logit)
exp(coef(logit))

#d
preds_logit <- predict(logit, type = "response")
range(preds_logit)

#5
avg_slopes(logit)

#c
modelsummary(list("Lpm" = lpm, "Logit" = logit),
             vcov = list ("robust", NULL), output = "markdown")
#6
p1 = plot_predictions(logit, condition = "education")
p1
ggsave("pred_prob_education.png", p1, width = 6, height = 4)

#b
p2 = plot_predictions(logit, condition = c("age", "female"))
p2
ggsave("pred_prob_age_gender.png", p2, width = 6, height =4)


#7
p3 = modelplot(list(lpm, "Logit" = logit),
               vcov = list("robust", NULL))
p3
ggsave("coefplot_lpm_logit.png", p3, width = 6, height = 4)







  
  
  
  
  
  










