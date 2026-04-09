install.packages("survival")
library(survival)
library(broom) 
library(ggplot2) 
library(marginaleffects)

data("lung", package = "survival")
lung_df <- survival::lung
head(lung_df)



#2.1a--------------------------------------------------------------------------------
lung_df$death <- ifelse(lung_df$status == 2, 1, 0)
surv_obj <- Surv(lung_df$time, lung_df$death)
print(surv_obj)
table(lung_df$death)
# There are 228 observations, 165 deaths, and 63 cases that are censored. Around 28% is censored and this
#can be seen as a moderate amount. This means we dont know about their fate due to limitations of the study. 


#2.1b--------------------------------------------------------------------------------------
km_fit <- survfit(Surv(time, death) ~ 1, data = lung_df)
print(km_fit)
#The mean survival time is 310 days. This means that half of the patients 
# survived for 310 days or longer, the other half lived less than 310 days. 

#2.1c---------------------------------------------------------------------------------------
km_sex <- survfit(Surv(time, death) ~ sex, data = lung_df)
km_tidy <- tidy(km_sex)
km_plot <- ggplot(km_tidy, aes(x = time, y = estimate, color = strata, fill = strata)) +
  geom_step(linewidth = 1) + # Plot the survival lines
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2, color = NA) + # CIs
  labs(title = "Survival Probability by Sex",
       subtitle = "Lung Cancer Patients (1 = Male, 2 = Female)",
       x = "Time (Days)",
       y = "Survival Probability") +
  theme_minimal()

print(km_plot)
ggsave("km_sex_plot.pdf", plot = km_plot, width = 8, height = 6)

surv_diff <- survdiff(Surv(time, death) ~ sex, data = lung_df)
print(surv_diff)

#Based on the results of the graph, the gender than survives the longest is females. The confidence intervals for the two
#groups overlaps in the beginning of the study but through time, men start to die more often. The log-rank test p-value is 0.001 which indicates a statistical significance difference. 
#The test evaluates whether the survival curves of the two groups are similar. 
#Since the p-value is less than 0.05 we conclude that sex significantly affects survival time. 

#2.2a-----------------------------------------------------------------------------------------
cox_fit <- coxph(Surv(time, death) ~ age + sex + ph.ecog, data = lung_df)
summary(cox_fit)

#The hazard ratio for sex is 0.0575, and bc this value is below 1, it means that females have a lower 
#hazard and longer survival times than males. This results it highly significant. 

#2.2b
#The hazard ratio for ph.econg is 1.59, meaning that one unit increase in the EcOG performance score is associated with a 59% increase of death. Also, patients with higher ECOG scores have a 
#significant higher risk of death at any given time compared to those with better physical performance. 

#2.2c
ph_test <- cox.zph(cox_fit)
print(ph_test)
#The p-values for the covariates are 0.66, 0.13, and 0.15. For the global test it is 0.22
# but because all the p-values are greater than 0.05, we fail to reject the null hypothesis. this means that none of the variables 
#violate the proportional hazards assumption. In substantive terms, this indicates that the effects
#of age, sex, and phyiscal functioning on the hazard of death remain constant over the entire
#duration of the follow-up period rather than changing as the disease progresses. 


#2.2d
#The Kaplan-Meier analysis suggested survival differences by sex suggested that there are significant survival differences by sex with females 
#surviving longer than males. In the Cox model, sex and physical functioning were significant predictors, while age was not statistically significant. Being 
#female reduces the hazard of death, whereas a higher ECOG score indicates worse physical functioning increases the hazard.
#The cox text confirmed that the hazard assumptions holds for all variables and the global model, as all p-values were above 0.05. These results
#indicate that a patient's initial physical performance and their biological sex are much stronger predictors of long cancer survival than their
#age at the time of diagnosis. 





