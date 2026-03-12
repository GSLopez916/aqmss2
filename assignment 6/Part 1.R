
df = read.csv("https://raw.githubusercontent.com/franvillamil/AQM2/refs/heads/master/datasets/other/minwage.csv")


library(dplyr) 
library(tidyr) 
library(ggplot2) 
library(fixest) 
library(modelsummary)


#1.1
#a
df = df %>% mutate(NJ = ifelse(location != "PA", 1, 0))
table(df$NJ)

df %>% 
  group_by(NJ) %>% 
  summarise( mean_wage_before = mean(wageBefore, na.rm = TRUE), 
             mean_wage_after = mean(wageAfter, na.rm = TRUE))

#Wages in NJ and PA where nearly identical. After NJ rose the min, wage
# their wages rose while PA wages remained flat. 

#b
means=df%>% 
  group_by(NJ)%>% 
  summarise( 
    before=mean(fullBefore,na.rm=TRUE), 
    after =mean(fullAfter, na.rm=TRUE), 
    change=after-before) 
mean

nj_change=means$change[means$NJ==1] 
pa_change=means$change[means$NJ==0] 
did_est =nj_change-pa_change 
cat("DiDestimate:",round(did_est,3), "\n")

#the DiD is the dif between in within the group changes.A positive value means
# that the employment grew more in NJ than in PA after the min wage
#increase, this contradicts the standard prediction that the high min wage reduce employment

#c
df_long = df %>% 
  mutate(id = row_number()) %>%
  pivot_longer( 
    cols = c(fullBefore, fullAfter), 
    names_to = "period", 
    values_to = "full_emp") %>% 
  mutate(
    post = ifelse(period == "fullAfter", 1, 0), 
    NJ = ifelse(location != "PA", 1, 0))
nrow(df_long)
nrow(df)
#The long formate is needed for the Did regression bc the intraction post NJ
# is the DiD estimator, it captures how the within NJ changes in employment 
# post and pre, differs from the corresponding within PA change


#1.2
#a 
m_did = feols(full_emp ~ post * NJ, data = df_long, cluster = ~id)
modelsummary(m_did, stars = TRUE, gof_map = c("nobs", "r.squared"), 
             output = "markdown")
#the coef on the post Nj is the DiD estimator and should match the manual calculation from 1.b
#The post coef captures the pre and post change in Pa, the NJ coef captures the baseline NJ Pa gap
#and the interaction captures the additional change in Nj relative to that trend.

#b
m_did_fe = feols(full_emp ~ post * NJ | chain, data = df_long, cluster = ~id)
modelsummary( 
  list("DiD" = m_did, "DiD + Chain FE" = m_did_fe), 
  stars = TRUE, gof_map = c("nobs", "r.squared"), 
  output = "markdown")
#The DiD estimate does not change when chain fixed effects are added
#chain fe absorb baseline differences in staffing levels across fast food chains
#but since chain type is roughly balanced across states, controlling for it has little impact
# on the Did coef.

#c
#The parallel trends assumption for this example requires that absent the NJ wage increase
#employment trends in NJ and PA fast-food restaurants would have been the same from Feb to Nov
#This is possible bc both states share a sim. economic environment and the 2 suverys 
#were close together in time, limiting opportunities for divering trends. A concrete violation wouold occur if NJ experienced an independent economic shock
# during this period, would change NJ employment for reasons unrelated to the min. wage. This would be a bias to the DiD estimate.

#1.3
#a
df_long_wage = df %>% 
  mutate(id = row_number()) %>% 
  pivot_longer( 
    cols = c(wageBefore, wageAfter), 
    names_to = "period", 
    values_to = "wage") %>% 
  mutate( 
    post = ifelse(period == "wageAfter", 1, 0), 
    NJ = ifelse(location != "PA", 1, 0)) 
m_wage = feols(wage ~ post * NJ, data = df_long_wage, cluster = ~id)
modelsummary(m_wage, stars = TRUE, gof_map = c("nobs", "r.squared"), 
             output = "markdown")
#NJ is positive and statistically significant, wages rose substantially in Nj 
#relative to PA after the policy change, and the magnitude is consistent with the .80 min. wage increase
#This would be expected if the law was a binding one. 

#b The wage result is importatnt for interpreting the employment DiD because
#it is the first stage or manipulation check. If wages had not risen in NJ after the law
#change it would not be clear if the study is truly estimating the effect of a min wage change
#at all. The law might not have been binding or stores have already been paying above the new min.
#The fact that wages did increase in NJ gives us confidence that the treatment actually occcured as the law intended
# also the employment DiD can be interpreted as a casual response to the min. wage increase rather than a 
# spurious or null comp. 











