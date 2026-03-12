

#Part 1

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


#Part 2

#2.1
install.packages("did")
library(did) 
data(mpdta)

#a
length(unique(mpdta$countyreal))
unique(mpdta$first.treat)
table(mpdta$first.treat)
#There are 500 counties in the data set. There are 4 unique treatment cohorts.
#There are 3 different years that adopted the policy= 2004 2006 and 2007
#the problem only comparing treated vs untreated is that it might compare the 07 group with 04.
#the 04 year is already treated, they arent the clean control group

#b
library(dplyr)
library(ggplot2)

mpdta_avg = mpdta %>% 
  mutate(cohort = factor(first.treat, 
                         levels = c(0, 2004, 2006, 2007), 
                         labels = c("Never treated", "Adopted 2004", 
                                    "Adopted 2006", "Adopted 2007"))) %>% 
  group_by(year, cohort) %>% 
  summarise(mean_lemp = mean(lemp, na.rm = TRUE)) 

ggplot(mpdta_avg, aes(x = year, y = mean_lemp, color = cohort)) + 
  geom_line() +
  geom_point() + 
  theme_minimal() + 
  labs(x = "Year", y = "Log teen employment", color = "Treatment cohort")
ggsave("cohort_trends_plot.png", width = 8, height = 6)

#The cohorts appear to follow somewhat similar trends before their treatment years
# between the years 2003 and 2004 where they mostly show a decrease. 
#After the treatment we see some divergence. The cohort that is most problematic is
# 2006 and 07 because they stat at lower baseline employment levels.

#2.2
#a
mpdta <- mpdta %>%
  mutate(treated_post = ifelse(first.treat > 0 & year >= first.treat, 1, 0))

m_naive <- feols(lemp ~ treated_post | countyreal + year, 
                 data = mpdta, 
                 cluster = ~countyreal)

modelsummary(m_naive, stars = TRUE)

#The coef. of -0.037** suggest that the min wage average is associated with a 
#-3.7% decrease in teen employment in treated counties
#The model pools all treatment cohorts together which assumes 
#that the treatment effect is homogenous. It also assumes that the policy's effect is 
#the same for counties treated in 04 as it is for those in 06 or 07 and the effect does not
#change overtime

#b
m_cs <- att_gt(yname = "lemp",
               tname = "year",
               idname = "countyreal",
               gname = "first.treat",
               data = mpdta,
               control_group = "nevertreated")
overall_att <- aggte(m_cs, type = "group")
summary(overall_att)

#They are quite similar, the overall ATT estimate is -0.031. and a small negative effect on
#teen employment. 

#c
event_study <- aggte(m_cs, type = "dynamic")
summary(event_study)

ggdid(event_study)

ggsave("event_study_plot.png")

#The Pre-treatment estimates are not statistally distinguishable from 0 bc their 
#confidence intervals cross the 0 line. This supports the parallel trends assumption
#that there were no significant difs between groups b4 treatment.
#This shows a negative trend, suggesting that there was a reduction of teen employment 
#becoming more pronouced a year or two after the policy is implemented

#2.3
#a
m_cs_boot <- att_gt(yname = "lemp",
                    tname = "year",
                    idname = "countyreal",
                    gname = "first.treat",
                    data = mpdta,
                    control_group = "nevertreated",
                    bstrap = TRUE,
                    cband = TRUE)
summary(m_cs_boot)

#the p-value is 0.16812. What the test is doing is checking the null hypothesis
#that the parallel trends assumption holds. We cannot reject this. the large p-value tells us
#we dont have the evidence of a violation of parallel trends b4 the treatment occured

#b
ggdid(m_cs_boot)
ggsave("group_time_att_plot.png")
#the pre-treatment ATT estimates are close to 0 and their confidence intervals overlap
# this suggests that b4 min wage changes took effect these counties were following 
#the same employment trends.
#c
#Even if we cannot reject parallel trends in the pre-period 
#we cannot be fully certain that the assumption holds post treat bc 
#we cannot observe what would have happened to the treated counties in the absence of the 
#policy. The pre test shows that the groups were comparable in the past
# but it cannot rule out unobserved factors that might change the trends at the same time
#the treatment is in place


#2.4
#a
m_cs_not_yet <- att_gt(yname = "lemp",
                       tname = "year",
                       idname = "countyreal",
                       gname = "first.treat",
                       data = mpdta,
                       control_group = "notyettreated")

overall_att_not_yet <- aggte(m_cs_not_yet, type = "group")
summary(overall_att_not_yet)
#the overall ATT is -0.0305. This is very similar in both signs and magnitude
#to the never treated estimate of -0.031

#b
event_study_not_yet <- aggte(m_cs_not_yet, type = "dynamic")
ggdid(event_study_not_yet)

ggsave("event_study_not_yet_plot.png")
#Using the broader control group does not change the conclusions. 

#c
#Using the not treated group provides a larger control group, which can provide 
#more precise estimates. But it requires an assumption that there are no anticipation
#effects b4 they are actually treated.
#I would prefer to use the never treated group if I believe that the counties 
# adjusted their behaviors b4 the law. The not yet treated group would be preferable when
#the never treated group is too small to provide a decent comp. WE would need more data to compare
#imporve the estimates.

#2.5
#a
#The TWFE model fails because it uses the already treated counties to 
# be the control group for mew treated counties. this is consider the forbidden comparison problem
#It is a problem because if the policy effect changes over time the already treated counties are no longer a stable baseline.
#Their changing trends get mixed into the caulation, which can lead to biased or wrong estamates. 

#b
#When comparing the estimate from Q2.2a to the Q 2.2b they are very similar (-0.037 vs -0.031).
#I contend that the Callaway estimate is more credible. Though both models are both very close, the CS model 
#is more reliable because it is designed to handle staggered timing without the risk of the forbidden comparison bias.















