df_raw <- read.csv("qog.csv")


library(dplyr)
library(tidyr)
library(ggplot2)
library(broom)
library(modelsummary)


df <- df_raw %>%
  select(
    country = cname,        # country name
    epi = epi_epi,          # Environmental Performance Index
    women_parl = wdi_wip,   # share of women in parliament (%)
    gov_eff = wbgi_gee,     # government effectiveness
    green_seats = cpds_lg   # green party seat share (%)
  )
df <- df %>%
  filter(
    !is.na(country),
    !is.na(epi),
    !is.na(women_parl),
    !is.na(gov_eff),
    !is.na(green_seats)
  )
summary(df)
ggplot(df, aes(x = women_parl, y = epi)) +
  geom_point() +
  labs(
    x = "Women in Parliament (%)",
    y = "Environmental Performance Index (EPI)",
    title = "Women in Parliament and Environmental Performance"
  )
ggplot(df, aes(x = women_parl, y = epi)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE) +
  labs(
    x = "Women in Parliament (%)",
    y = "Environmental Performance Index (EPI)",
    title = "Women in Parliament and Environmental Performance"
  )
# there is a positive relationship between the share of women
# in parliment and the enivormental performance index
# countries with a higher percentage of women in parliment tend to
# have higher EPI scores. the relationship looks linear
# some variation around the fitted line
m1 <- lm(epi ~ women_parl, data = df)

summary(m1)
tidy(m1)
#the coefficient is 0.1938
#1 % increase in the share of women
# the coefficient is statistically significant at the 5% level
# there is a positive relationship between women's representation and environmental performance
quantile(df$women_parl, probs = c(0.25, 0.75))
#25th is 25.68 and 75th is 41.11 the difference is 15.43
# the coefficient is 0.19379
#this suggests that countries with higher representation of women have 3 points higher EPI scores on average
model_mult <- lm(epi ~ women_parl + gov_eff, data = df)
results_mult <- tidy(model_mult)

?print
#The results suggest that countries with women in parliament also tend to have more effective governments. when controling for government effectiveness it is no longer significant
#1.5a cof for women_parl: 0.19379
# reg cof for women_parl: 0.0751
# reg cof for gov_eff: 4.22
#1.5
# B1= 0.194
#B1=0.0751
#B2=4.22
lm(gov_eff ~ women_parl, data = df) 
#β1 = ˆβ1 + ˆβ2 · ˜ δ
#0.0751+(4.22x0.02816)=0.19393
#the coefficient women_parl change when we add gov_eff because women are positively correlated with government effectiveness and government effectiveness helps the enviroment 
# and the more simple model gave women credit for the work the effective governments were doing. When the control is added
#the true effect became smaller and less significant

#1.6
install.packages("rstudioapi")
install.packages("sandwich")
modelsummary(model_mult)
modelsummary(model_mult, vcov = "robust")
#when comparing both models the robust are larger than the classical standard errors
#it increased for women 0.092 to 0.132 and for gov_eff from 1.68  to 1.864
# despite the increases women in parliment still remains statistically insignificant while gov eff remains a predictor of environmental performance

#1.7
modelsummary(list("Bivariate" = m1, "Multivariate" = model_mult), 
             vcov = "robust",
             stars = TRUE,
             title = "Table 1: Reg Analysis of EPI")
             
plot_qog <- modelplot(list("Bivariate" = m1, "Multivariate" = model_mult), 
                      vcov = "robust") +
  labs(title = "Comparison of Coefficients",
       subtitle = "Bivariate vs. Multivariate with Robust SEs",
       x = "Estimated Coefficient",
       y = "Variable") +
  theme_minimal()
print(plot_qog)
ggsave("coefficient_plot_qog.png", plot = plot_qog, width = 8, height = 6)
             

#1.8
#the coefficient of women to gov eff the gov has a larger impact
#If there is a large increase in women in parliament it will lead to a tiny movement, and the effect is minimal 








             
             