
library (dplyr)
library (ggplot2)

library (fixest)
library (modelsummary)

df = read.csv("https://raw.githubusercontent.com/franvillamil/AQM2/refs/heads/master/datasets/presidential_approval/presidential_approval.csv")

#1.a
length (unique(df$State))
length (unique(df$Year))
table(table(df$State))


#1.b
summary(df$PresApprov)
summary(df$UnemPct)
df_sub = df %>%
  filter(State %in% c("California", "Texas", "NewYork"))
ggplot(df_sub, aes(x = Year, y = PresApprov, color = State)) + 
  geom_line() + 
  theme_minimal() + 
  labs(x = "Year", y = "Presidential approval (%)", color = "State")
#the 3 states move closely together over time, with large swings in approval.
#Suggesting that common national factors are the dominate driver of approcal while state 
#level differences are relatively stable.

ggplot(df, aes(x = UnemPct, y = PresApprov)) + 
  geom_point(alpha = 0.4) + 
  geom_smooth(method = "lm") + 
  theme_minimal() + 
  labs(x = "Unemployment rate (%)", y = "Presidential approval (%)")
# 1.c
#Across state-year observations higher unemployment is associated with
#lower presidential approval. This cross sectional patterns pools oberservations across states and years
# so it is both within state variation over time and permanetnt between state differences 
#making it difficult to draw cuasual conclusions

#1.2a
m_pooled = lm(PresApprov ~ UnemPct, data = df) 
summary(m_pooled)
# the coefficient for unempct is negative and increases one % in the unemployment rate
# is associated with a decrease in the Pres approval rating. The relationship is singificant 
#but it conflates variation within state over time.

#1.2b
m_pooled2 = lm(PresApprov ~ UnemPct + South, data = df) 
summary(m_pooled2)
#Controling for southern states changes the coef on unempct only modestly. This suggests that the bivariate
#OLs estimate was not stronlgy confounded by the north south distinction.

#1.2c
#Pooled OLS is problematic for pnale data bc it ingnotes unobserved time invariant differences across states
#that may by correlated with unemployment.An ex. would be states with historical weaker economies may have structurally higher unemployment 
# and different political cultures that shape the baseline approval.
# states in particular regions may have persistent partisan leanings that affect how residents evaluate the president independently 
# of economic conditions. 

#1.3a
install.packages("fixest")
library(fixest) 

m_fe = feols(PresApprov ~ UnemPct | State, data = df)
summary(m_fe)

modelsummary(
  list("Pooled OLS" = m_pooled, "State FE" = m_fe ), 
  vcov = ~State, 
  stars = TRUE, 
  gof_map = c("r.squared", "nobs"), 
  output = "markdown")
  
#1.3b
#the state fixed effect is absorbed all time invariant differences across states
#this is why the south drops from the model. Any time invariant variable is collinear with the set 
# of state dummies and cannot be estimated separately

#1.3c
#The coefficient on UNemPct is the state FE model identifies a within state effect
#it measures how approval changes in a given state when it unemployment rate rises or falls
# compared to that state's own average. This is different from pooled OLS which comps states
# with dif unemployment levels to each other

#1.4a
m_twfe = feols(PresApprov ~ UnemPct | State + Year, data = df)
#b
modelsummary( 
  list("Pooled OLS" = m_pooled, "State FE" = m_fe, "Two-Way FE" = m_twfe), 
  vcov = ~State, 
  stars = TRUE, 
  gof_map = c("r.squared", "nobs"))

#c
#What the year fixed effects are controlling for common time shocks, national economic cycles 
# presidentail schandals, wars or any other event that affects approval in all states simultaneously in a given year.
# If the national unemployment rises during a recession both the unemployment rate and president approval will move together in all states at once
#not bc of a state effect but because of the shared macro envirmoment. Adding year dummies removes this source of confounding and identifies the effect of a states
# unemployment relative to the national average in each year. If the coef on UnemPct changes notieably after adding year FEs, it suggest that common time trends were partly driving the relationship 
# estimated with atate FEs alone. 



















