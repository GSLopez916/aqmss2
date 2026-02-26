
library(readstata13)      
library(ggplot2)        
library(dplyr)           
library(broom)           
library(modelsummary)     
library(marginaleffects)
library(readstata13)
df2 <- read.dta13("https://raw.githubusercontent.com/franvillamil/AQM2/master/datasets/other/infantmortality.dta")
#2.1
#a
summary(df2)
#there are 101 countries 

#b
ggplot(df2, aes(x = infant)) +
  geom_histogram(fill = "steelblue", color = "white", bins = 20) +
  labs(title = "Distribution of Infant Mortality", x = "Infant Mortality (per 1000)") +
  theme_minimal()

ggplot(df2, aes(x = income)) +
  geom_histogram(fill = "darkgreen", color = "white", bins = 20) +
  labs(title = "Distribution of Income", 
       x = "Per-capita income (dollars)", y = "Count") +
  theme_minimal()

# yes both are to the right.

#c
ggplot(df2, aes(x = income, y = infant, color = region)) +
  geom_point(alpha = 0.7) +
  theme_minimal() +
  labs(title = "Infant Mortality vs. Income",
       x = "Per-capita income",
       y = "Infant mortality rate")

#the relationship is negative and non linear, the mortality rate drops at low levels of 
#income. but the effect remains flat as income increases. 

#d 
ggplot(df2, aes(x = log(income), y = log(infant), color = region)) +
  geom_point(alpha = 0.7) +
  geom_smooth(method = "lm", se = FALSE, color = "black", linetype = "dashed") +
  theme_minimal() +
  labs(title = "Log-Log Relationship: Infant Mortality and Income",
       x = "log(Income)",
       y = "log(Infant Mortality)")

#yes the log log relationship does look more linear than the level level plot.
#it now looks more linear

#2.2
m1 <- lm(infant ~ income, data = df2)          
m2 <- lm(infant ~ log(income), data = df2)     
m3 <- lm(log(infant) ~ income, data = df2)     
m4 <- lm(log(infant) ~ log(income), data = df2) 

#a
m1 = lm(infant ~ income, data = df2)
m1

#b
m2 = lm(log(infant) ~ log(income), data = df2)
m2

#c
# M1= The predicted change in infant mortality for a 1k increase is 20.61 per 1K births
# a 10% increase in income is associated with a 5.118% decrease in infant mortality

#D 

plot(m1, which = 1)

plot(m2, which = 1)

# The one with the better residual pattern is M2 because it shows a clear non linear trend
# and also heteroskedasticity. meaning that the level level linear model fails to capture 
# the relationship correctly

#2.3
#a
m3 = lm(log(infant) ~ log(income) + region + oil, data = df2)
m3 <- lm(log(infant) ~ log(income) + region + oil, data = df2)
summary(m3)

#b 
# the coeficient on log income is -0.33985, which means there is a 1% increase in income with a 0.34 decrease in infant mortality
# does controling for region and oil status change the income effect? yes it does

#c 
#It tells me that for infant mortality in africa and controling for income that it has the highest rate out of any region

#d 
avg_slopes(m3, variables = "income")
#the AME of income is -0.00159 this implies that 1k increase in income is associated with 
# a decrease of 1.59 deaths per 1k births

#2.4
#a
m4 = lm(log(infant) ~ log(income) * oil + region, data = df2)
summary(m4)
#b
avg_slopes(m4, variables = "income", by = "oil")
#c
#yes, the income effect in the gen model, non oil countries have lower infant mortality
#but for oil countries, the relationship is positive. Why? because the wealth generate from the oil
#goes to a small elite and isnt towards the public health

#d
plot_slopes(m4, variables = "income", condition = "oil")

#2.5

#a
predictions(m3, 
            newdata = datagrid( 
              income = c(1000, 20000, 10000), 
              region = c("Africa", "Europe", "Americas"), 
              oil = c("no", "no", "yes")))
#b
#no they are not plausible. the gap between africa and europe is large that even when 
#at the same income level, africa is predicted to have significant higher infant mortality rate than europe


#2.6
#a
plot_predictions(m3, condition = c("income", "region")) +
  labs(
    title = "Impact of Income on Infant Mortality Across Regions",
    subtitle = "Predicted log(Infant Mortality) decreases as per-capita income rises",
    x = "Per-capita Income (USD)",
    y = "Predicted log(Infant Mortality)",
    color = "Region",
    fill = "Region"
  )
# This plot tells the general audience about the relationship between wealth and infent mortality
#it decreases across all regions. Showing that there is a strong relationship between 
#wealth and health. Geography also plays a really important role as there are shifts between regional lines.
#For any level of income, a country in africa will have high infant mortality than a country in Europe.
#The main limitations of this analysis as other factors such as education level or qualtiy of health care are not 
#included within the data. This could leave for reverse causality. This also does not control for the indivual wealth of the families.


#2.7
#a
plot(m3, which = 1)
# yes the plot does suggest heteroskedasticity. 

#b
modelsummary( 
  list("Level" = m1, "Log-Log" = m2,
       "Controls" = m3, "Interaction" = m4), 
  vcov = "robust", stars = TRUE, 
  gof_map = c("r.squared", "nobs"))

#C
models_list <- list(
  "Default SE" = m3,
  "Robust SE"  = m3
)
modelsummary(models_list, 
             vcov = list("iid", "robust"),
             stars = TRUE,
             gof_omit = 'AIC|BIC|Log.Lik|F|RMSE')
#No, the main conclusions for income and region do not change but the conclusion for the oil variable does.
#Using the robust provides a better test for accounting for the non constant variance in the data. 




















