

library(dplyr)
library(broom)
library(ggplot2)
library(modelsummary)
library(marginaleffects)
library(readstata13)
df <- read.dta13("https://raw.githubusercontent.com/franvillamil/AQM2/master/datasets/other/corruption.dta")

#1.1
df <- df %>% 
  rename(corruption_index = ti_cpi,
         gdp_per_capita = undp_gdp)

df <- df %>%
  filter(!is.na(corruption_index) & !is.na(gdp_per_capita))
nrow(df)

summary(df$corruption_index)
sd(df$corruption_index)

summary(df$gdp_per_capita)
sd(df$gdp_per_capita)

#1.2
#a
ggplot(df, aes(x = gdp_per_capita, y = corruption_index)) +
  geom_point() +
  geom_smooth(method = "lm") + 
  theme_minimal() +
  labs(title = "Relationship between GDP and Corruption",
       x = "GDP per capita (UNDP)",
       y = "Transparency International CPI")

#b
#The relationship is positive, richer countries tend to be less corruption
#but the pattern is non-linear. Most of the countries cluster at lower GDP
#and the linear doesnt capture the curv well enough. 

#c
ggplot(df, aes(x = log(gdp_per_capita), y = corruption_index)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(x = "log(GDP per capita)", 
       y = "Corruption Perceptions Index",
       title = "Relationship with Log-Transformed GDP")

#d
#The transformation is spreading out the lower income countries and compresses 
# the upper tail, this produces a more linear relationship.

#1.3
#a
m1 <- lm(corruption_index ~ gdp_per_capita, data = df)
#b
library(broom)
tidy(m1)
#The predicted change in the corruption index for one-dollar in GDP per cap is
#1.72
coef(m1)["gdp_per_capita"] * 10000
#c
q25 <- quantile(df$gdp_per_capita, 0.25)
q75 <- quantile(df$gdp_per_capita, 0.75)
library(marginaleffects)
pred_m1 <- predictions(m1, newdata = datagrid(gdp_per_capita = c(q25, q75)))
print(pred_m1)
diff(pred_m1$estimate)
# the difference between corruption between country at the 75th % and in the 25%
# of GDP captures the interquartile range effect. The confidence intervals
#indicate the precision of these predictions


#1.4
#a
m2 <- lm(corruption_index ~ log(gdp_per_capita), data = df)
tidy(m2)
#b
#In a level log model, a 1% increase in GDP per capita is associated with a change
#with a change of B1/100 in the corruption index for a doubling of 
# GDP(log(2)=0.693)\

#c
m3 <- lm(corruption_index ~ gdp_per_capita + I(gdp_per_capita^2), data = df)
tidy(m3)

#d
summary(m1)$r.squared  
summary(m2)$r.squared 
summary(m3)$r.squared

#The log specification fits the data the best, it is consistent with the scatterplot
#showing a concave relationship. A non linear specification is appropiate because the marginal return to additional 
# GDP diminishes at higher income levels, moving from 1k to 5k
#matters more for governance quality than moving from 25k to 29k

#5
#a
avg_slopes(m2, variables = "gdp_per_capita")
#b
#The AME differs from the raw coef on log(gdp) because the marginal effect of GDP
#in a level log model is dependant on the GDP. The AMF averages this over all observed 
#values. It tells us the average predicted change in the curruption index for
# a dollar increase in GDP across all countries in the sample.
#c
slopes(m3, variables = "gdp_per_capita", 
       newdata = datagrid(gdp_per_capita = c(2000, 10000, 30000)))

#c 
#the marginal effect of GDP on curruption diminishes as countries become richer. At low GDP levels
# an additional dollar of income has a larger predicted effect on corruption than at high GDP levels.
#this is a consistent with the concave shape of the relationship

#1.6
#a
p1 = plot_predictions(m2, condition = "gdp_per_capita")
p1
ggsave("pred_plot_m3.png",p1,width=6,height=4)
p2 <- plot_predictions(m3, condition = "gdp_per_capita") +
  labs(title = "Prediction Plot: Quadratic Model",
       x = "GDP per capita",
       y = "Predicted Corruption Index") +
  theme_minimal()
ggsave("pred_plot_m3.png", p2, width = 6, height = 4)
#both models show the same story, curruption decreases sharply with intitial increaces in GDP
#and then levels off at higher income levels. The log produces a smoother curve while the other model
# can curve back upward at very high GDP values 

#1.7
m1_aug <- augment(m1)
ggplot(m1_aug,aes(x=.fitted,y=.resid))+ 
  geom_point()+ geom_hline(yintercept=0,linetype= "dashed")+ 
  labs(x="Fittedvalues",y="Residuals",title="ResidualsvsFitted:Level-Level(m1)")

#this plot shows a clear curved pattern showing that the linear specification misses
#the non linear relationship. the spread of residuals also appears to increase with fitted values, suggesting heteroskedasticity

#b
m2_aug=augment(m2)
ggplot(m2_aug,aes(x=.fitted,y=.resid))+ 
  geom_point()+ 
  geom_hline(yintercept=0,linetype= "dashed")+ labs(x="Fittedvalues",y="Residuals",title="ResidualsvsFitted:Level-Log(m2)")

# the log transformation substantially improves the residual pattern the curvature is
#reduced but there is heteroskedasticity may remain


n =nrow(df) 
threshold=4/n 
cooks_d=cooks.distance(m2) 
influential=which(cooks_d>threshold) 
df$cname[influential]

plot(m2,which=4)

#d
#Influentical obervations should not be removed automatically. they may represent genuine cases
#wealthy or very corrupt rather than data errors. A recommendation reobutness check would be to reestimate
#the model excluding these observations and compare the coefficients. If the results are similar 
# the orginal estimates are rebust


#1.8

modelsummary( list("Level-Level"=m1,"Level-Log"=m2,"Quadratic"=m3), 
              vcov="robust", 
              stars=TRUE, 
              gof_map=c("r.squared","nobs"), 
              output="markdown")

# the level log model (m2) is the preferred specification. it has the highest R2
#produces the best residual and its function form has a clear substantive interpretation
# the relationship between wealth and corruption is one of diminishing returns 
#the log transformation also aviods the quadratic model's problem of an eventual sign reversal 
#at extreme values























