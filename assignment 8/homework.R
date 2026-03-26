install.packages("QCA")

library(ggplot2)
library(sf) 
library(spData) 
install.packages("spdep")
library(spdep) 
install.packages("spatialreg")
library(spatialreg) 
data(world)

#1.1.a
world = world[!is.na(world$gdpPercap) & !is.na(world$lifeExp), ] 
world = world[world$continent != "Antarctica", ] 
world$log_gdp = log(world$gdpPercap)
nrow(world)
#160 observations remain.We logtransform GDP per cap because the raw variable is strongly
#right skewed, and a handful of rich countries have values far above the bulk of the distribution.
#This makes it more linear, an assumption of OLS.

#1.1.b
ols_fit = lm(lifeExp ~ log_gdp, data = world) 
summary(ols_fit)
#The estimated on log_gdp is positive and statistically significant. 
#It means that it is one unit increase in log GDP per cap, associated with higher life 
# expectancy by approximately that many years on ave. It explains the sahre of cross country 
#variation in life expectancy.

#1.1.c

world$ols_resid = residuals(ols_fit) 

ggplot(world) + 
  geom_sf(aes(fill = ols_resid), color = "white", linewidth = 0.2) + 
  scale_fill_gradient2(low = "#2166ac", mid = "white", high = "#d6604d", 
                       midpoint = 0, name = "OLS residual") + 
  theme_void() + 
  labs(title = "OLS residuals: life expectancy ~ log GDP per capita")

ggsave("ols_residuals_map.pdf", width = 10, height = 5)

#The map is showing clear geographical. Southern Africa is showing a negative 
#residuals, with lower life expectany than the model predicts given their income level
#Western Europe and East Asia have postive residuals bc of higher life expectancy than income alone predicts
#these are non random geographic pattern in the residuals is a visual signal of spatial autocorrelation.

#1.2a
nb = poly2nb(world, queen = TRUE) 
listw = nb2listw(nb, style = "W", zero.policy = TRUE) 
summary(nb)
#Some countries have no neighbors. They are island nations that do not have land 
#boundaries or common border points with any other polygon in the data
#Queen contiguity requires that at least one point isolated nodes in the weights graph.
#The zero policy allows these units to remain in the analysis despite having no neighbors.

#1.2b
moran.test(world$ols_resid, listw = listw, zero.policy = TRUE)
#The Moran's I statistic is positive and the pvalue is below 0.05, and significant positive
#spatial autocorrelation in the OLS. Countries close to each other tend to have similar residuals 
#both overestimated or both underestimated, which violates the OLD assumption of independent errors
#ignoring this patern yields inefficient estimates and invalid standard errors. 

#1.3
lm_tests = lm.LMtests(ols_fit, listw = listw, test = c("LMerr", "LMlag", "RLMerr", "RLMlag"), zero.policy = TRUE) 
summary(lm_tests)
#a
#LMerr tests whether there is spatial dependence in the error term. LMlag tests is a spatially lagged dependent variable 
#belongs in the model. Both tests are singificant meaning both types of spatial dependence appear to be present
# in some form when tested alone. When both tests are significant we turn to the robust versions to discriminate.

#b
#The robust tests each control for the presence of the other type of spatial dependence
#Comparing them: if RLMerr is more significant than RLMlag, the evidence favors the SEM; if RLMlag dominates, the SLM is preferred
#Based on the decision rule, select the model whose robust test is more significant.


#1.4
#a
sem_fit = errorsarlm(lifeExp ~ log_gdp, data = world, 
                     listw = listw, zero.policy = TRUE) 
summary(sem_fit)
#the coef on log_gdp from SEM and the OLS estimate are both reported above
#The SEM coef may shift somewhere from OLS because the error structure correction absorbs spatial cofoudning
#The parameter captures spatial autocorrelation in the errors. If it is significant, the SEM has
#identified genuine spatial dependence in the residual varation.

#b
#In the SEM it governs the spatial autoregressive process in the disturbances. A Positive
#and significant means that the unmeasured factors driving life expectancy are spatially 
#correlated and are geographically clustered. the SEM filters this spaital correlation out of the 
#residuals without positing that the life expectancy itself diffuses across borders. 

#c
world$sem_resid = residuals(sem_fit) 
moran.test(world$sem_resid, listw = listw, zero.policy = TRUE)
#Comparing the Morans 1 to the one from the question 2b, the SEM substantially 
# reduces the spatial autocorrelation in the residuals. To test statistic is now 
#much closer to 0 and the pvalue is no longer significant indicating 
# that the spatial error correction has absorbed most of the geographic clustering 
#that OLS left behind in the residuals

#1.5
#a
coords = st_centroid(st_geometry(world)) 
nb_dist = dnearneigh(coords, d1 = 0, d2 = 300) 
summary(nb_dist)
#Compared to 1.2a there are now 114 countries with 0 neighbors. This is much higher
#than the 7 countries found with the queen contiguity neighborhood. This is likely because the chosen
#distance threshold is too small to reach most other countries. Because they are island nations or countries with large land areas. 

#B
listw_dist = nb2listw(nb_dist, style = "W", zero.policy = TRUE) 
sem_dist = errorsarlm(lifeExp ~ log_gdp, data = world, 
                      listw = listw_dist, zero.policy = TRUE) 
summary(sem_dist)
#Comparing the log_gdp coef and this model to the contiguity based SEM in 1.4a
#this model shows a higher coefficient and much small lamba. This shows that the spatial models are highly
#sensitive to neighborhood definitions. This leds to  a result that is closer 
#to standard OLS regression than the contiguity based model.

#c
world$sem_dist_resid <- residuals(sem_dist)

moran.test(world$sem_dist_resid, listw_dist, zero.policy = TRUE)
#The model succeeds in removing spatial autocorrelation from the residuals 
#as the Moran I;s pvalue is not statistically significant. Compared to 1.4c
#both models effectively handle spatial dependence, but because the distance based neighborhoods 
#have so many isolate, there was less spatial autocorrelation compared to the contiguity based model

#Part 2

#2.1a
slm_fit <- lagsarlm(lifeExp ~ log_gdp, 
                    data = world_clean, 
                    listw = listw, 
                    zero.policy = TRUE)

summary(slm_fit)
#the estimate Rho is -0.0043 with a pvalue as 0.805.
#The coefficient for log_gdp is 5.548. It is not statistically significant.

#b
#Rho represents the effect of neighbors life expectancy on a country's own life
#expectancy. If they were positive and significant, it would mean that living next to healthy countries
# makes your own country healthier. But bc our rho is almost zero and not significant,
#this model suggests there is no direct spatial diffusion or spillover of life expectancy 
#across borders in this specific setup.

#c
#the log_gdp in the SLM output is not the marginal effect of GDP on life expectancy bc
#the SLM includes the feedback loop. This model changing GDP in one country triggers 
#a ripple effect through the neighbor network. This means the total impact of GDP is the sum 
# of the direct effect on the country itself and the indirect effects that bounce back
#and forth between negihbors. 

#2.2
#a

set.seed(123)

slm_impacts <- impacts(slm_fit, listw = listw, R = 500)

summary(slm_impacts, zstats = TRUE, short = TRUE)

#The direct effect is 5.548 and the indirect effect is -0.024 and the total effect is 
#5.52. The direct effect is almost identical to the raw log_gdp coefficient from the 
#SLM output because rho is nearly zero, meaning there is very little spatial feedback. 
#compared to the OLS coefficient the results are very similar, suggesting that the spatial lag
#model does not significantly change our understanding of the relationship
#between GDP and life expectancy for this data.

#b
#The indirect effect represents the spatial spillover of a change in an independent variable.
# If log GDP per cap in one country increases by 1 unit, the indirect effect tells us the tool resulting change
#in life expectancy across all other countries in the network. It captures how country A 
# higher wealth improves its own life expectancy which then spills over to help its neighbors
#and continues to ripple through the entire map until the system reaches a dif equalibrium.

#c
#The total effect being larger than the direct effect is an expected feature of the SLM
#when RHO is positive, as it accounts for the additional spillover from neighbors. The size of this indirect 
#effect depends directly on the strength of rho. As rho grows larger the spatial feedback loop becomes much stronger 
# causing the indirect effect to grow and potentially even exceed the direct effect. 

#2.3
#a
aic_ols <- AIC(ols_fit)
aic_sem <- AIC(sem_fit)
aic_slm <- AIC(slm_fit)
aic_ols
aic_sem
aic_slm
#The model with the lowest AIC is aic_sem with 894.7021.This result does agree with the 
#LM test based model choice in 1.3b.

#b
#Significant Moran I results confirmed that OLS residuals has strong autocorrelation.
#I selected the SEM because the LM error tests were just more significant than the LM lag tests.
#the log_gdp coefficient remained positive across all models but decreased in the SEM as spatial noise
#was filtered out. The SLM suggests that life expectancy can spill over to neighbors, though out specific
#results found this effect to be weak. A limitation of queen contiguity is that it misses connections for the island
#nations that do not share a physical land border. 

#2.4
#a
sdm_fit <- lagsarlm(lifeExp ~ log_gdp, 
                    data = world_clean, 
                    listw = listw, 
                    Durbin = TRUE, 
                    zero.policy = TRUE)
summary(sdm_fit)
# The coefficient for lag.log_gdp is -3.82746 and is highly statistically 
#significant. a significant lag.log_gpd means that the gdp of neighboring countries is a strong predictor 
#of a country's life expectancy even after accounting for that country;s own GDP. This suggests
#that the economic conditions of the surrounding region have an independent effect
# on local health outcomes, though the negative sign here is a complex results often seen when the 
#model is balancing the effects of local wealth and regional spatial error. 

#b
#Though the SDM of 939.83 is a much better fit than the OLS or the SLM, it is still has significantly
#higher AIC than the SEM 894.70. this means the added of SDM is not fully justified for this specific data.
#The SEM remains the most efficient and best fitting model because it achieves a much better fit with less parmeters
# because it focuses on spatial error rather than lagged predictors. 


