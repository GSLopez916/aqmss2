

install.packages("carData") 
install.packages("MASS")
install.packages("nnet")
install.packages("marginaleffects")
install.packages("pscl")
install.packages("AER")
library(AER) 
library(marginaleffects) 
library(ggplot2)
library(pscl)
library(carData) 
library(MASS) 
library(nnet) 
library(marginaleffects) 
data(BEPS)



#1.1a-------------------------------------------------------------------------------
table(BEPS$economic.cond.national)
BEPS$econ_ord = factor(BEPS$economic.cond.national, ordered = TRUE)

#The category most common is 3. Due to most of the respondents that chose a middle number
#this would be problematic because there is very little difference between the respondents 
#than. The effect wouldn't be large because it automattically dilutes the findinds. 

#b
m_ologit = polr(econ_ord ~ age + gender + Europe + political.knowledge, 
                data = BEPS, Hess = TRUE) 
summary(m_ologit)

#The raw coefficient is -0.122693. Because the raw coef is negative, meaning that respondents with stronger pro EU attitudes tend to be 
#perceive national economic conditions as having improved. 

#c
avg_slopes(m_ologit)
#The AMEs show the average change in the probability of each response category
#associated with a one unit increases in each predictor. AME from 4 and 5 are postive
# which is consistent with a positive association between pro EU sentiment and more
#optimist economic assessments. AMEs for any predictor must sum to zero across the 5 categories because the probabilities 
#are constrained to sum to one. 

#d
predictions(m_ologit, newdata = datagrid(gender = c("female", "male")))
# The most pessimistic and optimistic category are shown separately for male and female respondents.
#There is only a modest difference between the genders. This suggests that gendered optimism is not large in this
#dataset.

#1.2a------------------------------------------------------------------------------------------
BEPS$vote=relevel(BEPS$vote,ref="Conservative") 
m_mlogit=multinom(vote~economic.cond.national+Blair+Hague+ 
                    Kennedy+Europe,data=BEPS,trace=FALSE) 
summary(m_mlogit)
#The coefficient on Blair in the LvC equation is strongly positive: higher approval of Blair
# is associated with greater substantially log-odds of voting Labour rather than Conservative. 
#This makes sense as he was the leader of the Labour party. 

#1.2b
avg_slopes(m_mlogit)
#The AME on the probability of voting Labour is positive and substantial. There is one unit increase
#in the average probability of voting Labour holding all other variables constant.

#1.2c
#The multinational logit assumes IIA, that the odds between any two alternative is unaffected by the 
#presence or characteristics of the third alternative. The IIA fails because 2 alternatives are near perfect subs,
#and removing one simply shifts its probalility to the other rather than distributing it propotionally. 
#The IIA is plausible for Conservative vs the others but is a more legitimate worry for 
#labour/liberal democrat distinction. 


#1.3a----------------------------------------------------------------------------------------------------------------------
data(bioChemists)

summary(bioChemists$art) 
var(bioChemists$art) 
ggplot(bioChemists, aes(x = art)) + 
  geom_histogram(binwidth = 1, fill = "#294b66", color = "white") + 
  theme_minimal() + 
  labs(title = "Publications in last 3 years of PhD", 
       x = "Number of articles", y = "Count")
pdf("art_histogram.pdf", width = 6, height = 4) 
hist(bioChemists$art, breaks = 20, main = "Distribution of articles", 
     xlab = "Number of articles", col = "gray80") 
dev.off()

#The distribution ratio is right skewed with a modde at zero and a long upper reach. 
#This means that the mean is around 1 to 3. Under the Poisson model, the variance should = 
# the mean, and that it underestimate uncertainty and produce anti-conservative SE.


#1.3b
m_pois = glm(art ~ fem + mar + kid5 + phd + ment, 
             data = bioChemists, family = poisson) 
summary(m_pois)
exp(coef(m_pois)["ment"])
#The coefficient on ment is 0.025543 and when doing the IRR for ment it is 1.025872.The residual deviance is substantially larger than the
#residual degrees of freedom which is a sign of over dispersion. The Poisson model doesnot adequately capture the 
#variation in publication counts. 

#1.3c
dispersiontest(m_pois)
#The dispersion parameter is 1.82454. Because it is well above 1, confirms the variance in art 
#exceeding its mean. This means that the Poisson SE are too small. This implies for the validity of the Poisson SE
#above that it does account for over dispersion such as the negative binomial. 

#1.4a----------------------------------------------------------------------------------------------------------------------
m_nb = glm.nb(art ~ fem + mar + kid5 + phd + ment, 
              data = bioChemists) 
summary(m_nb)
#The coefficient is smilar to the Poisson estimate, showing that the point estimate is 
#reasonable stable. The main difference is in the SE, the negative binomial model produces larger
#more honest uncertainty estimates. A smaller theta means more severe overdispersion. Here because it is more
#moderate, which is meaningful but not extreme extra Poisson variation. 

#1.4b
AIC(m_pois)

AIC(m_nb)

#The model with the the AIC is m_nb of 3314.113, depite the NB model having one 
#additional parameter. Under AIC, it compensates for the added complexity. This confirms the overdispersion of being a genuine feature of the data.
#The negative binomial is the better model for these publication counts. 

#1.4c
predictions(m_nb, newdata = datagrid(fem = c("Men", "Women")))
#The predicted number of articles for men exceeds that than for women, this is held for relation status, children, 
#PhD, and productivity. The coef intervals provide info on whether this gender
#gap is statistically distinguishable. The gap reflects a persistent within group gender difference in publications productivety
#that is not simply an artifact of other observable characteristics.

#1.4d
#The Poisson model is not adequate for this dataset, the variance to mean ratio of art is roughly double, the 
#residual deviance far exceeds the degrees of freedom and rejects dispersion with a pvalue below 0.001. The negative binomial model achieves a lower AIC and produces a more reliable
#standard errors. The ment has a positive and significant effect, with an IRR above 1, and each mentor article is associated with an increase in student articles. 
#The predictors that are statistically significant in the negative binomial model is gender, children under the age of 5. One substantive conclusion about the factors driving publications
#productively amoung PhD students is that prestige and relationship status is not significant in the negative binomial model. 

























