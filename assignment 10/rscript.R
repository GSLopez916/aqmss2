library(tidyverse)
library(modelsummary)

if (!dir.exists("tab")) dir.create("tab", recursive = TRUE)
if (!dir.exists("img")) dir.create("img", recursive = TRUE)

star <- read.csv("https://raw.githubusercontent.com/franvillamil/AQM2/refs/heads/master/datasets/star/star.csv")

df <- star %>%
  mutate(
    small = ifelse(classtype == 1, 1, 0),
    race = factor(race, levels = 1:6, labels = c("White", "Black", "Asian", "Hispanic", "Native American", "Other"))
  ) %>%
  filter(!is.na(hsgrad))

model1 <- lm(hsgrad ~ small, data = df)
model2 <- lm(hsgrad ~ small + race + yearssmall, data = df)

modelsummary(list("LPM biv." = model1, "LPM ctrl." = model2), 
             vcov = "robust",
             output = "tab/regression_table.tex",
             stars = TRUE,
             title = "Effect of Small Class Size on Graduation")

plot_out <- modelplot(list("LPM Bivariate" = model1, "LPM Controlled" = model2), 
                      coef_omit = "Intercept") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "red") +
  theme_minimal() +
  labs(title = "Coefficient Plot: Predictors of High School Graduation",
       subtitle = "Estimates with 95% Confidence Intervals",
       x = "Coefficient Estimate",
       y = "Variable")

ggsave("img/graduation_plot.pdf", plot = plot_out, width = 8, height = 6)


















