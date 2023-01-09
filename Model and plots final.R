library(dplyr)
library(tidyverse)
library(ggplot2)
library(lmtest)
library(lme4)
library(sjPlot)

setwd("D:/Userdata/Danielle/Downloads/R Downloads")
bigdata <- read_csv("bigdata.csv")
view(bigdata)


## Copypaste interaction variable model with age as a continuous variable
### Since we are no longer working with binary data, the family of our regression is no longer 'binomial'
### As such I have changed it to 'Gaussian', which effectively just makes it an lm model according to this website:
### https://www.datacamp.com/tutorial/generalized-linear-models 

m_base <- glm(turnout ~ age + education + income + employed + white_collar + self_employed + member + prevvote + party_id + vote_effect + urban_degree, data = bigdata, family = "gaussian", na.action = na.exclude)
summary(m_base)

m_interact_IMF <- glm(turnout ~ age*IMF + education + income + employed + white_collar + self_employed + member + prevvote + party_id + vote_effect + urban_degree, data = bigdata, family = "gaussian")
summary(m_interact_IMF)

m_interact_WB <- glm(turnout ~ age*WB + education + income + employed + white_collar + self_employed + member + prevvote + party_id + vote_effect + urban_degree, data = bigdata, family = "gaussian")
summary(m_interact_WB)

m_interact_PFI <- glm(turnout ~ age*PFI + education + income + employed + white_collar + self_employed + member + prevvote + party_id + vote_effect + urban_degree, data = bigdata, family = "gaussian")
summary(m_interact_PFI)

tab_model(m_base, m_interact_IMF, m_interact_WB, m_interact_PFI)

?tab_model

### Now, the table and model work.
### The changed coefficient estimates seem to be normal due to the different interaction variable that is also controlled for


plot_model(m_interact_IMF, type = "int")



plot_model(m_interact_WB, type = "int")



plot_model(m_interact_PFI, type = "int")
