library(tidyverse)
library(dplyr)
library(ggplot2)

fulldur8_full <- read_rds("/Users/jayjinsing/Documents/Projects/inProgress/2018_01 RFID/data/Duration Data/dur8full.rds")

dur8y1 <- fulldur8_full %>%
  filter(starttime < "2018-06-05")

sexes <- read_csv("/Users/jayjinsing/Documents/Projects/inProgress/2015 - Genetic Sexing/data/geneticsexingcompiled.csv")
plus2018 <- read_rds("/Users/jayjinsing/Documents/Projects/inProgress/2018_01 RFID/data/2018plus.rds")
corr2018 <- read_rds("/Users/jayjinsing/Documents/Projects/inProgress/2018_01 RFID/data/2018corrugations.rds")

#dtagged is the number of days from the capture and tag date to the called end date
#pfeeder is the same as dtagged except it's weighted by the number of available feeders
#daysaround is the number of days in which the bird fed at least once in the data set
#daystagged, perfeeder, and dayspresent are all functions in "Visit Frequency.R"
birdsy1 <- dur8y1 %>%
  inner_join(sexesplus2018, by = "id") %>%
  group_by(id, gensex) %>%
  summarize(feeds = n(), meandur = mean(feeding)) %>%
  filter(is.na(gensex) == FALSE) %>%
  mutate(dtagged = daystagged(capturestaginfo, id, "2018-06-05")) %>%
  mutate(pfeeder = perfeeder(capturestaginfo, id, "2018-06-05")) %>%
  mutate(daysaround = dayspresent(dur8y1, id)) 

birdsy1info <- birdsy1 %>%
  inner_join(plus2018, by = 'id') %>%
  inner_join(corr2018, by = 'id') %>%
  mutate(type = if_else(
    gensex == "m" & plutype == "m", "mlm", if_else(
      gensex == "f" & plutype == "f", "flf", if_else(
        gensex == "f" & plutype == "m", "mlf", "mix"))))

###########################################################
#Comparing males and females and their number of feeds per days tagged

test <- birdsy1info %>% 
  mutate(feedsperdtagged = feeds / dtagged)

#plot of feeds per day in which the bird was tagged
ggplot(test, aes(x = gensex, y = log(feedsperdtagged))) + 
  geom_point()

model <- lm(log(feedsperdtagged) ~ gensex, data = test)
summary(model)

# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)  
# (Intercept) -0.04668    0.34889  -0.134   0.8939  
# gensexm      0.97515    0.44437   2.194   0.0315 *

testtype <- birdsy1info %>% 
  mutate(feedsperdtagged = feeds / dtagged) %>%
  filter(type != 'mix') %>%
  filter(is.na(type) == FALSE)

res.aov <- aov(log(feedsperdtagged) ~ type, data = testtype)
summary(res.aov)

# Df Sum Sq Mean Sq F value Pr(>F)
# type         2  15.97   7.984   2.347  0.104
# Residuals   65 221.10   3.402    


###########################################################

#Comparing males and females and their number of feeds per days tagged per available feeders
test <- birdsy1info %>% 
  mutate(feedsperdtaggedfeeder = feeds / pfeeder) 

ggplot(test, aes(x = gensex, y = log(feedsperdtaggedfeeder))) + 
  geom_point()

model <- lm(log(feedsperdtaggedfeeder) ~ gensex, data = test)
summary(model)

# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)   3.0720     0.3530   8.702 8.36e-13 ***
#   gensexm       0.9317     0.4497   2.072   0.0419 * 

testtype <- birdsy1info %>% 
  mutate(feedsperdtaggedfeeder = feeds / pfeeder) %>%
  filter(type != 'mix') %>%
  filter(is.na(type) == FALSE)

res.aov <- aov(log(feedsperdtaggedfeeder) ~ type, data = testtype)
summary(res.aov)

# Df Sum Sq Mean Sq F value Pr(>F)
# type         2  14.91   7.453   2.135  0.126
# Residuals   65 226.85   3.490   

###########################################################

test <- birdsy1info %>% 
  mutate(feedsperdayspresent = feeds / daysaround)

ggplot(test, aes(x = gensex, y = log(feedsperdayspresent))) + 
  geom_point()

model <- lm(log(feedsperdayspresent) ~ gensex, data = test)
summary(model)
#
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)   1.0777     0.2442   4.413 3.57e-05 ***
#   gensexm       0.6684     0.3111   2.149   0.0351 *  

testtype <- birdsy1info %>% 
  mutate(feedsperdayspresent = feeds / daysaround) %>%
  filter(type != 'mix') %>%
  filter(is.na(type) == FALSE)

model <- aov(log(feedsperdayspresent) ~ type, data = testtype)
summary(model)
#
# Df Sum Sq Mean Sq F value Pr(>F)  
# type         2   8.15   4.075   2.461 0.0933 .
# Residuals   65 107.63   1.656   

###########################################################

#looks at the relationship between the number of feeds per day and the mean duration of those feeds in the day

feedsperday <- dur8y1 %>%
  inner_join(sexes, by = c("id" = "uniquerfid")) %>%
  select(id, starttime, station, duration, feeding, gensex) %>%
  mutate(feedsinday = as_date(starttime)) %>%
  group_by(id, feedsinday, gensex) %>%
  summarize(feedsintheday = n(), meandur = mean(duration)) %>%
  filter(is.na(gensex) == FALSE)
  
ggplot(feedsperday, aes(x = sqrt(feedsintheday), y = meandur, color = gensex)) +
  geom_point()

library(lme4)
library(lmerTest)
library(emmeans)

modelthing <- lmer((meandur / sqrt(feedsintheday)) ~ gensex + (1|id), data = feedsperday)
summary(modelthing)

# Fixed effects:
#   Estimate Std. Error     df t value Pr(>|t|)    
# (Intercept)    8.669      1.000 62.089   8.664 2.77e-12 ***
#   gensexm       -0.386      1.254 59.681  -0.308    0.759    

feedsperdaytypes <- feedsperday %>%
  inner_join(plus2018, by = "id") %>%
  mutate(type = if_else(
    gensex == "m" & plutype == "m", "mlm", if_else(
      gensex == "f" & plutype == "f", "flf", if_else(
        gensex == "f" & plutype == "m", "mlf", "mix"))))

feedsperdayflf <- feedsperday %>% filter(type == "flf")
feedsperdaymlf <- feedsperday %>% filter(type == "mlf")
feedsperdaymlm <- feedsperday %>% filter(type == 'mlm')

mlmmodel <- lmer(meandur ~ sqrt(feedsintheday) + (1|id), data = feedsperdaymlm)
summary(mlmmodel)
#
# Estimate Std. Error        df t value Pr(>|t|)    
# (Intercept)           19.5227     0.8777   60.8428  22.242  < 2e-16 ***
#   sqrt(feedsintheday)   -0.9055     0.1374 1218.2461  -6.589 6.56e-11 ***

flfmodel <- lmer(meandur ~ sqrt(feedsintheday) + (1|id), data = feedsperdayflf)
summary(flfmodel)
#
# Estimate Std. Error       df t value Pr(>|t|)    
# (Intercept)          15.2261     1.8162  23.9115   8.384  1.4e-08 ***
#   sqrt(feedsintheday)   0.4771     0.3644 309.1683   1.309    0.191  

mlfmodel <- lmer(meandur ~ sqrt(feedsintheday) + (1|id), data = feedsperdaymlf)
summary(mlfmodel)
#
# Estimate Std. Error      df t value Pr(>|t|)    
# (Intercept)          14.5518     2.7714 37.7934   5.251 6.14e-06 ***
#   sqrt(feedsintheday)   1.4914     0.9579 78.8570   1.557    0.124  
