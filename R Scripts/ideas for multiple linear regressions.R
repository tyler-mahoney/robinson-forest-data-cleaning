
#from R help article "how to run linear regression models at once", answer by djmuseR
#create a vector of covariate names (in this case, precip) 

x <- c("Camp_p", "FR_B_p", "HFR_p", "LM_p", "MSC_p", "RIDGE_p")

#initialize list whose length is that of vector x

rl1 <- vector("list", 6)
rl2 <- vector("list", 6)
rl3 <- vector("list", 6)
rl4 <- vector("list", 6)

for(i in 1:6){
	fm1 <- as.formula(paste("FR_s", x[i], sep = "~"))
	fm2 <- as.formula(paste("LM_s", x[i], sep = "~"))
	fm3 <- as.formula(paste("Clemons_s", x[i], sep = "~"))
	fm4 <- as.formula(paste("Coles_s", x[i], sep = "~"))
	rl1[[i]] <- lm(fm1, data = NO3)
	rl2[[i]] <- lm(fm2, data = NO3)
	rl3[[i]] <- lm(fm3, data = NO3)
	rl4[[i]] <- lm(fm4, data = NO3)
	}

library(plyr) 
# R^2 values: 
ldply(rl1, function(x) summary(x)$r.squared) 

# Model coefficients: 
ldply(rl1, function(x) coef(x)) 

# p-values of significance tests for intercept and slope 
ldply(rl1, function(x) summary(x)$coefficients[, 4]) 

# residuals from each model 
res1 <- t(ldply(rl1, function(x) resid(x)))   # produces a matrix 

##Another example, from answer to question "how to run lm regression for 
#every column in R"

library(tidyverse)
library(broom)
df <- data.frame(x=rnorm(100),y1=rnorm(100),y2=rnorm(100))

result <- df %>% 
  gather(measure, value, -x) %>%
  nest(-measure) %>%
  mutate(fit = map(data, ~ lm(value ~ x, data = .x)),
         tidied = map(fit, tidy)) %>%
  unnest(tidied)


##Another example, from answer to question "how can I operate linear
#regression for each column in a dataframe?

fit <- lm(cbind(le.1, le.2, le.3, le.4) ~ samples.L + samples.T, data = data)
summary(fit) %>% map_dfr(glance)
summary(fit) %>% map_dfr(tidy)


