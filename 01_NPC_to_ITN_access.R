#### Script 1: ITNs per capita (NPC) into ITN access transformation

library(tidyverse)
library(haven)
library(quantreg)

## Read in the regional level hhsize, access, npc data:

Ddta <- read_dta("/Users/hannahkoenker/Dropbox/A DHS MIS Datasets/Analysis/Analysis ITN Indicators compiled/regional_ITN_indicators.dta") 


D <- with(Ddta, data.frame(y = access_merg, x = itnpers, z = meanhh))

A <- as.matrix(D)
B <- as.data.frame(A)

taus <- 1:3/4 # make list of 0.25, .5, .75 for confidence bounds
xx <- seq(min(B[,2]), max(B[,2]),length=100) # make list of values .01-0.99

## Quantile regression:

G <- matrix(0,length(xx),length(taus))
for(i in 1:length(taus)){
  f <- rqss(y ~ qss(x, constraint = "I", lambda = .5), tau = taus[i], data = B)
  G[,i] <- predict(f, newdata = list(x = xx))
}

#### 
## Make some subsets of the data by hh size groupings that we felt were worthwhile
## keep the 4 highest npc/access pairs available for all datasets

highest <- B %>% 
  filter(x>.80)

small <- B %>% 
  filter(z<4) %>% 
  bind_rows(highest) %>% 
  arrange(x)
med1  <- B %>% 
  filter(z>=4 & z<5) 
med2  <- B %>% 
  filter(z>=5 & z<6) 
large <- B %>% 
  filter(z>=6) %>% 
  bind_rows(highest) %>% 
  arrange(x)

taus <- 1:3/4 # make list of 0.25, .5, .75 for confidence bounds

## Small hh size countries:
xs <- seq(min(small[,2]), max(small[,2]),length=100) # make list of values .01-0.99
S1 <- matrix(0,length(xs),length(taus))
for(i in 1:length(taus)){
  f <- rqss(y ~ qss(x, constraint = "I", lambda = .5), tau = taus[i], data = small)
  S1[,i] <- predict(f, newdata = list(x = xs))
}

## Medium hh size countries
xm1 <- seq(min(med1[,2]), max(med1[,2]),length=100) # make list of values .01-0.99
S2 <- matrix(0,length(xm1),length(taus))
for(i in 1:length(taus)){
  f <- rqss(y ~ qss(x, constraint = "I", lambda = .5), tau = taus[i], data = med1)
  S2[,i] <- predict(f, newdata = list(x = xm1))
}

## Medium2 hh size countries
xm2 <- seq(min(med2[,2]), max(med2[,2]),length=100) # make list of values .01-0.99
S3 <- matrix(0,length(xm2),length(taus))
for(i in 1:length(taus)){
  f <- rqss(y ~ qss(x, constraint = "I", lambda = .5), tau = taus[i], data = med2)
  S3[,i] <- predict(f, newdata = list(x = xm2))
}

## Large hh size countries
xl <- seq(min(large[,2]), max(large[,2]),length=100) # make list of values .01-0.99
S4 <- matrix(0,length(xl),length(taus))
for(i in 1:length(taus)){
  f <- rqss(y ~ qss(x, constraint = "I", lambda = .5), tau = taus[i], data = large)
  S4[,i] <- predict(f, newdata = list(x = xl))
}

# Save as dataframes and create npc variable (will be divided by 100 and rounded later in Stata)
S1 <- as.data.frame(S1) %>%
  mutate(hhsize=0,
         npc=row_number()) # small
S2 <- as.data.frame(S2) %>%  
  mutate(hhsize=1,
         npc=row_number()) # med1
S3 <- as.data.frame(S3) %>% 
  mutate(hhsize=2,
         npc=row_number()) # med2
S4 <- as.data.frame(S4) %>% 
  mutate(hhsize=3,
         npc=row_number()) # large

fithhsize <- S1 %>% 
  bind_rows(S2, S3, S4)

write.csv(fithhsize, "data/npc_access/itnpers_into_access_transformation_hh.csv")
