
## Script which is currently the code chunk for one of my papers to produce a plot including "npc" at the end

## From RKoenker's script to produce the original graph 

## Why do we not use "z" (mean hh size) in the fitting of the lines; I would like to be able to pass this value to the
## calculation when I'm converting nets-per-capita to ITN access for different countries. It wouldn't necessarily have to be 
## super specific; see final plot where I manually cut hhsize into five groups to see how the fit differs.

library(tidyverse) # sorry, I have some ggplot and pipes at the end
library(haven) 
library(quantreg)

Ddta <- read_dta("/Users/hannahkoenker/Dropbox/A DHS MIS Datasets/Analysis/Analysis ITN Indicators compiled/regional_ITN_indicators.dta") 


D <- with(Ddta, data.frame(y = access_merg, x = itnpers, z = meanhh))

A <- as.matrix(D)
B <- as.data.frame(A)

taus <- 1:3/4 # make list of 0.25, .5, .75 for confidence bounds
xx <- seq(min(B[,2]), max(B[,2]),length=100) # make list of values .01-0.99
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

write.csv(fithhsize, "data/itnpers_into_access_transformation_hh.csv")



### Make G into a dataframe and rename the variables for ggplot later: 
gg <- as.data.frame(G) %>% 
  mutate(access=V2,
         lb=V1,
         ub=V3) %>% 
  arrange(access) %>% 
  mutate(npc=1:n()/100) %>% # make a new variable which is the row count and also NPC because Dad did not include this!
  mutate(delta=ub-lb)

npc <- ggplot() +
  geom_point(data=B, aes(x,y), alpha=0.5, shape=1, size=0.5) +
  geom_line(data=gg, aes(npc, access)) +
  geom_line(data=gg, aes(npc, lb, color="50% confidence bounds")) +
  geom_line(data=gg, aes(npc, ub, color="50% confidence bounds")) +
  theme_minimal() +
  theme(legend.position="bottom",
        text = element_text(size = 9)) +
  labs(x="ITNs per capita (NPC)",
       y="% ITN Access",
       color="")

fithhsize %>% 
  mutate(npcd=npc/100) %>% 
  ggplot() +
  geom_line(aes(npcd, V2, color=as.factor(hhsize))) +
  geom_ribbon(aes(npcd, ymin=V1, ymax=V3, fill=as.factor(hhsize)), alpha=0.2) +
  labs(x="Nets per capita",
       y="Estimated bednet access",
       color="Avg household size")+
  scale_color_hue(labels=c("<4","4-4.9","5-5.9",">=6")) +
  guides(fill="none")

# title="ITN Access as a function \n of nets per capita")


broken <- Ddta %>%
  # mutate(hhsize = factor(round_half_up(meanhh))) %>%
  mutate(hhsize = factor(cut(meanhh, breaks=c(1,4,5,6,14), include.lowest=TRUE))) 

## check what's included in brackets; but very long decimal points so all quite clearly in bins.
bcheck <- broken %>% 
  dplyr::select(meanhh, hhsize) %>% 
  arrange(meanhh)

broken %>%
  # filter(hhsize=="(1,4]") %>%
  # mutate(hhsize = factor(round_half_up(meanhh))) %>%
  # mutate(hhsize = factor(cut(meanhh, breaks=c(2,3,4,5,6,7,8,10,14)))) %>% 
  ggplot(aes(x = itnpers, y = access_merg, color=hhsize)) +
  geom_point(alpha = 0.5, shape=1) +
  stat_smooth(method="loess", size=1.2, aes(color=hhsize), alpha=0.3) +
  theme_classic()

Ddta %>%
  # mutate(hhsize = factor(round_half_up(meanhh))) %>%
  mutate(hhsize = factor(cut(meanhh, breaks=c(2,4,6,8,10,14)))) %>% 
  ggplot() +
  # geom_point(aes(x = itnpers, y = access_merg, color=hhsize), alpha = 0.1) +
  stat_smooth(aes(x = itnpers, y = access_merg, color=hhsize), method="loess", size=1.2, alpha=0.3) +
  theme_classic() +
  # geom_point(data=B, aes(x,y), alpha=0.1, shape=1, size=0.5) +
  geom_line(data=gg, aes(npc, access)) +
  geom_line(data=gg, aes(npc, lb, color="50% confidence bounds"), size=1) +
  geom_line(data=gg, aes(npc, ub, color="50% confidence bounds"), size=1)

## IF LOOP: ALSO NOT WORKING 
slist <- list(small,med1,med2,large) 
n <- 0
for(d in 1:length(slist)) {
  taus <- 1:3/4 # make list of 0.25, .5, .75 for confidence bounds
  
  xx <- seq(min(d[,2]), max(d[,2]),length=100) # make list of values .01-0.99 from within the dataset
  
  # dfname <- paste0("S",n)
  Sn <- matrix(0,length(xx),length(taus))
  
  for(i in 1:length(taus)){
    f <- rqss(y ~ qss(x, constraint = "I", lambda = .5), tau = taus[i], data = d)
    Sn[,i] <- predict(f, newdata = list(x = xx))
  }
}

## FUNCTION: Not sure how to make the naming of the 'list' (was G) work:

fitit <- function(df, dfname) {
  
  taus <- 1:3/4 # make list of 0.25, .5, .75 for confidence bounds
  
  xx <- seq(min(df[,2]), max(df[,2]),length=100) # make list of values .01-0.99

  dfname <- matrix(0,length(xx),length(taus))

  

  for(i in 1:length(taus)){
    f <- rqss(y ~ qss(x, constraint = "I", lambda = .5), tau = taus[i], data = df)
    dfname[,i] <- predict(f, newdata = list(x = xx))
  }
}
