
## Script which is currently the code chunk for one of my papers to produce a plot including "npc" at the end

## From RKoenker's script to produce the original graph 

## Why do we not use "z" (mean hh size) in the fitting of the lines; I would like to be able to pass this value to the
## calculation when I'm converting nets-per-capita to ITN access for different countries. It wouldn't necessarily have to be 
## super specific; see final plot where I manually cut hhsize into five groups to see how the fit differs.

library(tidyverse) # sorry, I have some ggplot and pipes at the end

Ddta <- read_dta("/Users/hannahkoenker/Dropbox/A DHS MIS Datasets/Analysis/Analysis ITN Indicators compiled/regional_ITN_indicators.dta") 


D <- with(Ddta, data.frame(y = access_merg, x = itnpers, z = meanhh))

A <- as.matrix(D)
B <- as.data.frame(A)

# Dad's plot:
## Ideally I'd like to have code for this that just passes everything to a dataframe. If I turn off the pdf->dev(off) section, I still get dataframe G, but
## only the first column has any data. 

pdf(
  "Access_NPC.pdf",
  height = 6,
  width = 6,
)
with(B,
     plot(
       x,
       y,
       cex = .5,
       col = "grey",
       ylim = c(0, 105),
       xlab = "ITNs per capita",
       ylab = "Population ITN Access (percent)")
)
# title(main = "Title")

taus <- 1:3/4 # make list of 0.25, .5, .75 for confidence bounds

xx <- 1:99/100 # make list of values .01-0.99
colors <- c(2,1,2)

G <- matrix(0,99,length(taus))

for(i in 1:length(taus)){
  f <- rqss(y ~ qss(x, constraint = "I", lambda = .5), tau = taus[i], data = B)
  G[,i] <- predict(f, newdata = list(x = xx))
  lines(xx,G[,i], col = colors[i], lwd = 2)
}
invisible(dev.off()) # wrapped in invisible otherwise it prints in my draft paper output.


### Make G into a dataframe and rename the variables for ggplot later: 
gg <- as.data.frame(G) %>% 
  mutate(access=V2,
         lb=V1,
         ub=V3) %>% 
  arrange(access) %>% 
  mutate(npc=1:n()/100) %>% # make a new variable which is the row count and also NPC because Dad did not include this!
  mutate(delta=ub-lb)

npc <- ggplot() +
  geom_point(data=B, aes(x,y), alpha=0.1, shape=1, size=0.5) +
  geom_line(data=gg, aes(npc, access)) +
  geom_line(data=gg, aes(npc, lb, color="50% confidence bounds")) +
  geom_line(data=gg, aes(npc, ub, color="50% confidence bounds")) +
  theme_minimal() +
  theme(legend.position="bottom",
        text = element_text(size = 9)) +
  labs(x="ITNs per capita (NPC)",
       y="% ITN Access",
       color="")
# title="ITN Access as a function \n of nets per capita")

## I would like to have something that would accommodate the differences we see in the NPC-access relationship by hhsize, e.g.:

Ddta %>%
  # mutate(hhsize = factor(round_half_up(meanhh))) %>%
  mutate(hhsize = factor(cut(meanhh, breaks=c(2,4,6,8,10,14)))) %>% 
  ggplot(aes(x = itnpers, y = access_merg, color=hhsize)) +
  geom_point(alpha = 0.1) +
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
