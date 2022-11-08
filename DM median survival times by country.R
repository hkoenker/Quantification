## Standalone plot of DM and MAP retention/median survival times

library(tidyverse)
library(haven)
library(janitor)

## Read in MAP retention times 
ls <- read_excel("data/MAP_retentiontime_DM_medianlifespan.xlsx", sheet="Sheet1") %>%
  clean_names() %>%
  filter(type=="MAP") %>% 
  drop_na(country) %>%
  rename(lifespan=median_retention_years) %>%
  separate(x95_percent_ci, into = c("lb", "ub"),
           sep = "\\s*(–|-)\\s*", convert = TRUE) %>% 
  select(-site)

dm <- read_excel("../DM Maps/Median lifespan DM multisite.xlsx") %>% 
  clean_names() %>% 
  rename(lifespan=medianlifespan) %>%
  separate(x95_percent_ci, into = c("lb", "ub"),
           sep = "\\s*(–|-)\\s*", convert = TRUE) %>% 
  drop_na(lifespan) %>% 
  select(country, lifespan, lb, ub) %>% 
  mutate(type="DM")

lsdm <- ls %>% 
  bind_rows(dm) %>% 
  mutate(iso3=countrycode(country, origin="country.name", destination="iso3c")) %>% 
  mutate(iso3=case_when(type=="MAP" ~ as.character(iso3)))

levels <- lsdm$country[ls$type=='MAP'][rev(order(ls$lifespan[ls$type=='MAP']))]
lsdm$country_ord <- factor(lsdm$country, levels = levels)

# save a small version of retention times for joining into results tables:
rettimes <- lsdm %>% 
  filter(type=="MAP") %>% 
  mutate(name=case_when(country=="Burkina" ~ "Burkina Faso",
                        # country=="Cote d'Ivoire" ~ "Côte d'Ivoire",
                        country=="Congo (Republic of)" ~ "Rep. Congo",
                        TRUE~as.character(country))) %>% 
  dplyr::select(-type, -country, -lb, -ub, -both, -country_ord) %>% 
  mutate(name=stri_trans_general(name,"Latin-ASCII")) %>% # remove o-hat from Cote d'Ivoire
  mutate(iso_a2 = countrycode(name, origin = "country.name", destination = "iso2c"))

a2levels <- rettimes$iso_a2 # make a list of the isoa2 countries included in retention times for later

lsdm %>%
  ggplot() +
  geom_point(aes(
    y = lifespan,
    x = country_ord,
    color = type,
    alpha = type,
    shape = type)) +
  # ,
  # alpha=0.8) 
  geom_linerange(aes(
    x = country_ord,
    ymin = lb,
    ymax = ub,
    color = type
    # alpha = both
  ),
  alpha=0.8) +
  theme_classic() +
  scale_alpha_discrete(range=c(1,0), guide="none") +
  # scale_alpha_continuous(range = c(0.25, 1), guide = "none") +
  scale_color_hue(
    # palette="Dark2",
    name = "Study Type", 
    labels = c("DM", "MAP")) +
  scale_shape_manual(
    name = "Study Type",
    labels = c("DM", "MAP"),
    values = c(19, 0)
  ) + # if guide=="none" here, shape won't appear correctly in legend
  geom_text(aes(
    y=lifespan,
    x=country_ord,
    color=type,
    label=iso3),
    size=1,
  ) +
  theme(legend.position="right",
        text = element_text(size = 9),
        axis.title.y = element_text(size=8),
        #       axis.text.x = element_text(
        #   angle = 45,
        #   vjust = 0.9,
        #   hjust = 1,
        #   size = 7
        # ),
        axis.text.x=element_blank()) +
  labs(x = "",
       y = "Retention / median lifespan (years)") +
  scale_y_continuous(breaks=seq(0,6, by=1))


ggsave("figs/MAP_vs_DM_lifespans.png", width=6, height=5, dpi=300)