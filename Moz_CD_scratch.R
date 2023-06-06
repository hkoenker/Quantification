# Mozambique CD Graphs 

library(tidyverse)
library(haven)
library(janitor)


read_dta("output/runs/229.dta") %>% 
  filter(iso3=="MOZ") %>% 
  select(iso3, accrk, accub_npcub, acclb_npclb, year, retention, scenario) %>% 
  ggplot(aes(y=accrk, x=year, color=iso3, fill=iso3)) +
  geom_line() +
  geom_ribbon(aes(ymin=acclb_npclb, ymax=accub_npcub), alpha=.2, color="white") +
  theme_minimal() +
  scale_color_brewer(palette="Set2") +
  scale_fill_brewer(palette="Set2") +
  theme(legend.position = "none") +
  scale_y_continuous(breaks=seq(0,100, by=20), limits=c(0,100)) +
labs(x="",
       y="Estimated population ITN access",
       title="Full scale CD at population x 29% annually")
ggsave("figs/moz/moz_229.png")

read_dta("output/runs/217.dta") %>% 
  filter(iso3=="TGO") %>% 
  select(iso3, accrk, accub_npcub, acclb_npclb, year, retention, scenario) %>% 
  ggplot(aes(y=accrk, x=year, color=iso3, fill=iso3)) +
  geom_line() +
  geom_ribbon(aes(ymin=acclb_npclb, ymax=accub_npcub), alpha=.2, color="white") +
  theme_minimal() +
  scale_color_brewer(palette="Set2") +
  scale_fill_brewer(palette="Set2") +
  theme(legend.position = "none") +
  scale_y_continuous(breaks=seq(0,100, by=20), limits=c(0,100)) +
  labs(x="",
       y="Estimated population ITN access",
       title="Full scale CD at population x 17% annually")
ggsave("figs/moz/tgo_217.png")

read_dta("output/runs/105.dta") %>% 
  filter(iso3=="MOZ") %>% 
  select(iso3, accrk, accub_npcub, acclb_npclb, year, retention, scenario) %>% 
  ggplot(aes(y=accrk, x=year, color=iso3, fill=iso3)) +
  geom_line() +
  geom_ribbon(aes(ymin=acclb_npclb, ymax=accub_npcub), alpha=.2, color=NA) +
  theme_minimal() +
  scale_color_brewer(palette="Set2") +
  scale_fill_brewer(palette="Set2") +
  theme(legend.position = "none") +
  scale_y_continuous(breaks=seq(0,100, by=20), limits=c(0,100)) +
  labs(x="",
       y="Estimated population ITN access",
       title="3-year campaigns at pop/1.8")
ggsave("figs/moz/moz_3ucc.png")

read_dta("output/runs/105.dta") %>% 
  filter(iso3=="TGO") %>% 
  select(iso3, accrk, accub_npcub, acclb_npclb, year, retention, scenario) %>% 
  ggplot(aes(y=accrk, x=year, color=iso3, fill=iso3)) +
  geom_line() +
  geom_ribbon(aes(ymin=acclb_npclb, ymax=accub_npcub), alpha=.2, color=NA) +
  theme_minimal() +
  scale_color_brewer(palette="Set2") +
  scale_fill_brewer(palette="Set2") +
  theme(legend.position = "none") +
  scale_y_continuous(breaks=seq(0,100, by=20), limits=c(0,100)) +
  labs(x="",
       y="Estimated population ITN access",
       title="3-year campaigns at pop/1.8")
ggsave("figs/tgo/tgo_3ucc.png")


pypmz <- read_dta("output/totalnetspyp_realpop.dta") %>% 
  clean_names() %>% 
  filter(iso3=="MOZ") %>% 
  mutate(bestlabel=case_when(group==4 ~ paste0("pop/",round((scenario-400)/10, digits=1), " \nevery 3 years"),
                             group==5 ~ paste0("pop/",round((scenario-500)/10, digits=1), " \nevery 2 years"),
                             group==6 ~ "pop/1.8 \nevery 2 years",
                             group==1 ~ paste0("pop/1.8\nevery 3 years"),
                             group==2 ~ paste0("annual CD \nat pop x ",(scenario-200),"%"),
                             group==3 ~ paste0("CD between campaigns \nat pop x ",(scenario-300),"%"),
                             TRUE ~ "")) %>% 
  mutate(bestlabel=case_when(bestlabel=="pop/1 every 2 years" ~ "pop/1.0 \nevery 2 years",
                             bestlabel=="pop/2 every 2 years" ~ "pop/2.0 \nevery 2 years",
                             bestlabel=="pop/1 every 3 years" ~ "pop/1.0 \nevery 3 years",
                             bestlabel=="pop/2 every 3 years" ~ "pop/2.0 \nevery 3 years",
                             TRUE ~ as.character(bestlabel)),
         wrap_lbl = stringr::str_wrap(bestlabel, width = 25),
         iso_a2=countrycode(iso3, origin = "iso3c", destination="iso2c")) %>% 
  left_join(rettimes)

table80t <- read_csv("output/table80.csv", show_col_types = FALSE) %>% 
  mutate(iso3=countrycode(name, origin="country.name", destination="iso3c"))

## merge in the recommended quantifiers from the 80% target table into the PYP df:
pypmz80 <- pypmz %>% 
  left_join(table80t, by="iso3") %>% 
  mutate(scenario_2=200+scenario_2,
         scenario_3=300+scenario_3,
         scenario_4=400+10*scenario_4,
         scenario_5=500+10*scenario_5,
         # scenario_6=600+10*scenario_6,
         bestq=case_when(scenario==107 ~ 1,
                         scenario==scenario_2 ~ 1,
                         scenario==scenario_3 ~ 1,
                         scenario==scenario_4 ~ 1,
                         scenario==scenario_5 ~ 1,
                         scenario==606 ~ 1,
                         TRUE ~ 0),
         pyppitn=pyp/totalnets)

pypmz80 %>% 
  ggplot() +
  geom_point(aes(x=scenario, y=pyppitn, color=as.factor(group), alpha=best)) +
  theme_minimal() +
  scale_alpha_binned(range=c(.1,1))

#---- Compare vs other countries retention times ----

pypmulti <- read_dta("output/totalnetspyp.dta") %>% 
  clean_names() %>% 
  filter(iso3 %in% c("MOZ","TOG","TZA", "COG", "CMR")) %>% 
  mutate(bestlabel=case_when(group==4 ~ paste0("pop/",round((scenario-400)/10, digits=1), " \nevery 3 years"),
                             group==5 ~ paste0("pop/",round((scenario-500)/10, digits=1), " \nevery 2 years"),
                             group==6 ~ "pop/1.8 \nevery 2 years",
                             group==1 ~ paste0("pop/1.8\nevery 3 years"),
                             group==2 ~ paste0("annual CD \nat pop x ",(scenario-200),"%"),
                             group==3 ~ paste0("CD between campaigns \nat pop x ",(scenario-300),"%"),
                             TRUE ~ "")) %>% 
  mutate(bestlabel=case_when(bestlabel=="pop/1 every 2 years" ~ "pop/1.0 \nevery 2 years",
                             bestlabel=="pop/2 every 2 years" ~ "pop/2.0 \nevery 2 years",
                             bestlabel=="pop/1 every 3 years" ~ "pop/1.0 \nevery 3 years",
                             bestlabel=="pop/2 every 3 years" ~ "pop/2.0 \nevery 3 years",
                             TRUE ~ as.character(bestlabel)),
         wrap_lbl = stringr::str_wrap(bestlabel, width = 25),
         iso_a2=countrycode(iso3, origin = "iso3c", destination="iso2c")) %>% 
  left_join(rettimes)

table80t <- read_csv("output/table80.csv", show_col_types = FALSE) %>% 
  mutate(iso3=countrycode(name, origin="country.name", destination="iso3c"))

## merge in the recommended quantifiers from the 80% target table into the PYP df:
pypmulti80 <- pypmulti %>% 
  left_join(table80t, by="iso3") %>% 
  mutate(scenario_2=200+scenario_2,
         scenario_3=300+scenario_3,
         scenario_4=400+10*scenario_4,
         scenario_5=500+10*scenario_5,
         # scenario_6=600+10*scenario_6,
         bestq=case_when(scenario==107 ~ 1,
                         scenario==scenario_2 ~ 1,
                         scenario==scenario_3 ~ 1,
                         scenario==scenario_4 ~ 1,
                         scenario==scenario_5 ~ 1,
                         scenario==606 ~ 1,
                         TRUE ~ 0),
         pyppitn=pyp/totalnets) %>% 
  filter(bestq==1) %>% 
  filter(group<3)

pypmulti80 %>% 
  mutate(group=case_when(group==1 ~ "3-year campaigns",
                         group==2 ~ "Full-scale CD to maintain 80% ITN access",
                         TRUE ~ as.character(group))) %>% 
  ggplot() +
  geom_col(aes(x=as.factor(lifespan), y=totalnets, fill=iso3), position=position_dodge()) +
  theme_minimal() +
  scale_y_continuous(labels = unit_format(unit = "M", scale = 1e-6)) +
  facet_wrap(~group) +
  labs(x="ITN retention time (years)",
       y="Total nets required in pop of 10m",
       fill="")
ggsave("figs/moz/compare.png")  
