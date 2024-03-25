#----- 900 R Script for Prepping csv files for ITN Quantification Website components ----

#----Purpose ----
# This file will run code used in the 2023 ITN Quantification paper by Koenker et al and produce csv files for use in the
# ITN Quantification Website 


#---- Load Libraries ----
library(tidyverse)
library(haven)
library(readxl)
library(janitor)
library(viridis)
library(broom)
library(english)
library(magrittr)
library(lubridate)
library(countrycode)
library(data.table)
library(todor)

# Load map related libraries
library(sf)
library(tmap)
library(maptools)
library(maps)
library(spData)
library(distill)
library(geofacet)

# Load tables and plot libraries
library(flextable)
library(scales) # for heatmap in flextable
library(officer) # for setting color of vlines in flextable
library(gtsummary)
library(gt)
library(jtools)
library(kableExtra)
library(float)
library(ggstance)
library(knitr)
library(stringr)
library(stringi) # for removing accents from country names
library(ggpubr) # includes ggarrange and cowplot 
library(patchwork) # for insetting plots and nicer combos than ggarrange and cowplot, somehow
library(ggrepel)

# Load QR libraries
library(quantreg)
library(splines)

#---- Extract Africa population data from UN WPP 2022 Demographic Indicators Medium File ----
# (https://population.un.org/wpp/Download/Standard/CSV/)

## Read in MAP retention times 
ls <- read_excel("data/MAP_retentiontime_DM_medianlifespan.xlsx", sheet="Sheet1") %>%
  clean_names() %>%
  filter(type=="MAP") %>% 
  filter(country!="Myanmar") %>% 
  drop_na(country) %>%
  rename(lifespan=median_retention_years) %>%
  separate(x95_percent_ci, into = c("lb", "ub"),
           sep = "\\s*(–|-)\\s*", convert = TRUE) %>% 
  select(-site) %>% 
  mutate(iso3=countrycode(country, origin="country.name", destination="iso3c"))

dm <- read_excel("../DM Maps/Median lifespan DM multisite.xlsx") %>% 
  clean_names() %>% 
  filter(country!="Myanmar") %>% 
  rename(lifespan=medianlifespan) %>%
  separate(x95_percent_ci, into = c("lb", "ub"),
           sep = "\\s*(–|-)\\s*", convert = TRUE) %>% 
  drop_na(lifespan) %>% 
  select(country, lifespan, lb, ub) %>% 
  mutate(type="DM",
         country=case_when(country=="Democratic Republic of Congo" ~ "DRC",
                           TRUE ~ as.character(country)))

lsdm <- ls %>% 
  bind_rows(dm) %>% 
  mutate(iso3=countrycode(country, origin="country.name", destination="iso3c")) 

levels <- lsdm$country[ls$type=='MAP'][rev(order(ls$lifespan[ls$type=='MAP']))]
lsdm$country_ord <- factor(lsdm$country, levels = levels)


levels3 <- lsdm$iso3[ls$type=='MAP'][rev(order(ls$lifespan[ls$type=='MAP']))]
lsdm$iso3ord <- factor(lsdm$iso3, levels = levels3)


# save a small version of retention times for joining into results tables:
rettimes <- lsdm %>% 
  filter(type=="MAP") %>% 
  mutate(name=case_when(country=="Burkina" ~ "Burkina Faso",
                        # country=="Cote d'Ivoire" ~ "Côte d'Ivoire",
                        country=="Congo (Republic of)" ~ "Rep. Congo",
                        TRUE~as.character(country))) %>% 
  dplyr::select(-type, -country, -lb, -ub, -both, -country_ord, -iso3ord) %>% 
  mutate(name=stri_trans_general(name,"Latin-ASCII")) %>% # remove o-hat from Cote d'Ivoire
  mutate(iso_a2 = countrycode(name, origin = "country.name", destination = "iso2c"))

a2levels <- rettimes$iso_a2 # make a list of the isoa2 countries included in retention times for later

afpop <- as.data.frame(read_csv("data/WPP2022_Demographic_Indicators_Medium.csv")) %>% 
  clean_names() %>% 
  select(iso2_code, iso3_code, location, t_population1july, time) %>% 
  mutate(t_population1july=t_population1july*1000) %>% 
  rename(pop=t_population1july,
         year=time,
         iso_a2=iso2_code,
         country_code=iso3_code,
         country_name=location) %>% 
  filter(year>2019 & year<2041) %>% 
  filter(iso_a2 %in% a2levels) %>% 
  pivot_wider(names_from=year, names_prefix="x", values_from=pop)

write_csv(afpop, "site/afpop.csv", col_names = TRUE)

#---- Read in the Stata files that have the minimum quantifiers for each scenario and target access level -----
s2min <- read_dta("data/s2_min_npp.dta") %>% 
  clean_names()

s3min <- read_dta("data/s3_min_npp.dta") %>% 
  clean_names()

s4min <- read_dta("data/s4_min_acc.dta") %>% 
  clean_names() 

s5min <- read_dta("data/s5_min_acc.dta") %>% 
  clean_names() 

t2 <- s2min %>% 
  pivot_wider(names_from = target, values_from = q) %>% 
  relocate(`70`, .before = `80`) %>% 
  relocate(`90`, .after = `80`) %>% 
  #mutate(scenario = "s2")  
  rename("s270" = `70`,
         "s280" = `80`,
         "s290" = `90`)

t3 <- s3min %>% 
  pivot_wider(names_from = target, values_from = q) %>% 
  relocate(`70`, .before = `80`) %>% 
  relocate(`90`, .after = `80`) %>% 
  #mutate(scenario = "s3") 
  rename("s370" = `70`,
         "s380" = `80`,
         "s390" = `90`)

# join tables
table_q_s2s3long <- t2 %>% 
  left_join(t3, by = "iso_a2") %>% 
  arrange(iso_a2) %>%
  left_join(rettimes) %>% 
  mutate(lifespan=round(lifespan, digits=1),
         iso3=countrycode(iso_a2, origin="iso2c", destination="iso3c")) %>% 
  dplyr::select(iso3, lifespan, s270, s280, s290, s370, s380, s390) %>% 
  arrange(lifespan)

write_csv(table_q_s2s3long, "site/table_q_s2s3long.csv", col_names = TRUE)

#---- Read in Campaign Scenario ----
# Note that these only target the 80% access minimum between campaigns. Not sure we will use them.
t4 <- read_dta("data/s4q.dta") %>% 
  rename(s4=best) %>% 
  mutate(s4=(s4-400)/10)

t5 <- read_dta("data/s5q.dta") %>% 
  rename(s5=best) %>% 
  mutate(s5=(s5-500)/10)

t470 <- read_dta("data/s4q70.dta") %>% 
  rename(s4=best) %>% 
  mutate(s4=(s4-400)/10)

t490 <- read_dta("data/s4q90.dta") %>% 
  rename(s4=best) %>% 
  mutate(s4=(s4-400)/10)

t570 <- read_dta("data/s5q70.dta") %>% 
  rename(s5=best) %>% 
  mutate(s5=(s5-500)/10)

t590 <- read_dta("data/s5q90.dta") %>% 
  rename(s5=best) %>% 
  mutate(s5=(s5-500)/10)

#---- Make the full table for 80% Access Target  ----
qt80 <- table_q_s2s3long %>% 
  select(iso3, s280, s380) %>% # just keep the target=80 columns for Scen 2 & 3, for merging below
  mutate(iso_a2 = countrycode(iso3, origin="iso3c", destination="iso2c"))


sumtable <- t4 %>% 
  right_join(t5) %>% 
  right_join(qt80) %>% 
  mutate(name = countrycode(iso_a2, origin = "iso2c", destination = "country.name")) %>% 
  arrange(iso_a2)

table80 <- sumtable %>% 
  select(name, s280, s380,s4, s5) %>% 
  rename(scenario_2 = s280,
         scenario_3 = s380,
         scenario_4=s4,
         scenario_5=s5) %>% 
  select(name, scenario_2, scenario_3, scenario_4, scenario_5)

table80rtlong <- table80 %>% 
  mutate(name=case_when(name=="Congo - Kinshasa" ~ "DRC",
                        name=="Congo - Brazzaville" ~ "Rep. Congo",
                        name=="Côte d'Ivoire" ~ "Cote d'Ivoire",
                        TRUE~as.character(name))) %>% 
  mutate(name=stri_trans_general(name,"Latin-ASCII")) %>% # remove o-hat from Cote d'Ivoire
  right_join(rettimes) %>% 
  filter(iso_a2!="MM") %>% 
  mutate(lifespan=round(lifespan, digits=1),
         iso3=countrycode(iso_a2, origin="iso2c", destination="iso3c")) %>% 
  arrange(iso3, scenario_3) %>% 
  select(-iso_a2) %>% 
  select(iso3, lifespan, scenario_2, scenario_3, scenario_4, scenario_5)%>% 
  mutate(target=80)





## ---- Table for 70% target ----

qt70 <- table_q_s2s3long %>% 
  select(iso3, s270, s370) %>% # just keep the target=80 columns for Scen 2 & 3, for merging below
  mutate(iso_a2 = countrycode(iso3, origin="iso3c", destination="iso2c"))


sumtable70 <- t470 %>% 
  right_join(t570) %>% 
  right_join(qt70) %>% 
  mutate(name = countrycode(iso_a2, origin = "iso2c", destination = "country.name")) %>% 
  arrange(iso_a2)

table70 <- sumtable70 %>% 
  select(name, s270, s370,s4, s5) %>% 
  rename(scenario_2 = s270,
         scenario_3 = s370,
         scenario_4=s4,
         scenario_5=s5) %>% 
  select(name, scenario_2, scenario_3, scenario_4, scenario_5)

table70rtlong <- table70 %>% 
  mutate(name=case_when(name=="Congo - Kinshasa" ~ "DRC",
                        name=="Congo - Brazzaville" ~ "Rep. Congo",
                        name=="Côte d'Ivoire" ~ "Cote d'Ivoire",
                        TRUE~as.character(name))) %>% 
  mutate(name=stri_trans_general(name,"Latin-ASCII")) %>% # remove o-hat from Cote d'Ivoire
  right_join(rettimes) %>% 
  filter(iso_a2!="MM") %>% 
  mutate(lifespan=round(lifespan, digits=1),
         iso3=countrycode(iso_a2, origin="iso2c", destination="iso3c")) %>% 
  arrange(iso3, scenario_3) %>% 
  select(-iso_a2) %>% 
  select(iso3, lifespan, scenario_2, scenario_3, scenario_4, scenario_5)%>% 
  mutate(target=70)


## ---- Table for 90% target ----

qt90 <- table_q_s2s3long %>% 
  select(iso3, s290, s390) %>% # just keep the target=80 columns for Scen 2 & 3, for merging below
  mutate(iso_a2 = countrycode(iso3, origin="iso3c", destination="iso2c"))


sumtable90 <- t490 %>% 
  right_join(t590) %>% 
  right_join(qt90) %>% 
  mutate(name = countrycode(iso_a2, origin = "iso2c", destination = "country.name")) %>% 
  arrange(iso_a2)

table90 <- sumtable90 %>% 
  select(name, s290, s390,s4, s5) %>% 
  rename(scenario_2 = s290,
         scenario_3 = s390,
         scenario_4=s4,
         scenario_5=s5) %>% 
  select(name, scenario_2, scenario_3, scenario_4, scenario_5)

table90rtlong <- table90 %>% 
  mutate(name=case_when(name=="Congo - Kinshasa" ~ "DRC",
                        name=="Congo - Brazzaville" ~ "Rep. Congo",
                        name=="Côte d'Ivoire" ~ "Cote d'Ivoire",
                        TRUE~as.character(name))) %>% 
  mutate(name=stri_trans_general(name,"Latin-ASCII")) %>% # remove o-hat from Cote d'Ivoire
  right_join(rettimes) %>% 
  filter(iso_a2!="MM") %>% 
  mutate(lifespan=round(lifespan, digits=1),
         iso3=countrycode(iso_a2, origin="iso2c", destination="iso3c")) %>% 
  arrange(iso3, scenario_3) %>% 
  select(-iso_a2) %>% 
  select(iso3, lifespan, scenario_2, scenario_3, scenario_4, scenario_5) %>% 
  mutate(target=90)


# bind the tables and write to CSV
table_qs <- table70rtlong %>% 
  bind_rows(table80rtlong, table90rtlong) %>% 
  mutate(country=countrycode(iso3, origin="iso3c", destination="country.name"))

write_csv(table_qs, "site/table_qs.csv", col_names = TRUE)




#TODO:  it would be ideal to turn this into a function (to turn tableXXrtlong into tnetsXX) but I don't have time to figure it out.

#---- Total Nets Needed 80% ----
tnets80 <- table80rtlong %>% 
  # mutate(scenario_4=na_if(scenario_4,0.1)) %>% 
  left_join(afpop, by=c("iso3"= "country_code")) %>% 
  mutate(routine=.06,
         basescen0=x2022/1.8+x2025/1.8+x2028/1.8+x2031/1.8,
         baseroutine=x2022*routine+x2023*routine+x2024*routine+x2025*routine+x2026*routine+x2027*routine+x2028*routine+x2029*routine+x2030*routine+x2031*routine+x2032*routine,
         basescen2=x2023*(scenario_2/100)+x2024*(scenario_2/100)+x2025*(scenario_2/100)+x2026*(scenario_2/100)+x2027*(scenario_2/100)+x2028*(scenario_2/100)+x2029*(scenario_2/100)+x2030*(scenario_2/100)+x2031*(scenario_2/100)+x2032*(scenario_2/100),
         basescen3_cd=x2023*(scenario_3/100)+x2024*(scenario_3/100)+x2026*(scenario_3/100)+x2027*(scenario_3/100)+x2029*(scenario_3/100)+x2030*(scenario_3/100)+x2032*(scenario_3/100),
         basescen3_mass=x2022/1.8+x2025/1.8+x2028/1.8+x2031/1.8,
         basescen4=x2022/scenario_4+x2025/scenario_4+x2028/scenario_4+x2031/scenario_4,
         basescen5=x2022/scenario_5+x2024/scenario_5+x2026/scenario_5+x2028/scenario_5+x2030/scenario_5+x2032/scenario_5,
         basescen6=x2022/1.8+x2024/1.8+x2026/1.8+x2028/1.8+x2030/1.8+x2032/1.8,
         # status quo - campaigns / 1.8 every three years starting in y1, routine all the time
         tot0=basescen0+baseroutine,
         #scenario 2 - campaign in y1, routine all the time, CD starts in Y3
         tot2=x2022/1.8+basescen2+baseroutine,
         # scenario 3 - campaign every three yrs starting y1, routine all the time, CD between campaigns
         tot3=basescen3_mass+basescen3_cd+baseroutine,
         # scenario 4 - campaign every three years starting y1, routine all the time
         tot4=basescen4+baseroutine,
         # scenario 5 - campaign every two years starting y1, routine all the time
         tot5=basescen5+baseroutine,
         # scenario 6 - campaign every two years pop/1.8, routine all the time 
         tot6=basescen6+baseroutine,
         s0vs2=tot2/tot0,
         s0vs3=tot3/tot0,
         s0vs4=tot4/tot0,
         s0vs5=tot5/tot0,
         s0vs6=tot6/tot0,
         tot0_routine=baseroutine,
         tot0_mass=basescen0,
         tot0_cd=0,
         tot2_routine=baseroutine,
         tot2_mass=x2022/1.8,
         tot2_cd=basescen2,
         tot3_routine=baseroutine,
         tot3_mass=basescen3_mass,
         tot3_cd=basescen3_cd,
         tot4_routine=baseroutine,
         tot4_mass=basescen4,
         tot4_cd=0,
         tot5_routine=baseroutine,
         tot5_mass=basescen5,
         tot5_cd=0,
         tot6_routine=baseroutine,
         tot6_mass=basescen6,
         tot6_cd=0
         ) %>% 
  select(-contains("base")) ## drop the intermediate variables

totalnets80 <- tnets80 %>% 
  select(iso3, lifespan, contains("tot")) %>% 
  pivot_longer(contains("tot"), names_to = "scenario", names_prefix = "tot", values_to = "totalnets")%>% 
  mutate(target=80)

# scenpct80 <- tnets80 %>% 
#   select(iso_a2, iso3, lifespan, contains("s0vs")) %>% # s2vs0, s2vs3, s2vs4, s2vs5
#   mutate(iso3=case_when(iso_a2=="ER" ~ "ERI",
#                         TRUE ~ as.character(iso3))) %>% 
#   mutate(s0vs0=1,
#          code=iso3) %>% 
#   pivot_longer(contains("s0"), names_to="scenario", names_prefix = "s0vs", values_to = "pct") %>% 
#   mutate(iso2=countrycode(code, origin="iso3c", destination="iso2c")) %>% 
#   mutate(name=countrycode(code, origin="iso3c", destination="country.name"))
# 
# avgscenpct80 <- scenpct80 %>% 
#   group_by(scenario) %>% 
#   summarize(mean=mean(pct, na.rm = TRUE)) 
# 
# scenpct_lifespan80 <- scenpct80 %>% 
#   mutate(glife=cut(lifespan, breaks = c(0,1.5, 2.0, 2.5, 3.0, 4) )) %>% 
#   group_by(scenario, glife) %>% 
#   summarize(mean=mean(pct, na.rm=TRUE))

#---- Total Nets Needed 70% ----
tnets70 <- table70rtlong %>% 
  # mutate(scenario_4=na_if(scenario_4,0.1)) %>% 
  left_join(afpop, by=c("iso3"= "country_code")) %>% 
  mutate(routine=.06,
         basescen0=x2022/1.8+x2025/1.8+x2028/1.8+x2031/1.8,
         baseroutine=x2022*routine+x2023*routine+x2024*routine+x2025*routine+x2026*routine+x2027*routine+x2028*routine+x2029*routine+x2030*routine+x2031*routine+x2032*routine,
         basescen2=x2023*(scenario_2/100)+x2024*(scenario_2/100)+x2025*(scenario_2/100)+x2026*(scenario_2/100)+x2027*(scenario_2/100)+x2028*(scenario_2/100)+x2029*(scenario_2/100)+x2030*(scenario_2/100)+x2031*(scenario_2/100)+x2032*(scenario_2/100),
         basescen3_cd=x2023*(scenario_3/100)+x2024*(scenario_3/100)+x2026*(scenario_3/100)+x2027*(scenario_3/100)+x2029*(scenario_3/100)+x2030*(scenario_3/100)+x2032*(scenario_3/100),
         basescen3_mass=x2022/1.8+x2025/1.8+x2028/1.8+x2031/1.8,
         basescen4=x2022/scenario_4+x2025/scenario_4+x2028/scenario_4+x2031/scenario_4,
         basescen5=x2022/scenario_5+x2024/scenario_5+x2026/scenario_5+x2028/scenario_5+x2030/scenario_5+x2032/scenario_5,
         basescen6=x2022/1.8+x2024/1.8+x2026/1.8+x2028/1.8+x2030/1.8+x2032/1.8,
         # status quo - campaigns / 1.8 every three years starting in y1, routine all the time
         tot0=basescen0+baseroutine,
         #scenario 2 - campaign in y1, routine all the time, CD starts in Y3
         tot2=x2022/1.8+basescen2+baseroutine,
         # scenario 3 - campaign every three yrs starting y1, routine all the time, CD between campaigns
         tot3=basescen3_mass+basescen3_cd+baseroutine,
         # scenario 4 - campaign every three years starting y1, routine all the time
         tot4=basescen4+baseroutine,
         # scenario 5 - campaign every two years starting y1, routine all the time
         tot5=basescen5+baseroutine,
         # scenario 6 - campaign every two years pop/1.8, routine all the time 
         tot6=basescen6+baseroutine,
         s0vs2=tot2/tot0,
         s0vs3=tot3/tot0,
         s0vs4=tot4/tot0,
         s0vs5=tot5/tot0,
         s0vs6=tot6/tot0,
         tot0_routine=baseroutine,
         tot0_mass=basescen0,
         tot0_cd=0,
         tot2_routine=baseroutine,
         tot2_mass=x2022/1.8,
         tot2_cd=basescen2,
         tot3_routine=baseroutine,
         tot3_mass=basescen3_mass,
         tot3_cd=basescen3_cd,
         tot4_routine=baseroutine,
         tot4_mass=basescen4,
         tot4_cd=0,
         tot5_routine=baseroutine,
         tot5_mass=basescen5,
         tot5_cd=0,
         tot6_routine=baseroutine,
         tot6_mass=basescen6,
         tot6_cd=0
  ) %>% 
  select(-contains("base")) ## drop the intermediate variables


totalnets70 <- tnets70 %>% 
  select(iso3, lifespan, contains("tot")) %>% 
  pivot_longer(contains("tot"), names_to = "scenario", names_prefix = "tot", values_to = "totalnets")%>% 
  mutate(target=70)

# scenpct70 <- tnets70 %>% 
#   select(iso_a2, iso3, lifespan, contains("s0vs")) %>% # s2vs0, s2vs3, s2vs4, s2vs5
#   mutate(iso3=case_when(iso_a2=="ER" ~ "ERI",
#                         TRUE ~ as.character(iso3))) %>% 
#   mutate(s0vs0=1,
#          code=iso3) %>% 
#   pivot_longer(contains("s0"), names_to="scenario", names_prefix = "s0vs", values_to = "pct") %>% 
#   mutate(iso2=countrycode(code, origin="iso3c", destination="iso2c")) %>% 
#   mutate(name=countrycode(code, origin="iso3c", destination="country.name"))
# 
# avgscenpct70 <- scenpct70 %>% 
#   group_by(scenario) %>% 
#   summarize(mean=mean(pct, na.rm = TRUE)) 
# 
# scenpct_lifespan70 <- scenpct70 %>% 
#   mutate(glife=cut(lifespan, breaks = c(0,1.5, 2.0, 2.5, 3.0, 4) )) %>% 
#   group_by(scenario, glife) %>% 
#   summarize(mean=mean(pct, na.rm=TRUE))


#---- Total Nets Needed 90% ----
tnets90 <- table90rtlong %>% 
  # mutate(scenario_4=na_if(scenario_4,0.1)) %>% 
  left_join(afpop, by=c("iso3"= "country_code")) %>% 
  mutate(routine=.06,
       basescen0=x2022/1.8+x2025/1.8+x2028/1.8+x2031/1.8,
       baseroutine=x2022*routine+x2023*routine+x2024*routine+x2025*routine+x2026*routine+x2027*routine+x2028*routine+x2029*routine+x2030*routine+x2031*routine+x2032*routine,
       basescen2=x2023*(scenario_2/100)+x2024*(scenario_2/100)+x2025*(scenario_2/100)+x2026*(scenario_2/100)+x2027*(scenario_2/100)+x2028*(scenario_2/100)+x2029*(scenario_2/100)+x2030*(scenario_2/100)+x2031*(scenario_2/100)+x2032*(scenario_2/100),
       basescen3_cd=x2023*(scenario_3/100)+x2024*(scenario_3/100)+x2026*(scenario_3/100)+x2027*(scenario_3/100)+x2029*(scenario_3/100)+x2030*(scenario_3/100)+x2032*(scenario_3/100),
       basescen3_mass=x2022/1.8+x2025/1.8+x2028/1.8+x2031/1.8,
       basescen4=x2022/scenario_4+x2025/scenario_4+x2028/scenario_4+x2031/scenario_4,
       basescen5=x2022/scenario_5+x2024/scenario_5+x2026/scenario_5+x2028/scenario_5+x2030/scenario_5+x2032/scenario_5,
       basescen6=x2022/1.8+x2024/1.8+x2026/1.8+x2028/1.8+x2030/1.8+x2032/1.8,
       # status quo - campaigns / 1.8 every three years starting in y1, routine all the time
       tot0=basescen0+baseroutine,
       #scenario 2 - campaign in y1, routine all the time, CD starts in Y3
       tot2=x2022/1.8+basescen2+baseroutine,
       # scenario 3 - campaign every three yrs starting y1, routine all the time, CD between campaigns
       tot3=basescen3_mass+basescen3_cd+baseroutine,
       # scenario 4 - campaign every three years starting y1, routine all the time
       tot4=basescen4+baseroutine,
       # scenario 5 - campaign every two years starting y1, routine all the time
       tot5=basescen5+baseroutine,
       # scenario 6 - campaign every two years pop/1.8, routine all the time 
       tot6=basescen6+baseroutine,
       s0vs2=tot2/tot0,
       s0vs3=tot3/tot0,
       s0vs4=tot4/tot0,
       s0vs5=tot5/tot0,
       s0vs6=tot6/tot0,
       tot0_routine=baseroutine,
       tot0_mass=basescen0,
       tot0_cd=0,
       tot2_routine=baseroutine,
       tot2_mass=x2022/1.8,
       tot2_cd=basescen2,
       tot3_routine=baseroutine,
       tot3_mass=basescen3_mass,
       tot3_cd=basescen3_cd,
       tot4_routine=baseroutine,
       tot4_mass=basescen4,
       tot4_cd=0,
       tot5_routine=baseroutine,
       tot5_mass=basescen5,
       tot5_cd=0,
       tot6_routine=baseroutine,
       tot6_mass=basescen6,
       tot6_cd=0
) %>% 
  select(-contains("base")) ## drop the intermediate variables

totalnets90 <- tnets90 %>% 
  select(iso3, lifespan, contains("tot")) %>% 
  pivot_longer(contains("tot"), names_to = "scenario", names_prefix = "tot", values_to = "totalnets") %>% 
  mutate(target=90)

# scenpct90 <- tnets90 %>% 
#   select(iso_a2, iso3, lifespan, contains("s0vs")) %>% # s2vs0, s2vs3, s2vs4, s2vs5
#   mutate(iso3=case_when(iso_a2=="ER" ~ "ERI",
#                         TRUE ~ as.character(iso3))) %>% 
#   mutate(s0vs0=1,
#          code=iso3) %>% 
#   pivot_longer(contains("s0"), names_to="scenario", names_prefix = "s0vs", values_to = "pct") %>% 
#   mutate(iso2=countrycode(code, origin="iso3c", destination="iso2c")) %>% 
#   mutate(name=countrycode(code, origin="iso3c", destination="country.name"))
# 
# avgscenpct90 <- scenpct90 %>% 
#   group_by(scenario) %>% 
#   summarize(mean=mean(pct, na.rm = TRUE)) 
# 
# scenpct_lifespan90 <- scenpct90 %>% 
#   mutate(glife=cut(lifespan, breaks = c(0,1.5, 2.0, 2.5, 3.0, 4) )) %>% 
#   group_by(scenario, glife) %>% 
#   summarize(mean=mean(pct, na.rm=TRUE))

# Bind total nets objects
totalnetsall <- totalnets70 %>% 
  bind_rows(totalnets80, totalnets90) %>% 
  mutate(scenlabel=case_when(scenario==0 ~ "Mass campaigns: Every 3 years using pop/1.8",
                             scenario==2 ~ "CD: Annual distributions",
                             scenario==3 ~ "CD: Between mass campaigns",
                             scenario==4 ~ "Mass campaigns: Every 3 years using tailored approach",
                             scenario==5 ~ "Mass campaigns: Every 2 years using tailored approach",
                             scenario==6 ~ "Mass campaigns: Every 2 years using pop/1.8"),
         country=countrycode(iso3, origin="iso3c", destination="country.name"))

write_csv(totalnetsall, "site/totalnetsall.csv", col_names = TRUE)


#--- Three year total nets 80 ----
# This will be 2024-2026 inclusive, as for Global Fund Grant Cycle 7
# With one mass campaign per cycle for campaign options , in 2025.
# No mass campaign in the full CD option

tnets_3y_80 <- table80rtlong %>% 
  # mutate(scenario_4=na_if(scenario_4,0.1)) %>% 
  left_join(afpop, by=c("iso3"= "country_code")) %>% 
  mutate(routine=.06,
         basescen0=x2025/1.8,
         baseroutine=x2024*routine+x2025*routine+x2026*routine,
         basescen2=x2024*(scenario_2/100)+x2025*(scenario_2/100)+x2026*(scenario_2/100),
         basescen3_cd=x2024*(scenario_3/100)+x2026*(scenario_3/100),
         basescen3_mass=x2025/1.8,
         basescen4=x2025/scenario_4,
         basescen5=x2024/scenario_5+x2026/scenario_5,
         basescen6=x2024/1.8+x2026/1.8,
         # status quo - campaigns / 1.8 every three years starting in y1, routine all the time
         tot0=basescen0+baseroutine,
         #scenario 2 - campaign in y1, routine all the time, CD starts in Y3
         tot2=basescen2+baseroutine,
         # scenario 3 - campaign every three yrs starting y1, routine all the time, CD between campaigns
         tot3=basescen3_mass+basescen3_cd+baseroutine,
         # scenario 4 - campaign every three years starting y1, routine all the time
         tot4=basescen4+baseroutine,
         # scenario 5 - campaign every two years starting y1, routine all the time
         tot5=basescen5+baseroutine,
         # scenario 6 - campaign every two years pop/1.8, routine all the time 
         tot6=basescen6+baseroutine,
         s0vs2=tot2/tot0,
         s0vs3=tot3/tot0,
         s0vs4=tot4/tot0,
         s0vs5=tot5/tot0,
         s0vs6=tot6/tot0,
         tot0_routine=baseroutine,
         tot0_mass=basescen0,
         tot0_cd=0,
         tot2_routine=baseroutine,
         tot2_mass=0,
         tot2_cd=basescen2,
         tot3_routine=baseroutine,
         tot3_mass=basescen3_mass,
         tot3_cd=basescen3_cd,
         tot4_routine=baseroutine,
         tot4_mass=basescen4,
         tot4_cd=0,
         tot5_routine=baseroutine,
         tot5_mass=basescen5,
         tot5_cd=0,
         tot6_routine=baseroutine,
         tot6_mass=basescen6,
         tot6_cd=0
  ) %>% 
  select(-contains("base")) ## drop the intermediate variables


totalnets_3y_80 <- tnets_3y_80 %>% 
  select(iso3, lifespan, contains("tot")) %>% 
  pivot_longer(contains("tot"), names_to = "scenario", names_prefix = "tot", values_to = "totalnets")%>% 
  mutate(target=80)

#--- Three year total nets 70 ----
# This will be 2024-2026 inclusive, as for Global Fund Grant Cycle 7
# With one mass campaign per cycle for campaign options 

tnets_3y_70 <- table70rtlong %>% 
  # mutate(scenario_4=na_if(scenario_4,0.1)) %>% 
  left_join(afpop, by=c("iso3"= "country_code")) %>% 
  mutate(routine=.06,
         basescen0=x2025/1.8,
         baseroutine=x2024*routine+x2025*routine+x2026*routine,
         basescen2=x2024*(scenario_2/100)+x2025*(scenario_2/100)+x2026*(scenario_2/100),
         basescen3_cd=x2024*(scenario_3/100)+x2026*(scenario_3/100),
         basescen3_mass=x2025/1.8,
         basescen4=x2025/scenario_4,
         basescen5=x2024/scenario_5+x2026/scenario_5,
         basescen6=x2024/1.8+x2026/1.8,
         # status quo - campaigns / 1.8 every three years starting in y1, routine all the time
         tot0=basescen0+baseroutine,
         #scenario 2 - campaign in y1, routine all the time, CD starts in Y3
         tot2=basescen2+baseroutine,
         # scenario 3 - campaign every three yrs starting y1, routine all the time, CD between campaigns
         tot3=basescen3_mass+basescen3_cd+baseroutine,
         # scenario 4 - campaign every three years starting y1, routine all the time
         tot4=basescen4+baseroutine,
         # scenario 5 - campaign every two years starting y1, routine all the time
         tot5=basescen5+baseroutine,
         # scenario 6 - campaign every two years pop/1.8, routine all the time 
         tot6=basescen6+baseroutine,
         s0vs2=tot2/tot0,
         s0vs3=tot3/tot0,
         s0vs4=tot4/tot0,
         s0vs5=tot5/tot0,
         s0vs6=tot6/tot0,
         tot0_routine=baseroutine,
         tot0_mass=basescen0,
         tot0_cd=0,
         tot2_routine=baseroutine,
         tot2_mass=0,
         tot2_cd=basescen2,
         tot3_routine=baseroutine,
         tot3_mass=basescen3_mass,
         tot3_cd=basescen3_cd,
         tot4_routine=baseroutine,
         tot4_mass=basescen4,
         tot4_cd=0,
         tot5_routine=baseroutine,
         tot5_mass=basescen5,
         tot5_cd=0,
         tot6_routine=baseroutine,
         tot6_mass=basescen6,
         tot6_cd=0
  ) %>% 
  select(-contains("base")) ## drop the intermediate variables


totalnets_3y_70 <- tnets_3y_70 %>% 
  select(iso3, lifespan, contains("tot")) %>% 
  pivot_longer(contains("tot"), names_to = "scenario", names_prefix = "tot", values_to = "totalnets")%>% 
  mutate(target=70)

#--- Three year total nets 90 ----
# This will be 2024-2026 inclusive, as for Global Fund Grant Cycle 7
# With one mass campaign per cycle for campaign options 

tnets_3y_90 <- table90rtlong %>% 
  # mutate(scenario_4=na_if(scenario_4,0.1)) %>% 
  left_join(afpop, by=c("iso3"= "country_code")) %>% 
  mutate(routine=.06,
         basescen0=x2025/1.8,
         baseroutine=x2024*routine+x2025*routine+x2026*routine,
         basescen2=x2024*(scenario_2/100)+x2025*(scenario_2/100)+x2026*(scenario_2/100),
         basescen3_cd=x2024*(scenario_3/100)+x2026*(scenario_3/100),
         basescen3_mass=x2025/1.8,
         basescen4=x2025/scenario_4,
         basescen5=x2024/scenario_5+x2026/scenario_5,
         basescen6=x2024/1.8+x2026/1.8,
         # status quo - campaigns / 1.8 every three years starting in y1, routine all the time
         tot0=basescen0+baseroutine,
         #scenario 2 - campaign in y1, routine all the time, CD starts in Y3
         tot2=basescen2+baseroutine,
         # scenario 3 - campaign every three yrs starting y1, routine all the time, CD between campaigns
         tot3=basescen3_mass+basescen3_cd+baseroutine,
         # scenario 4 - campaign every three years starting y1, routine all the time
         tot4=basescen4+baseroutine,
         # scenario 5 - campaign every two years starting y1, routine all the time
         tot5=basescen5+baseroutine,
         # scenario 6 - campaign every two years pop/1.8, routine all the time 
         tot6=basescen6+baseroutine,
         s0vs2=tot2/tot0,
         s0vs3=tot3/tot0,
         s0vs4=tot4/tot0,
         s0vs5=tot5/tot0,
         s0vs6=tot6/tot0,
         tot0_routine=baseroutine,
         tot0_mass=basescen0,
         tot0_cd=0,
         tot2_routine=baseroutine,
         tot2_mass=0,
         tot2_cd=basescen2,
         tot3_routine=baseroutine,
         tot3_mass=basescen3_mass,
         tot3_cd=basescen3_cd,
         tot4_routine=baseroutine,
         tot4_mass=basescen4,
         tot4_cd=0,
         tot5_routine=baseroutine,
         tot5_mass=basescen5,
         tot5_cd=0,
         tot6_routine=baseroutine,
         tot6_mass=basescen6,
         tot6_cd=0
  ) %>% 
  select(-contains("base")) ## drop the intermediate variables



totalnets_3y_90 <- tnets_3y_90 %>% 
  select(iso3, lifespan, contains("tot")) %>% 
  pivot_longer(contains("tot"), names_to = "scenario", names_prefix = "tot", values_to = "totalnets")%>% 
  mutate(target=90)

# Bind 3year totalnets objects and write to csv

totalnets_3y_all <- totalnets_3y_70 %>% 
  bind_rows(totalnets_3y_80,totalnets_3y_90) %>% 
  mutate(scenlabel=case_when(scenario==0 ~ "Mass campaigns: Every 3 years using pop/1.8",
                             scenario==2 ~ "CD: Annual distributions",
                             scenario==3 ~ "CD: Between mass campaigns",
                             scenario==4 ~ "Mass campaigns: Every 3 years using tailored approach",
                             scenario==5 ~ "Mass campaigns: Every 2 years using tailored approach",
                             scenario==6 ~ "Mass campaigns: Every 2 years using pop/1.8"),
         country=countrycode(iso3, origin="iso3c", destination="country.name"))
write_csv(totalnets_3y_all, "site/totalnets_3y_all.csv", col_names = TRUE)


#---- Access over time for best scenarios (geofacet outputs but not in geofacet form) ----

# Load and bind together all the "output/runs" files and drop extraneous variables
runs <- read_dta("output/quant_runs.dta") %>% 
  select(iso3, year, retention, scenario, accrk, acclb_npclb, accub_npcub, group, iso_a2)

write_csv(runs, "site/access_over_time_all.csv", col_names=TRUE)

# merge in the best 80, best 70, best 90 info 
best70 <- table70rtlong %>% 
  mutate(s270=scenario_2+200,
         s370=scenario_3+300,
         s470=scenario_4*10+400,
         s570=scenario_5*10+500) %>% 
  select(iso3, s270, s370, s470, s570)

best80 <- table80rtlong %>% 
  mutate(s280=scenario_2+200,
         s380=scenario_3+300,
         s480=scenario_4*10+400,
         s580=scenario_5*10+500) %>% 
  select(iso3, s280, s380, s480, s580)

best90 <- table90rtlong %>% 
  mutate(s290=scenario_2+200,
         s390=scenario_3+300,
         s490=scenario_4*10+400,
         s590=scenario_5*10+500) %>% 
  select(iso3, s290, s390, s490, s590)

runs_best <- runs %>% 
  left_join(best70) %>% 
  left_join(best80) %>% 
  left_join(best90) %>% 
  mutate(best=case_when(scenario==106 ~ 1,
                        scenario==s270 ~ 70,
                        scenario==s370 ~ 70,
                        scenario==s470 ~ 70,
                        scenario==s570 ~ 70,
                        scenario==s280 ~ 80,
                        scenario==s380 ~ 80,
                        scenario==s480 ~ 80,
                        scenario==s580 ~ 80,
                        scenario==s290 ~ 90,
                        scenario==s390 ~ 90,
                        scenario==s490 ~ 90,
                        scenario==s590 ~ 90,
                        scenario==606 ~ 1,
                        TRUE ~ 0),
         country=countrycode(iso3, origin="iso3c", destination="country.name")) %>% 
  filter(best>0)

write_csv(runs_best, "site/access_over_time_best.csv", col_names=TRUE)

#---- Person-years of protection (at 80% only) ---- # this drew from the 10,000,000 indicative population for total nets calculations, which means it won't match the realpop totalnets

pyp <- read_dta("output/totalnetspyp_realpop.dta") %>% 
  clean_names() %>% 
  mutate(bestlabel=case_when(group==4 ~ paste0("Mass campaign (pop/",round((scenario-400)/10, digits=1), ") \nevery 3 years"),
                             group==5 ~ paste0("Mass campaign (pop/",round((scenario-500)/10, digits=1), ") \nevery 2 years"),
                             group==6 ~ "Mass campaign (pop/1.8) \nevery 2 years",
                             group==1 ~ paste0("Mass campaign (pop/1.8)\nevery 3 years"),
                             group==2 ~ paste0("Annual CD \nat pop x ",(scenario-200),"%"),
                             group==3 ~ paste0("CD between campaigns \nat pop x ",(scenario-300),"%"),
                             TRUE ~ "")) %>% 
  mutate(bestlabel=case_when(bestlabel=="Mass campaign (pop/1) every 2 years" ~ "Mass campaign (pop/1.0) \nevery 2 years",
                             bestlabel=="Mass campaign (pop/2) every 2 years" ~ "Mass campaign (pop/2.0) \nevery 2 years",
                             bestlabel=="Mass campaign (pop/1) every 3 years" ~ "Mass campaign (pop/1.0) \nevery 3 years",
                             bestlabel=="Mass campaign (pop/2) every 3 years" ~ "Mass campaign (pop/2.0) \nevery 3 years",
                             TRUE ~ as.character(bestlabel)),
         wrap_lbl = stringr::str_wrap(bestlabel, width = 25),
         iso_a2=countrycode(iso3, origin = "iso3c", destination="iso2c")) %>% 
  left_join(rettimes)

table80t <- table80 %>% 
  mutate(iso3=countrycode(name, origin="country.name", destination="iso3c"))
table70t <- table70 %>% 
  mutate(iso3=countrycode(name, origin="country.name", destination="iso3c"))
table90t <- table90 %>% 
  mutate(iso3=countrycode(name, origin="country.name", destination="iso3c"))

## merge in the recommended quantifiers from the 80% target table into the PYP df:
pyp80 <- pyp %>% 
  left_join(table80t, by="iso3") %>% 
  mutate(scenario_2=200+scenario_2,
         scenario_3=300+scenario_3,
         scenario_4=400+10*scenario_4,
         scenario_5=500+10*scenario_5,
         bestq=case_when(scenario==107 ~ 1,
                         scenario==scenario_2 ~ 1,
                         scenario==scenario_3 ~ 1,
                         scenario==scenario_4 ~ 1,
                         scenario==scenario_5 ~ 1,
                         scenario==607 ~ 1,
                         TRUE ~ 0),
         target=80)

pyp70 <- pyp %>% 
  left_join(table70t, by="iso3") %>% 
  mutate(scenario_2=200+scenario_2,
         scenario_3=300+scenario_3,
         scenario_4=400+10*scenario_4,
         scenario_5=500+10*scenario_5,
         bestq=case_when(scenario==107 ~ 1,
                         scenario==scenario_2 ~ 1,
                         scenario==scenario_3 ~ 1,
                         scenario==scenario_4 ~ 1,
                         scenario==scenario_5 ~ 1,
                         scenario==607 ~ 1,
                         TRUE ~ 0),
         target=70)

pyp90 <- pyp %>% 
  left_join(table90t, by="iso3") %>% 
  mutate(scenario_2=200+scenario_2,
         scenario_3=300+scenario_3,
         scenario_4=400+10*scenario_4,
         scenario_5=500+10*scenario_5,
         bestq=case_when(scenario==107 ~ 1,
                         scenario==scenario_2 ~ 1,
                         scenario==scenario_3 ~ 1,
                         scenario==scenario_4 ~ 1,
                         scenario==scenario_5 ~ 1,
                         scenario==607 ~ 1,
                         TRUE ~ 0),
         target=90)

rm(table80t, table70t, table90t)

pyp_all <- pyp70 %>% 
  bind_rows(pyp80, pyp90) %>% 
  filter(bestq==1) %>% 
  select(-minbest, -maxbest, -ismaxpyp, -best, -name.x, -name.y, -scenario_2, -scenario_3, -scenario_4, -scenario_5) %>% 
  mutate(country=countrycode(iso3, origin="iso3c", destination="country.name"))

write_csv(pyp_all, "site/pyp_all.csv", col_names = TRUE)

#---- G runs - the half-year retention times ----

# (called "G" runs because the for loop for the 1, 1.5, 2 year etc was foreach g of numlist etc)

# we can generate the right quantifier for each country if a different ITN retention time is assumed. I.e. Liberia
# has a 1 year estimated retention time in Amelia's paper, and a 4+year median lifespan in their 2019-2022 durability study,
# and the national malaria programme thinks it's probably somewhere in between. 

# What is then needed from this is not a 70/80/90 set of targets and total nets and PYP, but rather (I hope)
# only the quantifier table, and a total number of nets over three years table. With some big caveats that if the 
# durability is shorter than their assumption, they may have less actual ITN access than estimated here.

## Read in the recommended quantifiers
s2ming <- read_dta("data/s2_min_npp_gruns_all.dta") %>% 
  clean_names()

s3ming <- read_dta("data/s3_min_npp_gruns_all.dta") %>% 
  clean_names()

#- 70% target
t70s2 <- s2ming %>% 
  filter(target==70)

t70s3 <- s3ming %>% 
  filter(target==70)

#- 80% target
t80s2 <- s2ming %>% 
  filter(target==80)

t80s3 <- s3ming %>% 
  filter(target==80)

#- 90% target
t90s2 <- s2ming %>% 
  filter(target==90)

t90s3 <- s3ming %>% 
  filter(target==90)

s270wide <- t70s2 %>% 
  select(-target) %>% 
  pivot_wider(id_cols=iso_a2, names_from=retention, values_from=q)

s370wide <- t70s3 %>% 
  select(-target) %>% 
  pivot_wider(id_cols=iso_a2, names_from=retention, values_from=q) %>% 
  mutate(`3.5`=case_when(`3`==0 & `3.5`==1 ~ 0,
                         TRUE ~ as.numeric(`3.5`))) # replace odd 1%s at 3.5y with zero, when 3y is zero.

s280wide <- t80s2 %>% 
  select(-target) %>% 
  pivot_wider(id_cols=iso_a2, names_from=retention, values_from=q)

s380wide <- t80s3 %>% 
  select(-target) %>% 
  pivot_wider(id_cols=iso_a2, names_from=retention, values_from=q) %>% 
  mutate(`3.5`=case_when(`3`==0 & `3.5`==1 ~ 0,
                         TRUE ~ as.numeric(`3.5`))) # replace odd 1%s at 3.5y with zero, when 3y is zero.

s290wide <- t90s2 %>% 
  select(-target) %>% 
  pivot_wider(id_cols=iso_a2, names_from=retention, values_from=q)

s390wide <- t90s3 %>% 
  select(-target) %>% 
  pivot_wider(id_cols=iso_a2, names_from=retention, values_from=q)

g_all <- bind_rows(s270wide, s280wide, s290wide, s370wide, s380wide, s390wide,.id = "id") %>% 
  mutate(id=case_when(id==1 ~ "Full CD: 70% target",
                      id==2 ~ "Full CD: 80% target",
                      id==3 ~ "Full CD: 90% target",
                      id==4 ~ "CD between campaigns: 70% target",
                      id==5 ~ "CD between campaigns: 80% target",
                      id==6 ~ "CD between campaigns: 90% target"),
         country=countrycode(iso_a2, origin="iso2c", destination="country.name")) %>% 
  clean_names()

write_csv(g_all, "site/half_year_quantifiers.csv", col_names = TRUE)

#---- Total Nets for the half-year retention times ----

# pivot the g_all file
gall_long <- g_all %>% 
  pivot_longer(cols=starts_with("x"), names_to = "lifespan", values_to = "quantifier") %>% 
  mutate(lifespan=gsub("x","",lifespan),
         lifespan=gsub("_",".",lifespan),
         lifespan=as.numeric(lifespan))

# merge in the population for each country for 2024-2026

gall_pop <- gall_long %>% 
  left_join(afpop) %>% 
  drop_na(country_name) %>% 
  mutate(qpct=quantifier/100)

# "distribute" the nets (does not include Routine)
# include a note about how many nets for the status quo? 

gall_tnets <- gall_pop %>% 
  mutate(totalnets=case_when(id=="Full CD: 90% target" | id=="Full CD: 80% target" | id=="Full CD: 70% target" ~ x2024*(qpct)+x2025*(qpct)+x2026*(qpct),
                             id=="CD between campaigns: 90% target" | id=="CD between campaigns: 80% target" | id=="CD between campaigns: 70% target"~ x2024/1.8 + x2025*(qpct) + x2026*(qpct),
                             TRUE ~ 0))

write_csv(gall_tnets, "site/gall_tnets.csv", col_names = TRUE)

