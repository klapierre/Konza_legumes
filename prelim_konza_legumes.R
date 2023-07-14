################################################################################
##  prelim_konza_legumes.R: Preliminary observational data on legume abundances and coexistence at Konza Prairie Biological Station.
##
##  Authors: Kimberly Komatsu
################################################################################

library(readxl)
library(PerformanceAnalytics)
library(tidyverse)

setwd('C:\\Users\\kjkomatsu\\Dropbox (Smithsonian)\\konza projects\\legume dynamics')

theme_set(theme_bw())
theme_update(axis.title.x=element_text(size=20, vjust=-0.35, margin=margin(t=15)), axis.text.x=element_text(size=16),
             axis.title.y=element_text(size=20, angle=90, vjust=0.5, margin=margin(r=15)), axis.text.y=element_text(size=16),
             plot.title = element_text(size=24, vjust=2),
             panel.grid.major=element_blank(), panel.grid.minor=element_blank(),
             legend.title=element_blank(), legend.text=element_text(size=14))


#### Load Data ####

all <- read_xlsx('LegumeDynamics_KNZ_Summer2022.xlsx') %>% 
  select(-year, -season, -date, -site, -notes, -sppnum)


#### Calculate Relative Cover ####

totCover <- all %>% 
  group_by(set, plot) %>% 
  summarise(total_cover=sum(cover)) %>% 
  ungroup()

functionalGroup <- all %>% 
  left_join(totCover) %>% 
  mutate(rel_cover=cover/total_cover) %>% 
  mutate(functional_group=ifelse(taxa=='Total grass', 'graminoid',
                          ifelse(taxa=='Total forb', 'forb',
                          'legume'))) %>% 
  group_by(set, plot, functional_group) %>% 
  summarise(group_cover=sum(rel_cover)) %>% 
  ungroup() %>% 
  pivot_wider(names_from=functional_group, values_from=group_cover, values_fill=0)


#### Legume Cover ####

surroundingSpp <- all %>% 
  select(set, plot, surroundingspp) %>% 
  na.omit()

cover <- all %>% 
  filter(!(taxa %in% c('Total grass', 'Total forb'))) %>% 
  left_join(totCover) %>% 
  mutate(rel_cover=cover/total_cover) %>% 
  select(-stemcount, -cover, -total_cover, -surroundingspp) %>% 
  pivot_wider(names_from=taxa, values_from=rel_cover, values_fill=0) %>% 
  left_join(functionalGroup) %>% 
  left_join(surroundingSpp)

#figures
chart.Correlation(cover[,3:15], histogram=T, pch=19)





#### Legume Stem Densities ####

stems <- all %>% 
  filter(!(taxa %in% c('Total grass', 'Total forb'))) %>% 
  select(-cover, -surroundingspp) %>% 
  pivot_wider(names_from=taxa, values_from=stemcount, values_fill=0)

#figures
chart.Correlation(stems[,3:12], histogram=T, pch=19)
