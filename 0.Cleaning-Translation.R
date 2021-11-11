## FlexCAT project
## (0) Data Cleaning
## Tasos Psychogyiopoulos
## c.05/10/2021
## test
library(tidyverse)
mydir <- "/Users/tasospsy/Google Drive/_UvA/Master Thesis/"
setwd(mydir)

library(haven)
dat <- read_sav("SVL-i_Vlaanderen_TOT2_HV.sav")

tdat  <-  dat %>% 
  dplyr::select(LG1:LG16) %>%         # select specific cols
  mutate(across(.cols = everything(), 
                ~ if_else(.x <= 2, 0, 1))) %>% # make all 0 and 1s
  data.frame()                 #  transform to df
# save(tdat, file = "SVL_clean.Rdat")

#### TRANSLATE THE VARIABLE NAMES ####
library(labelled)
NL <- var_label(dat[,20:35]) %>% 
  unlist() 
library(deeplr)
EN <- translate2(NL,
                 target_lang = "EN",
                 source_lang = "NL",
                 auth_key = "1a0547bd-4258-c398-9af6-35441324b8ee:fx")

VAR_labels <- tibble(VAR = paste("LG",1:16, sep = ""),
                     NL_labels = NL,
                     EN_Labels = EN)
