## FlexCAT project
## (0) Data Preparation
## Tasos Psychogyiopoulos
## c.05/10/2021/ m.31.12.2021


mydir <- "/Users/tasospsy/Google Drive/_UvA/Master Thesis/"
setwd(mydir)

## 1. SVL 

dat <- read_sav("SVL-i_Vlaanderen_TOT2_HV.sav")

tdat  <-  dat %>% 
  dplyr::select(LG1:LG16) %>%         # select specific cols
  mutate(across(.cols = everything(), 
                ~ if_else(.x <= 2, 0, 1))) %>% # make all 0 and 1s
  data.frame()                 #  transform to df
# save(tdat, file = "SVL_clean.Rdat")

## 2. Transitive Reasoning 1 
data("transreas") # from package 'mokken'
TRANS1 <- transreas[,-1]
save(file = "TRANS1.Rdat", TRANS1)

## 3. Transitive Reasoning 2
data("transreas2") # from package 'mokken'
TRANS2 <- transreas2
save(file = "TRANS2.Rdat", TRANS2)

## --
## OLD CODE :

## Subseting
### Subsets of Data
#X08 <- tdat %>% slice_sample(prop = 0.8) # random proportion of 80%
#X01 <- tdat %>% slice_sample(prop = 0.1)
#X02 <- tdat %>% slice_sample(prop = 0.2)
#X05 <- tdat %>% slice_sample(prop = 0.5)

#### TRANSLATE THE VARIABLE NAMES ####
# library(labelled)
# NL <- var_label(dat[,20:35]) %>% 
#   unlist() 
# library(deeplr)
# EN <- translate2(NL,
#                  target_lang = "EN",
#                  source_lang = "NL",
#                  auth_key = "1a0547bd-4258-c398-9af6-35441324b8ee:fx")
# 
# VAR_labels <- tibble(VAR = paste("LG",1:16, sep = ""),
#                      NL_labels = NL,
#                      EN_Labels = EN)
# 