## FlexCAT project
## (2.1) Simulate - TR1
## Tasos Psychogyiopoulos
## c. 31.12.2021

load("simDat.Rdat")

TR1dat <- simDat[[2]]

## CHUNK 1 (1:20)
set.seed(1992)
startt <- Sys.time()
TR1_ch_1  <- TidyEstFun(TR1dat[1:20], class.to = 10)
save(file = "TR1_ch_1.Rdat", TR1_ch_1)
rm(TR1_ch_1)
(endt <- Sys.time() - startt)
# Time difference of 43.55045 mins

## CHUNK 2/5 (21:40)
set.seed(1992)
TR1_ch_2  <- TidyEstFun(TR1dat[21:40], class.to = 10)
save(file = "TR1_ch_2.Rdat", TR1_ch_2)
rm(TR1_ch_2)

## CHUNK 3/5 (41:60)
set.seed(1992)
TR1_ch_3  <- TidyEstFun(TR1dat[41:60], class.to = 10)
save(file = "TR1_ch_3.Rdat", TR1_ch_3)
rm(TR1_ch_3)

## CHUNK 4/5 (61:80)
set.seed(1992)
TR1_ch_4  <- TidyEstFun(TR1dat[61:80], class.to = 10)
save(file = "TR1_ch_4.Rdat", TR1_ch_4)
rm(TR1_ch_4)

## CHUNK 5/5 (81:100)
set.seed(1992)
TR1_ch_5  <- TidyEstFun(TR1dat[81:100], class.to = 10)
save(file = "TR1_ch_5.Rdat", TR1_ch_5)
rm(TR1_ch_5)