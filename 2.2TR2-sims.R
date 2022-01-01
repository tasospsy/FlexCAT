## FlexCAT project
## (2.2) Simulate - TR2
## Tasos Psychogyiopoulos
## c. 31.12.2021

load("simDat.Rdat")

TR2dat <- simDat[[3]]

## CHUNK 1/5 (1:20)
set.seed(1992)
startt <- Sys.time()
TR2_ch_1  <- TidyEstFun(TR2dat[1:20], class.to = 20)
save(file = "TR2_ch_1.Rdat", TR2_ch_1)
rm(TR2_ch_1)
(endt <- Sys.time() - startt)

## CHUNK 2/5 (21:40)
set.seed(1992)
TR2_ch_2  <- TidyEstFun(TR2dat[21:40], class.to = 20)
save(file = "TR2_ch_2.Rdat", TR2_ch_2)
rm(TR2_ch_2)

## CHUNK 3/5 (41:60)
set.seed(1992)
TR2_ch_3  <- TidyEstFun(TR2dat[41:60], class.to = 20)
save(file = "TR2_ch_3.Rdat", TR2_ch_3)
rm(TR2_ch_3)

## CHUNK 4/5 (61:80)
set.seed(1992)
TR2_ch_4  <- TidyEstFun(TR2dat[61:80], class.to = 20)
save(file = "TR2_ch_4.Rdat", TR2_ch_4)
rm(TR2_ch_4)

## CHUNK 5/5 (81:100)
set.seed(1992)
TR2_ch_5  <- TidyEstFun(TR2dat[81:100], class.to = 20)
save(file = "TR2_ch_5.Rdat", TR2_ch_5)
rm(TR2_ch_5)