## FlexCAT project
## (2.1) Simulate - SVL
## Tasos Psychogyiopoulos
## c. 31.12.2021

load("simDat.Rdat")

SVLdat <- simDat[[1]]


## NEW !
## For 10 Reps, NEW Ns:
## CHUNK 1-10
#tblmodels10b <- tblmodels10
#save(file = "tblmodels10b", tblmodels10b)
### CHUNK 11-30
#tblmodels111_30
#save(file = "tblmodels111_30.Rdat", tblmodels111_30)
### CHUNK 31-40
#set.seed(1992)
#startt <- Sys.time()
#tblmod.31_40 <- TidyEstFun(dat.31_40)
#(endt <- Sys.time() - startt)
#save(file = "tblmod.31_40.Rdat", tblmod.31_40)
#rm(tblmod.31_40)

## CHUNK 41-50
set.seed(1992)
svl_ch_5  <- TidyEstFun(SVLdat[41:50], class.to = 25)
save(file = "svl_ch_5.Rdat", svl_ch_5)
rm(svl_ch_5)
## CHUNK 51-60
set.seed(1992)
svl_ch_6  <- TidyEstFun(SVLdat[51:60] , class.to = 25)
save(file = "svl_ch_6.Rdat", svl_ch_6)
## CHUNK 61-70
set.seed(1992)
svl_ch_7  <- TidyEstFun(SVLdat[61:70], class.to = 25)
save(file = "svl_ch_5.Rdat", svl_ch_7)
## CHUNK 71-80
set.seed(1992)
svl_ch_8  <- TidyEstFun(SVLdat[71:80] , class.to = 25)
save(file = "svl_ch_8.Rdat", svl_ch_8)
## CHUNK 81-90
set.seed(1992)
svl_ch_9  <- TidyEstFun(SVLdat[81:90], class.to = 25 )
save(file = "svl_ch_9.Rdat", svl_ch_9)
## CHUNK 91-100
set.seed(1992)
svl_ch_10 <- TidyEstFun(SVLdat[91:100], class.to = 25)
save(file = "svl_ch_10.Rdat", svl_ch_10)


