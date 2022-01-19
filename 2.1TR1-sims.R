## FlexCAT project
## (2.1) Simulate - TR1
## Tasos Psychogyiopoulos
## c. 31.12.2021

load("simDat.Rdat")

TR1dat <- simDat[[2]]

## New chunks
# We use the 'sliceL' function to subset the 'TR1dat' list into 
# chunks to prevent for memory overload. 
## It has length = 100, thus 5 chunks of 20 frames seems doable. 
TR1dat.sliced <- sliceL(TR1dat, 5) 

## With for_loop
set.seed(1992)
startt <- Sys.time()
TR1.all.ch <- list()
## Set progress bar
pb <- txtProgressBar(min = 0, max = nrow(TR1dat.sliced),
                     style = 3,width = 50, char = "=")   
for (i in 1:nrow(TR1dat.sliced)){
  TR1.all.ch[[i]]  <- invisible(TidyEstFun(TR1dat.sliced[[1]][[i]], class.to = 10))
  save(file = "TR1.all.ch.Rdat", TR1.all.ch)
  setTxtProgressBar(pb, i)
}
close(pb)
(endt <- Sys.time() - startt)
## Time difference of 49.62849 secs w/ for LOOP


## Old chunks
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