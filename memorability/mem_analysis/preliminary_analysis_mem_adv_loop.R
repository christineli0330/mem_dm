setwd("C:/Users/Christine/Desktop/python/mem_dm/memorability/mem_analysis")
library(tidyverse)
library(plyr)

dat = read.csv("input data/memorability_raw_full.csv")

FIXATION = 0
TARGET = 1
REPEAT = 2
FILLER = 3
VIGILANCE = 4

HIT = 11
MISS = 12
FALSEALARM = 13
CORRECTREJECTION = 14

TargetHit = 211
TargetFA = 113
TargetMiss = 212
FillerHit = 411
FillerFA = 313
FillerMiss = 412
TCorrectRej = 114
FCorrectRej = 314

summary(dat)

# convert original data to all of the response + image type for all participants
# row = response, column = participant
dat.trans = matrix(, nrow = 410, ncol = nrow(dat))
for (i in 1:nrow(dat)) {
  dat.1 = as.numeric(strsplit(dat$Answer.perfseq[i], split = ',' )[[1]])
  dat.1.i = as.numeric(strsplit(dat$Answer.imtypeseq[i], split = ',' )[[1]])*100
  mem.dat1 = dat.1+dat.1.i
  dat.trans[,i] = mem.dat1 
}
# dat.trans = t(dat.trans)
dat.trans = data.frame(dat.trans)

#count number of response
levels=unique(do.call(c,dat.trans)) #all unique values in df
response.frequency = sapply(levels,function(x)colSums(dat.trans==x)) #count occurrences of x in each row
colnames(response.frequency) = levels
# 

# rename columns
# response.frequency is the count of HIT/FA/CorrectRej for Targets and Fillers for 
# each participants
colnames(response.frequency)[colnames(response.frequency) == as.character(TargetHit)] = "TargetHit"
colnames(response.frequency)[colnames(response.frequency) == as.character(TargetFA)] = "TargetFA"
colnames(response.frequency)[colnames(response.frequency) == as.character(TCorrectRej)] = "TCorrectRej"
colnames(response.frequency)[colnames(response.frequency) == as.character(FillerHit)] = "FillerHit"
colnames(response.frequency)[colnames(response.frequency) == as.character(FillerFA)] = "FillerFA"
colnames(response.frequency)[colnames(response.frequency) == as.character(FCorrectRej)] = "FCorrectRej"
colnames(response.frequency)[colnames(response.frequency) == as.character(FIXATION)] = "Fixation"
colnames(response.frequency)[colnames(response.frequency) == as.character(TargetMiss)] = "TargetMiss"
colnames(response.frequency)[colnames(response.frequency) == as.character(FillerMiss)] = "FillerMiss"
response.frequency = response.frequency[,order(colnames(response.frequency))]
# adding rates
response.frequency = data.frame(response.frequency)
response.freq.rate = response.frequency %>% mutate(THitRate = TargetHit/29,
                                                           TFalseARate = TargetFA/(TargetFA+TargetHit),
                                                           FHitRate = FillerHit/40,
                                                           FFalseARate = FillerFA/(FillerFA+FillerHit))



#transform the response in fixation
dat.trans.orig = matrix(, nrow = 410, ncol = nrow(dat))
for (i in 1:nrow(dat)) {
  dat.1.i = as.numeric(strsplit(dat$Answer.imtypeseq[i], split = ',' )[[1]])
  mem.dat1 = dat.1.i
  dat.trans.orig[,i] = mem.dat1 
}

dat.trans.orig = dat.trans.orig[seq(1,nrow(dat.trans.orig),2), ]
dat.trans.image = dat.trans[seq(1,nrow(dat.trans),2), ]
dat.trans.fixation = dat.trans[seq(2,nrow(dat.trans),2), ]
dat.trans.perf = dat.trans.image

for (i in 1:nrow(dat.trans.image)) {
  for (j in 1:ncol(dat.trans.image)){
    if (dat.trans.fixation[i,j] != 0){
      dat.trans.perf[i,j] = dat.trans.orig[i,j]*100 + dat.trans.fixation[i,j]
    }
  }
}

# tabeling for dat.trans.perf (corrected hit)
levels=unique(do.call(c,dat.trans.perf)) #all unique values in df
response.frequency.add = sapply(levels,function(x)colSums(dat.trans.perf==x)) #count occurrences of x in each row
colnames(response.frequency.add) = levels
table(dat.trans.orig)

# rename columns
# response.frequency.add is the fixation corrected version of response.frequency
colnames(response.frequency.add)[colnames(response.frequency.add) == as.character(TargetHit)] = "TargetHit"
colnames(response.frequency.add)[colnames(response.frequency.add) == as.character(TargetFA)] = "TargetFA"
colnames(response.frequency.add)[colnames(response.frequency.add) == as.character(TCorrectRej)] = "TCorrectRej"
colnames(response.frequency.add)[colnames(response.frequency.add) == as.character(FillerHit)] = "FillerHit"
colnames(response.frequency.add)[colnames(response.frequency.add) == as.character(FillerFA)] = "FillerFA"
colnames(response.frequency.add)[colnames(response.frequency.add) == as.character(FCorrectRej)] = "FCorrectRej"
colnames(response.frequency.add)[colnames(response.frequency.add) == as.character(FIXATION)] = "Fixation"
colnames(response.frequency.add)[colnames(response.frequency.add) == as.character(TargetMiss)] = "TargetMiss"
colnames(response.frequency.add)[colnames(response.frequency.add) == as.character(FillerMiss)] = "FillerMiss"
response.frequency.add = response.frequency.add[,order(colnames(response.frequency.add))]


# calculating rate
response.frequency.add = data.frame(response.frequency.add)
response.freq.add.rate = response.frequency.add %>% mutate(THitRate = TargetHit/29,
                                                           TFalseARate = TargetFA/(TargetFA+TargetHit),
                                                           FHitRate = FillerHit/40,
                                                           FFalseARate = FillerFA/(FillerFA+FillerHit))

response.freq.add.rate = response.freq.add.rate %>% mutate(keyPress = FillerFA+FillerHit+TargetFA+TargetHit, 
                                                           HitTotal = TargetHit+FillerHit, 
                                                           CRTotal = TCorrectRej + FCorrectRej)

# get reject worker id
loop.reject = 1
reject.id  = matrix(, nrow = nrow(dat), ncol = 1)
for (i in 1:nrow(dat)){
  if (response.freq.add.rate$keyPress[i] <= 1){
    reject.id[loop.reject,] = dat$WorkerId[i]
    loop.reject = loop.reject+1
  }
}



# csv output
write.csv(response.freq.rate, "output/count_hr_far_p_all.csv")
write.csv(response.freq.add.rate, "output/count_hr_far_p_all_fixed.csv")


table(dat$Answer.gender)
table(dat$Answer.ethnicity)
table(dat$Answer.education)
table(dat$Answer.age)
