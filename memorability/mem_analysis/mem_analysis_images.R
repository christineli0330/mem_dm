setwd("C:/Users/Christine/Desktop/python/mem_dm/memorability/mem_analysis")

# need to run preliminary_analysis_mem_adv_loop.R first
library(tidyverse)
library(plyr)

# exclude subjects with too low performance
row_to_keep = (response.freq.add.rate$HitTotal > 3 & response.freq.add.rate$CRTotal >3)
dat = dat[row_to_keep,]
dat.trans.orig = dat.trans.orig[,row_to_keep]
dat.trans.perf = dat.trans.perf[,row_to_keep]

write.csv(dat, "output/all_mem_exclude_mat.csv")


dat.img = matrix(, nrow = 410, ncol = nrow(dat))
for (i in 1:nrow(dat.img)) {
  dat.1 = str_split(dat$Answer.imseq[i], ",", simplify = TRUE )
  dat.img[,i] = dat.1 
}

# transform to 205 image strings for each participant
dat.img = data.frame(dat.img[seq(1,nrow(dat.img),2), ])
labels = union(dat.img[,1], dat.img[,2])

# creating empty target dataframe with image names
img.target.count = as.data.frame( matrix(0, 1, ncol = length(labels)))
colnames(img.target.count) = labels
img.target.hit = img.target.count
img.HR = img.target.count
img.target.FA = img.target.count
img.target.CR = img.target.count

# count the times of the image being an target
for (i in 1:nrow(dat.img)){
  for (j in 1:ncol(dat.img)){
    if (dat.trans.orig[i,j] == TARGET){
      target.name = dat.img[i,j]
      img.target.count[1,target.name] = img.target.count[1,target.name]+1
    }
  }
}

# count the Hit times of the image while it's a target
for (i in 1:nrow(dat.img)){
  for (j in 1:ncol(dat.img)){
    if (dat.trans.perf[i,j] == TargetHit){
      target.hit.name = dat.img[i,j]
      img.target.hit[1,target.hit.name] = img.target.hit[1,target.hit.name]+1
    }
  }
}


# calculating FAR
# count the time of a target image being FA
for (i in 1:nrow(dat.img)){
  for (j in 1:ncol(dat.img)){
    if (dat.trans.perf[i,j] == TargetFA){
      target.FA.name = dat.img[i,j]
      img.target.FA[1,target.FA.name] = img.target.FA[1,target.FA.name]+1
    }
  }
}

# count correct rej for target
for (i in 1:nrow(dat.img)){
  for (j in 1:ncol(dat.img)){
    if (dat.trans.perf[i,j] == TCorrectRej){
      target.hit.name = dat.img[i,j]
      img.target.CR[1,target.hit.name] = img.target.CR[1,target.hit.name]+1
    }
  }
}

# count HR and FAR for each image
img.HR = img.target.hit/img.target.count
img.TargetFAR = img.target.FA/(img.target.FA+img.target.CR)

# memorability = HR-FAR
img.mem = img.HR - img.TargetFAR

# output together
img.values = img.target.count
img.values[2,] = img.target.hit
img.values[3,] = img.target.FA
img.values[4,] = img.HR
img.values[5,] = img.TargetFAR
img.values[6,] = img.mem

rownames(img.values) = c("Target.count", "Hit.count", "FA.count", "HR", "FAR", "Memorability")
img.values = data.frame(t(img.values))

summary(img.values)

img.values %>% filter(Memorability == 0.9)

write.csv(img.values, "output/all_mem_values.csv")
response.freq.excluded.rate = response.freq.add.rate[row_to_keep, ]
 
# frequency of image being target
hist(img.values$Target.count, main = "Frequency for stimuli being target")
# distribution of memorability values
hist(img.values$Memorability, main = "Memorability")
