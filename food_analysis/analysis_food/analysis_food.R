setwd("C:/Users/Christine/Desktop/python/mem_dm")
library(MASS)
library(plyr)
library(corrplot)
library(RColorBrewer)
suppressWarnings(library(ggplot2))
library(tidyverse)
library(nFactors)


dat.f = read.csv("food_analysis/stimchar_final_touse/stimchar_final_touse.csv")
dat.m = read.csv("memorability/mem_analysis/output/all_mem_values.csv")

# filter age
dat.f = dat.f %>% filter(age == "18-25" | age == "26-30" | age == "31-35")

# get mean of ratings
ratings = dat.f %>% group_by(stimulus) %>%
  summarise_at(vars(rating.calories, rating.carbohydrates, rating.disgusting), 
               funs(mean(., na.rm=TRUE)))
# translate mem and food rating names
name = read.csv("mem.csv")
rownames(name) = name$stimulus
write.csv(name, "food_analysis/analysis_food/name_food.csv")

# only do it once
ratings = ratings[ , order(ratings$stimulus)]
dat.m = dat.m[order(dat.m$X),]
write.csv(ratings, "ratings.csv")
# write.csv(dat.m, "mem.csv")


# followed what stated in the paper, we will also flip 4 or the measures
# sugar-low sugar
# disgusting-not disgusting
# healthy-unhealthy
# vitamins-lowvitamins
rdat = dat.f %>% select(stimulus, choice.rating, starts_with("rating"))
colnames(rdat) = sub("rating[.]", "", colnames(rdat))
colnames(rdat) = sub("[.]rating", "", colnames(rdat))

rdat$sugar = 10 - rdat$sugar
rdat$disgusting = 10 - rdat$disgusting
rdat$healthy = 10 - rdat$healthy
rdat$vitamins = 10 - rdat$vitamins
## rename
cinds = sapply(c("sugar", "disgusting", "healthy", "vitamins"), function(x) which(colnames(rdat) == x))
colnames(rdat)[cinds] = c("low-sugar", "not-disgusting", "unhealthy", "low-vitamins")

rmat = as.matrix(rdat[,-1])
rownames(rmat) = rdat$stimulus
rmat.nochoice = rmat[,-1]


# average of ratings for each image
# follow the order on paper
new.ord <- c("tasty", "othertasty", "feel", "texture", "not-disgusting", "familiar", "filling", "unhealthy", "sweetsaltysavory", "calories", "carbohydrates", "fat", "low-vitamins", "gluten", "sodium", "low-sugar", "protein", "stimulus")

ave.mat = rdat[,new.ord]
ave.mat = rdat[,new.ord] %>% 
  group_by(stimulus) %>% 
  summarise_all(mean) %>% 
  select(-stimulus)
M = cor(ave.mat)
diag(M) = 0
corrplot(M, method="circle", diag = F, #order="hclust", 
         #is.corr=F, cl.lim=c(-0.85,0.85), 
         col=rev(colorRampPalette(brewer.pal(n=11, name="RdBu"))(256)))


# factor analysis
nFactors::nCng(as.data.frame(rmat.nochoice), model="factors") 

# cols2 <- colorRampPalette(c("#67001F", "#B2182B", "#D6604D", 
#                            "#F4A582", "#FDDBC7", "#FFFFFF", "#D1E5F0", "#92C5DE", 
#                            "#4393C3", "#2166AC", "#053061"))(200)

fit <- factanal(rmat.nochoice, 3, scores="regression")

d <- dist(fit$loadings)
hc <- hclust(d, method="average")
dend <- as.dendrogram(hc)
loads <- fit$loadings[order.dendrogram(dend),]
loads <- rbind(choice=loads[rownames(loads) == "choice",], loads[rownames(loads) != "choice",])
# Make tasty first, then health, and finally sweet/savory
loads2 <- loads[,c(2,1,3)]
colnames(loads2) <- sprintf("Factor-%i", 1:3)
# Put the sweet/savory to the end
#loads2 <- rbind(loads2[-c(1:4),], loads2[4:1,])
loads2 <- rbind(loads2[-c(1:4),], loads2[1:4,])
# Just for reporting purposes
sort(loads2[,1], decreasing = T)
sort(loads2[,2], decreasing = T)
sort(loads2[,3], decreasing = T)
# Plot
corrplot::corrplot(loads2, diag=T, col=rev(cols2), cl.pos="n")

scores3 <- fit$scores
ind1 <- which.max(fit$loadings[rownames(fit$loadings) == "unhealthy",])
ind2 <- which.max(fit$loadings[rownames(fit$loadings) == "tasty",])
ind3 <- which.max(fit$loadings[rownames(fit$loadings) == "sweetsaltysavory",])
colnames(scores3) <- c("Food.UnHealth", "Food.Taste", "Food.SweetProtein")[c(ind1,ind2,ind3)]
outdf <- data.frame(subjectId=dat.f$subjectId, stimulus=dat.f$stimulus, scores3)

# score of factor for each food item
ave.factor = outdf %>% group_by(stimulus) %>% 
  summarise_all(mean) 

# add mem score to the factor data frame
labels = sapply(name$stimulus, function(x) which(ave.factor$stimulus == x))
ave.factor.mem = data.frame(Memorability = name$Memorability, mem.stimulus = name$X, ave.factor)

# do correlation among all averaged ratings
ave.mat.r = rdat[,new.ord] %>% 
  group_by(stimulus) %>% 
  summarise_all(mean) 
ave.mat.r = data.frame(Memorability = name$Memorability, mem.stimulus = name$X, ave.mat.r)
M.mem = ave.mat.r %>% select(-mem.stimulus, -stimulus) %>% cor()
diag(M.mem) = 0
corrplot(M.mem, method="circle", diag = F, #order="hclust", 
         #is.corr=F, cl.lim=c(-0.85,0.85), 
         col=rev(colorRampPalette(brewer.pal(n=11, name="RdBu"))(256)))



# perform linear regression
fit1 = lm(Memorability ~ Food.Taste, data = ave.factor.mem)
summary(fit1)

# plot mem~food.taste
ave.factor.mem %>% ggplot(aes(x = Food.Taste, y = Memorability))+
  geom_point()+
  geom_smooth(method='lm', se = FALSE)

table(dat.f$gender)
table(dat.f$degree)
table(dat.f$age)
