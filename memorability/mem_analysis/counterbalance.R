# Run mem_analysis_images and preliminary_analysis_mem_adv_loop first

# find images with less chance
less.target = img.values %>% filter(Target.count< 40)
less.labels = rownames(less.target)

less.30.target = img.values %>% filter(Target.count>25 & Target.count<=30) %>% rownames()


# generate stimuli array for each trial
img.array.index = matrix(, nrow = 10, ncol = 1)
for (i in 1:10){
  #get target lists
  less.target.core = c(less.labels, sample(less.30.target, 23))
  #get filler lists
  target.index = match(less.target.core, rownames(img.values))
  filler.img = img.values[-target.index,] %>% rownames()
  #shuffle lists
  less.target = sample(less.target.core)
  less.filler = sample(filler.img)
  img.array = c(less.target, less.filler)
  #add url string
  quotes = c("\",\"https://voices.uchicago.edu/bakkourlab/files/2019/04/")
  img.array = paste(img.array, collapse=quotes)
  img.array = paste0("\"https://voices.uchicago.edu/bakkourlab/files/2019/04/", img.array)
  img.array = paste0(img.array, "\"")
  img.array.index[i,]=img.array
}
  
# output cvs file
write.csv(img.array.index, "counterbalance_20201111.csv")



# another way (without placeholder)
label.target.solid = rownames(img.values[order(img.values$Target.count, decreasing = FALSE),])[1:29]
target.index = match(label.target.solid, rownames(img.values))
filler.img = img.values[-target.index,] %>% rownames()
less.target = sample(label.target.solid)
less.filler = sample(filler.img)
img.array = c(less.target, less.filler)
quotes = c("\",\"https://voices.uchicago.edu/bakkourlab/files/2019/04/")
img.array = paste(img.array, collapse=quotes)
img.array = paste0("\"https://voices.uchicago.edu/bakkourlab/files/2019/04/", img.array)
img.array = paste0(img.array, "\"")

write.csv(img.array, "single_array_20201111.csv")
