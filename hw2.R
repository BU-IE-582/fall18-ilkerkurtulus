install.packages("data.table")
install.packages("anytime")
install.packages("stringr")
install.packages("dplyr")
install.packages("ggplot2")
install.packages("plotly")
install.packages("jpeg")
install.packages("fields")

library(data.table)
library(anytime)
library(stringr)
library(dplyr)
library(ggplot2)
library(plotly)
library(jpeg)
library(fields)
oo = readRDS("/Users/ilkerkurtulus/Documents/cse-master/ie582/data/hw1/df9b1196-e3cf-4cc7-9159-f236fe738215_odd_details.rds")
oo = data.table(oo)
oo$date = anytime(oo$date)

mm = readRDS("/Users/ilkerkurtulus/Documents/cse-master/ie582/data/hw1/df9b1196-e3cf-4cc7-9159-f236fe738215_matches.rds")
mm = data.table(mm)
mm[, c("leagueId","type"):=NULL]
mm$date = anytime(mm$date)
mm = mm[is.na(mm$score)==FALSE]
mm = mm[, c("home_goal", "away_goal") := tstrsplit(score, ":", fixed=TRUE)]
mm$home_goal = as.numeric(mm$home_goal)
mm$away_goal = as.numeric(mm$away_goal)
mm[, total_goals:=home_goal+away_goal]
mm = mutate(mm, is_over = ifelse(total_goals > 2.5, 1,0))
mm = data.table(mm)
bookmakers = unique(oo$bookmaker)

df = oo[bookmaker == bookmakers[2]]
pdf = dcast(df, matchId + bookmaker ~ betType + oddtype, value.var = c("odd"),  fun.aggregate = mean)
pdf = na.omit(pdf)
all = na.omit(pdf[mm[, c("matchId", "is_over")], on = "matchId"])
pca = prcomp(all[, 3:9], center = TRUE, scale. = TRUE)
plot(pca, type = "l")
all_pca = predict(pca, newdata = all[,3:9])
all_pca3d = all_pca[,1:3]
all_pca3d = data.table(all_pca3d)
all_pca3d[,is_over:= all$is_over]
#2d plot with 1st and 2nd components
plot_ly(all_pca3d,x = ~PC1, y = ~PC2,color = ~is_over, colors = c('#BF382A', '#0C4B8E'), type = "scatter")
#3d plot with 1st, 2nd and 3rd components
plot_ly(all_pca3d,x = ~PC1, y = ~PC2, z = ~PC3 , color = ~is_over, colors = c('#BF382A', '#0C4B8E'), type = "scatter3d")


func_1a = function(n_bm){
  df = oo[bookmaker == bookmakers[n_bm]]
  pdf = dcast(df, matchId + bookmaker ~ betType + oddtype, value.var = c("odd"),  fun.aggregate = mean)
  pdf = na.omit(pdf)
  all = na.omit(pdf[mm[, c("matchId", "is_over")], on = "matchId"])
  pca = prcomp(all[, 3:9], center = TRUE, scale. = TRUE)
  
  eigs = pca$sdev^2
  exp_var_ratio = eigs/sum(eigs)
  cum_exp_var_ratio = cumsum(exp_var_ratio)
  
  plot(cum_exp_var_ratio, type = "l", xlab = "# of Principle Components", ylab = "Cumulative Explained Variance")
  title(paste("Cumulative Explained Variance Ratio of PCA for " , bookmakers[n_bm], sep = ""))
  
  all_pca = predict(pca, newdata = all[,3:9])
  all_pca3d = all_pca[,1:3]
  all_pca3d = data.table(all_pca3d)
  all_pca3d[,is_over:= all$is_over]
  #2d plot with 1st and 2nd components
  plot_ly(all_pca3d,x = ~PC1, y = ~PC2,color = ~is_over, colors = c('#BF382A', '#0C4B8E'), type = "scatter", mode = "markers")
  #3d plot with 1st, 2nd and 3rd components
  plot_ly(all_pca3d,x = ~PC1, y = ~PC2, z = ~PC3 , color = ~is_over, colors = c('#BF382A', '#0C4B8E'), type = "scatter3d", mode = "markers")
}

f

# part b

dist_euc = dist(all[,3:9], method = "euclidian")
dist_man = dist(all[,3:9],method = "manhattan")

mds_euc2 = cmdscale(dist_euc, eig = TRUE, k = 2)
mds_euc3 = cmdscale(dist_euc, eig = TRUE, k = 3)
mds_man2 = cmdscale(dist_man, eig = TRUE, k = 2)
mds_man3 = cmdscale(dist_man, eig = TRUE, k = 3)

# task 2 
mm[, is_1x2 := ifelse(home_goal > away_goal, "1", ifelse(home_goal == away_goal, "x","2"))]
all_1x2 = na.omit(pdf[mm[, c("matchId", "is_1x2")], on = "matchId"])


# task 3
img = readJPEG("/Users/ilkerkurtulus/Downloads/IMG_7955.jpg")
str(img)
dim(img)

if (exists("rasterImage")) {
  plot(1:2, type='n')
  rasterImage(img, 1, 1, 2, 2)
}

if (exists("rasterImage")) {
  plot(1:4, type='n')
  rasterImage(img[,,1], 1, 1, 2, 2)
  rasterImage(img[,,2], 2, 1, 3, 2)
  rasterImage(img[,,3], 3, 1, 4, 2)
}


noise = runif(512*512*3, min = 0, max = 1)
dim(noise) = c(512,512,3)
nimg = noise + img
nimg_scaled = (nimg-min(nimg))/(max(nimg)-min(nimg))

if (exists("rasterImage")) {
  plot(1:2, type='n')
  rasterImage(nimg_scaled, 1, 1, 2, 2)
}


image.plot(img[,,1],img[,,2],img[,,3])
image.plot(nimg_scaled[,,1],nimg_scaled[,,2],nimg_scaled[,,3])

plot_ly(mds_man2,x = ~V1, y = ~V2, z = ~V3 , color = ~is_over, type = "scatter3d", mode = "markers",domain = list(x = c(0, 1), y = c(0.5, 1))) %>% layout(title = paste("MDS Manhattan and is_over results",bookmakers[n_bm], sep = " "))

extract_path = function(data, i,j,n){
  dw = (n-1)/2
  as.vector(t(data[(i-dw):(i + dw), (j-dw):(j+dw)]))
}
tmp = c()
for(i in 2:511){
  for(j in 2:511){
    x = extract_path(grey_nimg,i,j,3)
    tmp = c(tmp, x)
  }
}

res = matrix(tmp, nrow = 260100,byrow = TRUE)



