getwd()
setwd("C:/Users/com/Desktop")
getwd()

data_df <- read.table("dmp.csv", 
               header = T,
               sep = "\t")

str(df)
View(df)



str(data_df)
km.df <- data_df[,-c(7,11,12)]
str(km.df)


km.df$사고년도 <- as.factor(km.df$사고년도)
km.df$사고유형구분 <- as.factor(km.df$사고유형구분)
km.df$시군명 <- as.factor(km.df$시군명)
km.df$사망구분 <- as.factor(km.df$사망구분)
km.df$최종점수 <- as.numeric(km.df$최종점수)

str(km.df)


library(cluster)
library(dplyr)
library(ggplot2)
library(readr)
library(Rtsne)

#install.packages("Rtsne")

#' Compute Gower distance

str(km.df)
gower_dist <- daisy(km.df, metric = "gower")
gower_mat <- as.matrix(gower_dist)
#' Print most similar clients
km.df[which(gower_mat == min(gower_mat[gower_mat != min(gower_mat)]), arr.ind = TRUE)[1, ], ]
#' Print most dissimilar clients
km.df[which(gower_mat == max(gower_mat[gower_mat != max(gower_mat)]), arr.ind = TRUE)[1, ], ]

sil_width <- c(NA)

for(i in 2:8){  
    pam_fit <- pam(gower_dist, diss = TRUE, k = i)  
    sil_width[i] <- pam_fit$silinfo$avg.width  
}

plot(1:8, sil_width,
     xlab = "Number of clusters",
     ylab = "Silhouette Width")
lines(1:8, sil_width)

k <- 3
pam_fit <- pam(gower_dist, diss = TRUE, k)
pam_results <- km.df %>%
    mutate(cluster = pam_fit$clustering) %>%
    group_by(cluster) %>%
    do(the_summary = summary(.))
pam_results$the_summary

tsne_obj <- Rtsne(gower_dist, is_distance = TRUE)
str(tsne_obj)

tsne_data <- tsne_obj$Y %>%
    data.frame() %>%
    setNames(c("X", "Y")) %>%
    mutate(cluster = factor(pam_fit$clustering))

str(tsne_data)
ggplot(aes(x = X, y = Y), data = tsne_data) +
    geom_point(aes(color = cluster))



