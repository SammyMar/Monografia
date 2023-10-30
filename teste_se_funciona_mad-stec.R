x <- runif(300,0,1)
y <- runif(300,0,1)
t <- runif(300,0,1)
xc <- runif(50,.95,1)
yc <- runif(50,.95,1)
tc <- runif(50,.95,1)
df <-as.data.frame( rbind(cbind(x,y,t),cbind(xc,yc,tc)))
colnames(df) <- c('x','y','t')
E <- df <- df[order(df$t),]
for(i in 2:nrow(df)){
A = 150
clusters <- MAD_STEC(df, 0.3, A, i)
if(length(clusters) > 0){
  print("Foi")
  print(clusters)
  }
}
# Visualize os clusters
library(ggplot2)
ggplot(E, aes(x, y)) +
  geom_point()  +
  labs(title = "Clusters Identificados")
