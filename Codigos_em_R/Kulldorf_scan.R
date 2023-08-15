#-------------------------
# Spatial cluster detection using SpatialEpi R package
#-------------------------
library(SpatialEpi)

# required data sets
df <- data.frame(x=runif(100),y=runif(100))

# pop data
df$pop <- floor(runif(100)*1000)

# case data
df$case_data <- rbinom(100,df$pop,.1)

# overall rate
orate <- sum(df$case_data)/sum(df$pop)

# expected cases
df$expected <- df$pop*orate

# geotable
coords <- df[,c("x","y")]

# spatial scan
output <- kulldorff(coords,df$case_data,df$pop,df$expected,pop.upper.bound=0.25,
                    n.simulations=999,alpha.level=0.1)

#keep only the locations in most likely cluster
hits <- coords[c(output$most.likely.cluster$location.IDs.included),]
hits$cluster <- 1
misses <- coords[-c(output$most.likely.cluster$location.IDs.included),]
misses$cluster <- 0
results <- rbind(hits,misses)

#plot the results

library(ggplot2)

ggplot(data=results,aes(x=results$x,y=results$y,group=results$cluster)) +
  geom_point(aes(color=results$cluster),size=2) +
  theme_classic() +
  theme(legend.position="none")



