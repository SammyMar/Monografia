set.seed(1234)
x <- runif(1000,0,1)
y <- runif(1000,0,1)
t <- runif(1000,0,1)
xc <- runif(100,.95,1)
yc <- runif(100,.95,1)
tc <- runif(100,.95,1)
df <-as.data.frame( rbind(cbind(x,y,t,FALSE),cbind(xc,yc,tc,TRUE)))
colnames(df) <- c('x','y','t','cluster')
df <- df[order(df$t),]
library(surveillance)
#Parameters for the SR detection
epsilon <- c(3,1,0.5,0.2,0.1) # relative change within the cluster
radius <- c(0.1,0.01,0.05) # radius
threshold <- c(100,161,200,300) # threshold limit

# Crie todas as combinações possíveis entre os parâmetros
param_combinations <- expand.grid(epsilon = epsilon, radius = radius, threshold = threshold)

# Crie o DataFrame final com as colunas desejadas
resultado <- data.frame(epsilon = param_combinations$epsilon,
                        radius = param_combinations$radius,
                        threshold = param_combinations$threshold,
                        x = NA,
                        y = NA,
                        t = NA,
                        cluster = NA)


for(e in epsilon){
  for(r in radius){
    for(t in threshold){
      res <- stcd(x=df$x,
            y=df$y,
            t=df$t,
            radius=r,
            epsilon=e,
            areaA=1,
            areaAcapBk=1,
            threshold=t)
    #Index of the event
    obs <- which.max(res$R >= t)
    resultado[resultado$epsilon == e &
                resultado$radius == r &
                resultado$threshold == t,c('x','y','t','cluster')] <- df[obs,c('x','y','t','cluster')]

  }}}

