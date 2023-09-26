# DADOS DO CORREIA-----------------------------------
require("splancs")
# load the data from package "splancs"
data(burkitt, package="splancs")

# order the times
df <- burkitt[order(burkitt$t), ]

resultados <- list()
B <- c(100,200,500)
nu <- c(0.01,0.05,0.1)
df$SVM <- 0
df1 <- cria_variaveis(df,rho = 20, time_window = 100)
for(n in nu){
  for(i in B){
    df$SVM <- 0
    for(b in 1:i){

      #embaralhar os tempos
      amostrab <- df %>%
        mutate(t = sample(t))

      #criar as features
      amostrab <- cria_variaveis(amostrab,rho = 20, time_window = 100)

      #fitar o modelo
      svm.model<-svm(amostrab[c('var1','var2','var3')],y=NULL,
                     scale = TRUE,
                     type='one-classification',
                     nu=nu,
                     kernel="radial")

      pred <- ((predict(svm.model,df1[c('var1','var2','var3')]) %>%
                  as.numeric()) - 1) %>% abs()
      df1$SVM <- df1$SVM  + pred


    }
    saveRDS(df1,file = paste0('Resultados_dados_correia/Metodo1/dataframes/dados_nu-',n,'_B-',i,'.RdS'))
  }
}
plot(burkitt$x,burkitt$y)
if (require("splancs")) {
  # load the data from package "splancs"
  data(burkitt, package="splancs")

  # order the times
  burkitt <- burkitt[order(burkitt$t), ]

  #Parameters for the SR detection
  epsilon <- 0.5 # relative change within the cluster
  radius <- 20 # radius
  threshold <- 161 # threshold limit

  res <- stcd(x=burkitt$x,
              y=burkitt$y,
              t=burkitt$t,
              radius=radius,
              epsilon=epsilon,
              areaA=1,
              areaAcapBk=1,
              threshold=threshold)

  #Index of the event
  which.max(res$R >= threshold)
}
burkitt[148,]
# Gráfico 1
n = 0.01
i = 500
aux <- readRDS(file = paste0('Resultados_dados_correia/Metodo1/dataframes/dados_nu-',n,'_B-',i,'.RdS'))
aux[aux$SVM >= i*0.95,]
plot(aux$x, aux$y, col = ifelse(aux$SVM >= i*0.95, "blue", "red"), pch = 16,
     xlab = "X", ylab = "Y")
legend("topright", legend = c("FALSE", "TRUE"), col = c("red", "blue"), pch = 16,
       title = "Cluster")
