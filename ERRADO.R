## FUNCOES E BIBLIOTECAS
library(e1071)
library(dplyr)
source('FUNCAO_CRIAR_VARIAVEL.R')
#ESTUDO 1 ----------------------------------------------------------------------
#Dividindo em treio, onde sera equivalente a uma pequena porcao dos dados reais,
#e dar predict nas observacoes uma de cada vez simulando entrada de um novo dado,
#Caso nao seja classificada como anomalia, entra para o modelo como treino.
#FOI DEFINIDO COMO ALARME QUANDO 5 OBSERVACOES SAO CLASSIFICADAS COMO CLUSTER
#NO MESMO CILINDRO
set.seed(1234)
nu <- 0.01
n_teste <- 0.1
resultados <- final <- data.frame(No.Cluster.Corr = NA,
                         Cluster.Incor = NA,
                         Allarm.Incor = NA)

#MUDANCAS: APLICAR OUTRO METODO, SEM ADICIONAR AS NAO CLUSTERS AO TREINO
#MUDANCAS: MUDAR EM VEZ DE UMA A UMA OBSERVAR, GRUPO DE OBSERVACOES

for(m in 1:10){
  x <- runif(300,0,1)
  y <- runif(300,0,1)
  t <- runif(300,0,1)
  df <-as.data.frame (rbind(cbind(x,y,t,FALSE)))
  colnames(df) <- c('x','y','t','cluster')
  df <- df[order(df$t),]
  cluster <- data.frame()

  for(n in (nrow(df)*n_teste):nrow(df)){
    teste <- df[1:(n+1),] |>
      cria_variaveis()
    teste <- teste[nrow(teste),]
    classi <- 0
    for(b in 1:30){
      treino <- df[1:n,] |>
                  mutate(t = sample(t)) |>
                    cria_variaveis()
      svm.model<-svm(amostrab[c('var1','var2','var3')],y=NULL,
                     scale = TRUE,
                     type='one-classification',
                     nu=nu,
                     kernel="radial")

      pred <- ((predict(svm.model,teste[c('var1','var2','var3')]) %>%
                 as.numeric()) - 1) %>% abs()
      if(length(pred) == 0)pred <- 0

      classi <- classi + pred
    }
    if(classi >= B*0.9){
      alarme <- 0
      for(i in 1:nrow(cluster)){
        distancia_xy <- sqrt((cluster$x[i] - teste$x)^2 + (cluster$y[i] - teste$y)^2)
        alarme <- ((distancia_xy <= 0.03 & abs(cluster$t[i] - teste$t) <= 0.02) |> as.numeric()) + alarme
      }
      cluster <- rbind(teste,cluster)
      n <- n+1
      resultados$Cluster.Incor = resultados$Cluster.Incor + 1
      if(alarme >=5){
        resultados$Allarm.Incor =  resultados$Allarm.Incor + 1
      }
    }else{
      resultados$No.Cluster.Corr = resultados$No.Cluster.Corr + 1
    }
  }
  resultados <- resultados/length((nrow(df)*n_teste):nrow(df))
final <- resultados + final
}
saveRDS(final,'Resultado_Estudo1.RDS')

#ESTUDO 2 ---------------------------------------------------------------------

nu <- 0.01
n_teste <- 0.1
resultados <- final <- data.frame(No.Cluster.Corr = NA,
                                  No.Cluster.Incor = NA,
                                  Cluster.Incor = NA,
                                  Cluster.Corr = NA,
                                  Allarm.Incor = NA,
                                  Allarm.Corr = NA)
for(m in 1:100){
  x <- runif(500,0,1)
  y <- runif(500,0,1)
  t <- runif(500,0,1)
  xc <- runif(70,.95,1)
  yc <- runif(70,.95,1)
  tc <- runif(70,.95,1)
  df <-as.data.frame( rbind(cbind(x,y,t,FALSE),cbind(xc,yc,tc,TRUE)))
  colnames(df) <- c('x','y','t','cluster')
  df <- df[order(df$t),]
  cluster <- data.frame()

  for(n in (nrow(df)*n_teste):nrow(df)){
    teste <- df[1:(n+1),] |>
      cria_variaveis()
    teste <- teste[nrow(teste),]
    classi <- 0
    for(b in 1:100){
      treino <- df[1:n,] |>
        mutate(t = sample(t)) |>
        cria_variaveis()
      svm.model<-svm(amostrab[c('var1','var2','var3')],y=NULL,
                     scale = TRUE,
                     type='one-classification',
                     nu=nu,
                     kernel="radial")

      pred <- ((predict(svm.model,teste[c('var1','var2','var3')]) %>%
                  as.numeric()) - 1) %>% abs()
      if(length(pred) == 0)pred <- 0
      classi <- classi + pred
    }
    if(classi >= B*0.9){
      alarme <- 0
      for(i in 1:nrow(cluster)){
        distancia_xy <- sqrt((cluster$x[i] - teste$x)^2 + (cluster$y[i] - teste$y)^2)
        alarme <- ((distancia_xy <= 0.03 & abs(cluster$t[i] - teste$t) <= 0.02) |> as.numeric()) + alarme
      }
      cluster <- rbind(teste,cluster)
      n <- n+1
      if(teste$cluster == 1){
        resultados$Cluster.Corr = resultados$Cluster.Corr + 1
        if(alarme >=5)resultados$Allarm.Corr = resultados$Allarm.Corr + 1
      }else{
        resultados$Cluster.Incor = resultados$Cluster.Incor + 1
        if(alarme >=5)resultados$Allarm.Incor = resultados$Allarm.Incor + 1
      }
    }else{
      if(teste$cluster == 0){
      resultados$No.Cluster.Corr = resultados$No.Cluster.Corr + 1
      }else{
        resultados$No.Cluster.Incor = resultados$No.Cluster.Incor + 1
      }
    }
  }
  resultados <- resultados/length((nrow(df)*n_teste):nrow(df))
  final <- resultados + final
}
saveRDS(final,'Resultado_Estudo2.RDS')

#ESTUDO 3 ----------------------------------------------------------------------


nu <- 0.01
n_teste <- 0.1
resultados <- final <- data.frame(No.Cluster.Corr = NA,
                                  No.Cluster.Incor = NA,
                                  Cluster.Incor = NA,
                                  Cluster.Corr = NA,
                                  Allarm.Incor = NA,
                                  Allarm.Corr = NA)
for(m in 1:100){
  x <- runif(500,0,1)
  y <- runif(500,0,1)
  t <- runif(500,0,1)
  xc <- runif(70,.95,1)
  yc <- runif(70,.95,1)
  tc <- runif(70,.95,1)
  xc2 <- runif(70,.50,.55)
  yc2 <- runif(70,.70,.75)
  tc2 <- runif(70,.60,.65)
  df <-as.data.frame( rbind(cbind(x,y,t,FALSE),cbind(xc,yc,tc,TRUE),cbind(xc2,yc2,tc2,TRUE)))
  colnames(df) <- c('x','y','t','cluster')
  df <- df[order(df$t),]
  cluster <- data.frame()

  for(n in (nrow(df)*n_teste):nrow(df)){
    teste <- df[1:(n+1),] |>
      cria_variaveis()
    teste <- teste[nrow(teste),]
    classi <- 0
    for(b in 1:100){
      treino <- df[1:n,] |>
        mutate(t = sample(t)) |>
        cria_variaveis()
      svm.model<-svm(amostrab[c('var1','var2','var3')],y=NULL,
                     scale = TRUE,
                     type='one-classification',
                     nu=nu,
                     kernel="radial")

      pred <- ((predict(svm.model,teste[c('var1','var2','var3')]) %>%
                  as.numeric()) - 1) %>% abs()
      if(length(pred) == 0)pred <- 0
      classi <- classi + pred
    }
    if(classi >= B*0.9){
      alarme <- 0
      for(i in 1:nrow(cluster)){
        distancia_xy <- sqrt((cluster$x[i] - teste$x)^2 + (cluster$y[i] - teste$y)^2)
        alarme <- ((distancia_xy <= 0.03 & abs(cluster$t[i] - teste$t) <= 0.02) |> as.numeric()) + alarme
      }
      cluster <- rbind(teste,cluster)
      n <- n+1
      if(teste$cluster == 1){
        resultados$Cluster.Corr = resultados$Cluster.Corr + 1
        if(alarme >=5)resultados$Allarm.Corr = resultados$Allarm.Corr + 1
      }else{
        resultados$Cluster.Incor = resultados$Cluster.Incor + 1
        if(alarme >=5)resultados$Allarm.Incor = resultados$Allarm.Incor + 1
      }
    }else{
      if(teste$cluster == 0){
        resultados$No.Cluster.Corr = resultados$No.Cluster.Corr + 1
      }else{
        resultados$No.Cluster.Incor = resultados$No.Cluster.Incor + 1
      }
    }
  }
  resultados <- resultados/length((nrow(df)*n_teste):nrow(df))
  final <- resultados + final
}
saveRDS(final,'Resultado_Estudo3.RDS')
