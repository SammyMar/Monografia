## FUNCOES E BIBLIOTECAS
library(e1071)
library(dplyr)
source('FUNCAO_CRIAR_VARIAVEL.R')
#ESSE METODO NAO ADICIONA NOVAS VARIAVEIS AO TREINO
#ESTUDO 1 ----------------------------------------------------------------------
#FOI DEFINIDO COMO ALARME QUANDO 3 OBSERVACOES SAO CLASSIFICADAS COMO CLUSTER
#NO MESMO CILINDRO
set.seed(1234)
nu <- 0.01
tamanho_treino <- 0.20 #PROPORCAO DE TREINO
final <- data.frame(No.Cluster.Corr = NA,
                                  Cluster.Incor = NA,
                                  Allarm.Incor = NA)




#ITERACOES MONTE CARLO
for(m in 1:100){
  #ARMAZENAR OS MODELOS EM UMA LISTA
  modelos <- list()
  #CRIAR A AMOSTRA SEM CLUSTER
  x <- runif(300,0,1)
  y <- runif(300,0,1)
  t <- runif(300,0,1)
  df <-as.data.frame (rbind(cbind(x,y,t,FALSE)))
  colnames(df) <- c('x','y','t','cluster')
  df <- df[order(df$t),]

  #DATAFRAME PARA ARMAZENAR OBSERVACOES CONSIDERADAS CLUSTERS
  cluster <- cluster_aux <-  data.frame()
  alarme <- data.frame()

  #BANCO DE DADOS TESTE
  teste <- df |>
    cria_variaveis()

  #SELECIONANDO OS 90% DAS OBSERVACOES PARA TESTE
  teste <- tail(teste, nrow(df)*(1-tamanho_treino))

  #REAMOSTRAGEM COM EMBARALHAMENTO DOS TEMPOS E CRIACAO DO MODELO
  for(b in 1:30){

    #EMBARALHAR OS TEMPOS E CRIAR O BANCO TREINO, O TAMANHO VAI SER O TAMANHO
    #PRE DEFINIDO MAIS A JANELA DE TEMPO PRA CONSIDERAR AS OBSERVACOES EXCLUIDAS
    treino <- df[1:(nrow(df)*tamanho_treino),] |>
      mutate(t = sample(t)) |>
      cria_variaveis()

    #TREINAR O MODELO (ARMAZENAR OS MODELOS AO INVES DE TREINAR SEMPRE)
    modelos[[b]] <- svm(treino[c('var1','var2','var3')],y=NULL,
                   scale = TRUE,
                   type='one-classification',
                   nu=nu,
                   kernel="radial")
  }
  # Inicialize a coluna 'class' com zeros
  teste$class <- 0
  #PREDICAO DA AMOSTRA UMA DE CADA VEZ E TESTAR SE O ALARME SOA
  for(n in 1:nrow(teste)){
    observacao <- teste[n, c('var1', 'var2', 'var3')] # Extrai a observação atual
    pred <- 0  # Inicialize a variável pred com zero
    for(b in 1:30){
      # Realize a predição para o modelo atual (modelos[[b]])
      pred_model <- ((predict(modelos[[b]], observacao) %>% as.numeric()) - 1) %>% abs()
      pred <- pred + pred_model  # Adicione a predição atual à soma
    }
    teste$class[n] <- pred  + 1# Armazene a soma das classificações na coluna 'class'
    if(teste$class[n] > 0.9 * 30){
      cluster <- rbind(cluster, teste[n,])
      cluster_aux <- rbind(cluster_aux, teste[n,])
      obs <- teste[n,]
      x_center <- obs$x
      y_center <- obs$y
      t_center <- obs$t

      # Filtrar observações dentro do cilindro
      observacoes_cilindro <- cluster_aux[abs(cluster$x - x_center) <= 0.03 &
                                        abs(cluster$y - y_center) <= 0.03 &
                                        cluster$t >= (t_center - 0.02) &
                                        cluster$t <= t_center, ]

      # Se houver pelo menos 3 observações no cilindro, adicione a observação de alarme
      if (nrow(observacoes_cilindro) >= 3) {
        alarme <- rbind(alarme, obs)
        cluster_aux <- anti_join(cluster_aux, observacoes_cilindro)
      }
    }
  }

  # Adicionar as medidas ao DataFrame final
  final[m, "No.Cluster.Corr"] <- (nrow(teste) - nrow(cluster))
  final[m, "Cluster.Incor"] <- nrow(cluster)
  final[m, "Allarm.Incor"] <- nrow(alarme)
}
saveRDS(final,'Resultado_Estudo1_1.RDS')
plot(df$x,df$y)

#ESTUDO 2 -----------------------------------------------------
final <- data.frame(No.Cluster.Corr = NA,
                    No.Cluster.Incor =NA,
                    Cluster.Corr = NA,
                    Cluster.Incor = NA,
                    Allarm.Incor = NA,
                    Allarm.Corr =NA,
                    Dist.Primeiro.Alarm.corr =NA)

#ITERACOES MONTE CARLO
for(m in 1:100){

  #CRIAR A AMOSTRA SEM CLUSTER
  x <- runif(300,0,1)
  y <- runif(300,0,1)
  t <- runif(300,0,1)
  xc <- runif(50,.95,1)
  yc <- runif(50,.95,1)
  tc <- runif(50,.95,1)
  df <-as.data.frame( rbind(cbind(x,y,t,FALSE),cbind(xc,yc,tc,TRUE)))
  colnames(df) <- c('x','y','t','cluster')
  df <- df[order(df$t),]

  #DATAFRAME PARA ARMAZENAR OBSERVACOES CONSIDERADAS CLUSTERS
  cluster <- cluster_aux <-  data.frame()
  alarme <- data.frame()

  #BANCO DE DADOS TESTE
  teste <- df |>
    cria_variaveis()

  #SELECIONANDO OS 90% DAS OBSERVACOES PARA TESTE
  teste <- tail(teste, nrow(df)*(1-tamanho_treino))

  #REAMOSTRAGEM COM EMBARALHAMENTO DOS TEMPOS E CRIACAO DO MODELO
  for(b in 1:30){

    #EMBARALHAR OS TEMPOS E CRIAR O BANCO TREINO, O TAMANHO VAI SER O TAMANHO
    #PRE DEFINIDO MAIS A JANELA DE TEMPO PRA CONSIDERAR AS OBSERVACOES EXCLUIDAS
    treino <- df[1:(nrow(df)*tamanho_treino),] |>
      mutate(t = sample(t)) |>
      cria_variaveis()

    #TREINAR O MODELO (ARMAZENAR OS MODELOS AO INVES DE TREINAR SEMPRE)
    modelos[[b]] <- svm(treino[c('var1','var2','var3')],y=NULL,
                        scale = TRUE,
                        type='one-classification',
                        nu=nu,
                        kernel="radial")
  }
  # Inicialize a coluna 'class' com zeros
  teste$class <- 0
  #PREDICAO DA AMOSTRA UMA DE CADA VEZ E TESTAR SE O ALARME SOA
  for(n in 1:nrow(teste)){

    observacao <- teste[n, c('var1', 'var2', 'var3')] # Extrai a observação atual
    pred <- 0  # Inicialize a variável pred com zero
    for(b in 1:30){
      # Realize a predição para o modelo atual (modelos[[b]])
      pred_model <- ((predict(modelos[[b]], observacao) %>% as.numeric()) - 1) %>% abs()
      pred <- pred + pred_model  # Adicione a predição atual à soma
    }
    teste$class[n] <- pred  # Armazene a soma das classificações na coluna 'class'
    if(teste$class[n] > 0.9 * 30){
      cluster <- rbind(cluster, teste[n,])
      cluster_aux <- rbind(cluster_aux, teste[n,])
      obs <- teste[n,]
      x_center <- obs$x
      y_center <- obs$y
      t_center <- obs$t

      # Filtrar observações dentro do cilindro
      observacoes_cilindro <- cluster_aux[abs(cluster$x - x_center) <= 0.03 &
                                            abs(cluster$y - y_center) <= 0.03 &
                                            cluster$t >= (t_center - 0.02) &
                                            cluster$t <= t_center, ]

      # Se houver pelo menos 3 observações no cilindro, adicione a observação de alarme
      if (nrow(observacoes_cilindro) >= 3 ) {
        alarme <- rbind(alarme, obs)
        cluster_aux <- anti_join(cluster_aux, observacoes_cilindro)
      }
    }
  }
  # Adicionar as medidas ao DataFrame final
  Nao_cluster <- anti_join(teste, cluster)
  final[m, "No.Cluster.Corr"] <- nrow(Nao_cluster[!(Nao_cluster$t >= 0.95 &
                                                    Nao_cluster$t <= 1 &
                                                    Nao_cluster$x >= 0.95 &
                                                    Nao_cluster$x <= 1 &
                                                    Nao_cluster$y >= 0.95 &
                                                    Nao_cluster$y <= 1 ),])
  final[m, "No.Cluster.Incor"] <- nrow(Nao_cluster[Nao_cluster$t >= 0.95 &
                                                     Nao_cluster$t <= 1 &
                                                     Nao_cluster$x >= 0.95 &
                                                     Nao_cluster$x <= 1 &
                                                     Nao_cluster$y >= 0.95 &
                                                     Nao_cluster$y <= 1,])
  final[m, "Cluster.Incor"] <- nrow(cluster[!(cluster$t >= 0.95 &
                                              cluster$t <= 1 &
                                              cluster$x >= 0.95 &
                                              cluster$x <= 1 &
                                              cluster$y >= 0.95 &
                                              cluster$y <= 1),] )
  final[m, "Cluster.Corr"] <- nrow(cluster[cluster$t >= 0.95 &
                                     cluster$t <= 1 &
                                     cluster$x >= 0.95 &
                                     cluster$x <= 1 &
                                     cluster$y >= 0.95 &
                                     cluster$y <= 1,]  )
  final[m, "Allarm.Incor"] <- nrow(alarme[!(alarme$t >= 0.95 &
                                            alarme$t <= 1 &
                                            alarme$x >= 0.95 &
                                            alarme$x <= 1 &
                                            alarme$y >= 0.95 &
                                            alarme$y <= 1),])
  final[m, "Allarm.Corr"] <- nrow(alarme[alarme$t >= 0.95 &
                                           alarme$t <= 1 &
                                           alarme$x >= 0.95 &
                                           alarme$x <= 1 &
                                           alarme$y >= 0.95 &
                                           alarme$y <= 1,])
  aux <- alarme[alarme$t >= 0.95 &
           alarme$t <= 1 &
           alarme$x >= 0.95 &
           alarme$x <= 1 &
           alarme$y >= 0.95 &
           alarme$y <= 1,]
  final[m,"Dist.Primeiro.Alarm.corr"] <- aux[1,"t"] - 0.95
}

saveRDS(final,'Resultado_Estudo2_1.RDS')

#ESTUDO 3 -----------------------------------------------------
final <- data.frame(No.Cluster.Corr = NA,
                    No.Cluster.Incor =NA,
                    Cluster.Corr = NA,
                    Cluster.Incor = NA,
                    Allarm.Incor = NA,
                    Allarm.Corr =NA,
                    Dist.Primeiro.Alarm.corr.cluster1 =NA,
                    Dist.Primeiro.Alarm.corr.cluster2 =NA)


#ITERACOES MONTE CARLO
for(m in 1:100){

  #CRIAR A AMOSTRA SEM CLUSTER
  x <- runif(300,0,1)
  y <- runif(300,0,1)
  t <- runif(300,0,1)
  xc <- runif(50,.95,1)
  yc <- runif(50,.95,1)
  tc <- runif(50,.95,1)
  xc2 <- runif(50,.50,.55)
  yc2 <- runif(50,.70,.75)
  tc2 <- runif(50,.60,.65)
  df <-as.data.frame( rbind(cbind(x,y,t,FALSE),cbind(xc,yc,tc,TRUE),cbind(xc2,yc2,tc2,TRUE)))
  colnames(df) <- c('x','y','t','cluster')
  df <- df[order(df$t),]

  #DATAFRAME PARA ARMAZENAR OBSERVACOES CONSIDERADAS CLUSTERS
  cluster <- cluster_aux <-  data.frame()
  alarme <- data.frame()

  #BANCO DE DADOS TESTE
  teste <- df |>
    cria_variaveis()

  #SELECIONANDO OS 90% DAS OBSERVACOES PARA TESTE
  teste <- tail(teste, nrow(df)*(1-tamanho_treino))

  #REAMOSTRAGEM COM EMBARALHAMENTO DOS TEMPOS E CRIACAO DO MODELO
  for(b in 1:30){

    #EMBARALHAR OS TEMPOS E CRIAR O BANCO TREINO, O TAMANHO VAI SER O TAMANHO
    #PRE DEFINIDO MAIS A JANELA DE TEMPO PRA CONSIDERAR AS OBSERVACOES EXCLUIDAS
    treino <- df[1:(nrow(df)*tamanho_treino),] |>
      mutate(t = sample(t)) |>
      cria_variaveis()

    #TREINAR O MODELO (ARMAZENAR OS MODELOS AO INVES DE TREINAR SEMPRE)
    modelos[[b]] <- svm(treino[c('var1','var2','var3')],y=NULL,
                        scale = TRUE,
                        type='one-classification',
                        nu=nu,
                        kernel="radial")
  }
  # Inicialize a coluna 'class' com zeros
  teste$class <- 0
  #PREDICAO DA AMOSTRA UMA DE CADA VEZ E TESTAR SE O ALARME SOA
  for(n in 1:nrow(teste)){

    observacao <- teste[n, c('var1', 'var2', 'var3')] # Extrai a observação atual
    pred <- 0  # Inicialize a variável pred com zero
    for(b in 1:30){
      # Realize a predição para o modelo atual (modelos[[b]])
      pred_model <- ((predict(modelos[[b]], observacao) %>% as.numeric()) - 1) %>% abs()
      pred <- pred + pred_model  # Adicione a predição atual à soma
    }
    teste$class[n] <- pred  # Armazene a soma das classificações na coluna 'class'
    if(teste$class[n] > 0.9 * 30){
      cluster <- rbind(cluster, teste[n,])
      cluster_aux <- rbind(cluster_aux, teste[n,])
      obs <- teste[n,]
      x_center <- obs$x
      y_center <- obs$y
      t_center <- obs$t

      # Filtrar observações dentro do cilindro
      observacoes_cilindro <- cluster_aux[abs(cluster$x - x_center) <= 0.03 &
                                            abs(cluster$y - y_center) <= 0.03 &
                                            cluster$t >= (t_center - 0.02) &
                                            cluster$t <= t_center, ]

      # Se houver pelo menos 3 observações no cilindro, adicione a observação de alarme
      if (nrow(observacoes_cilindro) >= 3 ) {
        alarme <- rbind(alarme, obs)
        cluster_aux <- anti_join(cluster_aux, observacoes_cilindro)
      }
    }
  }
  # Adicionar as medidas ao DataFrame final
  Nao_cluster <- anti_join(teste, cluster)
  final[m, "No.Cluster.Corr"] <- nrow(Nao_cluster[!(Nao_cluster$t >= 0.95 &
                                                      Nao_cluster$t <= 1 &
                                                      Nao_cluster$x >= 0.60 &
                                                      Nao_cluster$x <= .65 &
                                                      Nao_cluster$y >= 0.95 &
                                                      Nao_cluster$y <= 1 ) & !(Nao_cluster$t >= 0.6 &
                                                      Nao_cluster$t <= .65 &
                                                      Nao_cluster$x >= 0.50 &
                                                      Nao_cluster$x <= .55 &
                                                      Nao_cluster$y >= 0.70 &
                                                      Nao_cluster$y <= .75 ),])
  final[m, "No.Cluster.Incor"] <- nrow(Nao_cluster[((Nao_cluster$t >= 0.95 &
                                                     Nao_cluster$t <= 1 &
                                                     Nao_cluster$x >= 0.95 &
                                                     Nao_cluster$x <= 1 &
                                                     Nao_cluster$y >= 0.95 &
                                                     Nao_cluster$y <= 1) | (Nao_cluster$t >= 0.6 &
                                                                              Nao_cluster$t <= .65 &
                                                                              Nao_cluster$x >= 0.50 &
                                                                              Nao_cluster$x <= .55 &
                                                                              Nao_cluster$y >= 0.70 &
                                                                              Nao_cluster$y <= .75 )),])
  final[m, "Cluster.Incor"] <- nrow(cluster[!(cluster$t >= 0.95 &
                                                cluster$t <= 1 &
                                                cluster$x >= 0.95 &
                                                cluster$x <= 1 &
                                                cluster$y >= 0.95 &
                                                cluster$y <= 1) & !(cluster$t >= 0.6 &
                                                                     cluster$t <= .65 &
                                                                     cluster$x >= 0.5 &
                                                                     cluster$x <= .55 &
                                                                     cluster$y >= 0.7 &
                                                                     cluster$y <= .75),] )
  final[m, "Cluster.Corr"] <- nrow(cluster[(cluster$t >= 0.95 &
                                              cluster$t <= 1 &
                                              cluster$x >= 0.95 &
                                              cluster$x <= 1 &
                                              cluster$y >= 0.95 &
                                              cluster$y <= 1) | (cluster$t >= 0.6 &
                                                                   cluster$t <= .65 &
                                                                   cluster$x >= 0.5 &
                                                                   cluster$x <= .55 &
                                                                   cluster$y >= 0.7 &
                                                                   cluster$y <= .75),]  )
  final[m, "Allarm.Incor"] <- nrow(alarme[!(alarme$t >= 0.95 &
                                              alarme$t <= 1 &
                                              alarme$x >= 0.95 &
                                              alarme$x <= 1 &
                                              alarme$y >= 0.95 &
                                              alarme$y <= 1) & !(alarme$t >= 0.6 &
                                                                  alarme$t <= .65 &
                                                                  alarme$x >= 0.5 &
                                                                  alarme$x <= .55 &
                                                                  alarme$y >= 0.7 &
                                                                  alarme$y <= .75),])
  final[m, "Allarm.Corr"] <- nrow(alarme[(alarme$t >= 0.95 &
                                           alarme$t <= 1 &
                                           alarme$x >= 0.95 &
                                           alarme$x <= 1 &
                                           alarme$y >= 0.95 &
                                           alarme$y <= 1) | (alarme$t >= 0.6 &
                                                              alarme$t <= .65 &
                                                              alarme$x >= 0.5 &
                                                              alarme$x <= .55 &
                                                              alarme$y >= 0.7 &
                                                              alarme$y <= .75),])
  aux1 <- alarme[alarme$t >= 0.95 &
                  alarme$t <= 1 &
                  alarme$x >= 0.95 &
                  alarme$x <= 1 &
                  alarme$y >= 0.95 &
                  alarme$y <= 1,]
  aux2 <- alarme[alarme$t >= 0.6 &
                   alarme$t <= .65 &
                   alarme$x >= 0.5 &
                   alarme$x <= .55 &
                   alarme$y >= 0.7 &
                   alarme$y <= .75,]
  final[m,"Dist.Primeiro.Alarm.corr.cluster1"] <- aux1[1,"t"] - 0.95
  final[m,"Dist.Primeiro.Alarm.corr.cluster2"] <- aux2[1,"t"] - 0.6
}


saveRDS(final,'Resultado_Estudo3_1.RDS')
