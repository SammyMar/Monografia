#Comparação nosso método com do marquinhos
source('modelo_MAD-STEC.R')
source('FUNCAO_CRIAR_VARIAVEL.R')
library(dplyr)
library(e1071)
library(isotree)
set.seed(2244)
#1: Sem cluster ----------
#parametros do metodo OCC
nu <- 0.01
rho <- 0.03
tw <- 0.02
AlarmeOCC <- 3
tamanho_treino <- 0.20
IFOREST_resultados <- list()
OCC_resultados <-  list()
MAD_STEC_resultados <- list()
for(M in 1:20){
  cluster_mad.stec <- list()
  modelos_svm <- list()
  modelos_if <- list()
  #DADOS
  x <- runif(300,0,1)
  y <- runif(300,0,1)
  t <- runif(300,0,1)
  df <-as.data.frame(cbind(x,y,t))
  colnames(df) <- c('x','y','t')
  df <- df[order(df$t),]


  #DATAFRAMES AUXILIARES PARA ARMAZENAR METODO OCC
  cluster_svm <- cluster_aux_svm <- cluster_if <- cluster_aux_if <-  data.frame()
  alarme_svm <- alarme_if <- data.frame()
  teste <- df |>
    cria_variaveis(rho = rho,time_window = tw)
  teste <- tail(teste, nrow(df)*(1-tamanho_treino))
  treino_if <- treino_svm <- df[1:(nrow(df)*tamanho_treino),]
  teste$class <- 0
  indicador_svm <- indicador_if <- T
  contador_if <- contador_svm <- 0
  for(n in 1:nrow(teste)){
    #REAMOSTRAGEM COM EMBARALHAMENTO DOS TEMPOS E CRIACAO DO MODELO
    if(indicador_svm == T){
      for(b in 1:30){
        #EMBARALHAR OS TEMPOS E CRIAR O BANCO TREINO, O TAMANHO VAI SER O TAMANHO
        #PRE DEFINIDO MAIS A JANELA DE TEMPO PRA CONSIDERAR AS OBSERVACOES EXCLUIDAS
        treino_b_svm <- treino_svm |>
          mutate(t = sample(t)) |>
          cria_variaveis(rho = rho,time_window = tw)
        #TREINAR O MODELO (ARMAZENAR OS MODELOS AO INVES DE TREINAR SEMPRE)
        modelos_svm[[b]] <- svm(treino_b_svm[c('var1','var2','var3')],y=NULL,
                                scale = TRUE,
                                type='one-classification',
                                nu=nu,
                                kernel="radial")
      }
    }
    if(indicador_if == T){
      for(b in 1:30){

        #EMBARALHAR OS TEMPOS E CRIAR O BANCO TREINO, O TAMANHO VAI SER O TAMANHO
        #PRE DEFINIDO MAIS A JANELA DE TEMPO PRA CONSIDERAR AS OBSERVACOES EXCLUIDAS
        treino_b_if <- treino_if |>
          mutate(t = sample(t)) |>
          cria_variaveis(rho = rho,time_window = tw)
        modelos_if[[b]] <- isolation.forest(treino_b_if[c('var1','var2','var3')],
                                            ndim=1,
                                            ntrees=100,
                                            nthreads=1)
      }
    }
    n_mad.stec <- n + (nrow(df)*tamanho_treino)
    cluster_mad.stec[[n]] <- MAD_STEC(E = df[1:n_mad.stec,],rho = rho,A = n_mad.stec, n = n_mad.stec)
    indicador <- F
    observacao <- teste[n, c('var1', 'var2', 'var3')] # Extrai a observação atual
    pred_svm <- 0# Inicialize a variável pred com zero
    pred_if <- 0
    for(b in 1:30){
      # Realize a predição para o modelo atual (modelos[[b]])
      pred_model <- ((predict(modelos_svm[[b]], observacao) %>% as.numeric()) - 1) %>% abs()
      pred_svm <- pred_svm + pred_model  # Adicione a predição atual à soma
      pred_model <- (predict(modelos_if[[b]], observacao) %>% as.numeric())
      pred_if <- pred_if + pred_model  # Adicione a predição atual à soma
    }
    teste$class_svm[n] <- pred_svm  + 1# Armazene a soma das classificações na coluna 'class'
    teste$class_if[n] <- pred_if + 1
    if(teste$class_svm[n] > 0.9 * 30){
      cluster_svm <- rbind(cluster_svm, teste[n,])
      cluster_aux_svm <- rbind(cluster_aux_svm, teste[n,])
      obs <- teste[n,]
      x_center <- obs$x
      y_center <- obs$y
      t_center <- obs$t

      # Filtrar observações dentro do cilindro
      observacoes_cilindro <- cluster_aux_svm[abs(cluster_svm$x - x_center) <= 0.03 &
                                            abs(cluster_svm$y - y_center) <= 0.03 &
                                            cluster_svm$t >= (t_center - 0.02) &
                                            cluster_svm$t <= t_center, ]

      # Se houver pelo menos 3 observações no cilindro, adicione a observação de alarme
      if (nrow(observacoes_cilindro) >= AlarmeOCC) {
        alarme_svm <- rbind(alarme_svm, obs)
        cluster_aux_svm <- anti_join(cluster_aux_svm, observacoes_cilindro)
      }
    }
    if(teste$class_if[n] > 0.7 * 30){
      cluster_if <- rbind(cluster_if, teste[n,])
      cluster_aux_if <- rbind(cluster_aux_if, teste[n,])
      obs <- teste[n,]
      x_center <- obs$x
      y_center <- obs$y
      t_center <- obs$t

      # Filtrar observações dentro do cilindro
      observacoes_cilindro <- cluster_aux_if[abs(cluster_if$x - x_center) <= 0.03 &
                                               abs(cluster_if$y - y_center) <= 0.03 &
                                               cluster_if$t >= (t_center - 0.02) &
                                               cluster_if$t <= t_center, ]

      # Se houver pelo menos 3 observações no cilindro, adicione a observação de alarme
      if (nrow(observacoes_cilindro) >= AlarmeOCC) {
        alarme_if <- rbind(alarme_if, obs)
        cluster_aux_if <- anti_join(cluster_aux_if, observacoes_cilindro)
      }
    }
    if(!((teste$class_svm[n] > 0.9 * 30))){
      treino_svm <- rbind(treino_svm,teste[n,c('x','y','t')])
      contador_svm <- contador_svm + 1
      if(contador_svm == 10){
        indicador_svm <- T
        contador_svm <- 0
      }
    }
    if(!((teste$class_if[n] > 0.6 * 30))){
      treino_if <- rbind(treino_if,teste[n,c('x','y','t')])
      contador_if <- contador_if + 1
      if(contador_if == 10){
        indicador_if <- T
        contador_if <- 0
      }
    }
  }
  IFOREST_resultados[[M]] <- list( alarme_if, cluster_if)
  OCC_resultados[[M]] <- list( alarme_svm, cluster_svm)
  MAD_STEC_resultados[[M]] <- cluster_mad.stec
  print(M )
}
saveRDS(OCC_resultados, file = 'Comparativo_OCC_MADSTEC/OCC_1-2.RDS')
saveRDS(IFOREST_resultados, file = 'Comparativo_OCC_MADSTEC/iforest_1-2.RDS')
saveRDS(MAD_STEC_resultados, file = 'Comparativo_OCC_MADSTEC/MAD-STEC_1-2.RDS')
#2: 1 Cluster -------------

OCC_resultados <-  list()
MAD_STEC_resultados <- list()
IFOREST_resultados <- list()
for(M in 1:20){
  cluster_mad.stec <- list()
  modelos_if <- modelos_svm <- list()
  #DADOS
  x <- runif(300,0,1)
  y <- runif(300,0,1)
  t <- runif(300,0,1)
  xc <- runif(50,.95,1)
  yc <- runif(50,.95,1)
  tc <- runif(50,.95,1)
  df <-as.data.frame( rbind(cbind(x,y,t),cbind(xc,yc,tc)))
  colnames(df) <- c('x','y','t')
  df <- df[order(df$t),]

  #DATAFRAMES AUXILIARES PARA ARMAZENAR METODO OCC
  cluster_svm <- cluster_aux_svm <- cluster_if <- cluster_aux_if <-  data.frame()
  alarme_svm <- alarme_if <- data.frame()
  teste <- df |>
    cria_variaveis(rho = rho,time_window = tw)
  teste <- tail(teste, nrow(df)*(1-tamanho_treino))
  treino_if <- treino_svm <- df[1:(nrow(df)*tamanho_treino),]
  teste$class <- 0
  indicador_svm <- indicador_if <- T
  contador_if <- contador_svm <- 0
  for(n in 1:nrow(teste)){
    #REAMOSTRAGEM COM EMBARALHAMENTO DOS TEMPOS E CRIACAO DO MODELO
    if(indicador_svm == T){
      for(b in 1:30){
        #EMBARALHAR OS TEMPOS E CRIAR O BANCO TREINO, O TAMANHO VAI SER O TAMANHO
        #PRE DEFINIDO MAIS A JANELA DE TEMPO PRA CONSIDERAR AS OBSERVACOES EXCLUIDAS
        treino_b_svm <- treino_svm |>
          mutate(t = sample(t)) |>
          cria_variaveis(rho = rho,time_window = tw)
        #TREINAR O MODELO (ARMAZENAR OS MODELOS AO INVES DE TREINAR SEMPRE)
        modelos_svm[[b]] <- svm(treino_b_svm[c('var1','var2','var3')],y=NULL,
                                scale = TRUE,
                                type='one-classification',
                                nu=nu,
                                kernel="radial")
      }
    }
    if(indicador_if == T){
      for(b in 1:30){

        #EMBARALHAR OS TEMPOS E CRIAR O BANCO TREINO, O TAMANHO VAI SER O TAMANHO
        #PRE DEFINIDO MAIS A JANELA DE TEMPO PRA CONSIDERAR AS OBSERVACOES EXCLUIDAS
        treino_b_if <- treino_if |>
          mutate(t = sample(t)) |>
          cria_variaveis(rho = rho,time_window = tw)
        modelos_if[[b]] <- isolation.forest(treino_b_if[c('var1','var2','var3')],
                                            ndim=1,
                                            ntrees=100,
                                            nthreads=1)
      }
    }
    n_mad.stec <- n + (nrow(df)*tamanho_treino)
    cluster_mad.stec[[n]] <- MAD_STEC(E = df[1:n_mad.stec,],rho = rho,A = n_mad.stec, n = n_mad.stec)
    indicador <- F
    observacao <- teste[n, c('var1', 'var2', 'var3')] # Extrai a observação atual
    pred_svm <- 0# Inicialize a variável pred com zero
    pred_if <- 0
    for(b in 1:30){
      # Realize a predição para o modelo atual (modelos[[b]])
      pred_model <- ((predict(modelos_svm[[b]], observacao) %>% as.numeric()) - 1) %>% abs()
      pred_svm <- pred_svm + pred_model  # Adicione a predição atual à soma
      pred_model <- (predict(modelos_if[[b]], observacao) %>% as.numeric())
      pred_if <- pred_if + pred_model  # Adicione a predição atual à soma
    }
    teste$class_svm[n] <- pred_svm  +1# Armazene a soma das classificações na coluna 'class'
    teste$class_if[n] <- pred_if + 1
    if(teste$class_svm[n] > 0.9 * 30){
      cluster_svm <- rbind(cluster_svm, teste[n,])
      cluster_aux_svm <- rbind(cluster_aux_svm, teste[n,])
      obs <- teste[n,]
      x_center <- obs$x
      y_center <- obs$y
      t_center <- obs$t

      # Filtrar observações dentro do cilindro
      observacoes_cilindro <- cluster_aux_svm[abs(cluster_svm$x - x_center) <= 0.03 &
                                            abs(cluster_svm$y - y_center) <= 0.03 &
                                            cluster_svm$t >= (t_center - 0.02) &
                                            cluster_svm$t <= t_center, ]

      # Se houver pelo menos 3 observações no cilindro, adicione a observação de alarme
      if (nrow(observacoes_cilindro) >= AlarmeOCC) {
        alarme_svm <- rbind(alarme_svm, obs)
        cluster_aux_svm <- anti_join(cluster_aux_svm, observacoes_cilindro)
      }
    }
    if(teste$class_if[n] > 0.7 * 30){
      cluster_if <- rbind(cluster_if, teste[n,])
      cluster_aux_if <- rbind(cluster_aux_if, teste[n,])
      obs <- teste[n,]
      x_center <- obs$x
      y_center <- obs$y
      t_center <- obs$t

      # Filtrar observações dentro do cilindro
      observacoes_cilindro <- cluster_aux_if[abs(cluster_if$x - x_center) <= 0.03 &
                                               abs(cluster_if$y - y_center) <= 0.03 &
                                               cluster_if$t >= (t_center - 0.02) &
                                               cluster_if$t <= t_center, ]

      # Se houver pelo menos 3 observações no cilindro, adicione a observação de alarme
      if (nrow(observacoes_cilindro) >= AlarmeOCC) {
        alarme_if <- rbind(alarme_if, obs)
        cluster_aux_if <- anti_join(cluster_aux_if, observacoes_cilindro)
      }
    }
    if(!((teste$class_svm[n] > 0.9 * 30))){
      treino_svm <- rbind(treino_svm,teste[n,c('x','y','t')])
      contador_svm <- contador_svm + 1
      if(contador_svm == 10){
        indicador_svm <- T
        contador_svm <- 0
      }
    }
    if(!((teste$class_if[n] > 0.6 * 30))){
      treino_if <- rbind(treino_if,teste[n,c('x','y','t')])
      contador_if <- contador_if + 1
      if(contador_if == 10){
        indicador_if <- T
        contador_if <- 0
      }
    }
  }
  IFOREST_resultados[[M]] <- list( alarme_if, cluster_if)
  OCC_resultados[[M]] <- list( alarme_svm, cluster_svm)
  MAD_STEC_resultados[[M]] <- cluster_mad.stec
  print(M)
}
saveRDS(OCC_resultados, file = 'Comparativo_OCC_MADSTEC/OCC_2-2.RDS')
saveRDS(IFOREST_resultados, file = 'Comparativo_OCC_MADSTEC/iforest_2-2.RDS')
saveRDS(MAD_STEC_resultados, file = 'Comparativo_OCC_MADSTEC/MAD-STEC_2-2.RDS')
#3: 2 Clusters ------------
set.seed(2244)
OCC_resultados <-  list()
MAD_STEC_resultados <- list()
IFOREST_resultados <- list()
for(M in 1:20){
  cluster_mad.stec <- list()
  modelos_svm <- modelos_if <- list()
  #DADOS
  x <- runif(300,0,1)
  y <- runif(300,0,1)
  t <- runif(300,0,1)
  xc <- runif(50,.95,1)
  yc <- runif(50,.95,1)
  tc <- runif(50,.95,1)
  xc2 <- runif(50,.50,.55)
  yc2 <- runif(50,.70,.75)
  tc2 <- runif(50,.60,.65)
  df <-as.data.frame( rbind(cbind(x,y,t),cbind(xc,yc,tc),cbind(xc2,yc2,tc2)))
  colnames(df) <- c('x','y','t')
  df <- df[order(df$t),]

  #DATAFRAMES AUXILIARES PARA ARMAZENAR METODO OCC
  cluster_svm <- cluster_aux_svm <- cluster_if <- cluster_aux_if <-  data.frame()
  alarme_svm <- alarme_if <- data.frame()
  teste <- df |>
    cria_variaveis(rho = rho,time_window = tw)
  teste <- tail(teste, nrow(df)*(1-tamanho_treino))
  treino_if <- treino_svm <- df[1:(nrow(df)*tamanho_treino),]
  teste$class <- 0
  indicador_svm <- indicador_if <- T
  contador_if <- contador_svm <- 0
  for(n in 1:nrow(teste)){
    #REAMOSTRAGEM COM EMBARALHAMENTO DOS TEMPOS E CRIACAO DO MODELO
    if(indicador_svm == T){
      for(b in 1:30){
        #EMBARALHAR OS TEMPOS E CRIAR O BANCO TREINO, O TAMANHO VAI SER O TAMANHO
        #PRE DEFINIDO MAIS A JANELA DE TEMPO PRA CONSIDERAR AS OBSERVACOES EXCLUIDAS
        treino_b_svm <- treino_svm |>
          mutate(t = sample(t)) |>
          cria_variaveis(rho = rho,time_window = tw)
        #TREINAR O MODELO (ARMAZENAR OS MODELOS AO INVES DE TREINAR SEMPRE)
        modelos_svm[[b]] <- svm(treino_b_svm[c('var1','var2','var3')],y=NULL,
                                scale = TRUE,
                                type='one-classification',
                                nu=nu,
                                kernel="radial")
      }
    }
    if(indicador_if == T){
      for(b in 1:30){

        #EMBARALHAR OS TEMPOS E CRIAR O BANCO TREINO, O TAMANHO VAI SER O TAMANHO
        #PRE DEFINIDO MAIS A JANELA DE TEMPO PRA CONSIDERAR AS OBSERVACOES EXCLUIDAS
        treino_b_if <- treino_if |>
          mutate(t = sample(t)) |>
          cria_variaveis(rho = rho,time_window = tw)
        modelos_if[[b]] <- isolation.forest(treino_b_if[c('var1','var2','var3')],
                                            ndim=1,
                                            ntrees=100,
                                            nthreads=1)
      }
    }
    n_mad.stec <- n + (nrow(df)*tamanho_treino)
    cluster_mad.stec[[n]] <- MAD_STEC(E = df[1:n_mad.stec,],rho = rho,A = n_mad.stec, n = n_mad.stec)
    indicador <- F
    observacao <- teste[n, c('var1', 'var2', 'var3')] # Extrai a observação atual
    pred_svm <- 0# Inicialize a variável pred com zero
    pred_if <- 0
    for(b in 1:30){
      # Realize a predição para o modelo atual (modelos[[b]])
      pred_model <- ((predict(modelos_svm[[b]], observacao) %>% as.numeric()) - 1) %>% abs()
      pred_svm <- pred_svm + pred_model  # Adicione a predição atual à soma
      pred_model <- (predict(modelos_if[[b]], observacao) %>% as.numeric())
      pred_if <- pred_if + pred_model  # Adicione a predição atual à soma
    }
    teste$class_svm[n] <- pred_svm + 1  # Armazene a soma das classificações na coluna 'class'
    teste$class_if[n] <- pred_if + 1
    if(teste$class_svm[n] > 0.9 * 30){
      cluster_svm <- rbind(cluster_svm, teste[n,])
      cluster_aux_svm <- rbind(cluster_aux_svm, teste[n,])
      obs <- teste[n,]
      x_center <- obs$x
      y_center <- obs$y
      t_center <- obs$t

      # Filtrar observações dentro do cilindro
      observacoes_cilindro <- cluster_aux_svm[abs(cluster_svm$x - x_center) <= 0.03 &
                                            abs(cluster_svm$y - y_center) <= 0.03 &
                                            cluster_svm$t >= (t_center - 0.02) &
                                            cluster_svm$t <= t_center, ]

      # Se houver pelo menos 3 observações no cilindro, adicione a observação de alarme
      if (nrow(observacoes_cilindro) >= AlarmeOCC) {
        alarme_svm <- rbind(alarme_svm, obs)
        cluster_aux_svm <- anti_join(cluster_aux_svm, observacoes_cilindro)
      }
    }
    if(teste$class_if[n] > 0.7 * 30){
      cluster_if <- rbind(cluster_if, teste[n,])
      cluster_aux_if <- rbind(cluster_aux_if, teste[n,])
      obs <- teste[n,]
      x_center <- obs$x
      y_center <- obs$y
      t_center <- obs$t

      # Filtrar observações dentro do cilindro
      observacoes_cilindro <- cluster_aux_if[abs(cluster_if$x - x_center) <= 0.03 &
                                               abs(cluster_if$y - y_center) <= 0.03 &
                                               cluster_if$t >= (t_center - 0.02) &
                                               cluster_if$t <= t_center, ]

      # Se houver pelo menos 3 observações no cilindro, adicione a observação de alarme
      if (nrow(observacoes_cilindro) >= AlarmeOCC) {
        alarme_if <- rbind(alarme_if, obs)
        cluster_aux_if <- anti_join(cluster_aux_if, observacoes_cilindro)
      }
    }
    if(!((teste$class_svm[n] > 0.9 * 30))){
      treino_svm <- rbind(treino_svm,teste[n,c('x','y','t')])
      contador_svm <- contador_svm + 1
      if(contador_svm == 10){
        indicador_svm <- T
        contador_svm <- 0
      }
    }
    if(!((teste$class_if[n] > 0.6 * 30))){
      treino_if <- rbind(treino_if,teste[n,c('x','y','t')])
      contador_if <- contador_if + 1
      if(contador_if == 10){
        indicador_if <- T
        contador_if <- 0
      }
    }
  }
  IFOREST_resultados[[M]] <- list( alarme_if, cluster_if)
  OCC_resultados[[M]] <- list( alarme_svm, cluster_svm)
  MAD_STEC_resultados[[M]] <- cluster_mad.stec
  print(M)
}
saveRDS(OCC_resultados, file = 'Comparativo_OCC_MADSTEC/OCC_3-2.RDS')
saveRDS(IFOREST_resultados, file = 'Comparativo_OCC_MADSTEC/iforest_3-2.RDS')
saveRDS(MAD_STEC_resultados, file = 'Comparativo_OCC_MADSTEC/MAD-STEC_3-2.RDS')
