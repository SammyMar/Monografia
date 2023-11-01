#Comparação nosso método com do marquinhos
source('modelo_MAD-STEC.R')
source('FUNCAO_CRIAR_VARIAVEL.R')
library(dplyr)
library(e1071)
set.seed(2244)
#1: Sem cluster ----------
#parametros do metodo OCC
nu <- 0.01
rho <- 0.03
tw <- 0.02
AlarmeOCC <- 3
tamanho_treino <- 0.20

OCC_resultados <-  list()
MAD_STEC_resultados <- list()
for(M in 1:10){
  cluster_mad.stec <- list()
  modelos <- list()
  #DADOS
  x <- runif(300,0,1)
  y <- runif(300,0,1)
  t <- runif(300,0,1)
  df <-as.data.frame(cbind(x,y,t))
  colnames(df) <- c('x','y','t')
  df <- df[order(df$t),]

  #DATAFRAMES AUXILIARES PARA ARMAZENAR METODO OCC
  cluster <- cluster_aux <-  data.frame()
  alarme <- data.frame()
  teste <- df |>
    cria_variaveis(rho = rho,time_window = tw)
  teste <- tail(teste, nrow(df)*(1-tamanho_treino))
  treino <- df[1:(nrow(df)*tamanho_treino),]
  teste$class <- 0
  indicador <- T
  contador <- 0
  for(n in 1:nrow(teste)){
    #REAMOSTRAGEM COM EMBARALHAMENTO DOS TEMPOS E CRIACAO DO MODELO
    if(indicador == T){
      for(b in 1:30){

        #EMBARALHAR OS TEMPOS E CRIAR O BANCO TREINO, O TAMANHO VAI SER O TAMANHO
        #PRE DEFINIDO MAIS A JANELA DE TEMPO PRA CONSIDERAR AS OBSERVACOES EXCLUIDAS
        treino_b <- treino |>
          mutate(t = sample(t)) |>
          cria_variaveis(rho = rho,time_window = tw)

        #TREINAR O MODELO (ARMAZENAR OS MODELOS AO INVES DE TREINAR SEMPRE)
        modelos[[b]] <- svm(treino_b[c('var1','var2','var3')],y=NULL,
                            scale = TRUE,
                            type='one-classification',
                            nu=nu,
                            kernel="radial")
      }
    }
    n_mad.stec <- n + (nrow(df)*tamanho_treino)
    cluster_mad.stec[[n]] <- MAD_STEC(E = df[1:n_mad.stec,],rho = rho,A = n_mad.stec, n = n_mad.stec)
    indicador <- F
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
      if (nrow(observacoes_cilindro) >= AlarmeOCC) {
        alarme <- rbind(alarme, obs)
        cluster_aux <- anti_join(cluster_aux, observacoes_cilindro)
      }
    } else{
      treino <- rbind(treino,teste[n,c('x','y','t')])
      contador <- contador + 1
      if(contador == 10){
        indicador <- T
        contador <- 0
      }
    }
  }
OCC_resultados[[M]] <- alarme
MAD_STEC_resultados[[M]] <- cluster_mad.stec
}
saveRDS(OCC_resultados, file = 'Comparativo_OCC_MADSTEC/OCC_1.RDS')

saveRDS(MAD_STEC_resultados, file = 'Comparativo_OCC_MADSTEC/MAD-STEC_1.RDS')
#2: 1 Cluster -------------

set.seed(2244)
OCC_resultados <-  list()
MAD_STEC_resultados <- list()
for(M in 1:10){
  cluster_mad.stec <- list()
  modelos <- list()
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
  cluster <- cluster_aux <-  data.frame()
  alarme <- data.frame()
  teste <- df |>
    cria_variaveis(rho = rho,time_window = tw)
  teste <- tail(teste, nrow(df)*(1-tamanho_treino))
  treino <- df[1:(nrow(df)*tamanho_treino),]
  teste$class <- 0
  indicador <- T
  contador <- 0
  for(n in 1:nrow(teste)){
    #REAMOSTRAGEM COM EMBARALHAMENTO DOS TEMPOS E CRIACAO DO MODELO
    if(indicador == T){
      for(b in 1:30){

        #EMBARALHAR OS TEMPOS E CRIAR O BANCO TREINO, O TAMANHO VAI SER O TAMANHO
        #PRE DEFINIDO MAIS A JANELA DE TEMPO PRA CONSIDERAR AS OBSERVACOES EXCLUIDAS
        treino_b <- treino |>
          mutate(t = sample(t)) |>
          cria_variaveis(rho = rho,time_window = tw)

        #TREINAR O MODELO (ARMAZENAR OS MODELOS AO INVES DE TREINAR SEMPRE)
        modelos[[b]] <- svm(treino_b[c('var1','var2','var3')],y=NULL,
                            scale = TRUE,
                            type='one-classification',
                            nu=nu,
                            kernel="radial")
      }
    }
    n_mad.stec <- n + (nrow(df)*tamanho_treino)
    cluster_mad.stec[[n]] <- MAD_STEC(E = df[1:n_mad.stec,],rho = rho,A = n_mad.stec, n = n_mad.stec)
    indicador <- F
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
      if (nrow(observacoes_cilindro) >= AlarmeOCC) {
        alarme <- rbind(alarme, obs)
        cluster_aux <- anti_join(cluster_aux, observacoes_cilindro)
      }
    } else{
      treino <- rbind(treino,teste[n,c('x','y','t')])
      contador <- contador + 1
      if(contador == 10){
        indicador <- T
        contador <- 0
      }
    }
  }
  OCC_resultados[[M]] <- list( alarme, cluster)
  MAD_STEC_resultados[[M]] <- cluster_mad.stec
}
saveRDS(OCC_resultados, file = 'Comparativo_OCC_MADSTEC/OCC_2.RDS')

saveRDS(MAD_STEC_resultados, file = 'Comparativo_OCC_MADSTEC/MAD-STEC_2.RDS')
#3: 2 Clusters ------------


OCC_resultados <-  list()
MAD_STEC_resultados <- list()
for(M in 1:10){
  cluster_mad.stec <- list()
  modelos <- list()
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
  cluster <- cluster_aux <-  data.frame()
  alarme <- data.frame()
  teste <- df |>
    cria_variaveis(rho = rho,time_window = tw)
  teste <- tail(teste, nrow(df)*(1-tamanho_treino))
  treino <- df[1:(nrow(df)*tamanho_treino),]
  teste$class <- 0
  indicador <- T
  contador <- 0
  for(n in 1:nrow(teste)){
    #REAMOSTRAGEM COM EMBARALHAMENTO DOS TEMPOS E CRIACAO DO MODELO
    if(indicador == T){
      for(b in 1:30){

        #EMBARALHAR OS TEMPOS E CRIAR O BANCO TREINO, O TAMANHO VAI SER O TAMANHO
        #PRE DEFINIDO MAIS A JANELA DE TEMPO PRA CONSIDERAR AS OBSERVACOES EXCLUIDAS
        treino_b <- treino |>
          mutate(t = sample(t)) |>
          cria_variaveis(rho = rho,time_window = tw)

        #TREINAR O MODELO (ARMAZENAR OS MODELOS AO INVES DE TREINAR SEMPRE)
        modelos[[b]] <- svm(treino_b[c('var1','var2','var3')],y=NULL,
                            scale = TRUE,
                            type='one-classification',
                            nu=nu,
                            kernel="radial")
      }
    }
    n_mad.stec <- n + (nrow(df)*tamanho_treino)
    cluster_mad.stec[[n]] <- MAD_STEC(E = df[1:n_mad.stec,],rho = rho,A = n_mad.stec, n = n_mad.stec)
    indicador <- F
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
      if (nrow(observacoes_cilindro) >= AlarmeOCC) {
        alarme <- rbind(alarme, obs)
        cluster_aux <- anti_join(cluster_aux, observacoes_cilindro)
      }
    } else{
      treino <- rbind(treino,teste[n,c('x','y','t')])
      contador <- contador + 1
      if(contador == 10){
        indicador <- T
        contador <- 0
      }
    }
  }
  OCC_resultados[[M]] <- list( alarme, cluster)
  MAD_STEC_resultados[[M]] <- cluster_mad.stec
}
saveRDS(OCC_resultados, file = 'Comparativo_OCC_MADSTEC/OCC_3.RDS')

saveRDS(MAD_STEC_resultados, file = 'Comparativo_OCC_MADSTEC/MAD-STEC_3.RDS')
