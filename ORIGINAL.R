## FUNCOES E BIBLIOTECAS
library(e1071)
library(dplyr)
source('FUNCAO_CRIAR_VARIAVEL.R')
#ESTUDO 1 ----------------------------------------------------------------------
#FOI DEFINIDO COMO ALARME QUANDO 3 OBSERVACOES SAO CLASSIFICADAS COMO CLUSTER
#NO MESMO CILINDRO
set.seed(1234)
nu <- 0.01
tamanho_treino <- 0.10 #PROPORCAO DE TREINO
final <- data.frame(No.Cluster.Corr = NA,
                    Cluster.Incor = NA,
                    Allarm.Incor = NA)

#MUDANCAS: APLICAR OUTRO METODO, SEM ADICIONAR AS NAO CLUSTERS AO TREINO
#MUDANCAS: MUDAR EM VEZ DE UMA A UMA OBSERVAR, GRUPO DE OBSERVACOES

p <- 0.05 #porcentagem de varivaeis de teste sendo testadas a cada iteracao

#ITERACOES MONTE CARLO
for(m in 1:10){

  #CRIAR A AMOSTRA SEM CLUSTER
  x <- runif(300,0,1)
  y <- runif(300,0,1)
  t <- runif(300,0,1)
  df <-as.data.frame (rbind(cbind(x,y,t,FALSE)))
  colnames(df) <- c('x','y','t','cluster')
  df <- df[order(df$t),]

  #DATAFRAME PARA ARMAZENAR OBSERVACOES CONSIDERADAS CLUSTERS
  cluster <- data.frame()
  alarme <- data.frame()
  #BANCO DE DADOS TESTE
  teste <- df |>
    cria_variaveis()
  #SELECIONANDO OS 90% DAS OBSERVACOES PARA TESTE
  teste <- tail(teste, nrow(df)*(1-tamanho_treino))
  treino <- df[1:(nrow(df)*tamanho_treino),]
  #NUMERO DE OBSERVACOES A SEREM PREDIZIDAS A CADA ITERACAO
  n_variaveis <- round(nrow(teste)*p)

  for(n in 0:(nrow(teste)/n_variaveis)){

    #SELECIONANDO AS OBSERVACOES
    observacoes <- teste[(n*n_variaveis):(n_variaveis*n + n_variaveis),]

    #COMO USAMOS ARREDONDAMENTOS O NUMERO AS VEZES NAO PODE SER EXATO E NA SAO
    #INCLUIDOS, AQUI RETIRAMOS LINHAS COM TODOS OS DADOS COMO NA
    observacoes <- observacoes[rowSums(is.na(observacoes)) != ncol(observacoes), ]
    observacoes$classi <- 0
    #REAMOSTRAGEM COM EMBARALHAMENTO DOS TEMPOS
    for(b in 1:30){

      #EMBARALHAR OS TEMPOS E CRIAR O BANCO TREINO, O TAMANHO VAI SER O TAMANHO
      #PRE DEFINIDO MAIS A JANELA DE TEMPO PRA CONSIDERAR AS OBSERVACOES EXCLUIDAS
      treino_amostra <- treino |>
        mutate(t = sample(t)) |>
        cria_variaveis()

      #TREINAR O MODELO
      svm.model<-svm(treino_amostra[c('var1','var2','var3')],y=NULL,
                     scale = TRUE,
                     type='one-classification',
                     nu=nu,
                     kernel="radial")
      #PREDIZA OS VALORES DO GRUPO DE OBSERVACOES
      pred <- ((predict(svm.model,observacoes[c('var1','var2','var3')]) %>%
                  as.numeric()) - 1) %>% abs()
      #CONTADOR DE VEZES CLASSIFICADO COMO CLUSTER
      observacoes$classi <-  observacoes$classi + pred
    }
    observacoes_cluster <- observacoes[observacoes$classi > 0.9 * 30, ]
    cluster <- rbind(cluster, observacoes_cluster)
    for (i in 1:nrow(observacoes_cluster)) {
      obs <- observacoes_cluster[i, ]
      x_center <- obs$x
      y_center <- obs$y
      t_center <- obs$t

      # Filtrar observações dentro do cilindro
      observacoes_cilindro <- cluster[abs(cluster$x - x_center) <= 0.03 &
                                        abs(cluster$y - y_center) <= 0.03 &
                                        cluster$t >= (t_center - 0.02) &
                                        cluster$t <= t_center, ]

      # Se houver pelo menos 3 observações no cilindro, adicione a observação de alarme
      if (nrow(observacoes_cilindro) >= 3) {
        alarme <- rbind(alarme, obs)
      }
    }
   treino <- rbind(treino, observacoes[observacoes$classi <= 0.9 * 30, c('x','y','t','cluster' )])
  }

  # Adicionar as medidas ao DataFrame final
  final[m, "No.Cluster.Corr"] <- (nrow(teste) - nrow(cluster))/nrow(teste)
  final[m, "Cluster.Incor"] <- nrow(cluster)/nrow(teste)
  final[m, "Allarm.Incor"] <- nrow(alarme)
}
saveRDS(final,'Resultado_Estudo1_var2.RDS')
plot(df$x,df$y)

#ESTUDO 2 -----------------------------------------------------
final <- data.frame(No.Cluster.Corr = NA,
                    No.Cluster.Incor =NA,
                    Cluster.Corr = NA,
                    Cluster.Incor = NA,
                    Allarm.Incor = NA,
                    Allarm.Corr =NA)

#ITERACOES MONTE CARLO
for(m in 1:10){

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
  cluster <- alarme <- alarme_falso<-data.frame()

  #BANCO DE DADOS TESTE
  teste <- df |>
    cria_variaveis()
  #SELECIONANDO OS 90% DAS OBSERVACOES PARA TESTE
  teste <- tail(teste, nrow(df)*(1-tamanho_treino))
  treino <- df[1:(nrow(df)*tamanho_treino),]
  #NUMERO DE OBSERVACOES A SEREM PREDIZIDAS A CADA ITERACAO
  n_variaveis <- round(nrow(teste)*p)

  for(n in 0:(nrow(teste)/n_variaveis)){

    #SELECIONANDO AS OBSERVACOES
    observacoes <- teste[(n*n_variaveis):(n_variaveis*n + n_variaveis),]

    #COMO USAMOS ARREDONDAMENTOS O NUMERO AS VEZES NAO PODE SER EXATO E NA SAO
    #INCLUIDOS, AQUI RETIRAMOS LINHAS COM TODOS OS DADOS COMO NA
    observacoes <- observacoes[rowSums(is.na(observacoes)) != ncol(observacoes), ]
    observacoes$classi <- 0
    #REAMOSTRAGEM COM EMBARALHAMENTO DOS TEMPOS
    for(b in 1:30){

      #EMBARALHAR OS TEMPOS E CRIAR O BANCO TREINO, O TAMANHO VAI SER O TAMANHO
      #PRE DEFINIDO MAIS A JANELA DE TEMPO PRA CONSIDERAR AS OBSERVACOES EXCLUIDAS
      treino_amostra <- treino |>
        mutate(t = sample(t)) |>
        cria_variaveis()

      #TREINAR O MODELO
      svm.model<-svm(treino_amostra[c('var1','var2','var3')],y=NULL,
                     scale = TRUE,
                     type='one-classification',
                     nu=nu,
                     kernel="radial")
      #PREDIZA OS VALORES DO GRUPO DE OBSERVACOES
      pred <- ((predict(svm.model,observacoes[c('var1','var2','var3')]) %>%
                  as.numeric()) - 1) %>% abs()
      #CONTADOR DE VEZES CLASSIFICADO COMO CLUSTER
      observacoes$classi <-  observacoes$classi + pred
    }
    observacoes_cluster <- observacoes[observacoes$classi > 0.9 * 30, ]
    cluster <- rbind(cluster, observacoes_cluster)
    if(nrow(observacoes_cluster) > 0){
      for (i in 1:nrow(observacoes_cluster)) {
        obs <- observacoes_cluster[i, ]
        x_center <- obs$x
        y_center <- obs$y
        t_center <- obs$t

        # Filtrar observações dentro do cilindro
        observacoes_cilindro <- cluster[abs(cluster$x - x_center) <= 0.03 &
                                          abs(cluster$y - y_center) <= 0.03 &
                                          cluster$t >= (t_center - 0.02) &
                                          cluster$t <= t_center, ]

        # Se houver pelo menos 3 observações no cilindro, adicione a observação de alarme
        if (nrow(observacoes_cilindro[observacoes_cilindro$cluster == 1,]) >= 3  ) {
          alarme <- rbind(alarme, obs)
        }
        if(nrow(observacoes_cilindro) >= 3 & nrow(observacoes_cilindro[observacoes_cilindro$cluster == 1,]) < 3  ){
          alarme_falso <- rbind(alarme_falso, obs)
        }
      }
    }
    treino <- rbind(treino, observacoes[observacoes$classi <= 0.9 * 30, c('x','y','t','cluster' )])
  }
  # Adicionar as medidas ao DataFrame final
  Nao_cluster <- anti_join(teste, cluster)
  final[m, "No.Cluster.Corr"] <- nrow(Nao_cluster[Nao_cluster$cluster == 0,])/nrow(Nao_cluster)
  final[m, "No.Cluster.Incor"] <- nrow(Nao_cluster[Nao_cluster$cluster == 1,])/nrow(Nao_cluster)
  final[m, "Cluster.Incor"] <- nrow(cluster[cluster$cluster == 0,] )/nrow(cluster)
  final[m, "Cluster.Corr"] <- nrow(cluster[cluster$cluster == 1,] )/nrow(cluster)
  final[m, "Allarm.Incor"] <- nrow(alarme_falso)
  final[m, "Allarm.Corr"] <- nrow(alarme)
}

saveRDS(final,'Resultado_Estudo2_var2.RDS')

#ESTUDO 3 -----------------------------------------------------
final <- data.frame(No.Cluster.Corr = NA,
                    No.Cluster.Incor =NA,
                    Cluster.Corr = NA,
                    Cluster.Incor = NA,
                    Allarm.Incor = NA,
                    Allarm.Corr =NA)

#ITERACOES MONTE CARLO
for(m in 1:10){

  #CRIAR A AMOSTRA SEM CLUSTER
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

  #DATAFRAME PARA ARMAZENAR OBSERVACOES CONSIDERADAS CLUSTERS
  cluster <- alarme <- alarme_falso<-data.frame()

  #BANCO DE DADOS TESTE
  teste <- df |>
    cria_variaveis()
  #SELECIONANDO OS 90% DAS OBSERVACOES PARA TESTE
  teste <- tail(teste, nrow(df)*(1-tamanho_treino))
  treino <- df[1:(nrow(df)*tamanho_treino),]

  #NUMERO DE OBSERVACOES A SEREM PREDIZIDAS A CADA ITERACAO
  n_variaveis <- round(nrow(teste)*p)

  for(n in 0:(nrow(teste)/n_variaveis)){

    #SELECIONANDO AS OBSERVACOES
    observacoes <- teste[(n*n_variaveis):(n_variaveis*n + n_variaveis),]

    #COMO USAMOS ARREDONDAMENTOS O NUMERO AS VEZES NAO PODE SER EXATO E NA SAO
    #INCLUIDOS, AQUI RETIRAMOS LINHAS COM TODOS OS DADOS COMO NA
    observacoes <- observacoes[rowSums(is.na(observacoes)) != ncol(observacoes), ]
    observacoes$classi <- 0
    #REAMOSTRAGEM COM EMBARALHAMENTO DOS TEMPOS
    for(b in 1:30){

      #EMBARALHAR OS TEMPOS E CRIAR O BANCO TREINO, O TAMANHO VAI SER O TAMANHO
      #PRE DEFINIDO MAIS A JANELA DE TEMPO PRA CONSIDERAR AS OBSERVACOES EXCLUIDAS
      treino_amostra <- treino |>
        mutate(t = sample(t)) |>
        cria_variaveis()

      #TREINAR O MODELO
      svm.model<-svm(treino_amostra[c('var1','var2','var3')],y=NULL,
                     scale = TRUE,
                     type='one-classification',
                     nu=nu,
                     kernel="radial")
      #PREDIZA OS VALORES DO GRUPO DE OBSERVACOES
      pred <- ((predict(svm.model,observacoes[c('var1','var2','var3')]) %>%
                  as.numeric()) - 1) %>% abs()
      #CONTADOR DE VEZES CLASSIFICADO COMO CLUSTER
      observacoes$classi <-  observacoes$classi + pred
    }
    observacoes_cluster <- observacoes[observacoes$classi > 0.9 * 30, ]
    cluster <- rbind(cluster, observacoes_cluster)
    if(nrow(observacoes_cluster) > 0){
      for (i in 1:nrow(observacoes_cluster)) {
        obs <- observacoes_cluster[i, ]
        x_center <- obs$x
        y_center <- obs$y
        t_center <- obs$t

        # Filtrar observações dentro do cilindro
        observacoes_cilindro <- cluster[abs(cluster$x - x_center) <= 0.03 &
                                          abs(cluster$y - y_center) <= 0.03 &
                                          cluster$t >= (t_center - 0.02) &
                                          cluster$t <= t_center, ]

        # Se houver pelo menos 3 observações no cilindro, adicione a observação de alarme
        if (nrow(observacoes_cilindro[observacoes_cilindro$cluster == 1,]) >= 3  ) {
          alarme <- rbind(alarme, obs)
        }
        if(nrow(observacoes_cilindro) >= 3 & nrow(observacoes_cilindro[observacoes_cilindro$cluster == 1,]) < 3  ){
          alarme_falso <- rbind(alarme_falso, obs)
        }
      }
    }
    treino <- rbind(treino, observacoes[observacoes$classi <= 0.9 * 30, c('x','y','t','cluster' )])
  }
  # Adicionar as medidas ao DataFrame final
  Nao_cluster <- anti_join(teste, cluster)
  final[m, "No.Cluster.Corr"] <- nrow(Nao_cluster[Nao_cluster$cluster == 0,])/nrow(Nao_cluster)
  final[m, "No.Cluster.Incor"] <- nrow(Nao_cluster[Nao_cluster$cluster == 1,])/nrow(Nao_cluster)
  final[m, "Cluster.Incor"] <- nrow(cluster[cluster$cluster == 0,] )/nrow(cluster)
  final[m, "Cluster.Corr"] <- nrow(cluster[cluster$cluster == 1,] )/nrow(cluster)
  final[m, "Allarm.Incor"] <- nrow(alarme_falso)
  final[m, "Allarm.Corr"] <- nrow(alarme)
}

saveRDS(final,'Resultado_Estudo3_var2.RDS')
