## FUNCOES E BIBLIOTECAS
library(e1071)
library(dplyr)
source('FUNCAO_CRIAR_VARIAVEL.R')
N <- c(70,100,300,500)

# DADOS SIMULADOS (1 UNICO CLUSTER) -------------------

## CRIAR DADOS SIMULADOS
for(amostra in N){
set.seed(1234)
x <- runif(amostra,0,1)
y <- runif(amostra,0,1)
t <- runif(amostra,0,1)
xc <- runif(.10*amostra,.95,1)
yc <- runif(.10*amostra,.95,1)
tc <- runif(.10*amostra,.95,1)
df <-as.data.frame( rbind(cbind(x,y,t,FALSE),cbind(xc,yc,tc,TRUE)))
colnames(df) <- c('x','y','t','cluster')
df1 <- cria_variaveis(df)

## TESTES ITERATIVOS
# PARAMETROS
resultados <- list()
B <- c(100,200,500)
nu <- c(0.01,0.05,0.1)
df1$SVM <- 0
df1$SVM2 <- 0
for(n in nu){
for(i in B){
  df1$SVM <- 0
  df1$SVM2 <- 0
  for(b in 1:i){

    #embaralhar os tempos
    amostrab <- df %>%
      mutate(t = sample(t))

    #criar as features
    amostrab <- cria_variaveis(amostrab)

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
  tabela <- table(Predicted = df1$SVM >= i*0.8,
                  Original = df1$cluster)
  saveRDS(tabela,file = paste0('Resultados/Amostra',amostra,'/Tabelas/Resultados_simulacao_nu-',n,'_B-',i,'.RdS'))
  saveRDS(df1,file = paste0('Resultados/Amostra',amostra,'/dataframes/dados_simulacao_nu-',n,'_B-',i,'.RdS'))
}
}
}


# VARIACAO 1: 2 CLUSTERS EM TEMPOS E LUGARES DIFERENTES -----

for(amostra in N){
set.seed(1234)
x <- runif(amostra,0,1)
y <- runif(amostra,0,1)
t <- runif(amostra,0,1)
xc <- runif(amostra*.1,.95,1)
yc <- runif(amostra*.1,.95,1)
tc <- runif(amostra*.1,.95,1)
xc2 <- runif(amostra*.1,.50,.55)
yc2 <- runif(amostra*.1,.70,.75)
tc2 <- runif(amostra*.1,.60,.65)
df <-as.data.frame( rbind(cbind(x,y,t,FALSE),cbind(xc,yc,tc,TRUE),cbind(xc2,yc2,tc2,TRUE)))
colnames(df) <- c('x','y','t','cluster')
df1 <- cria_variaveis(df)

## TESTES ITERATIVOS
# PARAMETROS
resultados <- list()
B <- c(100,200,500)
nu <- c(0.01,0.05,0.1)
df1$SVM <- 0
df1$SVM2 <- 0
for(n in nu){
  for(i in B){
    df1$SVM <- 0
    df1$SVM2 <- 0
    for(b in 1:i){

      #embaralhar os tempos
      amostrab <- df %>%
        mutate(t = sample(t))

      #criar as features
      amostrab <- cria_variaveis(amostrab)

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
    tabela <- table(Predicted = df1$SVM >= i*0.8,
                    Original = df1$cluster)
    saveRDS(tabela,file = paste0('Resultados_var/variacao1/Amostra',amostra,'/tabelas/Resultados_simulacao_nu-',n,'_B-',i,'.RdS'))
    saveRDS(df1,file = paste0('Resultados_var/variacao1/Amostra',amostra,'/dataframes/Resultados_simulacao_nu-',n,'_B-',i,'.RdS'))
  }
}
}

# VARIACAO 2: 2 CLUSTERS EM TEMPOS IGUAIS E LUGARES DIFERENTES ----
for(amostra in N){
set.seed(1234)
x <- runif(amostra,0,1)
y <- runif(amostra,0,1)
t <- runif(amostra,0,1)
xc <- runif(amostra*0.1,.95,1)
yc <- runif(amostra*.1,.95,1)
tc <- runif(amostra*.1,.95,1)
xc2 <- runif(amostra*.1,.50,.55)
yc2 <- runif(amostra*.1,.70,.75)
tc2 <- runif(amostra*.1,.95,1)
df <-as.data.frame( rbind(cbind(x,y,t,FALSE),cbind(xc,yc,tc,TRUE),cbind(xc2,yc2,tc2,TRUE)))
colnames(df) <- c('x','y','t','cluster')
df1 <- cria_variaveis(df)
plot(df1$x,df1$y)
## TESTES ITERATIVOS
# PARAMETROS
resultados <- list()
B <- c(100,200,500)
nu <- c(0.01,0.05,0.1)
df1$SVM <- 0
df1$SVM2 <- 0
for(n in nu){
  for(i in B){
    df1$SVM <- 0
    df1$SVM2 <- 0
    for(b in 1:i){

      #embaralhar os tempos
      amostrab <- df %>%
        mutate(t = sample(t))

      #criar as features
      amostrab <- cria_variaveis(amostrab)

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
    tabela <- table(Predicted = df1$SVM >= i*0.8,
                    Original = df1$cluster)
    saveRDS(tabela,file = paste0('Resultados_var/variacao2/Amostra',amostra,'/tabelas/Resultados_simulacao_nu-',n,'_B-',i,'.RdS'))
    saveRDS(df1,file = paste0('Resultados_var/variacao2/Amostra',amostra,'/dataframes/Resultados_simulacao_nu-',n,'_B-',i,'.RdS'))
  }
}
}
# VARIACAO 3: 2 CLUSTERS EM TEMPOS DIFERENTES E LUGARES IGUAIS ----
for(amostra in N){
set.seed(1234)
x <- runif(amostra,0,1)
y <- runif(amostra,0,1)
t <- runif(amostra,0,1)
xc <- runif(amostra*.1,.95,1)
yc <- runif(amostra*.1,.95,1)
tc <- runif(amostra*.1,.95,1)
xc2 <- runif(amostra*.1,.95,1)
yc2 <- runif(amostra*.1,.95,1)
tc2 <- runif(amostra*.1,.60,.65)
df <-as.data.frame( rbind(cbind(x,y,t,FALSE),cbind(xc,yc,tc,TRUE),cbind(xc2,yc2,tc2,TRUE)))
colnames(df) <- c('x','y','t','cluster')
df1 <- cria_variaveis(df)


## TESTES ITERATIVOS
# PARAMETROS
resultados <- list()
B <- c(100,200,500)
nu <- c(0.01,0.05,0.1)
df1$SVM <- 0
df1$SVM2 <- 0
for(n in nu){
  for(i in B){
    df1$SVM <- 0
    df1$SVM2 <- 0
    for(b in 1:i){

      #embaralhar os tempos
      amostrab <- df %>%
        mutate(t = sample(t))

      #criar as features
      amostrab <- cria_variaveis(amostrab)

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
    tabela <- table(Predicted = df1$SVM >= i*0.8,
                    Original = df1$cluster)
    saveRDS(tabela,file = paste0('Resultados_var/variacao3/Amostra',amostra,'/tabelas/Resultados_simulacao_nu-',n,'_B-',i,'.RdS'))
    saveRDS(df1,file = paste0('Resultados_var/variacao3/Amostra',amostra,'/dataframes/Resultados_simulacao_nu-',n,'_B-',i,'.RdS'))
  }
}
}
