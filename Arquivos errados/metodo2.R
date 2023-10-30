## FUNCOES E BIBLIOTECAS
library(e1071)
library(dplyr)
source('FUNCAO_CRIAR_VARIAVEL.R')

## CRIAR DADOS SIMULADOS
set.seed(1234)
x <- runif(1000,0,1)
y <- runif(1000,0,1)
t <- runif(1000,0,1)
xc <- runif(100,.95,1)
yc <- runif(100,.95,1)
tc <- runif(100,.95,1)
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
treino <- df[order(df$t),]
treino <- treino[treino$t < 0.90,]
teste <- df1[df1$t >= 0.90,]
for(n in nu){
  for(i in B){
    df1$SVM <- 0
    df1$SVM2 <- 0
    for(b in 1:i){

      #embaralhar os tempos
      amostrab <- treino %>%
        mutate(t = sample(t))

      #criar as features
      amostrab <- cria_variaveis(amostrab)

      #fitar o modelo
      svm.model<-svm(amostrab[c('var1','var2','var3')],y=NULL,
                     scale = TRUE,
                     type='one-classification',
                     nu=nu,
                     kernel="radial")

      pred <- ((predict(svm.model,teste[c('var1','var2','var3')]) %>%
                  as.numeric()) - 1) %>% abs()
      teste$SVM <- teste$SVM  + pred


    }
    tabela <- table(Predicted = teste$SVM >= i*0.8,
                    Original = teste$cluster)
    saveRDS(tabela,file = paste0('Resultados2/Tabelas/Resultados_simulacao_nu-',n,'_B-',i,'.RdS'))
    saveRDS(teste,file = paste0('Resultados2/dataframe/dados_simulacao_nu-',n,'_B-',i,'.RdS'))
  }
}


