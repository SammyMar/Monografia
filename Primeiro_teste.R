## FUNCOES E BIBLIOTECAS
library(e1071)
library(dplyr)
source('FUNCAO_CRIAR_VARIAVEL.R')

## CRIAR DADOS SIMULADOS

x <- runif(1000,0,1)
y <- runif(1000,0,1)
t <- runif(1000,0,1)
xc <- runif(100,.95,1)
yc <- runif(100,.95,1)
tc <- runif(100,.95,1)
df <-as.data.frame( rbind(cbind(x,y,t,FALSE),cbind(xc,yc,tc,TRUE)))
colnames(df) <- c('x','y','t','cluster')
plot(df$x,df$y)

## CRIACAO DE FEATURES

df1 <- cria_variaveis(df)
par(mfrow = c(2, 2))

# Gráfico 1
plot(df1$var1, df1$var3, col = ifelse(df1$cluster, "blue", "red"), pch = 16,
     xlab = "Variável 1", ylab = "Variável 3")
legend("topright", legend = c("FALSE", "TRUE"), col = c("red", "blue"), pch = 16,
       title = "Cluster")

# Gráfico 2
plot(df1$var1[df1$var2 > 0], df1$var2[df1$var2 > 0], col = ifelse(df1$cluster[df1$var2 > 0], "blue", "red"), pch = 16,
     xlab = "Variável 1", ylab = "Variável 2")
legend("topright", legend = c("FALSE", "TRUE"), col = c("red", "blue"), pch = 16,
       title = "Cluster")

# Gráfico 3
plot(df1$var3[df1$var2 > 0], df1$var2[df1$var2 > 0], col = ifelse(df1$cluster[df1$var2 > 0], "blue", "red"), pch = 16,
     xlab = "Variável 3", ylab = "Variável 2")
legend("topright", legend = c("FALSE", "TRUE"), col = c("red", "blue"), pch = 16,
       title = "Cluster")

## TESTES ITERATIVOS
# PARAMETROS
B <- c(100,200,500)
nu <- c(0.01,0.05,0.1)
df1$SVM <- 0
df1$SVM2 <- 0

for(i in B){
for(b in 1:B[i]){

  #embaralhar os tempos
  amostrab <- df %>%
    mutate(t = sample(t))

  #criar as features
  amostrab <- cria_variaveis(amostrab)

  #fitar o modelo
  svm.model<-svm(amostrab[c('var1','var2','var3')],y=NULL,
                 scale = TRUE,
                 type='one-classification',
                 nu=0.10,
                 kernel="radial")

  pred <- ((predict(svm.model,df1[c('var1','var2','var3')]) %>%
    as.numeric()) - 1) %>% abs()
  df1$SVM <- df1$SVM  + pred

  pred <- ((predict(svm.model,amostrab[c('var1','var2','var3')]) %>%
    as.numeric()) -1 ) %>% abs()
  df1$SVM2 <- df1$SVM2  + pred
}
tabela <- table(Predicted = df1$SVM >= 80,
      Original = df1$cluster)

par(mfrow = c(3, 2))

# Gráfico 1
plot(df1$var1, df1$var3, col = ifelse(df1$SVM >= 80, "blue", "red"), pch = 16,
     xlab = "Variável 1", ylab = "Variável 3")
legend("topright", legend = c("FALSE", "TRUE"), col = c("red", "blue"), pch = 16,
       title = "Cluster")
# Gráfico 1
plot(df1$var1, df1$var3, col = ifelse(df1$cluster, "blue", "red"), pch = 16,
     xlab = "Variável 1", ylab = "Variável 3")
legend("topright", legend = c("FALSE", "TRUE"), col = c("red", "blue"), pch = 16,
       title = "Cluster")

# Gráfico 2
plot(df1$var1[df1$var2 > 0], df1$var2[df1$var2 > 0], col = ifelse(df1$cluster[df1$var2 > 0], "blue", "red"), pch = 16,
     xlab = "Variável 1", ylab = "Variável 2")
legend("topright", legend = c("FALSE", "TRUE"), col = c("red", "blue"), pch = 16,
       title = "Cluster")
plot(df1$var1[df1$var2 > 0], df1$var2[df1$var2 > 0], col = ifelse(df1$SVM[df1$var2 > 0] >= 80, "blue", "red"), pch = 16,
     xlab = "Variável 1", ylab = "Variável 2")
legend("topright", legend = c("FALSE", "TRUE"), col = c("red", "blue"), pch = 16,
       title = "Cluster")
# Gráfico 3
plot(df1$var3[df1$var2 > 0], df1$var2[df1$var2 > 0], col = ifelse(df1$cluster[df1$var2 > 0], "blue", "red"), pch = 16,
     xlab = "Variável 3", ylab = "Variável 2")
legend("topright", legend = c("FALSE", "TRUE"), col = c("red", "blue"), pch = 16,
       title = "Cluster")
plot(df1$var3[df1$var2 > 0], df1$var2[df1$var2 > 0], col = ifelse(df1$SVM[df1$var2 > 0] >=80, "blue", "red"), pch = 16,
     xlab = "Variável 3", ylab = "Variável 2")
legend("topright", legend = c("FALSE", "TRUE"), col = c("red", "blue"), pch = 16,
       title = "Cluster")


# Gráfico 1
plot(df1$x, df1$y, col = ifelse(df1$SVM >= 100, "blue", "red"), pch = 16,
     xlab = "Variável 1", ylab = "Variável 3")
legend("", legend = c("FALSE", "TRUE"), col = c("red", "blue"), pch = 16,
       title = "Cluster")

