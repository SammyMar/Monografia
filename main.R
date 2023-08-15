## FUNCOES

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
