cria_variaveis <- function(df, rho = 0.03, time_window = 0.02) {
  df <- df[order(df$t), ]
  df$var1 <- 0
  df$var2 <- -1
  df$var3 <- 0
  row.names(df) <- c(1:nrow(df))
  inicio <- row.names(df[df$t >= ( time_window + df$t[1]),])[1] |> as.integer()
  for (i in inicio:nrow(df)) {
    # VARIAVEL 1: distância até a última ocorrência, caso tenha duas observações
    # com tempo igual, pega a de menor distância
    # (TESTE: SOMAR A DISTÂNCIA DAS ÚLTIMAS 5 OBSERVAÇÕES EM RELAÇÃO À OBSERVAÇÃO 'i')
    # verifique se realmente existem observações em tempos anteriores
    aux <- df[df$t >= (df$t[i] - time_window) & df$t <= df$t[i] & row.names(df) != i, c('x', 'y', 't')]
    if (nrow(aux) != 0) {

      # Atribua a var1 a soma das  distância, dentro da janela de tempo
      dists <- dist(rbind(df[i, c('x', 'y')], aux[,c('x','y')]))[1:nrow(aux)]
      df$var1[i] <- sum(dist(rbind(df[i, c('x', 'y')], aux[,c('x','y')]))[1:nrow(aux)])


    # VARIAVEL 2: Diferença de tempo até a observação mais recente do cilindro de raio rho
    # e comprimento time_window

    # caso exista alguma variável, continue
      # verifique se estão dentro da circunferência de raio rho
      aux$dist <- apply(aux[, c('x', 'y')], 1, function(point) dist(rbind(df[i, c('x', 'y')], point)))

      # calcula a distância de tempo
      aux$dif <- df$t[i] - aux$t

      # Atribua a variável 2 o menor dif
      if(length(aux$dist[aux$dist <= rho]) > 0 ){
      df$var2[i] <- min(aux$dist[aux$dist <= rho], na.rm = TRUE)
      df$var3[i] <- length(aux$dist <= rho)
      }
  }
  }
  return(df[inicio:nrow(df),])
}




