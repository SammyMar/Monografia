# NUMERO DE EVENTOS OBSERVADOS NO TEMPO n
NumEvents <- function(E, rho, n) {
  num_observations <- integer(n -1)  # Inicialize um vetor para armazenar o número de observações
  E <- E[1:n,]
  for (k in 1:(n-1)) {
    x_k <- E$x[k]
    y_k <- E$y[k]
    t_k <- E$t[k]
    t_n <- E$t[n]

    # Filtrar observações que estão dentro do cilindro (raio rho) e no intervalo de tempo [t_k, t_1]
    insideCylinderAndTime <- ((E$x - x_k)^2 + (E$y - y_k)^2 <= rho^2) & (E$t > t_k) & (E$t < t_n)

    # Contar o número de observações que satisfazem as condições
    num_observations[k] <- sum(insideCylinderAndTime)
  }

  return(num_observations)
}

# EVENTOS DENTRO DO DISCO INDEPENDENTE DO TEMPO
EventsCilynder_n <- function(E, rho, n) {
  # Inicialize um vetor para armazenar o número de eventos em cada observação7
  E <- E[1:n,]
  N.B <- numeric(length(E$x) - 1)

  for (k in 1:(length(E$x)-1)) {
    aux <- E[-k, ]
    # Filtrar observações que estão dentro da circunferência de raio rho e centro (E[i, c('x', 'y')])
    insideCylinder <- (aux$x - E$x[k])^2 + (aux$y - E$y[k])^2 <= rho^2

    # Contar o número de observações dentro do cilindro e subtrair 1 para desconsiderar a própria observação
    N.B[k] <- sum(insideCylinder)
  }

  return(N.B)
}

# EVENTOS DENTRO DO INTERVALO DE TEMPO IDEPENDENTE IDEPENDENTE DO LOCAL
EventsTime_n <- function(E, n) {
  # Inicialize um vetor para armazenar o número de observações em cada observação
  E <- E[1:n,]
  N.A <- numeric(length(E$t) - 1)
  for(k in 1:(length(E$t) - 1)){
    # Filtrar observações a partir de E$t[i] até max(E$t)
    inTimeInterval <- E$t > E$t[k] & E$t < max(E$t)

    # Contar o número de observações no intervalo de tempo
    N.A[k] <- sum(inTimeInterval)
  }

  return(N.A)
}

# NUMERO DE EVENTOS ESPERADOS NO TEMPO n
mu_hat_n <- function(E, rho, n){
  A <- EventsTime_n(E,n)
  B <- EventsCilynder_n(E,rho,n)
  return(((A*B)/(length(E$t[1:n])-1)))
}

# CALCULAR A ESTATISCA R_N
evaluate_Rn <- function(E, rho, Perc = 99, n) {
  # Numero de eventos dentro do cilindro, tamanho n - 1
  N <- NumEvents(E, rho, n)

  # Numero esperado de eventos dentro do cilindro, tamanho n - 1
  mu_hat <- mu_hat_n(E, rho, n)

  # Epsilon estimado
  aux <- N / mu_hat
  aux[is.na(aux)] <- 0
  epsilon.values <- pmax(0, aux - 1)
  epsilon.hat <- quantile(epsilon.values, probs = Perc / 100) |> as.numeric()
  # Vetor de porções de estatística R_n, tamanho n - 1
  Lambda <- (1 + epsilon.hat) ^ N * exp(-epsilon.hat * mu_hat)

  # Estatística R_n
  R_n <- sum(Lambda)

  result <- list(N = N, mu_hat = mu_hat, Lambda = Lambda, R_n = R_n)
  return(result)
}

findCluster <- function(E, Lambda, rho) {
    k <- which.max(Lambda)
    xk <- E$x[k]
    yk <- E$y[k]
    tk <- E$t[k]
    return(E[E$t > tk &  ((E$x - xk)^2 + (E$y - yk)^2 <= rho^2),])
}

removeExcessEvents <- function(E, C_plus, delta) {
  if (delta == 0) {
    return(E)  # Nenhum evento para remover
  }

  # Encontre os índices dos eventos que estão dentro do cluster C_plus
  indices <- which(E$x %in% C_plus$x & E$y %in% C_plus$y & E$t %in% C_plus$t)

  if (length(indices) == 0) {
    return(E)  # Nenhum evento dentro do cluster
  }

  # Escolha aleatoriamente 'delta' eventos dentro do cluster
  indices_to_remove <- sample(indices, delta)
  E <- E[-indices_to_remove, ]
  return(E)
}

removeAllIntersection <- function(E, C_minus) {
  if (nrow(C_minus) == 0) {
    return(E)  # Nenhuma interseção para remover
  }

    # Encontre os índices dos eventos que estão dentro do cluster C_minus[i]
    indices <- which(E$x %in% C_minus$x & E$y %in% C_minus$y & E$t %in% C_minus$t)

    if (length(indices) > 0) {
      # Remova os eventos dentro do cluster C_minus[i]
      E <- E[-indices, ]
    }

  return(E)
}

MAD_STEC <- function(E, rho, A, n, Perc = 99) {
  # Inicialize o conjunto de clusters identificados
  Cl <- list()
  E <- E[1:n,]
  # Avalie a estatística R_n e estimar o parâmetro ε
  result <- evaluate_Rn(E, rho, Perc, n)
  N <- result$N
  mu_hat <- result$mu_hat
  Lambda <- result$Lambda
  R_n <- result$R_n

  while (R_n >= A | nrow(E > 1)) {
    # Encontre um novo cluster
    C_plus <- findCluster(E, Lambda, rho)

    # Encontre a interseção entre o novo cluster e os clusters existentes
    Cl_aux <- dplyr::bind_rows(Cl) |> unique()
    if(length(Cl_aux) == 0)C_minus <- NULL else C_minus <- dplyr::semi_join(Cl_aux, C_plus, by = c('x','y', 't'))

    if (length(C_minus) == 0) {
      # Se não houver interseção, adicione o novo cluster a Cl
      Cl <- append(Cl, list(C_plus))
      # Calcule δ
      delta <- floor(max(nrow(C_plus) - mu_hat[which.max(Lambda)], 0))

      # Remova eventos em excesso
      E <- removeExcessEvents(E, C_plus, delta)

      # Atualize n
      n <- n - delta
    } else {
      # Remova todas as interseções entre E e C_minus
      E <- removeAllIntersection(E, C_minus)

      # Atualize n
      n <- n - nrow(C_minus)
    }
    # Avalie novamente a estatística R_n e estimar o parâmetro ε
    result <- evaluate_Rn(E, rho, Perc, n)
    N <- result$N
    mu_hat <- result$mu_hat
    Lambda <- result$Lambda
    R_n <- result$R_n
  }

  # Retorna o conjunto de clusters identificados
  return(Cl)
}

