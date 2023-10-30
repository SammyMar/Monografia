# ----------------------
NumEvents <- function(E,rho,n){
  E |>
    dplyr::rowwise() |>
    dplyr::mutate(
      num_eventos_cilindro = sum((x - x) ^ 2 + (y - y) ^ 2 <= rho^2) - 1
    ) |>
    dplyr::ungroup() |> return()
}



evaluete_Rn <- function(E, rho, Perc, n){
  #Numero de eventos dentro do cilindro, tamanho n - 1
  N <-  NumEvents(E,rho)

  #Numero esperado de eventos dentro do cilindro, tamanho n - 1
  mu.hat <-

  #Vetor de porcoes de estatistica R_n, tamanho n - 1
  Lambda <- (1 + epsilon.hat) ^ E$num_eventos_cilindro * exp(-epsilon.hat * mu_hat)

  #epsilon estimado
  epsilon.hat <- max(0, N / mu.hat - 1)

  #estatistica R_n
  R_n <- sum(Lambda)

  return(N,mu.hat,Lambda,R_n)
}




findCluster <- function(E, Lambda, rho){

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

  # Remova os primeiros 'delta' eventos dentro do cluster
  E <- E[-indices[1:delta], ]
  return(E)
}



removeAllIntersection <- function(E, C_minus) {
  if (length(C_minus) == 0) {
    return(E)  # Nenhuma interseção para remover
  }

  for (i in 1:length(C_minus)) {
    # Encontre os índices dos eventos que estão dentro do cluster C_minus[i]
    indices <- which(E$x %in% C_minus[[i]]$x & E$y %in% C_minus[[i]]$y & E$t %in% C_minus[[i]]$t)

    if (length(indices) > 0) {
      # Remova os eventos dentro do cluster C_minus[i]
      E <- E[-indices, ]
    }
  }

  return(E)
}



MAD_STEC <- function(E, rho, A, n, Perc = 99) {
  # Inicialize o conjunto de clusters identificados
  Cl <- list()

  # Avalie a estatística R_n e estimar o parâmetro ε
  result <- evaluate_Rn(E, rho, Perc, n)
  N <- result$N
  mu_hat <- result$mu_hat
  Lambda <- result$Lambda
  R_n <- result$R_n

  while (R_n >= A) {
    # Encontre um novo cluster
    C_plus <- findCluster(E, Lambda, rho)

    # Encontre a interseção entre o novo cluster e os clusters existentes
    C_minus <- Reduce(intersect, Cl, C_plus)

    if (length(C_minus) == 0) {
      # Se não houver interseção, adicione o novo cluster a Cl
      Cl <- append(Cl, list(C_plus))

      # Calcule δ
      delta <- max(N - mu_hat, 0)

      # Remova eventos em excesso
      E <- removeExcessEvents(E, C_plus, delta)

      # Atualize n
      n <- n - delta
    } else {
      # Remova todas as interseções entre E e C_minus
      E <- removeAllIntersection(E, C_minus)

      # Atualize n
      n <- n - length(C_minus)
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

