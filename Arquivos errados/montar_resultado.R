library(dplyr)
# Inicialize um quadro de dados vazio para armazenar os resultados
resultados_df <- data.frame()
diretorio <- "Resultados_var/variacao3"
N <- c(70,100,300,500,1000)
B <- c(100,200,500)
nu <- c(0.01,0.05,0.1)
# Loop pelos arquivos RDS no diretório
for (amostra in N) {  # Suponha que você tenha 10 amostras
  for (n in nu) {      # Suponha que n varia de 1 a 5
    for (i in B) {  # Suponha que i varia de 1 a 100
      arquivo <- paste0(
        diretorio, "/Amostra", amostra, "/Tabelas/Resultados_simulacao_nu-", n, "_B-", i, ".RdS"
      )

      # Carregue o arquivo RDS
      tabela <- readRDS(arquivo)

      # Calcule as métricas desejadas
      falsos_cluster <- tabela[2, 1] / sum(tabela[2, ]) * 100
      cluster_correto <- tabela[2, 2] / sum(tabela[2, ]) * 100
      nao_cluster_errado <- tabela[1, 2] / sum(tabela[1, ]) * 100
      nao_cluster_correto <- tabela[1, 1] / sum(tabela[1, ]) * 100

      # Crie uma linha com as métricas
      linha <- data.frame(
        B = i,
        nu = n,
        N = amostra,
        Falsos_Cluster = falsos_cluster,
        Cluster_Correto = cluster_correto,
        Nao_Cluster_Errado = nao_cluster_errado,
        Nao_Cluster_Correto = nao_cluster_correto
      )

      # Adicione a linha ao quadro de dados de resultados
      resultados_df <- bind_rows(resultados_df, linha)
    }
  }
}

# Visualize o quadro de dados de resultados
write.csv2(resultados_df, 'Resultado_modelo4.csv',fileEncoding = "UTF-8")
