



# Função de Aptidão
funcao_aptidao <- function(x) {
  return(x * sin(10 * pi * x) + 1)
}

# Estratégia Evolutiva (µ + λ)-EE
estrategia_evolutiva <- function(mu, lmbda, tmax) {
  # Inicialização da população de pais
  pais <- runif(mu, -1, 2)
  
  for (t in 1:tmax) {
    # Geração de filhos
    descendencia <- matrix(rnorm(mu * lmbda, mean = pais, sd = 0.1), 
                           nrow = lmbda, ncol = mu)
    
    # Avaliação da aptidão
    aptidao_pais <- funcao_aptidao(pais)
    aptidao_descendencia <- apply(descendencia, 1, funcao_aptidao)
    
    # Combinação de pais e filhos para seleção
    populacao_total <- c(pais, descendencia)
    todas_aptidoes <- c(aptidao_pais, aptidao_descendencia)
    
    # Seleção dos melhores µ indivíduos
    indices <- order(todas_aptidoes, decreasing = TRUE)[1:mu]
    pais <- populacao_total[indices]
    
    # Exibição do melhor resultado a cada geração
    melhor_individuo <- populacao_total[which.max(todas_aptidoes)]
    cat(sprintf("Geração %d: Melhor indivíduo - x=%.4f, Aptidão=%.4f\n", 
                t, melhor_individuo, max(todas_aptidoes)))
  }
  
  return(pais)
}

# Parâmetros reduzidos
valores_mu <- c(1, 3)
valores_lambda <- c(2, 5)
valores_tmax <- c(10, 20)


for(i in 1:30) {
  for (mu in valores_mu) {
    for (lmbda in valores_lambda) {
      for (tmax in valores_tmax) {
        cat("\nµ=", mu, ", λ=", lmbda, ", tmax=", tmax, "\n")
        estrategia_evolutiva(mu, lmbda, tmax)
      }
    }
  }
}


