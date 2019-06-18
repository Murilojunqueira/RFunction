# Retorna as palavras que aparecem após uma lista de prefixos

# x --> uma string onde retiraremos texto
# Prefixs -->  Lista. Lista de prefixos
# NWords --> Numerico. Quantas palavras retiraremos após o prefixo
# WithPrefix --> Lógico. Se a extração vai incluir ou não o prefixo.

# Debug:

# x <- "Alugue Apartamento no Parque Jardins R$ 2.100,00 Incluso o Condomínio "
# Prefixs <- c("Edifício", "Edificio", "Ed.", "Ed", "Condomínio", "Condominio", "Cond", "Cond.", "Residencial", "Res.", "Res")
# NWords = 3
# WithPrefix = FALSE

# extractPrefix(x, Prefixs, NWords, WithPrefix)


extractPrefix <- function(x, Prefixs, NWords = 3, WithPrefix = FALSE) {
  
  # Verifica quais prefixos existe em x (vetor lógico)
  Prefixs.Choice <- which(map_lgl(Prefixs, str_detect, string = x))
  
  # Seleciona o prefixo a ser usado (o maior prefixo)
  Prefixs.Select <- Prefixs %>% 
    # Transforma o vetor em banco de dados (para usar o dplyr)
    as.data.frame() %>%
    # deixa apenas os prefixos que tem match em x
    slice(Prefixs.Choice) %>% 
    # Coloca um nome da variável
    rename(Var1 = 1) %>% 
    # Retira espaços indevidos
    mutate(Var1 = trimws(Var1)) %>% 
    # Cria a variável com o número de caracteres dos prefixos
    mutate(Nchar = nchar(Var1)) %>% 
    # Filtra o prefixo maior
    filter(Nchar == max(Nchar)) %>% 
    # Volta a variável para vetor texto.
    select(Var1) %>% 
    unlist() %>%  as.character()
  
  # Caso WithPrefix == TRUE, se vai cortor a variável a partir do início prefixo,
  # caso contrário, a partir do final do prefixo.
  StringLocal <- ifelse(WithPrefix,
                        str_locate(x, Prefixs.Select)[[1]], 
                        str_locate(x, Prefixs.Select)[[2]]+1)
  
  # Se o prefixo estiver no final da expressão, retorna NA
  if(StringLocal >= nchar(trimws(x))) {
    return(NA)
  }
  
  Output <- x %>% 
    # Corta o que houver antes do prefixo.
    # StringLocal decide se isso inclui ou não o prefixo.
    substr(StringLocal, nchar(.))  %>% 
    # Retira pontuação
    gsub("[[:punct:][:blank:]]+", " ", .) %>% 
    # Retira espaços indevidos
    trimws() %>% 
    # Seleciona o número adequado de palavras
    # O ifelse existe caso o número de palavras do string for menor que NWords
    word(1, ifelse(str_count(., '\\w+') < NWords, 
                   str_count(., '\\w+'), 
                   NWords)
    )
  
  return(Output)
}
