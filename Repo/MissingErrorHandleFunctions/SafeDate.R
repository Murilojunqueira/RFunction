# Para corrigir datas sem ano
# Coloca datas em português para valor legível

# Debug:

# library(lubridate)
# x <- "29 Novembro 09:10"
# x <- "29 Novembro 2018 09:10"
# x <- Extracao.New$Oferta.DataInsercao[283]
# x
# 
# SafeDate(x)
#  
# teste <- Extracao.New$Oferta.DataInsercao[2360:2380]
# map_chr(teste, SafeDate)



SafeDate <- function(x) {
  
  # Encontra a posição dos dois pontos (":")
  PointsPosition <- gregexpr(":", x)[[1]][1] - 4
  
  # Estrai o que tem 4 casas antes do dois pontos. Se for
  # Número, tem ano. Se tiver letra, não tem ano.
  testeChar <- substr(x, PointsPosition, PointsPosition)
  
  # !suppressWarnings(!is.na(as.numeric(testeChar)))
  if (!suppressWarnings(!is.na(as.numeric(testeChar)))) {
    
    # Retira o mês da data
    Mes.x <- x %>% 
      substr(1, PointsPosition) %>% 
      substr(gregexpr(" ", .)[[1]][1], nchar(.)) %>% 
      trimws(which = "both") %>% 
      paste0("01-", ., "-2019") %>% 
      dmy() %>% 
      month()
    
    # Adivinha o mês correto. Se o mês da data for maior que o mês atual,
    # então o dado é do ano passado.
    Ano.x <- ifelse(Mes.x > month(today()), year(today())-1, year(today()))
    
    # Coloca o ano de novo na data
    Output <- paste0(substr(x, 1, PointsPosition), 
                     " ", Ano.x, " ", 
                     substr(x, PointsPosition + 1 , nchar(x))) %>% 
      # trimws(which = "both") %>% 
      dmy_hm() %>% 
      as.character()
    
    return(Output)
    
  } else {
    
    # Se já tiver ano na data, é só usar o lubridate.
    Output <- x %>% 
      dmy_hm() %>% 
      as.character()
    
    return(Output)
    
  }
}

# Fim