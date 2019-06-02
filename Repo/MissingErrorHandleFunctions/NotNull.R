# Função para evitar valores Nulos
NotNull <- function(x){
  ifelse(is.null(x), NA, x)
}