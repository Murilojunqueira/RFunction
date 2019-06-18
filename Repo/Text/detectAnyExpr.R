# Função para Detectar se dentro de uma string existe alguma das expressoes listadas

# Retorna TRUE se houver qualquer das expressões listadas Exprs em x.

detectAnyExpr <- function(x, Exprs) {
  any(
    map_lgl(Exprs, str_detect, string = x)
  )
}
