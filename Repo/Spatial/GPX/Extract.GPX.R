# Função para poder extrar os dados do XML do GPX
Extract.GPX <- function(x) {
  new.Row <- list()
  new.Row$Localizacao.Time[1] <- x$time[[1]]
  new.Row$Localizacao.Lat[1] <- x$.attrs[[1]]
  new.Row$Localizacao.Lon[1] <- x$.attrs[[2]]
  new.Row$Localizacao.Ele[1] <- x$ele[[1]]
  new.Row <- as.data.frame(new.Row, stringsAsFactors = FALSE)
  return(new.Row)
}
