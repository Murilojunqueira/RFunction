


# Função para transformar para transformar o GPX em um banco de dados
Process.GPXFiles <- function(x) {
  # x = caminho do arquivo a ser processado
  gpx.list <- xmlTreeParse(x, useInternalNodes = TRUE) %>% 
    xmlRoot() %>% 
    xmlToList() %>% 
    .$trk %>% 
    .$trkseg
  
  New.Data <- map_df(gpx.list, Extract.GPX)
  New.Data <- mutate(New.Data, Arquivo.Id = as.character(x))
  
  return(New.Data)
}

