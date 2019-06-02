# Anexa linhas a um banco de dados existente evitando repetições. 
# Se o arquivo não existir com menos nome no diretório destino, 
# salva novo arquivo.

# Debug: BaixaOLX.R

# New.Data <- Imoveis.Dataframe
# FileName <- "E:/Users/Murilo/Dropbox//Ideias, Ensaios e Projetos/2019-05 Busca de Imóveis Belém/Dados/Extracoes/OLX links 2019-06-02.csv"
# Output.Folder <- ExtrationFolder
# Output.Folder <- NULL
# IdCol = NULL


SafeAppend <- function(New.Data, FileName, Output.Folder = NULL, IdCol = NULL) {
  
  # Se não houve diretório de trabalho, se assume que FileName é o 
  # caminho completo. Caso contrário se controi o caminho
  if(is.null(Output.Folder)) {
    File.Path <- paste0(Output.Folder, FileName)
  } else {
    File.Path <- FileName
  }
  
  
  if (file.exists(File.Path)) {
    # Evita problemas de classe
    ColCass <- map(New.Data, class) %>% as.character()
    
    # Carrega o arquivo
    Old.File <- fread(File.Path, sep = ";", dec = ",",
                      stringsAsFactors = FALSE, 
                      colClasses = ColCass)
    
    # Evita problemas de classe
    class(New.Data) <- map(Old.File, class) %>% as.character()
    
    # Se não houve colinas de identificação, se assume que é a primeira coluna de
    # base de dados
    if(is.null(IdCol)) {
      IdCol <- names(Old.File)[[1]]
    }
    
    # Anexa os dados e retira repetições
    Output.Data <- rbind(Old.File, New.Data) %>% 
      distinct_(IdCol, .keep_all= TRUE)
    
  } else {
    Output.Data <- New.Data
  }
  
  write.table(Output.Data, File.Path, sep = ";", dec = ",", 
              row.names = FALSE)
}
