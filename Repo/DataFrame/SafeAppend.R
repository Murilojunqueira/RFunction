# Anexa linhas a um banco de dados existente evitando repetições. 
# Se o arquivo não existir com menos nome no diretório destino, 
# salva novo arquivo.

# Debug: BaixaOLX.R

# New.Data <- Extracao.New
# FileName <- "E:/Users/Murilo/Dropbox/Ideias, Ensaios e Projetos/2019-05 Busca de Imóveis Belém/Dados/Extracoes/OLX Ofertas 2019-06-02.csv"
# Output.Folder <- ExtrationFolder
# Output.Folder <- NULL
# IdCol = NULL


SafeAppend <- function(New.Data, FileName, IdCol = NULL, Output.Folder = NULL, KeepOld = TRUE) {
  
  # Funções dependência
  github_PrivateFunction("force_class")
  
  # Se não houve diretório de trabalho, se assume que FileName é o 
  # caminho completo. Caso contrário se controi o caminho
  if(is.null(Output.Folder)) {
    File.Path <- paste0(Output.Folder, FileName)
  } else {
    File.Path <- FileName
  }
  
  # Se já existir um arquivo antigo, apensar. Caso contrário, só salvar.
  if (file.exists(File.Path)) {
    # Evita problemas de classe
    # Pega as classes do banco novo.
    ColCass <- map(New.Data, class) %>% as.character() %>% 
      gsub("factor", "character", .)
    
    # Transforma as datas do lubridate em character
    ColCass[which(grepl("POSIXct", ColCass))] <- "character"
    
    
    # Carrega o arquivo antigo
    Old.File <- fread(File.Path, sep = ";", dec = ",",
                      stringsAsFactors = FALSE, 
                      colClasses = ColCass)
    
    
    # Se não houve colunas de identificação, se assume que é a primeira coluna da
    # base de dados
    if(is.null(IdCol)) {
      IdCol <- names(Old.File)[[1]]
    }
    
    # Anexa os dados e retira repetições
    Output.Data <- dplyr::bind_rows(Old.File, New.Data, .id = "id_grouping") %>% 
      # Se KeepOld = TRUE, mais velho fica, KeepOld = FALSE, mais novo fica.
      arrange_(ifelse(KeepOld, "id_grouping", "desc(id_grouping)")) %>% 
      select(-id_grouping) %>% 
      # retira as repetições
      distinct_(IdCol, .keep_all= TRUE)
    
  } else {
    Output.Data <- New.Data
  }
  
  write.table(Output.Data, File.Path, sep = ";", dec = ",", 
              row.names = FALSE)
}
