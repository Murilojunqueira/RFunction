
# Função para poder transferir arquivos de forma inteligente. 
# Ela compara dois diretórios e apenas transfere o arquivo caso no 
# diretório destino não tiver o arquivo ou tiver um arquivo menor com o 
# mesmo nome. Caso não transferir o arquivo, deleta ele.
CleverMove <- function(NewDir, OldDir, pattern = NULL, Noise = FALSE) {
  
  List.NewDir <- list.files(NewDir, pattern = pattern)
  List.OldDir <- list.files(OldDir, pattern = pattern)
  
  for(i in seq_along(List.NewDir)) {
    Path.File1 <- paste0(NewDir, List.NewDir[i])
    Path.File2 <- paste0(OldDir, List.NewDir[i])
    
    if(List.NewDir[i] %in% List.OldDir) {
      SizeFile1 <- file.info(Path.File1)$size 
      SizeFile2 <- file.info(Path.File2)$size
      
      if((SizeFile1 == SizeFile2) | (SizeFile1 < SizeFile2)) {
        unlink(Path.File1)
        if(Noise) print("Arquivo igual ou menor que o original - Deletar")
        next()
      } 
      
      if (SizeFile1 > SizeFile2) {
        file.rename(from = Path.File1,
                    to = Path.File2)
        if(Noise) print("Arquivo maior que o original - Copiar")
        next()
      }
      
    } else {
      file.rename(from = Path.File1,
                  to = Path.File2)
      if(Noise) print("Arquivo Novo - Copiar")
      next()
    }
  }
  return(invisible(""))
}