# Função para forçar as classes.

# Debug:BaixaOLX.R & SafeAppend.R

# New.Data$HoraExtracao %>% class()
# 
# New.Data$HoraExtracao %>% 
#   force_class("character") %>% 
#   class()

force_class <- function(x, NewClass) {
  
  if(NewClass == "character") {
    NewData <- as.character(x)
  }
  
  if(NewClass == "numeric") {
    NewData <- as.numeric(x)
  }
  
  if(NewClass == "integer") {
    NewData <- as.integer(x)
  }
  
  if(NewClass == "factor") {
    NewData <- as.factor(x)
  }
  
  return(NewData)
}
