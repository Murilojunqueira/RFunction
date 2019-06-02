
# Script para dar portabilidade a minhas funções de uso pessoal.

# Criação:
# Murilo Junqueira, m.junqueira@yahoo.com.br

# Data Criação: 2019-06-01

################# Prepara Área de trabalho ########################

library(devtools)
library(tidyverse)
library(RCurl)

################# Funções ########################

# Tutoriais

# https://www.r-bloggers.com/reading-an-r-file-from-github/
# github_PrivadeFunction("NotNull")

github_PrivadeFunction <- function(function.name) {
  # function.name <- "NotNull"
  Functions.BD.raw <- getURL("https://raw.githubusercontent.com/Murilojunqueira/RFunction/master/BD/Functions.csv")
  
  Colunas <- c("Function.Id", "Function.Name", "Function.Desc",
               "Function.DateCreation","Function.LastUpdate",
               "Folder.Id", "Folder.Path")
  
  Functions.BD <- Functions.BD.raw %>%
    gsub('"""', "", .) %>% 
    str_split("\n") %>% 
    as.data.frame(stringsAsFactors = FALSE) %>% 
    rename(Var1 = 1) %>% 
    filter(Var1 != "") %>% 
    slice(2:n()) %>% 
    separate(1, Colunas, ";")
  
  function.number <- which(Functions.BD$Function.Name == function.name)
  
  source.Path <- paste0("https://raw.githubusercontent.com/Murilojunqueira/RFunction/master/",
                        Functions.BD$Folder.Path[function.number], 
                        Functions.BD$Function.Name[function.number], ".R")
  
  source_url(source.Path)
}

# Fim