
# Script para dar portabilidade a minhas funções de uso pessoal.

# Criação:
# Murilo Junqueira, m.junqueira@yahoo.com.br

# Data Criação: 2019-06-01

################# Prepara Área de trabalho ########################

library(devtools)
library(tidyverse)
library(RCurl)

################# Tutoriais ########################

# https://www.r-bloggers.com/reading-an-r-file-from-github/
# github_PrivadeFunction("NotNull")


################# Debug ########################

# function.name <- "NotNull"
# github_PrivadeFunction("NotNull")
# FunctionRepo <- "E:/Users/Murilo/Dropbox/Aplicativos/RFunction/"

################# Funções ########################

github_PrivadeFunction <- function(function.name, 
                                   FunctionRepo = try(get("FunctionRepo", envir = globalenv()), silent = TRUE)) {
  
  # Checa se existe um repositório de scripts local. Caso contrário, busca do github.
  if(!exists("FunctionRepo")) {
    
    # Busca a lista de funções
    Functions.BD.raw <- getURL("https://raw.githubusercontent.com/Murilojunqueira/RFunction/master/BD/Functions.csv")
    
    # Como o github não lida bem com arquivos separados por ponto e vírucla, segue correções
    Colunas <- c("Function.Id", "Function.Name", "Function.Desc",
                 "Function.DateCreation","Function.LastUpdate",
                 "Folder.Id", "Folder.Path")
    
    Functions.BD <- Functions.BD.raw %>%
      # Retira duplicação das aspas
      gsub('"""', "", .) %>% 
      # Separa em linhas
      str_split("\n") %>%
      # Transforma em data frame
      as.data.frame(stringsAsFactors = FALSE) %>% 
      # Renomeia a variável e retira a linha que era para ser o Header
      rename(Var1 = 1) %>% 
      slice(2:n()) %>% 
      # Corta linhas vazias
      filter(Var1 != "") %>% 
      # Separa em Colunas
      separate(1, Colunas, ";")
    
    # Linha da função
    function.number <- which(Functions.BD$Function.Name == function.name)
    
    # URL da função no github
    source.Path <- paste0("https://raw.githubusercontent.com/Murilojunqueira/RFunction/master/",
                          Functions.BD$Folder.Path[function.number], 
                          Functions.BD$Function.Name[function.number], ".R")
    # Lê o Script
    source_url(source.Path)
    
  } else {
    
    # Busca a lista de funções
    Functions.BD <- read.table(paste0(FunctionRepo, "BD/Functions.csv"),
                               sep = ";", dec = ",", 
                               stringsAsFactors = FALSE, 
                               header = TRUE)
    
    # Linha da função
    function.number <- which(Functions.BD$Function.Name == function.name)
    
    #Caminho do script
    Script.Path <- paste0(FunctionRepo, 
                          Functions.BD$Folder.Path[function.number], 
                          function.name, ".R")
    
    # Lê o Script
    source(Script.Path)
  }
}

# Fim