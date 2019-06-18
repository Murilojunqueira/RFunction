

# Retorna o diretório do DropBox
get.dropbox.folder <- function () {
  info <- RJSONIO::fromJSON(if (file.exists(file.path(Sys.getenv("APPDATA"), 
                                                      "Dropbox", "info.json"))) {
    file.path(Sys.getenv("APPDATA"), "Dropbox", "info.json")
  }
  else {
    file.path(Sys.getenv("LOCALAPPDATA"), "Dropbox", "info.json")
  })
  dropbox_base <- paste0(gsub("\\\\", "/", info$personal$path), 
                         "/")
  return(dropbox_base)
}