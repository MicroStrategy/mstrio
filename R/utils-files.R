detectOS <- function() {
  switch(Sys.info()[['sysname']],
    Windows = {return('windows')},
    Linux  = {return('linux')},
    Darwin = {return('mac')})
}

baseFolderPath <- function() {
  switch(detectOS(),
    mac = {return(file.path('~','.mstrio'))},
    linux  = {return(file.path('~','.mstrio'))},
    windows = { #Windows Vista, 7, 8 and 10
      return(file.path(Sys.getenv(x = 'localappdata'),'mstrio'))
    })
}

baseFolderPathOld <- function() {
  basePath <- basePathOld()
  folderName <- 'mstrconnector'
  folderPath <-  file.path(basePath,folderName)
  return(folderPath)
}

basePathOld <- function() {
  switch(detectOS(),
    mac = {return('~/.rstudio-desktop')},
    linux  = {return('~/.rstudio-desktop')},
    windows = { #Windows Vista, 7, 8 and 10
      base <- Sys.getenv(x = 'localappdata')
      return(file.path(base,'RStudio-Desktop'))
    })
}

createUserConfigDirectory <- function() {
  folderPath <- baseFolderPath()
  folderPathOld <- baseFolderPathOld()
  if(!file.exists(folderPath)) {
    tryCatch({
      dir.create(baseFolderPath(),showWarnings=FALSE)
    },
    error = function(e){
      print(e$message)
      displayErrorMessage('RfolderError', e$message)
    })
  }
  if(file.exists(folderPathOld)) {
    # For backward compability:
    # moves user files from old location to new
    # and deletes old location
    tryCatch({
      filesList = list.files(folderPathOld)
      file.copy(file.path(folderPathOld, filesList), folderPath)
      unlink(folderPathOld, recursive=TRUE)
      setwd(folderPath)
      if(file.exists('recentProjects')) {
        unlink('recentProjects.txt')
        file.rename('recentProjects', 'recentProjects.txt')
      }
    },
    error = function(e){
    })
  }
}

loadLinesFromFile <- function(fileName) {
  path <- file.path(baseFolderPath(),fileName)

  if(file.exists(path)) {
    con = file(path,open="r")
    lines <- readLines(con)
    base::close(con)
    return(lines)
  }
  else {
    return(c())
  }
}

saveStringToFile <- function(path, string) {
    tryCatch({
      vector <- c(string)
      con = file(path,open="w")
      writeLines(text=vector,con=con)
      base::close(con)
  },
  error = function(e) {
    print(e$message)
    displayErrorMessage('RfileError', e$message)
  })
}

updateEnvSuggestions <- function(newSuggestions) {
  fileName = 'environments.txt'
  path <- file.path(baseFolderPath(),fileName)
  unlink(path)
  saveStringToFile(path,newSuggestions)
}

clearEnvSuggestions <- function() {
  fileName = 'environments.txt'
  path <- file.path(baseFolderPath(),fileName)
  unlink(path)
}

updateRecentProjects <- function(recentProjects) {
  fileName = 'recentProjects.txt'
  path <- file.path(baseFolderPath(),fileName)
  unlink(path)
  saveStringToFile(path,recentProjects)
}
