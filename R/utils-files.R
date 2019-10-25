baseFolderPath <- function() {
  basePath <- basePath()  
  folderName <- 'mstrconnector'
  folderPath <-  file.path(basePath,folderName)
  return(folderPath)
}

basePath <- function() {
  switch(detectOS(),
    mac = {return('~/.rstudio-desktop')},
    linux  = {return('~/.rstudio-desktop')},
    windows = { #Windows Vista, 7, 8 and 10
      base <- Sys.getenv(x = 'localappdata')
      return(file.path(base,'RStudio-Desktop'))
    }) 
}

clearEnvSuggestions <- function() {
  fileName = 'environments.txt'
  path <- file.path(baseFolderPath(),fileName)
  unlink(path)
}

createMstrconnectorDirectory <- function() {
  # Create folder `mstrconnector` folder in the rstudio-desktop directory
  # in user's appdata
  tryCatch({
    dir.create(baseFolderPath(),showWarnings=FALSE)
  },
  error = function(e){
    print(e$message)
    displayErrorMessage('RfolderError')
  })
}

detectOS <- function() {
  switch(Sys.info()[['sysname']],
    Windows = {return('windows')},
    Linux  = {return('linux')},
    Darwin = {return('mac')})
}


loadLinesFromFile <- function(fileName) {
  createMstrconnectorDirectory()
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

updateEnvSuggestions <- function(newSuggestions) {
  fileName = 'environments.txt'
  path <- file.path(baseFolderPath(),fileName)
  unlink(path)
  saveStringToFile(path,newSuggestions)
}

updateRecentProjects <- function(recentProjects) {
  fileName = 'recentProjects.txt'
  path <- file.path(baseFolderPath(),fileName)
  unlink(path)
  saveStringToFile(path,recentProjects)
}

updateDatasetProperties <- function(properties) {
  fileName = 'datasetProperties.txt'
  path <- file.path(baseFolderPath(),fileName)
  unlink(path)
  saveStringToFile(path,properties)
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
    displayErrorMessage('RfileError')
  })
}