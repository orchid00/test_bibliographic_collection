# utils functions

# save RData
savemyobject <- function(objecttosave, folder_name, file_name){
  save(objecttosave,
     file = paste0(folder_name, "/", file_name, "_", 
                   lubridate::today(), ".RData"))
}

savemyobjectRDS <- function(objecttosave, folder_name, file_name){
  saveRDS(objecttosave,
       file = paste0(folder_name, "/", file_name, "_", 
                     lubridate::today(), ".RData"))
}
