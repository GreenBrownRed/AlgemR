#' Algem Folder function
#'
#' This function allows you to read in all the Algem files that are present in the working directory. 
#' It returns a list with all the algem runs loaded and removes any spurious values from the OD
#' The OD is scaled to 0.1 for ease of comparison
#' @author Michiel Matthijs  \email{mm@@algenuity.com}
#' @param none
#' @keywords Algem
#' @export
#' @examples
#' list_experiment = algem.folder.O()


algem.folder.O = function() { 
  #Loads in all Algem files in the working directory usage: algem.folder()
  # removes outliers as well
  filelist = list.files(pattern = "-Growth") # Get all growth files from directory
  filelist = gsub("-Growth.txt","",filelist) # chop off end bit to be compatible with algem.read()
  list_algem = c()
  
  for (val in filelist) {
    r = "-\\d*-(.*)-"     
    m = gsub(r, "\\1", regmatches(val,gregexpr(r,val))[[1]]) # get first matching group
    m = paste("alg_", m, sep = "") # attach bit to overcome any numbers at start and get consistency
    m = gsub("\\s","_",m)
    list_algem = c(list_algem, m)
    assign(m, algem.OutlierRemoval(algem.read(val)), envir = .GlobalEnv)
    cat("Loaded in file:", val, "as", m ,"\n")
  }
  
  cat("List of files stored in list_algem")
  return(list_algem)
}