#' Algem Read function
#'
#' This function reads in the three files that are the output of an Algem run and returns it as a dataframe
#' @author Michiel Matthijs  \email{mm@@algenuity.com}
#' @param basename
#' @keywords Algem
#' @export
#' @examples
#' wt1 = algem.read("20170322-151513-WT_1-16060B")




algem.read = function(base) {
  #Read in Algem File
  name = paste(base,"-Growth.txt",sep="")
  strs = readLines(name)

  #See where the data starts, skip all the lines until [data]
  row_skip = grep("\\[Data\\]",strs) 
  growth = read.delim(file=name, skip = row_skip, nrows=length(strs)-23, header=FALSE)
  growth = data.frame(growth[,1],growth[,2])
  colnames(growth) = c("V1","V2") 

    
  growth = transform(growth, V1 = V1 - 1) 
  growth[1,1] = 10
  
  #Load in Light, Temp, pH
  colnames(growth) = c("Time","OD_scaled")
  name = paste(base,"-LightTemp.txt",sep="")
  strs = readLines(name)
  row_skip = grep("\\[Data\\]",strs)
  
  dat = read.delim(file=name, skip = row_skip, nrows=length(strs)-33, header=FALSE)
  colnames(dat) = c("Time","R","B","W","T","Tprof","Tmeas","Tcool","pH")
  #merge light and growth
  combi = merge(dat, growth, all = TRUE)

  #get rid of first line if OD was measured at T0
  if( combi$Time[1] == 0) {
    combi = combi[-1,]
  }

  #Set all algems runs to similar starting value by substracting first value, adding 0.1
  combi$OD_scaled = combi$OD_scaled - combi$OD_scaled[1] + 0.1
  #Set time in hours
  combi$Time = combi$Time / 3600
  return(combi)
}