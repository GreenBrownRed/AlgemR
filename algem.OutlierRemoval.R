#' Algem OutlierRemoval
#'
#' This function removes any OD values which are spurious. If the OD changes by more than 1 unit between two timepoints the value is removed
#' The function is quite conservative so sometimes it needs two passes to clean the data completely. This function is rather slow.
#' @author Michiel Matthijs  \email{mm@@algenuity.com}
#' @param alg_ object
#' @keywords Algem
#' @export
#' @examples
#' alg_wt1 = algem.OutlierRemoval(alg_wt1)



algem.OutlierRemoval = function(df1) {
  #removes the spurious OD values from the Algem run, because it is conservative it sometimes needs two passes
prev = 0
toggle = 0
counter = 0
a = c()
for (val in df1$OD_scaled) {
  if( is.na(val) == FALSE) {
    test = abs(val - prev) + abs(prev - val) 
  
    prev = val
    if (abs(test) < 1 ) {
      a = c(a,val)
      toggle = 0
    } else {

      if( toggle == 0) {
        a = c(a, NA)
        toggle = 1
        print("Outlier removed")
      }else {
        a = c(a, val)
      }
    }
  } else {
    a = c(a,NA)
  }
}
df1$OD_scaled = a
return(df1)
}