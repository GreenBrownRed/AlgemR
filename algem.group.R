#' Algem biological group function
#'
#' This function groups a list of alg dataframes that represent biological replicates, the mean and the standard deviation are calculated and stored.
#' It returns a dataframe with all of these.
#' @author Michiel Matthijs  \email{mm@@algenuity.com}
#' @param Add the grouping variable to name this experiment
#' @keywords Algem
#' @export
#' @examples
#' wt_combi = algem.group(list(alg_wt1,alg_wt2,alg_wt3),"Wild type")

algem.group = function(input_list, grouping_variable) {
  #Function to combine three replicates and add the mean and the standard deviation
  combined_avg = rbindlist(input_list)[,lapply(.SD,mean), list(Time)]
  combined_SD = rbindlist(input_list)[,lapply(.SD,sd), list(Time)]
  combined_SDplus = combined_avg$OD_scaled + combined_SD$OD_scaled 
  combined_SDmin = combined_avg$OD_scaled - combined_SD$OD_scaled 
  combined_avg$SD = combined_SD$OD_scaled
  combined_avg$SDmin = combined_SDmin
  combined_avg$SDplus = combined_SDplus
  
  combined_avg = cbind(combined_avg, grouping_variable)
  colnames(combined_avg)[14] = "Group" 
  
  return(combined_avg)
  
}