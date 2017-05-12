#' Algem quick plot function to visualise pH, temperature profile, light and OD.
#' Uses the ggplot2 package and the multiplot function included in this package (author unkown please contact me if you know the attribution)
#' @param a alg_ object
#' @author Michiel Matthijs  \email{mm@@algenuity.com}
#' @keywords Algem
#' @export
#' @examples
#' algem.quickplot(alg_wt1)



algem.quickplot = function(input_set) {
  #Takes a alg_ object and plots out temperature, pH, growth and light for a quick quality control
  require("ggplot2")
  ph = ggplot() + geom_point(data = na.omit(input_set), aes(x=Time, y=pH)) + ylim(3,12) + ylab("pH")
  growth = ggplot() + geom_point(data = na.omit(input_set), aes(x=Time, y=OD_scaled)) + ylab("OD 740nm initial value set to 0.1")
  t = ggplot() + geom_point(data = na.omit(input_set), aes(x=Time, y=Tmeas)) + ylim(4,35) + ylab("Measured temperature (°C)")
  l = ggplot(data = na.omit(input_set)) + geom_line(aes(x=Time, y=W), colour = "black") + geom_line(aes(x=Time, y=R), colour = "red") + geom_line(aes(x=Time, y=B), colour = "blue") + ylab("Light Intensity")
  multiplot(growth,l,t,ph,cols=2)  
}