#' @export
#' @title sm_plot
#' @param z [real] Vector of depth values
#' @param sigma [real] Vector of corresponding parameter values
#' @description Plots data, piecewise-linear fitted curve, and mixed-layer depth

#importFrom("graphics", "abline", "lines", "par", "plot", "points", "title")

sm_plot <- function(z=z, sigma=sigma, errornorm=0.02, nsegments=3) {
  ## Specifying error norm
  uncons <- by_s_m(thres=errornorm,z0=1,zmax=150,z=z[depth_filter(z)],sigma=sigma[depth_filter(z)])
  ## To see congruence between two methods
  # nr <- uncons[["nimax"]]
  ## Specifying # of segments
  cons <- by_s_m3(nr=nsegments,z0=1,zmax=150,z=z[depth_filter(z)],sigma=sigma[depth_filter(z)])


  par(mfrow=c(1,2))
  plot(y = z, x = sigma, ylim = rev(range(z)), type="l")
  abline(h=uncons[["by_s_m"]])
  points(y=uncons[["smz"]], x=uncons[["sms"]], col= 'red')
  lines(y=uncons[["smz"]], x=uncons[["sms"]], col= 'red')
  title("by_s_m")

  plot(y = z, x = sigma, ylim = rev(range(z)), type="l")
  abline(h=cons[["by_s_m"]])
  points(y=cons[["smz"]], x=cons[["sms"]], col= 'red')
  lines(y=cons[["smz"]], x=cons[["sms"]], col= 'red')
  title("by_s_m3")
}

