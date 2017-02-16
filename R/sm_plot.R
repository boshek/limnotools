sm_plot <- function(z=z, sigma=sigma) {
  ## NOT specifying # of segments
  uncons <- by_s_m(thres=0.1,z0=1,zmax=150,z=z[depth_filter(z)],sigma=sigma[depth_filter(z)])
  ## To see congruence between two methods
  nr <- uncons[["nimax"]]
  ## Specifying # of segments
  cons <- by_s_m3(nr=nr,z0=1,zmax=150,z=z[depth_filter(z)],sigma=sigma[depth_filter(z)])


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

