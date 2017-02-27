#' @export
#' @title computing a norm value for the segment from point k1 to k2-1
#' @param k1 [INTEGER] start point
#' @param k2 [INTEGER] end point+1
#' @param x [REAL(?)] input x-axis array (predictor)
#' @param y [REAL(?)] input y-axis array (response)
#' @return (err,a,b) [LIST] err is error norm calculated as the maximum pointwise geometrical distance
#' measured perpendicular to the regression line. a is the regression slope. b is the regression intercept.
#' @description Finds the least-squares regression line through the data points with index >= k1 and < k2.
#' Computes the perpendicular distance of each point from the regression line and returns the maximum.
#' @examples
#' ni <- c( 1, 201, 402 )
#' i <- 1
#' k1 <- ni[i]
#' k2 <- ni[i+1]
#'
#'
#' r2b(k1, k2, y=t11$temper, x=t11$depth)

# Doug could not find anywhere in the original code where outputs a and b are actually used
# he has taken then out to avoid having to return a data.frame.

## Sam has replaced r2b (again)

r2b = function(k1,k2,x,y) {
  n = k2 - k1  # Number of points in interval.

    # if(n <= 2) {
    # There are only two points here so the error must be zero.
    r2b=0
  # } else {
    is = k1:(k2-1)
    sxx = sum(x[is]^2)
    sxy = sum(x[is]*y[is])
    sy = sum(y[is])
    sx = sum(x[is])

    #a = 0.0
    # if(k1 > 1) {
      a = (n*sxy-sy*sx)/(n*sxx-sx*sx) # The slope of the regression line
    # }
    b = (sy-a*sx)/n # Y intercept of the regression line

    # The following formula calculates the geometrical distance from
    # each point to the regression line and takes the maximum.
    r2b = max( abs( y[is] - a*x[is] - b )/sqrt(a^2 + 1) )
  # }

  #return(data.frame(r2b=r2b,a=a,b=b))
  return( list(error=r2b, a=a, b=b) )
}
