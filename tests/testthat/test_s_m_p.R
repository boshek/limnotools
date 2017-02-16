context("Testing s_m_p.")

test_that("s_m_p gets answers similar to the Fortran program.", {
  # The following test gets a similar but not identical answer to the Fortran
  # program because it considers the last point, where the Fortran
  # simply arbitrarily (apparently) eliminates it.
  eps = .01
  x = c(0.0, 5.0, 10.0)
  y = c(1.0, 10.0, 10.0)
  expect_equal( s_m_p(eps,x,y), c(1,3,4) )

  # The same applies to this test. The Fortran program in this case
  # was also apparently getting the wrong answer.
  eps = .01
  x = c(0.0, 5.0, 10.0, 15.0, 20.0, 25.0,30.0,35.0,40.0,45.0)
  y = c(1.0, 2.0, 3.0,   4.0,  5.0,  5.0, 4.0, 4.0, 3.0, 2.0)
  expect_equal( s_m_p(eps,x,y), c(1,6,8,11) )
})

test_that("s_m_p recovers segments from artificially segmented data", {
  interp = function(x0,x1,N) {
    frax = (0:(N-1))/N
    return( x0 + frax*(x1-x0) )
  }

  n = 100  # Number of data points on each segment
  x = interp(0,1,n); y = interp(0,10,n)
  x = c(x,interp(1,2,n)); y = c(y,interp(10,10,n))
  x = c(x,interp(2,3,n)); y = c(y,interp(10,20,n))
  eps = .01
  expect_identical(s_m_p(eps,x,y), c(1,101,201,301))

  # Add some random noise and retest.
  noise = .5/n # Maximum amplitude of noise
#  x = x + noise*(2*runif(length(x))-1)
  y = y + noise*(2*runif(length(y))-1)
  eps = 2*noise
  expect_equal( length(s_m_p(eps,x,y)), length(c(1,101,201,301)) )

  # N = 100
  # eps = .01
  # angle = (0:(N-1))*2*pi/N
  # x = angle
  # y = sin(angle)
  #
  # result = c(1,  3, 12, 19, 26, 32, 38, 44, 59, 65, 71, 77, 82, 87, 93,100)
  # expect_equal( s_m_p(eps,x,y), result)

})

test_that("s_m_p fails when x is not an increasing vector", {
  eps = .005
  x1 <- c(0.0,1.5,2.5,4.5,6.0,5.0,7.1,9.7,10.1,12.0) ## Non increasing vector
  y = c(2.0,7.0,3.0,4.0,5.0,7.0,7.1,8.3,9.1,10.0)
  expect_error(s_m_p(eps,x1,y), "x is not an increasing vector")
})
