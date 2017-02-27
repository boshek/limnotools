context("Testing the r2b() function.")


test_that("r2b very basic two-point test.", {
  # r2b should always get zero error when only two points are given
  x = c(0,1) ; y = c(0,1)
  res = r2b(1,3,x,y)
  expect_identical(res,list(error=0,a=1,b=0))
})

test_that("r2b three points in a line, no error.", {
  # Straight line through three points, slope 1, intercept 1. Error should be zero.
  x = c(0,1,2) ; y = c(1,2,3)
  expect_identical( r2b(1,length(x)+1,x,y), list(error=0,a=1,b=1) )
  # expect_equal( r2b(1,length(x)+1,x,y), 0 )
})

test_that("Mostly horizontal straight line with four symmetrical outliers.", {
  x=c(0,1,2,3,4,5)
  y=c(1,2,0,0,2,1)
  err = .1
  res = r2b(1,length(x)+1,x,y)
  expect_equal( res$error, 1 )
  expect_equal( res$a, 0, 0)
  expect_equal( res$b, 0, 1)
})

test_that("Slope 1 straight line with four symmetrical outliers", {
  x=c(0,1,2,3,4,5)
  y=c(0,2,1,2,5,5)
  err = .1
  res = r2b(1,length(x)+1,x,y)
  expect_equal( res$error, sqrt(2)/2 )
  expect_equal( res$a, 0, 1)
  expect_equal( res$b, 0, 0)
})

test_that("More complex test with 10 data points.", {
  # No longer agrees with Fortran because of remove of special case on first segment.
  x = c(0.0, 1.5, 2.5, 4.5, 5.0, 6.0, 7.1, 9.7, 10.1, 12.0)
  y = c(2.0, 7.0, 3.0, 4.0, 5.0, 7.0, 7.1, 8.3,  9.1, 10.0)
  digs = 16 # number of digits to the right of decimal place we expect to be the same.
  res = r2b(1,1+length(x),x,y)
  expect_equal( round(res$error,digits=digs), round(2.82789379510204,digs))
  expect_equal( res$a, 0.580658601764317)
  expect_equal( res$b, 2.85895376569639)
})

# test_that("r2b gets the right answers. Duh.", {
#   x = c(0.0,1.5,2.5,4.5,5.0,6.0,7.1,9.7,10.1,12.0)
#   y = c(2.0,7.0,3.0,4.0,5.0,7.0,7.1,8.3,9.1,10.0)
#   digs = 16 # number of digits to the right of decimal place we expect to be the same.
#   expect_equal( round(r2b(1,length(x),x,y),digits=digs), round(3.833333333333333,digs))
#
# })

