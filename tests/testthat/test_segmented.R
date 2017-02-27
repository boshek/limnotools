context("Testing the segmented class.")

test_that("segmented constructor creates an appropriate object.", {
  s = segmented()
  expect_equal(class(s),"segmented")
  expect_equal(length(s$a),0)
  expect_equal(length(s$b),0)
})

test_that("length.segmented actually works.", {
  s = segmented()
  expect_equal(length(s), 0)
})

test_that("segmented insert works as advertised.", {
  s = segmented()
  s1 = insert(segs=s,index=1,newa=0,newb=0)
  expect_equal(length(s1$a),1)
  expect_equal(length(s1$b),1)
  expect_equal(s1$a[1], 0)
  expect_equal(s1$b[1], 0)
  s2 = insert(segs=s1,index=1,newa=1,newb=-1)
  expect_equal(length(s1$a),1)
  expect_equal(length(s1$b),1)
  expect_equal(s1$a[1], 0)
  expect_equal(s1$b[1], 0)
})

test_that("segmented delete works as advertised.", {
  s = segmented()
  s1 = insert(segs=s,index=1,newa=0,newb=0)
  s0 = delete(s1,1)
  expect_equal(length(s0),0)
  s2 = insert(s1,2,1,-1)
  expect_equal(length(s2),2)
  expect_equal(s2$a[1],0)
  expect_equal(s2$b[1],0)
  expect_equal(s2$a[2],1)
  expect_equal(s2$b[2],-1)
})

test_that("segmented bounds works as advertised.", {
  s = segmented()
  expect_identical( bounds(s,1), list(NULL) )
  s = insert(s,1,1,0)
  expect_identical( bounds(s,1), list(NULL) )
  s = insert(s,2,0,1)
  s = insert(s,3,1,-1)
  expect_equal( bounds(s,1), list(c(1,1)) )
  expect_equal( bounds(s,2), list(c(1,1),c(2,1)) )
  expect_equal( bounds(s,3), list(c(2,1)) )
})
