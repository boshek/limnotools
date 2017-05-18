#' @export
#' @title s_m_p
#' @param eps [real] error norm
#' @param x [real] input x-axis array, should be an increasing function of index
#' @param y [real] input y-axis array
#' @return [integer] array A of indices giving data segments.
#' A[i] start of interval; A[i+1] end of interval, for any i<length(A)
#' @description Segments the data in x,y into the intervals given in the output array A.
#' The data in each interval can be linearly fitted within an error, given by r2b(), less than eps.
#' Comments from original Fortran code.
#' SUBROUTINE s_m_p(n,eps,x,y,Nr,Ni)
#' C     Main subroutine for determining LINEAR SEGMENTS for a SPECIFIED ERROR NORM VALUE
#' c     (This subroutine determines the linear fit to the data for a specified error norm)
#' C     Input:
#' C          N   -[INTEGER] number of data points;
#' C          EPS -[REAL]    error norm;
#' C          X   -[REAL(N)] input x-axis data array, should be increasing function of index
#' C          Y   -[REAL(N)] input y-axis data array
#' C       Output:
#' C          NR  -[INTEGER] final number of segments;
#' C          NI  -[INTEGER] final array with segment start points
#' C
#' INTEGER NR, NI(n)
#' REAL X(n),Y(n)

# The dynamic arrays in R remove the need to know the number of data points, n, which is just
# the length of x and y. Similarly Nr is no longer needed and is length(Ni)-1, one less than the length
# of the output array.

s_m_p = function(eps,x,y) {
  # This code generates Nr intervals in vector Ni
  if(is.unsorted(x)==TRUE) stop("x is not an increasing vector")

  split_eps_intervals <- function(ni, x, y, eps) {
    # If any interval does not meet the r2b<eps test then split it into two with split_interval().
    # Note that ni will grow in length as this process proceeds.
    i = 1
    while( i<length(ni) ) {   # Rinse, repeat over all intervals in ni.
      k1=ni[i]
      k2=ni[i+1]
      r2bresults = r2b(k1,k2,x,y)
      if(r2bresults$error > eps ) {
        # N.B. We split an interval here so ni gets one element longer
        # N.B. If an interval is added we will have to test the first
        # of the two new intervals so we don't increment i.
        ni = split_interval(ni,i)
        # print("split") #DEBUG
      }
      else {
        # Prior intervals all meet eps test, go to next one.
        i = i + 1
      }
    }
    return(ni)
  }

  merge_eps_intervals <- function(ni, x, y, eps) {
    # Try to find adjacent intervals that can be merged and still meet
    # the eps criterion.

    # Must have at least 2 intervals in the set before a merge is possible.
    if( length(ni)-1 < 2) return(ni)  # Differs from criterion in original code.

    i = 2
    while( i <= length(ni)-1 ) {
    # for( i in 2:(length(ni)-1) ) {
      k1=ni[i-1]
      k2=ni[i+1]
      eps1=r2b( k1=k1, k2=k2, x=x, y=y )$error
      # print(sprintf("merge trial. len(ni): %d, i: %d, k1: %d, k2: %d eps:%f",length(ni),i,k1,k2,eps)) #DEBUG
      if( eps1<eps ) {
        if( length(ni)-1 <= 2 ) { # Are there sufficient intervals to make a merge?
          # No.
          # We are here because the last two intervals tested out to merge.
          # So we can just adjust the intervals and bail out entirely because,
          # apparently the entire data set lies on a line within eps!
          # TODO: The original code doesn't seem to do this correctly. It should
          # adjust ni[2] = ni[3] and nr = 1. I think this branch has never been
          # taken in actual operation. Obviously the data set is invalid if it lies
          # entirely on a straight line, right?
          # print(sprintf("last merge. ni[i]:%d",ni[i])) #DEBUG
          ni[1]=1  # TODO: Original code, but this should already be the case.
          return(ni)
        }
        # We have a candidate interval to merge so merge it.
        ni <- merge_intervals(i, ni)
        # print("merged") #DEBUG
      }
      i = i+1
    }
    # print("Merge return") #DEBUG
    return(ni)
  }

  adjust_eps_intervals <- function(ni, x, y, eps) {
    # Find optimum split points in the given intervals, ni.
    # Consider each adjacent interval pair in turn and try all possible alternative
    # split points to see if they would improve the maximum error over both.
    # At this point in the original code stands the comment:
    #  "{"R" algorithm: adjusting the endpoint}"

    # Scan all adjacent interval pairs.
    for( i in 2:(length(ni)-1) ) {
      # i is the midpoint so the
      # first of the pair is (ni[i-1],ni[i]), second (ni[i],ni[i+1])
      k1 = ni[i-1]
      kmid = ni[i]
      k2 = ni[i+1]
      # Find the error on both intervals and take the max.
      epsm = max( r2b(k1,kmid,x,y)$error, r2b(kmid,k2,x,y)$error ) # TODO: I don't think this is necessary. We'll find it below anyway.

      # We are going to move the midpoint one index at a time
      # and find the split that gives the best error.
      bestmid = ni[i]    # Keep track of best splitpoint so far.

      # Scan all alternative splitpoints between these two enpoints.
      for( trialmid in (k1+2):(k2-2) ) {
        epsr = max( r2b(k1,trialmid,x,y)$error, r2b(trialmid,k2,x,y)$error )  # Calculate max error
        if( epsr < epsm ) {
          epsm = epsr  # This split is the best so far
          bestmid = trialmid # Keep track of which index it is at.
        }
      }

      # print(sprintf("adjust trial. bestmid: %d, ni[i]:%d",bestmid,ni[i])) #DEBUG
      if( bestmid != ni[i] ) {  # Did we find a better splitpoint?
        ni[i] = bestmid  # Yes, change the split.
        # print("adjusted.") #DEBUG
      }

    } # for
    return(ni)
  }

  # Begin the split/merge algorithm.

  # Nr is fixed at two so we end up with 3 elements in Ni:
  # Ni = {1,m+1,n+1}
  # Clearly, it once spread Nr intervals out uniformly over all the data.
  # We will short-circuit it instead:
  ni = c( 1, round(length(x)/2)+1, length(x)+1 )

  repeat {
    # continue looping until there is no change in ni
    # print("New s/m/r loop.") #DEBUG
    originalni = ni
    ni = split_eps_intervals(ni,x,y,eps)
    ni = merge_eps_intervals(ni,x,y,eps)
    ni = adjust_eps_intervals(ni,x,y,eps)
    if( identical(originalni,ni) ) break
  }

  return(ni) # Q.E.D.
}
