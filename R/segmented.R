segmented <- function() {
  structure(list(a=NULL,b=NULL),class="segmented")
}

length.segmented <- function(segs) {
  return( length(segs$a) )
}

insert <- function(segs, ...) {
  UseMethod("insert", segs)
}
insert.default = function(X, ...) { stop("Class has no insert method.") }

insert.segmented = function(segs,index=index,newa=newa,newb=newb) {
# Insert a new segment at index index with slope a and intercept b.
  segs$a = append(segs$a, newa, after=index-1)
  segs$b = append(segs$b, newb, after=index-1)
  return(segs)
}

delete <- function(x, ...) { UseMethod("delete", x)}

delete.segmented = function(segs=segs, index=index) {
  # Remove the element at index index
  # TODO: I could not find a base function that does this! Is there one?
  l = length(segs)
  if(l == 0) stop("segs must have at least one element!")
  if(index<1 | index>l) stop("index out of bounds!")
  if(index==1 & l==1) {
    # Special case, return null.
    segs$a=NULL
    segs$b=NULL
  }
  else if(index==1) {
    segs$a = segs$a[2:l]
    segs$b = segs$b[2:l]   # There is always a b for every a
  }
  else if(index==l) {
    segs$a = segs$a[1:(l-1)]
    segs$b = segs$b[1:(l-1)]
  }
  else {
    segs$a = c(segs$a[1:(index-1)],segs$a[(index+1):l])
    segs$b = c(segs$b[1:(index-1)],segs$b[(index+1):l])
  }
  return(segs)
}

bounds <- function(x, ...) {
  UseMethod("bounds", x)
}
bounds.default = function(X, ...) { stop("Class has no bounds method.") }

bounds.segmented <- function(segs=segs,index=index) {
  # Calculate the endpoints of the indexed line segment; i.e. it's intercepts with its neighbour segments.
  l = length(segs$a)
  if(l == 0 | l == 1) return(list(NULL)) # No or only one segment => no intercepts.

  if(index>l | index<1) stop("index is out of bounds!")

  # There are at least two segments here.
  if(index==1) {
    # Special case, only one intercept to the right.
    xr = -(segs$b[index] - segs$b[index+1])/(segs$a[index]-segs$a[index+1])
    yr = segs$a[index]*xr + segs$b[index]
    return( list(c(xr,yr)) )
  }
  else if(index==l) {
    # Special case, only one intercept to the left.
    xl = -(segs$b[index] - segs$b[index-1])/(segs$a[index]-segs$a[index-1])
    yl = segs$a[index]*xl + segs$b[index]
    return( list(c(xl,yl)) )
  }
  else {
    # Intercepts both to the right and left.
    xl = -(segs$b[index] - segs$b[index-1])/(segs$a[index]-segs$a[index-1])
    yl = segs$a[index]*xl + segs$b[index]
    xr = -(segs$b[index] - segs$b[index+1])/(segs$a[index]-segs$a[index+1])
    yr = segs$a[index]*xr + segs$b[index]
    return( list(c(xl,yl),c(xr,yr)) )
  }
}
