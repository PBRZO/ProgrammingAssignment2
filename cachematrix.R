makeCacheMatrix <- function(x = matrix()) {
  #makeCacheMatrix takes x as a matrix
  #global inverse matrix im initialized to NULL
  im <<- NULL
  set <- function(y) {
    #overwrite global x with y
    x <<- y
    #initialize global im to NULL
    im <<- NULL
  }
  get <-function() x #return x
  getIm <- function() im #return im
  setIm <- function(inverseM) im <<- inverseM #set im to the inverse matrix
  list(set = set, get = get,
       getIm=getIm,
       setIm = setIm
       )
}

cacheSolve <- function(x, ...) {
  #check whether im is not NULL, a) get it b) if not null return inverse matrix
  im <- x$getIm()
  if (!is.null(im)) {
    message("get inverse matrix ")
    return(im)
  }
  #below is executed if im is NULL
  #a) get the matrix of the object x
  im_data <- x$get()
  #generate the reverse matrix
  im <- solve(im_data,...)
  #store the reverse matrix in the object x
  x$setIm(im)
  #return inverse matrix
  im
}

