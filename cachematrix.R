
##
## These 2 functions would cache the results of computing the inverse of a matrix,
## so that if the matrix is not changed, subsequent calls to invert the same matrix
## would return the cached results. This enhances performance as inversions of large
## matrices are computation intensive.
##
## When the matrix is changed, the onus is on the caller to update the functions.
## 
## The following is an example of how the functions are to be used.
##
## >>> func<-makeCacheMatrix()		## get a handle on the list of functions
## >>> func$set(mat)			## mat is the matrix to be inverted
## >>> inv_mat<-cacheSolve(func)	## cacheSolve returns inverted matrix
## >>> inv_mat<-cacheSolve(func)	## subsequent calls would return cached results
## >>> func$set(new_mat)		## change matrix to be inverted
## >>> inv_new_mat<-cacheSolve(func)	## compute inversion of new matrix
## 
## Alternatively, the following sequence would also compute the inverse of the matrix
##
## >>> func<-makeCacheMatrix(mat)
## >>> inv_mat<-cacheSolve(func)
##


##
## makeCacheMatrix(x = matrix())
##    Input  :
##	  x  : optional square matrix, whose inversion is to be calculated
##    Output :
##	The function returns a list of 4 functions
##	$get          : returns the the matrix to be inverted
##	$set(x)       : sets the matrix to be inverted to x and clears the cached results
##	$getResult    : get the cached inversion results
##	$setResult(x) : updates the cached result to x
##

makeCacheMatrix <- function(x = matrix()) {
  cachedResult <- NULL
  set <- function(y) {
    x <<- y
    cachedResult <<- NULL
  }
  get <- function() x
  setResult <- function(result) cachedResult <<- result
  getResult <- function() cachedResult
  list(set = set, get = get,
       setResult = setResult,
       getResult = getResult)
}


##
## cacheSolve(func)
##    Input  :
##     func  : list of functions from makeCacheMatrix()
##    Output : 
##	The function returns the inversion of the matrix stored previously
##	using func$set(mat). If a cached copy of results is available, that 
##	would be returned, otherwise, the inversion would be computed, the 
##	result cached and returned.
##

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getResult()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setResult(m)
  m
}
