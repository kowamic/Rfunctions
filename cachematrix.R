## the "makeCacheMatrix()" function takes a matrix as argument and creates a special matrix object which caches its inverse.
## the cacheSolve() function computes the inverse of the matrix return by the first function - if the result has already been calculated 
## then the function should return the inverse from the cache. 

## the function takes the matrix and check if it is invertible - then we define the set and get functions which will be used in the cacheSolve() function.
## the inversion function is also defined here.

makeCacheMatrix <- function(x = matrix()) {
  
  qr_x <- qr(x)
  if(!(qr_x$rank == ncol(x))){
    stop("The matrix is not invertible!")
  }
  
  m_inv <- NULL
    set <- function(y) {
      x <<- y
      m_inv <<- NULL
    }
    
  get <- function() x
  
    cacheInv <- function(){
      if(!is.null(m_inv)){
        message("getting cached data")
        return(m_inv)
      }else{
        message("calculation of the inverse matrix")
        m_inv <- solve(x)
        return(m_inv)
      }
    }
  
  list(set = set, get = get,cacheInv = cacheInv)
}


## the function takes the invertible matrix in argument and check if there is some cached data.
## If not, it computes the inverse.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$get()
      if(!is.null(m)) {
        message("getting cached data")
        return(m)
      }else{
        m_inv <- m$cacheInv()
      }
  solve(m)
}
