## This is a pair of functions that caching the inverse of a matrix for easy recall


#This function creates a special "matrix" object that can cache its inverse. 
makeCacheMatrix <- function(x = matrix()) {
  m<-NULL
  set<-function(y){
    x<<-y
    m<<-NULL
  }
  get<-function() x
  setInvMatrix<-function(solve) m<<- solve
  getInvMatrix<-function() m
  list(set=set, get=get,
       setInvMatrix=setInvMatrix,
       getInvMatrix=getInvMatrix)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
##If the inverse has already been calculated (and the matrix has not changed), 
##then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x=matrix(), ...) {
  m<-x$getInvMatrix()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  matrix<-x$get()
  m<-solve(matrix, ...)
  x$setInvMatrix(m)
  m
}

#usage (with any square matrix)
callMake<-makeCacheMatrix()
callMake$set(matrix(6:9,2,2))
cacheSolve(callMake)
#Test whether inverse is correct. 
(matrix(6:9,2,2)) %*% (cacheSolve(callMake)) # It should be 1 on the diagonal and zero elsewhere
