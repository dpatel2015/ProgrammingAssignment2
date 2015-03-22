makeCacheMatrix <- function(x = matrix()) {
 d<-NULL
  set<-function(b){
    x<<-b
    d<<-NULL
  }
  get<-function() x
  setmatrix<-function(solve) d<<- solve
  getmatrix<-function() d
  list(set=set, get=get,
       setmatrix=setmatrix,
       getmatrix=getmatrix)
}

cacheSolve <- function(x=matrix(), ...)  {
	 d<-x$getmatrix()
  if(!is.null(d)){
    message("getting cached data")
    return(d)
  }
  matrix<-x$get()
  d<-solve(matrix, ...)
  x$setmatrix(d)
  d
}

##Example
x <- matrix(rpois(25,3), nrow = 5)
cx <- makeCacheMatrix(x)
cx$get()
