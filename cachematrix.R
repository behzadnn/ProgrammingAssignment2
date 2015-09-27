#the first function puts the inverse of the matrix in cache

makeCacheMatrix <- function(x = matrix()) {
inv<-Null
  set<-function(y){
    x<<-y
    m<<-Null
  }
  get<-function() x
  setinv<-function(inverse) inv<-inverse
  getinv<-function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## this function calculates the inverse of a function if it already exists in cache it isnot re calclualted it is just loaded

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
         inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        inv
}
