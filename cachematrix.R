
## makeCacheMatrix: creates a special "matrix": caches its inverse.
makeCacheMatrix <- function(x = matrix()) {
	sm <- NULL
	set <- function(y){
		x <<- y
		m<<-NULL		
	}
	get<- function() x
	setinv <- function(solve) sm <<-mean
	getinv <- function() sm
	list(set=set,get = get, setinv = setinv, getinv=getinv)
}



##cacheSolve: Computes the invecacheSorse of "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.


cacheSolve <- function(x, ...) {
	sm <- sm$getinv()
	if(!is.null(sm)) {
		message("getting cached data")
		return(sm)	
	}
	data <- x$get()
	sm <- solve(data, ...)
      x$setinv(sm)
      sm
}
