## This is a pair of functions that cache and compute the inverse of a matrix.

## The first function creates a matrix object that caches its inverse. It creates a list that contains four functions: set, get, setinverse and getinverse.
## set = set the value of the matrix
## get = get the value of the matrix
## setinverse = set the value of the inverse of the matrix
## getinverse = get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL # here the result is stored
	set <- function(y){
		x <<- y
		inv <<- NULL # initialises inv to null
		}
		get <- function() x # return the input matrix
		setinverse <- function(inverse) inv <<- inverse
		getinverse <- function() inv
		list(set=set, get=get, setinverse=setinverse, getinverse=getinverse) # returns a list that contains all these functions, so that we can then change matrix, go to changed matrix, set inversed matrix and get inversed matrix

}


## The second function returns the inverse of the matrix. It first checks if the inverse has already been calculated and skips computation if so. If not, it computes the inverse and sets the value in the cache with setinverse.

cacheSolve <- function(x, ...) {
        inv <- x$getinverse() # get the inversed matrix from object x
        if(!is.null(inv)) {
        	message("getting cached data")
        	return(inv) # return the calculated inversion
        	}
        	data <- x$get() # if not, we get the matrix object
        	inv <- solve(data) # then solve it
        	x$setinverse(inv) # then set it to the object
        	inv # and return the solved result
}
