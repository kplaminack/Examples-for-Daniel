
makeCacheMatrix <- function(x = matrix()) {     	#creates a matrix
        i <- NULL                               	#sets inverse object to be used later in the code
        set <- function(y) {				
                x <<- y						#assigns the input argument to the x object in the parent environment
                i <<- NULL					#clears any value of i that had been cached by a prior execution of cacheSolve()
        }
        get <- function() x					#defines getter for matrix x
        setinverse <- function(inverse) i <<- inverse	#assigns the input argument to the value of i in the parent environment
        getinverse <- function() i				#defines getter for inverse i
        list(set = set, get = get,				#assigns each of these functions as an element within a list and returns it to the parent environment
             setinverse = setinverse,
             getinverse = getinverse)
}

cacheSolve <- function(x, ...) {				
        i <- x$getinverse()					#retrieves the inverse from makeCacheMatrix
        if(!is.null(i)) {					#if value is null, matrix is new; else, it's inverse is cached
                message("getting cached data")
                return(i)
        }
        data <- x$get()						#gets the matrix from the input object
        i <- solve(data, ...)					#calculates inverse
        x$setinverse(i)						#assigns inverse
        i								#prints inverse
}











































