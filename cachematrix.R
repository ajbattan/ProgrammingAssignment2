## The below functions takes as input a matrix and caches it. Once the cacheSolve function
## is called to get the inverse of the matrix, the inverse also gets cached. As long as the
## matrix doesn't change in value, everytime the inverse is invoked, the cached value of 
## inverse is returned w/o having to re-compute the inverse. 


## The first function takes a matrix as input. 
## set gives the ability to change the matrix values or input a completely new matrix
## get gets the matrix from cache
## setInverse sets the matrix that is passed in its arguement as the inverse of the matrix
## getInverse gets the inverse matrix that was set using the previous function

makeCacheMatrix <- function(x = matrix()) 
{

		i <- NULL
		set <- function(y)
		{
				x <<- y
				i <<- NULL
		}
		
		get <- function() x
		setInverse <- function(inverse) i <<- inverse
		getInverse <- function() i
		
		list(set = set, get = get,
			 setInverse = setInverse,
			 getInverse = getInverse)
			 
}	

## This function checks using getInverse if an inverse exists for the matrix in its cache.
## If yes, the cached inverse matrix is returned, else the inverse is computed and the new 
## inverse is returned
## **** Assumes the matrix that is input is always invertible *** 

cacheSolve <- function(x, ...) 
{
        ## Return a matrix that is the inverse of 'x'
        i <- x$getInverse()
        if(!is.null(i))
        {
        		message("getting cached data")
        		return(i)
        }
        
        matrix_data <- x$get()
        i <- solve(matrix_data)
        x$setInverse(i)
        i
}        		
