## Caching the Inverse of a Matrix
## Matrix inversion is usually a costly computation 
## therefore in these functions we are caching the inverse of a matrix rather
## than compute it repeatedly.

makeCacheMatrix <- function(x = matrix())
        
{
        ## This function creates a special "matrix" object that can cache its inverse.
        
        matInverse <- NULL
        set <- function(y) {
                x <<- y                     #set the values of the matrices
                matInverse <<- NULL
        }
        get <- function() x                        #get the value of the matrix 
        setInverse <- function(Minv) matInverse <<- Minv
        #set the value of the inverse
        
        getInverse <- function() matInverse        # get the value of the mean
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
        
}


cacheSolve <- function(x, ...) {   
        ## This function computes the inverse of the special "matrix" 
        ##returned by makeCacheMatrix above. 
        
        ## If the inverse has already been calculated 
        ##(and the matrix has not changed), 
        ## then the cachesolve should retrieve the inverse from the cache.
        ## Return a matrix that is the inverse of 'x'
        
        matInverse <- x$getInverse()
        
        #First check to see if the inverse has already been calculated
        if(!is.null(matInverse)) {
                message("getting cached data")
                return(matInverse)  ## Return a matrix that is the inverse of
                ##'x' - gets the inverse from the cache and skips the 
                ## computation.
                
        }
        
        data <- x$get()
        matInverse <- solve(data)# Computing the inverse of a square matrix 
        x$setInverse(matInverse) #Calculates the inverse and sets it in the 
        ## cache via the setmean function.
        matInverse
        
}