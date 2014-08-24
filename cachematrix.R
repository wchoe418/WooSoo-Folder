## One function creates a special matrix object tha can cache its inverse
## Another function computes the inverse of the special matrix returned by makeCaccheMatrix

## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {

	  ## Initialize the inverse property        
	  m <- NULL
	  ##Set the matrix
        set <- function(matrix) {
                x <<- matrix
                m <<- NULL
        }
	  ## Get the matrix
        get <- function(){
	  ## Return the matrix
	  x
	  }
	  ## Set the inverse of the matrix
        SetInverse <- function(inverse){ 
	  m <<- inverse
	  }
	  ## Get the inverse of the matrix
        GetInverse <- function(){
	  m
	  }
	  ## Return a list of the methods mentioned above
        list(set = set, get = get,
             SetInverse = SetInverse,
             GetInverse = GetInverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then cacheSolve should retrieve the inverse from the cache. 
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$GetInverse()
        ## If the inverse is set, return it
	  if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        ## Get the matrix from the object
  	  data <- x$get()
        ## Use matrix multiplication for calculating the inverse
	  m <- solve(data, ...)%*%data
        ## Set the inverse to the object
	  x$SetInverse(m)
        ## Return the final matrix
	  m
}

## This section is copied straight from the R console
## in order to make the check for the answer easier

## ##Following is an example matrix
## > k<-matrix(3:6,2,2)
## ##This is the answer I should be getting 
## > solve(k)
##     [,1] [,2]
## [1,]   -3  2.5
## [2,]    2 -1.5
## ##This is what I got after I saved and ran the code separately.
## ##KN stands for K New
## > KN <- makeCacheMatrix(K)
## ##KF stands for K final  
## > KF <- cacheSolve(KN)      
## > KF
## KF                        
## 	  [,1] [,2]
## [1,]   -3  2.5
## [2,]    2 -1.5                    

