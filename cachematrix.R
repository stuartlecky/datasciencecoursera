##
## ## Put comments here that give an overall description of what your
## functions do
## 
## Approach - Two functions support efficient calculation and storage
##            of matrix inversion calculations. Supports inversion for 
##            square invertable matrix.  
##            Cacluation efficiency comes in thru storing any prior
##            cacluation, inversion, of a matrix and then 
##            refers to the stored matrix not requiring a repeat
##            calculation.
##
## Functions  Two functions are implemented to support the 
##            efficient matrix calculations. Description is below
##            makeCacheMatrix - creates special vector with
##                 functions to store matrix and caclulate matrix
##                 actual calculation is done by second function
##                 cachematrix
##               agruments - input - matrix
##               logic -     (1) creates set function to store
##                               matrix in memory
##                           (2) creates get function to get value of matrix
##                               from memory
##                           (3) setsolve sets up function to store solved matrix
##                           (4) getsolve - gets prior solved inversion
##                           (5) list - returns an enumerated list of 
##                               set, get, setsolve, and getsolve
##            
## 
##            cacheSolve - uses the special matrix from makeCacheMatrix
##                          and then checks first for prior calcualted matrix
##                          and skips calcualtion or executes calculate 
##                          for new matrix and stores cached result
##                      arguments - x input - special matrix 
##                          created in makeCacheMatrix
##                      logic (1) - get matrix cacluation stored in memory
##                            (2) - check if cacluation exists or is null
##                            (3) - if not null, return the inverse matrix result
##                            (4) - if null, get matrix and store in local variable
##                            (5) -          calculate inverse using solve
##                            (6) -          cache value useing setsolve
##                            (7) -          return inverted matrix result
##          
##
## functions with comments ----------------------------------------
##
## makeCacheMatrix - Creates special matix to with embedded functions to 
##                   be executed by cacheSolve
##                   creates set function to save matrix to memory
##                   creates get function to get matrix from memory
##                   creates setsolve function to save inverse matrix 
##                           result to memory
##                   creates getsolve function to get solved inverse 
##                           marix from memory if result exists
##                   return all funtions in list
##
## *** see below functions example run - input and output ***
##
##                    
makeCacheMatrix <- function(x = matrix()) {
## - set return variable m to null
        m <- NULL
## - create set function an store in memory
##      in scoping level above (calling function) 
##      x in memory, set m value to null and 
##      store m in scoping level above (calling function)
##      in memory
##      
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
## - create get function to get x and save in get
        get <- function() x
## - create setsolve to store solve in memory 
##      value m, in scoping level above (calling function)
        setsolve <- function(solve) m <<- solve
## - create getsolve to get solved value m
        getsolve <- function() m
## - return get, set, getsolve, setsolve as list
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)

}

## cacheSolve - takes speical matrix and 
##              uses embedded functions to get a 
##              prior inverted matrix and return the
##              inverted matrix, or calculate a new
##              inverted matrix, store in memory
##              and return the inverted matrix

cacheSolve <- function(x, ...) {
## - use getsolve function to get a prior solved matrix
	 m <- x$getsolve()
## - check if inverted matrix was stored in memory 
##   checking if not null, if not null, return
##   message retrieving from cache and return 
##   inverted matrix
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
## - inverted, solved matrix is not in memory
##   store the uncalculated matrix in to data
##   variable
        data <- x$get()
## - solve for the inverted matrix and store in m
        m <- solve(data, ...)
## - save inverted matrix using setsolve
        x$setsolve(m)
## - return the inverted matri
        m    
}
##
##
## - example input and output 
##
##
## setting original matrix to my_matrix
## my_matrix <- matrix(c(1.0,2.0,3.0,4.0),nrow=2,ncol=2)
## > my_matrix
##     [,1] [,2]
## [1,]    1    3
## [2,]    2    4
## setting up my_spec_matrix with my_matrix as input
## > my_spec_matrix <- makeCacheMatrix(my_matrix)
## > my_spec_matrix
## $set
## function (y) 
## {
##     x <<- y
##     m <<- NULL
## }
## <environment: 0x0c525964>
## $get
## function () 
## x
## <environment: 0x0c525964>
## $setsolve
## function (solve) 
## m <<- solve
## <environment: 0x0c525964>
## $getsolve
## function () 
## m
## <environment: 0x0c525964>
##
## executing cache solve - not cached
## cacheSolve (my_spec_matrix)
##     [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5
## executing cache solve - cached
## > cacheSolve (my_spec_matrix)
## getting cached data
##     [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5