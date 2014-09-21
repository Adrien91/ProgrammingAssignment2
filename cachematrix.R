## These functions allow the user to inverse a given matrix (assuming the matrix)
## is invertible. If the inverse of a given matrix has just been calculated, then
## it is stored in the cache and when it is needed to calculate it again, the 
## cacheSolve function is going to look into the cache and return it. 

# I owe some credits to the example shown in the cours R programming from coursera.org

# makeCacheMatrix is a list of 4 functions:
## set: set up a matrix (the argument of the function is the matrix 
       #you want to invert)
## get : return the value of the matrix
## setinverse : set the value of the inverse you want to calculate
## getinverse : return the value of the inverse 
# makeCacheMatrix require a matrix to be its argument. 
# makeCacheMatrix will be used as the argument of the cacheSolve function

makeCacheMatrix <- function(x = matrix()) {

       I<-NULL #set I (which will be the inverse matrix) to NULL
       
       set<-function(y){
              x<<-y #Assign the argument of set to x in the makeCacheMatrix..
               #..environment (the parent environment)
              I<<-NULL #To erase the inverse if already exists in case you reset
       }
       
       get<-function() {x} # to get the matrix we currently have.  
       
       setinverse <- function(inv) {I <<-inv} 
       #Assign the argument of the function setinverse to I in the parent environment.
       
       getinverse <- function() {I} #a function to get the inverse stored in I
              
       list(set=set, get=get, 
             setinverse = setinverse,
             getinverse = getinverse) #return the above function in a list object
}


## cacheSolve function is a function that first checks if the inverse of the matrix
## we have set up in the makeCacheMatrix has already been calculated and if so it
## returns the value. If not, it will calulate the inverse.
## The argument of the cacheSolve function is the result of makeCacheMatrix.


cacheSolve <- function(x, ...) {
       I<-x$getinverse() # I takes the value of the Inverse (if already exists) 
      
       if(!is.null(I)){
                message("getting inverse from cached data :")
                return(I)
       } 
       #If I is not null, then the function return the Inverse stored in the cache.
       
       data<-x$get() 
       # If I is null then use the get function to assign the matrix to "data"
       
       I<-solve(data,...) 
       # I is assigned the Inverse of data in the function environment
       
       x$setinverse(I) # the Inverse value is set to I
       
       I  

        ## Return a matrix that is the inverse of 'x'
}
