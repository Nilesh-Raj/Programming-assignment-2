##pair of function cache the inverse of the matrix


#function stores matrix and its inverse
makeCacheMatrix <- function(x = matrix()) #object x set input as empty matrix
                                         {
  inv <- NULL          # object inv initiallized with NULL
  
  
  # set the matrix
  set <- function(my_matrix)  #set takes arguement as y
                    {
    x <<- my_matrix
    inv <<- NULL    #inv is set as NULL because of cached value 
  }
  
  #get the matrix
   get <- function(){x}   #x is retrieve from parent environment of makeCacheMatrix
  
  #set the inverse
  setinverse <- function(inverse){
    inv <<- inverse      #assign input arguementin the parent environment  
  }
  #get the inverse
  getinverse <- function(){inv} 
  
  # return the matrix object
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
                             #naming the list of object assigned with above four function help to use $ 
                            #operator rather than [[]]
   }   



#function uses arguement the return of makeCacheMatrix and calculate inverse of cached value
cacheSolve <- function(x,...)  
{
  inv <- x$getinverse()  #calls the getinverse on the input object
  if(!is.null(inv))      #checking whether inverse is not NULL and return inverse of matrix
  {
    message("getting cached data")
    return(inv)         
  }
  z <- x$get()   #if condition is false then get the matrix from input object
  inv <- solve(z,...)  #calculating inverse of the matrix
  x$setinverse(inv)         #set the inverse in the input object 
  inv                       #return the inverse of matrix 
  
}
