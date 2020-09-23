#followong function loads the matrix via keyboard
loadMatrix = function(){
  s = readline("Enter the size of the mxn matrix separated by coma (,):")
  sNum = strtoi(strsplit(s,",")[[1]]) #here we split and parse 
  #the input data to integer
  m = sNum[1]
  n = sNum[2]
  v = c()
  print("Enter the data by rows, separated by coma (,)")
  for (i in 1:m) {
    vS = readline(paste("Row ", i, ": ", sep = ""))
    v = c(v,strtoi(strsplit(vS, ",")[[1]])) #here we split the row data
    #and parse the input data to integer
  }
  #matrix creation by using the matrix data structure
  mat = matrix(v, nrow = m, byrow = T)
  return(mat)
}

#following function verifies if a vector is the zero vector
is_zero = function(c){
  for(num in c){
    if(num != 0){
      return(FALSE)
    }
  }
  return(TRUE)
}

#following function performs the type 1 elementary row operation
elementary_1 = function(m, i, j){
  aux = m[i,]
  m[i,] = m[j,]
  m[j,] = aux
  return(m)
}

#following function performs the type 2 elementary row operation
elementary_2 = function(m, i, a){
  m[i,] = a*m[i,] 
  return(m)
}

#following function performs the type 3 elementary row operation
elementary_3 = function(m, i, j, b){
  m[i,] = m[i,] + b*m[j,]
  return(m)
}

#Following function tries to compute the PA=LU
#decomposition via partial pivoting
#-----input--------
#M: R's matrix type
#-----output-------
#prints the PA=LU factorization and returns a list
#(P,L,U) containing Permutation, Upper and Lower R's matrices.
#
#If not factorization exists, it'll print an error message
#giving the position in the matrix were error was caused

PA_LU = function(M){
  #we find the matrix dimension
  n = dim(M)[1]
  
  L = diag(n)
  P = diag(n)
  U = M
  
  #iterating over every column:
  for (j in 1:(n-1)) {
    #finding the maximun element below 
    #the jth position in the jth column
    jth_col = U[,j]
    max_index = which.max(abs(jth_col[j:n]))
    
    #Modifying the Upper and Permutation matrix
    P = elementary_1(P, j, max_index)
    U = elementary_1(U, j, max_index)
    #iterating over rows below the jth row
    for (i in (j+1):n) {
      #sum 
      L[i,j] =  U[i,j]/U[j,j]
      U = elementary_3(U,i,j,-U[i,j]/U[j,j])
    }
  }
  print("P: ")
  print(P)
  print("U: ")
  print(U)
  print("L: ")
  print(L)
  print("A: ")
  print(M)
}

A = matrix(c(2,1,5,4,4,-4,1,3,1), nrow = 3, byrow = T)
PA_LU(A)
debug(PA_LU)

