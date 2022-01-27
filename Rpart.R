library(fOptions)
#b
binomial <- function(S0, u , d, N ) {
  S <- c()
  S[1] <- S0
  count <- 2
  
  for (i in 1:N) {
    for (j in 0:i) {
      S[count] <- S0 * d^j * u^(i-j)
      count <- count + 1
    }
  }
  return(S)
}

S <- binomial(100,1.155,0.9975,10)
S



#put the value of the underlying in a matrix
tree <- matrix(0,11,11)

z <- 1
for(i in 1:11){
  for(j in 1:i){
    tree[j,i] <- S[z]
  z <- z +1
  }
}

tree
BinomialTreePlot(tree, digits = 2)

discount<-function(m){
  for(j in ncol(m):1){
    for ( i in nrow(m):1){
    m[i,j] <- m[i,j]/(1.05^(j-1))
    }
  }
  m
}

disctree<-discount(tree)
disctree
BinomialTreePlot(disctree, digits = 4)

f <- matrix(0,nrow(tree),ncol(tree))

for(i in 1:nrow(tree)){
  if( tree[i,11] > 200){
    f[i,11] = 1
    }
  else{
    f[i,11] = 0
  }
  
}
 
f<-f/(1.05^10)
f



gains_tree <- function(matrix,p){
  for(j in 10:1){
    for (i in 1:j){
      matrix[i,j] <- p* matrix[i, j +1] + (1-p)*matrix[i+1,j+1]
    }
  }
  matrix
}
p=1/3
F0 = gains_tree(f,p)
F0

BinomialTreePlot(F0, digits = 2)


f0_discount <- discount(F0)

cat('The value of F0 is:' ,F0[1,1], 'Euros')


#Hence they are equal
#c


#for n=0
phi1_0<- (F0[1,2]-F0[2,2])/(disctree[1,2] - disctree[2,2])
phi1_0

phi0_0 <- F0[1,2]- (phi1_0*disctree[1,2])
phi0_0

#for n=1
phi1_1U<- (F0[1,3]-F0[2,3])/(disctree[1,3] - disctree[2,3])
phi1_1U

phi0_1U <- F0[1,3]- (phi1_1U*disctree[1,3])
phi0_1U

phi1_1D <- (F0[2,3]-F0[3,3])/(disctree[2,3] - disctree[3,3])
phi1_1D

phi0_1D <-F0[2,3]- (phi1_1D*disctree[2,3])
phi0_1D


#d
#simulation of S was already done in part 3.a and here we plot the path
plot(c(S))

ex3d <- function(){
  N=10
  phi_zeros <- matrix(0, N, N)
  phi_ones<-matrix(0, N, N)
  values <-matrix(0, N, N)
  for(j in 1:10){
    for (i in 1:j){
      phi_ones[i,j] <- ((F0[i, j+1] - F0[i+1, j+1]))*1.05/(disctree[i, j+1] - disctree[i+1, j+1])
      phi_zeros[i,j] <- F0[i+1, j+1] - (phi_ones[i,j]*disctree[i+1, j+1])
      values[i,j] <- phi_zeros[i,j] + (phi_ones[i,j]*disctree[i, j])
    }
  }
  return(list('phi ones' = phi_ones, 'phi zeros' = phi_zeros, 'values' = values))
}


rep <- ex3d()

plot(apply(t(rep$values),2,rev))
plot(apply(t(F0),2,rev))


#e.
pu=0.75
pd=1-pu
#probability binomial path
pITM = pu^10+ (choose(10,9)*pu^9*pd)+(choose(10,8)*pu^8*pd^2) + (choose(10,7)*pu^7*pd^3)+(choose(10,6)*pu^6*pd^4) + (choose(10,5)*pd^5*pu^5)

pITM
cat('The probability to end in the money is:', pITM)

