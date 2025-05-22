
#𝑓(𝑥) = 0.1e^x cosx + 2ln|x|
x <-seq (from = 3, to =6, by = 0.1)
cVec <- (0.1 * exp(x) * cos(x)+ 2*log(abs(x)))

ans <- sum(cVec);
print(paste("The sum of this vector is ",round(ans,4)))
plot(x,cVec, main = "My First Plot")

#summation formula
i <- 1:25
store <- (2^i/i + 3^i/i^2)

print(paste("summation is ",round(sum(store))))

#Create two vectors, Vec1 & Vec2, of random integers which are chosen 
#with replacement from the range of 0, 1, ... , 999. 
#The length of each vector is 100. Set the random seed to 75
set.seed(75)

Vec1 <- sample(0:999, size = 100, replace = TRUE)
Vec2 <- sample(0:999, size = 100, replace = TRUE)

Vec2a <- Vec2[Vec2 > 600]

print(Vec2a)

#find indexes where value are greater than 600
Vec2b <- which(Vec2 > 600)
print(Vec2b)

Vec1c <- Vec1[Vec2b]

print(Vec1c)
      
count <- sum(Vec1 %% 2 == 0)

print(paste("There are",count,"numbers divisable by 2"))
  
  #function
  myFun <- function(vvec1) {
    range <- -4:3
    index <- 1
    x <- -4
    while (x < 4) {
      if ( x >= 0 && x < 2 ) {
        vvec1[index] <- x + 3
      }else if(x < 0) {
          vvec1[index] <-  x^2 + 2*x +3
      }else if (x >= 2 ) {
        vvec1[index] <- x^2 + 4*x -7
      }
      x <- x+1
      index <- index +1
    }
    plot(range,vvec1)
    return (vvec1)
  }
  result <- myFun(numeric(length(8)))

  