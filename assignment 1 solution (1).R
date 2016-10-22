#1.	Write a function that takes three arguments - the first two should be numbers and the third should be a string.

fun=function(a,b,c){
  if(c=="add"){
    return(a+b)
  }else {
  if(c=="substract"){
    return(a-b)
  }else{
    if(c=="divide"){
      return(a/b)
    }else{
      if(c=="multiply"){
        return(a*b)
      }else{
        
        if(c=="log"){
          return(log(a,base=b))
        }else{
          if(c=="power"){
            return(a^b)
          }else{
            return(-1)
          }
    
  }}}}}}


fun(10,3,"add")


#2.	Write a function that takes a number as input and outputs a sequence of odd numbers starting from 1 to that number. 

for (i in 1:20) {
  if (i %% 2 ==1) print(i)
}

#4.	Write a function that returns the class of the supplied input

s="hai"
print(class(s))


#5.	Write a program that checks whether a number is prime and return TRUE for prime and FALSE for not prime

is.prime <- function(num) {
  if (num == 2) {
    TRUE
  } else if (any(num %% 2:(num-1) == 0)) {
    FALSE
  } else { 
    TRUE
  }
}

is.prime(2)

