#1.	Get Familiar with R
print("Hello, Shiyan Jiang")
i=0
print(i+1)

#2.	Log Gamma (Loop)
sum=0
log_gamma_loop <- function(n){
  while(n>1){
  sum <- log(n-1)+sum
  n<-n-1
  }
  return(sum)
}
print(log_gamma_loop(5))

#3.	Log Gamma (Recursive)
log_gamma_recursive <- function(n) {
  if (n == 1)    return (0)
  else           return (log(n-1) + log_gamma_recursive(n-1))
}
print(log_gamma_recursive(5))

#4.	Sum of Log Gamma
sum=0
sum_log_gamma_loop <- function(n){
  while(n>1){
    sum <- log_gamma_loop(n)+sum
    n<-n-1
  }
  return(sum)
}
sum_log_gamma_recursive <- function(n) {
  if (n == 1)    return (0)
  else           return (log_gamma_recursive(n) + sum_log_gamma_recursive(n-1))
}
print(sum_log_gamma_loop(5))
print(sum_log_gamma_recursive(5))

#5.	Compare Results to Built-In R Function
sum=0
sum_lgamma_loop <- function(n){
  while(n>1){
    sum <- lgamma(n)+sum
    n<-n-1
  }
  return(sum)
}
getOption('expressions')
options(expressions = 500000)
system.time(sum_log_gamma_loop(1500))
system.time(sum_log_gamma_recursive(1500))
system.time(sum_lgamma_loop(1500))
