# Grid Search
gridsearch <- function(start,end,fun,stepsize = .001){
  x <- seq(start,end,stepsize)
  y <- fun(x)
  maxi <- max(y)
  mini <- min(y)
  return(list("max" = maxi,"min" = mini))
}


#bisection
bisect <- function(start,end,fun, maxiter = 1000, ep=.001){
  if(fun(start)*fun(end)<0){
    for(i in 1:maxiter){
      mid <- (start+end)/2
      if(fun(start)*fun(mid)<0){
        end <- mid
      }else{start <- mid}
      if(abs(start-end)<ep){
        mid <- (start+end)/2
        break
      }
    }
  }else{stop("Invalid start points")}
  return(mid)
}


functionaliter <- function(start,fun,maxiter=1000,ep = .001){
  x1 <- start
  for(i in 1:maxiter){
    x2 <- fun(x1)
    if(abs(x2-x1)<ep){break}
    x1 <- x2
  }
  return(x2)
}


newton <- function(start,fun,dif,maxiter=1000){
  x1 <- start
  x2 <- matrix(rep(0,maxiter),nrow= maxiter)
  for(i in 1:maxiter){
    x2[i] <- x1 - fun(x1)/dif(x1)
    x1 <- x2[i]
  }
  return(x2)
}


errorplot <- function(iterx,realx,type = 1,title){
  l <- length(iterx)
  error1 <- x-realx
  error2 <- error1[2:l]^type
  y <- error2/error1[1:l-1]
  t <- 1:l
  plot(t,y,main=title,xlab = "Iteration",ylab = "Rate")
}


inv.cauchy <- function(n){
  u <- runif(n)
  x <- tan(pi*(u-1/2))
  x
}

inv.exp <- function(n, lambda){
  u <- runif(n)
  x <- -log(u)/lambda
  x
}


invgamma.int <- function(n,k,lambda){
  x <- matrix(inv.exp(n=n*k,lambda = lambda), ncol = k)
  apply(x,1,sum)
}


rej.sampling <- function(samplex,g,f,C){
  Gx <- g(samplex)
  Hx <- C*Gx
  u <- runif(length(samplex))
  y <- u*Hx
  Fx <- f(samplex)
  x <- samplex[y<Fx]
  x
}


montecarlo.int <- function(samplex, g, alpha){
  n <- length(samplex)
  est <- mean(g(samplex))
  variance <- var(g(samplex))
  interval <- c(est - qnorm(1-(alpha/2))*sqrt(variance/n), est + qnorm(1-(alpha/2))*sqrt(variance/n))
  return(list("Estimate" = est, "Variance" = variance, "CI" = interval))
}



imp.sampling <- function(x,phi,g_x,f_x){
  phi_x <- phi(x)
  w_x <- f_x/g_x
  theta <- w_x*phi_x
  est <- mean(theta)
  est
}


Midpoint <- function(lowerbound, upperbound,f,n){
  step <- (upperbound-lowerbound) / n
  x <- seq(lowerbound+step/2, upperbound-step/2, by = step)
  est <- mean(f(x))
  est
}


Trapezoidal <- function(lowerbound, upperbound,f,n){
  step <- (upperbound-lowerbound) / n
  x <- seq(lowerbound, upperbound, by = step)
  f_x <- f(x)
  w <- c(1,rep(2,n-1),1)
  est <- sum(step/2*(w*f_x))
  est
}


Simpsons <- function(lowerbound, upperbound,f,n){
  step <- (upperbound-lowerbound) / n
  x <- seq(lowerbound, upperbound, by = step)
  f_x <- f(x)
  w <- c(1,rep(2,n-1),1)
  w[(1:(n/2))*2] <- 4
  est <- sum(step/3*(w*f_x))
  est
}