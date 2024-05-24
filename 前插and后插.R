R <- function(Mt,t,h,times){
	lc <- cumprod(1:times)
	tt <- 1
	for(l in 1:times){
		tt <- tt*(t-l)
	}
	att <- abs(tt)
	tr <- h^(times+1)
	r <- (Mt/lc)*att*tr
	r
}
Mt = 0.565
t=0.48
h=0.1
times = 4
R(Mt,t,h,times)

newtons_forward <- function(x,y,h,xi,times){
	n <- length(x)
	f1 <- matrix(0,n,n)
	f1[,1] <- y
	for(j in n-1){
		for(i in n-j){
			f1[i,j] <- f1[i+1,j-1]-f1[i,j-1]
		}
	}
	t <- (xi-x[1])/h
	yi <- y[1]
	a <- matrix(1,times)
	for(k in 1:times){
		a[1] <- t
		if(k >1){
			a[k] <- a[k-1]*(t-k+1)*(1/k)
		}
	}
	for(m in 1:times){
		yi <- yi+a[m]*f1[1,m+1]
	}
	r <- R(0.565,t,h,times)
	print(yi)
	print(r)
}

x <- c(0,0.1,0.2,0.3,0.4,0.5,0.6)
y <- c(1,0.995,0.98007,0.95534,0.92106,0.87758,0.82534)
x1 <- 0.048
h = 0.1
times <- 4
newtons_forward(x,y,h,x1,times)

newtons_backward <-function(x,y,h,xi,times){
	n <- length(x)
	f1 <- matrix(0,n,n)
	f1[,1] <- y
	for(j in n-1){
		for(i in n-j){
			f1[i,j] <- f1[i+1,j-1]-f1[i,j-1]
		}
	}
	t <- (xi-x[n])/h
	yi <- y[n]
	a <- matrix(1,times)
	for(k in 1:times){
		a[1] <- t
		if(k >1){
			a[k] <- a[k-1]*(t+k+1)*(1/k)
		}
	}
	for(m in 1:times){
		yi <- yi+a[m]*f1[n-m,m+1]
	}
	r <- R(0.565,t,h,times)
	print(yi)
	print(r)
}

x <- c(0,0.1,0.2,0.3,0.4,0.5,0.6)
y <- c(1,0.995,0.98007,0.95534,0.92106,0.87758,0.82534)
x2 <- 0.566
h = 0.1
times <- 4
newtons_backward(x,y,h,x2,times)


