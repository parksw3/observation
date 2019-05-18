library(dplyr)
library(pomp)
source("../R/pomp_model.R")

data <- data.frame(
	time=seq(0, 10, by=0.1),
	cases=0
)

globals <- pomp::Csnippet("double N=100000;")

pomp_arg <- pomp_sir("mortality", delta.t=0.1)

pomp_arg <- append(pomp_arg, list(data=data, globals=globals,
								  times="time", t0=0))

pomp_model <- do.call(pomp, pomp_arg)

start <- c(R0=2, gamma=1, i0=0.0001, theta=10, rho=0.5)

set.seed(101)
ss <- simulate(pomp_model, param=start, as.data.frame=TRUE)

fakedata <- data.frame(
	time=ss$time,
	cases=ss$cases
)

save("fakedata", file="stochastic.rda")
