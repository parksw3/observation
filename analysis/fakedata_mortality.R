library(dplyr)
library(pomp)
source("../R/pomp_model.R")

load("../data/stochastic.rda")

globals <- pomp::Csnippet("double N=100000;")

pomp_arg <- pomp_sir("mortality", delta.t=0.1)

pomp_arg <- append(pomp_arg, list(data=fakedata, globals=globals,
								  times="time", t0=0))

pomp_model <- do.call(pomp, pomp_arg)

start <- c(R0=2, gamma=1, i0=0.0001, theta=10)

rwsd_arg <- list(R0=0.01, gamma=0.01, i0=0.01, theta=0.01)

set.seed(101)
mlist <- replicate(10, mif2(
	pomp_model,
	Nmif=50,
	start=start,
	Np=1000,
	cooling.fraction.50=0.95,
	rw.sd=do.call(rw.sd, rwsd_arg),
	transform=TRUE) %>%
	continue(Nmif=50, cooling.fraction=0.8) %>%
	continue(Nmif=50, cooling.fraction=0.6) %>%
	continue(Nmif=50, cooling.fraction=0.2) %>%
	continue(Nmif=50, cooling.fraction=0.1), simplify=FALSE)

which_mle <- mlist %>% 
	sapply(function(x) logmeanexp(replicate(10, logLik(pfilter(x, Np=1000))))) %>%
	which.max

mle <- mlist[[which_mle]]

R0vec <- seq(1.5, 5, by=0.2)
sumlist <- reslist <- vector('list', length(R0vec))

rwsd_arg2 <- rwsd_arg
rwsd_arg2$R0 <- 0

set.seed(101)
for (i in 1:length(R0vec)) {
	print(i)
	start_prof <- start
	start_prof[["R0"]] <- R0vec[i]
	
	tmp <- mif2(
		pomp_model,
		Nmif=50,
		start=start_prof,
		Np=1000,
		cooling.fraction.50=0.95,
		rw.sd=do.call(rw.sd, rwsd_arg2),
		transform=TRUE) %>%
		continue(Nmif=50, cooling.fraction=0.8) %>%
		continue(Nmif=50, cooling.fraction=0.6) %>%
		continue(Nmif=50, cooling.fraction=0.2) %>%
		continue(Nmif=50, cooling.fraction=0.1)
	
	pp <- logmeanexp(replicate(10, logLik(pfilter(tmp, Np=1000))))
	
	reslist[[i]] <- tmp
	sumlist[[i]] <- data.frame(
		R0=R0vec[i],
		logLik=pp
	)
}

## prediction from mle
predres <- simulate(mle, nsim=1000, as.data.frame=TRUE,
		 times=seq(0, 15, by=0.1))

save("mle", "reslist", "sumlist", "predres", file="fakedata_mortality.rda")
