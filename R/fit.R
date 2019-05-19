nll_sir <- function(param=c(log.R0=log(2), log.gamma=log(1), logit.I0=qlogis(1e-4), log.size=log(10),
							logit.rho=qlogis(0.5)),
				    data,
 				    N=1e5,
				    type=c("incidence", "mortality"),
					tlength=0.1) {
	type <- match.arg(type)
	
	tmax <- max(data$time)
	
	run_param <- with(as.list(param), 
		c(R0=exp(log.R0), gamma=exp(log.gamma), N=N, rho=plogis(logit.rho))
	)
	
	run_yini <- with(as.list(param),
		c(S=N * (1 - plogis(logit.I0)), I=N * plogis(logit.I0), R=0)
	)
	
	rr <- run_sir(run_param, run_yini, tlength=tlength, tmax=tmax)
	
	size <- exp(param[["log.size"]])
	
	yhat <- switch(type,
				   incidence=rr$incidence,
				   mortality=rr$mortality
	)
	
	nll <- -sum(dnbinom(data$incidence, mu=yhat, size=size, log=TRUE))
	
	nll
}

bbmle::parnames(nll_sir) <- c("log.R0", "log.gamma", "logit.I0", "log.size", "logit.rho")

fit_sir <- function(data,
					start=c(log.R0=log(2), log.gamma=log(1), logit.I0=qlogis(1e-4), log.size=log(10),
							logit.rho=qlogis(0.5)),
					N=1e5,
					type=c("incidence", "mortality"),
					tlength=0.1) {
	type <- match.arg(type)
	
	bb <- bbmle::mle2(
		nll_sir,
		start,
		"Nelder-Mead",
		data=list(data=data, tlength=tlength, type=type, N=N),
		control=list(maxit=5000)
	)
	
	cc <- bbmle::confint(bb, 1)
	
	while (class(cc)=="mle2") {
		new_start <- coef(cc)
		names(new_start) <- names(start)
		
		bb <- bbmle::mle2(
			nll_sir,
			new_start,
			"Nelder-Mead",
			data=list(data=data, tlength=tlength, type=type, N=N),
			control=list(maxit=5000)
		)		
		
		cc <- bbmle::confint(bb, 1)
	}
	
	list(
		R0=exp(bbmle::coef(bb)[[1]]),
		R0.confint=exp(cc),
		fit=bb
	)
}
