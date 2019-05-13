run_sir <- function(param=c(R0=2, gamma=1, N=1e5),
					yini=c(S=1e5-10, I=10, R=0),
					tmax=20,
					tlength=0.1) {
	param[["beta"]] <- param[["R0"]] * param[["gamma"]]
	
	tvec <- seq(0, tmax, by=tlength)
	
	dd <- ode(yini, tvec, sir, param)
	
	data.frame(
		time=tail(tvec, -1),
		incidence=-diff(dd[,"S"]),
		prevalence=tail(dd[,"I"],-1),
		mortality=diff(dd[,"R"])
	)
}
