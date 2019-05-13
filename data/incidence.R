library(deSolve)
source("../R/model.R")
source("../R/run.R")

rr <- run_sir()

set.seed(101)
incidence_data <- replicate(100, 
	data.frame(
		time=rr$time,
		incidence=rnbinom(200, mu=rr$incidence, size=10)
	),
	simplify=FALSE
)

save("incidence_data", file="incidence.rda")
