library(deSolve)
source("../R/model.R")
source("../R/run.R")

rr <- run_sir()

set.seed(101)
mortality_data <- replicate(100, 
	data.frame(
		time=rr$time,
		incidence=rnbinom(200, mu=rr$mortality, size=10)
	),
	simplify=FALSE
)

save("mortality_data", file="mortality.rda")
