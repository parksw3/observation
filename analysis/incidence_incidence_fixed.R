library(deSolve)
library(bbmle)
source("../R/model.R")
source("../R/run.R")
source("../R/fit.R")

load("../data/incidence.rda")

reslist <- vector('list', length(incidence_data))

for (i in 1:length(incidence_data)) {
	print(i)
	data <- incidence_data[[i]]
	
	fixed <- c(log.gamma=0)
	
	ff <- fit_sir(data, fixed=fixed)
	
	reslist[[i]] <- ff
}

save("reslist", file="incidence_incidence_fixed.rda")
