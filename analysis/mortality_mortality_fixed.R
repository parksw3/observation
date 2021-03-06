library(deSolve)
library(bbmle)
source("../R/model.R")
source("../R/run.R")
source("../R/fit.R")

load("../data/mortality.rda")

reslist <- vector('list', length(mortality_data))

for (i in 1:length(mortality_data)) {
	print(i)
	data <- mortality_data[[i]]
	
	fixed <- c(log.gamma=0)
	
	ff <- fit_sir(data, type="mortality", fixed=fixed)
	
	reslist[[i]] <- ff
}

save("reslist", file="mortality_mortality_fixed.rda")
