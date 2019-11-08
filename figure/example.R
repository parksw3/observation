library(ggplot2); theme_set(theme_bw())
library(tidyr)
library(dplyr)
library(deSolve)
library(directlabels)
source("../R/model.R")
source("../R/run.R")

scale_colour_discrete <- function(...,palette="Dark2") scale_colour_brewer(...,palette=palette)
scale_fill_discrete <- function(...,palette="Dark2") scale_fill_brewer(...,palette=palette)

rr <- run_sir(tmax=22)

rr_data <- rr %>%
	gather(key, value, -time) %>%
	mutate(
		key=factor(key, levels=c("incidence", "mortality", "prevalence"),
				   labels=c("measured upon infection", "measured upon recovery", "prevalence"))
	)

g1 <- ggplot(rr_data) +
	geom_line(aes(time, value, col=key, lty=key), lwd=1.5) +
  scale_x_continuous("Time", expand=c(0, 0), limits=c(0, 22)) +
	scale_y_log10("Number of cases", limits=c(0.5, 25000)) +
  scale_linetype_manual(values=c(1, 5, 4)) +
	theme(
		legend.title = element_blank(),
		legend.position = c(0.78, 0.9),
		legend.key.width = unit(4, "line"),
		panel.grid = element_blank(),
		panel.border = element_blank(),
		axis.line = element_line()
	)

ggsave("example.pdf", g1, width=6, height=4)
