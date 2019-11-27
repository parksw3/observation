library(ggplot2); theme_set(theme_bw())
library(tidyr)
library(dplyr)
library(gridExtra)
library(deSolve)
library(directlabels)
source("../R/model.R")
source("../R/run.R")

scale_colour_discrete <- function(...,palette="Dark2") scale_colour_brewer(...,palette=palette)
scale_fill_discrete <- function(...,palette="Dark2") scale_fill_brewer(...,palette=palette)

R0vec <- seq(1.1, 5, by=0.1)
fixed_inf_list <- vector('list', length(R0vec))

for (i in 1:length(R0vec)) {
  print(i)
  newpar <- c(
    R0=R0vec[i],
    gamma=1,
    N=1e5,
    rho=1
  )
  
  rr <- run_sir(param=newpar, tmax=200, tlength=0.1)
  
  fixed_inf_list[[i]] <- data.frame(
    peakdiff=1-max(rr$mortality)/max(rr$incidence),
    timediff=(which.max(rr$mortality)-which.max(rr$incidence))*0.1,
    R0=R0vec[i]
  )
}

fixed_inf_data <- fixed_inf_list %>%
  bind_rows

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
  ggtitle("A") +
	theme(
		legend.title = element_blank(),
		legend.position = c(0.78, 0.9),
		legend.key.width = unit(4, "line"),
		panel.grid = element_blank(),
		panel.border = element_blank(),
		axis.line = element_line()
	)

g2 <- ggplot(fixed_inf_data) +
  geom_smooth(aes(R0, peakdiff), lwd=1, se=FALSE, col=1) +
  geom_point(aes(R0, peakdiff), pch=1, size=3) +
  ggtitle("B") +
  scale_x_continuous("Basic reproduction number", limits=c(1, 5.1), expand=c(0, 0)) +
  scale_y_continuous("Relative difference in the epidemic peak", limits=c(0, 0.5), expand=c(0, 0)) +
  theme(
    panel.grid = element_blank(),
    panel.border = element_blank(),
    axis.line = element_line()
  )

g3 <- ggplot(fixed_inf_data) +
  geom_smooth(aes(R0, timediff), lwd=1, se=FALSE, col=1) +
  geom_point(aes(R0, timediff), pch=1, size=3) +
  ggtitle("c") +
  scale_x_continuous("Basic reproduction number", limits=c(1, 5.1), expand=c(0, 0)) +
  scale_y_continuous("Difference in the peak timing (unit time)") +
  theme(
    panel.grid = element_blank(),
    panel.border = element_blank(),
    axis.line = element_line()
  )

gfinal <- arrangeGrob(g1, g2, g3, layout_matrix = matrix(c(1, 1, 2, 3), 2, 2),
                      widths = c(2, 1))

ggsave("example.pdf", gfinal, width=9, height=6)
