library(dplyr)
library(ggplot2); theme_set(theme_bw())
library(gridExtra)
library(grid)

scale_colour_discrete <- function(...,palette="Dark2") scale_colour_brewer(...,palette=palette)
scale_fill_discrete <- function(...,palette="Dark2") scale_fill_brewer(...,palette=palette)

dir <- "../analysis/"
files <- c("incidence_incidence.rda",
		   "incidence_mortality.rda",
		   "mortality_incidence.rda",
		   "mortality_mortality.rda",
		   "incidence_incidence_fixed.rda",
		   "incidence_mortality_fixed.rda",
		   "mortality_incidence_fixed.rda",
		   "mortality_mortality_fixed.rda")

data <- rep(c("infection", "infection", "recovery", "recovery"), 2)
fit <- rep(c("infection", "recovery", "infection", "recovery"), 2)
inf <- rep(c("estimated", "fixed"), each=4)

comblist <- summlist <- vector('list', 8)

for (i in 1:8) {
	load(paste0(dir, files[i]))
	
	comblist[[i]] <- reslist
}

for (i in 1:8) {
	summlist[[i]] <- lapply(comblist[[i]], function(x) {
		data.frame(
			data=data[i],
			fit=fit[i],
			inf=inf[i],
			estimate=x[[1]],
			growth=(exp(coef(x[[3]])[[1]])-1) * exp(coef(x[[3]])[[2]]),
			period=1/exp(coef(x[[3]])[[2]]),
			rho=plogis(coef(x[[3]])[[5]]),
			coverage=(x[[2]][1] < 2 && 2 < x[[2]][2])
		)
	}) %>%
		bind_rows
}

summdata <- summlist %>%
	bind_rows

g1 <- ggplot(filter(summdata, inf=="estimated")) +
	geom_boxplot(aes(x=data, y=estimate, fill=fit), width=0.5) +
	geom_hline(yintercept=2, lty=2) +
	scale_x_discrete("Simulated timing") +
	scale_y_continuous("Basic reproductive number") +
	theme(
		legend.position="none"
	)

coverdata <- summdata %>%
	group_by(data, fit, inf) %>%
	summarize(
		coverage=mean(coverage)
	)

g2 <- ggplot(filter(coverdata, inf=="estimated")) +
	geom_rect(xmin=-Inf, xmax=Inf, ymin=binom.test(95, 100)[[4]][1], ymax=binom.test(95, 100)[[4]][2], alpha=0.1) +
	geom_hline(yintercept=0.95, lty=2) +
	geom_point(aes(x=data, y=coverage, col=fit, group=fit), size=5, position = position_dodge(width = 0.5)) +
	scale_x_discrete("Simulated timing") +
	scale_y_continuous("Coverage probability", limits=c(0, 1)) +
	scale_colour_discrete("Fitted timing") +
	theme(
		legend.position=c(0.76, 0.2)
	)

g3 <- g1 %+% filter(summdata, inf=="fixed")

g4 <- g2 %+% filter(coverdata, inf=="fixed") +
	theme(legend.position="none")

g5 <- grid.arrange(g1, g2, nrow=1, top=textGrob("A) Estimated infectious period", hjust=1.7))
g6 <- grid.arrange(g3, g4, nrow=1, top=textGrob("B) Fixed infectious period", hjust=2))

gtot <- arrangeGrob(g5, g6, nrow=2)

ggsave("compare_deterministic.pdf", gtot, width=8, height=6)
