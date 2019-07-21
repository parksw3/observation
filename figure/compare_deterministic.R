library(dplyr)
library(ggplot2); theme_set(theme_bw())
library(gridExtra)

scale_colour_discrete <- function(...,palette="Dark2") scale_colour_brewer(...,palette=palette)
scale_fill_discrete <- function(...,palette="Dark2") scale_fill_brewer(...,palette=palette)

dir <- "../analysis/"
files <- c("incidence_incidence.rda",
		   "incidence_mortality.rda",
		   "mortality_incidence.rda",
		   "mortality_mortality.rda")

data <- c("infection", "infection", "recovery", "recovery")
fit <- c("infection", "recovery", "infection", "recovery")

comblist <- summlist <- vector('list', 4)

for (i in 1:4) {
	load(paste0(dir, files[i]))
	
	comblist[[i]] <- reslist
}

for (i in 1:4) {
	summlist[[i]] <- lapply(comblist[[i]], function(x) {
		data.frame(
			data=data[i],
			fit=fit[i],
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

g1 <- ggplot(summdata) +
	geom_boxplot(aes(x=data, y=estimate, fill=fit), width=0.5) +
	geom_hline(yintercept=2, lty=2) +
	scale_x_discrete("Simulations") +
	scale_y_continuous("Basic reproductive number") +
	theme(
		legend.position="none"
	)

g2 <- ggplot(summdata) +
	geom_boxplot(aes(x=data, y=period, fill=fit), width=0.5) +
	geom_hline(yintercept=1, lty=2) +
	scale_x_discrete("Simulations") +
	scale_y_continuous("Mean infectious period") +
	theme(
		legend.position="none"
	)

coverdata <- summdata %>%
	group_by(data, fit) %>%
	summarize(
		coverage=mean(coverage)
	)

g3 <- ggplot(coverdata) +
	geom_rect(xmin=-Inf, xmax=Inf, ymin=binom.test(95, 100)[[4]][1], ymax=binom.test(95, 100)[[4]][2], alpha=0.1) +
	geom_hline(yintercept=0.95, lty=2) +
	geom_point(aes(x=data, y=coverage, col=fit, group=fit), size=5, position = position_dodge(width = 0.5)) +
	scale_x_discrete("Simulations") +
	scale_y_continuous("Coverage (R0)", limits=c(0, 1)) +
	scale_colour_discrete("Fitted") +
	theme(
		legend.position=c(0.76, 0.2)
	)

gtot <- arrangeGrob(g1, g2, g3, nrow=1)

ggsave("compare_deterministic.pdf", gtot, width=8, height=3)
