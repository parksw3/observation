library(dplyr)
library(ggplot2); theme_set(theme_bw())
library(gridExtra)

scale_colour_discrete <- function(...,palette="Dark2") scale_colour_brewer(...,palette=palette)
scale_fill_discrete <- function(...,palette="Dark2") scale_fill_brewer(...,palette=palette)

load("../data/stochastic.rda")

allpred <- allres <- allsum <- allmle <- list()

load("../analysis/fakedata_incidence.rda")

allpred$incidence <- predres
allres$incidence <- reslist
allsum$incidence <- sumlist
allmle$incidence <- mle

load("../analysis/fakedata_mortality.rda")

allpred$mortality <- predres
allres$mortality <- reslist
allsum$mortality <- sumlist
allmle$mortality <- mle

summdata <- allsum %>%
	lapply(bind_rows) %>%
	bind_rows(.id="type") %>%
	mutate(
		type=factor(type, levels=c("incidence", "mortality"), labels=c("infection", "recovery"))
	) %>%
	filter(
		R0 < 2.5
	)

horizontal <- data.frame(
	hh=c(max(predict(loess(logLik~R0, data=filter(summdata, type=="infection")))), 
		 max(predict(loess(logLik~R0, data=filter(summdata, type=="recovery")))))-qchisq(0.95, 1)/2,
	type=c("infection", "recovery")
)



g1 <- ggplot(summdata) +
	geom_smooth(aes(R0, logLik), lty=1, se=FALSE, method="loess", span=0.5, col="black", lwd=0.5) +
	geom_point(aes(R0, logLik), shape=1, size=2) +
	geom_hline(data=horizontal, aes(yintercept=hh), lty=2) +
	geom_vline(data=data.frame(type="infection"), aes(xintercept=1.34), lty=2) +
	geom_vline(data=data.frame(type="infection"), aes(xintercept=2.20), lty=2) +
	geom_vline(data=data.frame(type="recovery"), aes(xintercept=1.64), lty=2) +
	geom_vline(data=data.frame(type="recovery"), aes(xintercept=2.25), lty=2) +
	scale_x_continuous("Basic reproductive number") +
	scale_y_continuous("Profile log-likelihood") +
	facet_wrap(~type, ncol=1, scale="free_y") +
	theme(
		panel.grid = element_blank(),
		strip.background = element_blank()
	)

preddata <- allpred %>%
	lapply(function(x) {
		x %>% 
			group_by(time) %>%
			summarize(
				mean=mean(cases),
				lwr=quantile(cases, 0.025),
				upr=quantile(cases, 0.975)
			)
	}) %>%
	bind_rows(.id="type") %>%
	mutate(
		type=factor(type, levels=c("incidence", "mortality"), labels=c(" infection", " recovery"))
	)

g2 <- ggplot(preddata) +
	geom_ribbon(aes(time, ymin=lwr, ymax=upr, fill=type, col=type), alpha=0.3, lty=2) +
	geom_line(aes(time, mean, col=type)) +
	geom_point(data=fakedata, aes(time, cases), pch=1) +
	scale_x_continuous("Time", expand=c(0, 0)) +
	scale_y_continuous("Cases") +
	theme(
		legend.title = element_blank(),
		legend.position=c(0.15, 0.9)
	)

gtot <- arrangeGrob(g1, g2, nrow=1)

ggsave("compare_stochastic.pdf", gtot, width=8, height=4)
