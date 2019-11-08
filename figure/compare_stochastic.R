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
    type=factor(type, levels=c("incidence", "mortality"), 
                labels=c("measured upon infection", "measured upon recovery"))
  ) %>%
  filter(
    R0 < 2.5
  )

horizontal <- data.frame(
  hh=c(max(predict(loess(logLik~R0, data=filter(summdata, type=="measured upon infection")))), 
       max(predict(loess(logLik~R0, data=filter(summdata, type=="measured upon recovery")))))-qchisq(0.95, 1)/2,
  type=c("measured upon infection", "measured upon recovery")
)

mledata <- allmle %>% 
  lapply(function(x){
    data.frame(
      R0=coef(x)[1]
    )
  }) %>%
  bind_rows(.id="type") %>%
  mutate(
    type=factor(type, levels=c("incidence", "mortality"), 
                labels=c("measured upon infection", "measured upon recovery"))
  )

g1 <- ggplot(summdata) +
  geom_smooth(aes(R0, logLik), lty=1, se=FALSE, method="loess", span=0.5, col="black", lwd=0.5) +
  geom_point(aes(R0, logLik), shape=1, size=2) +
  geom_hline(data=horizontal, aes(yintercept=hh), lty=2) +
  geom_vline(data=mledata, aes(xintercept=R0)) +
  geom_rect(data=data.frame(type="measured upon infection"),
            aes(xmin=1.34, xmax=2.20, ymin=-Inf, ymax=Inf),
            alpha=0.1, fill="black") +
  geom_rect(data=data.frame(type="measured upon recovery"),
            aes(xmin=1.63, xmax=2.25, ymin=-Inf, ymax=Inf),
            alpha=0.1, fill="black") +
  scale_x_continuous("Basic reproductive number") +
  scale_y_continuous("Profile log-likelihood") +
  facet_wrap(~type, ncol=1, scale="free_y") +
  theme(
    panel.grid = element_blank(),
    strip.background = element_blank()
  )

ggsave("compare_stochastic.pdf", g1, width=8, height=6)
