pomp_sir <- function(type=c("incidence", "mortality"),
					 delta.t=0.1) {
	type <- match.arg(type)
	
	rprocess <- pomp::Csnippet("
    	double Beta;
		double rate[2], trans[2];
		Beta = R0 * gamma;
						 
		rate[0] = Beta * I / N;
		rate[1] = gamma;
		reulermultinom(1, S, &rate[0], dt, &trans[0]);
		reulermultinom(1, I, &rate[1], dt, &trans[1]);
		S += -trans[0];
		I += trans[0] - trans[1];
		C += trans[0];
		M += trans[1];
	")
	
	init <- pomp::Csnippet("
		S = nearbyint(N * (1-i0));
		I = nearbyint(N * i0);
		C = 0;
		M = 0;
	")
	
	if (type == "incidence") {
		dmeas <- pomp::Csnippet("lik = dnbinom_mu(cases, theta, C*rho, give_log);")
		rmeas <- pomp::Csnippet("cases = rnbinom_mu(theta, C*rho);")
	} else {
		dmeas <- pomp::Csnippet("lik = dnbinom_mu(cases, theta, M*rho, give_log);")
		rmeas <- pomp::Csnippet("cases = rnbinom_mu(theta, M*rho);")
	}
	
	list(
		rprocess=pomp::euler.sim(rprocess, delta.t=delta.t),
		initializer=init,
		rmeasure=rmeas,
		dmeasure=dmeas,
		toEstimationScale=pomp::Csnippet("
			TR0=log(R0);
			Tgamma=log(gamma);
			Ti0=logit(i0);
			Ttheta=log(theta);
			Trho=logit(rho);
		"),
		fromEstimationScale=pomp::Csnippet("
			TR0=exp(R0);
			Tgamma=exp(gamma);
			Ti0=expit(i0);
			Ttheta=exp(theta);
			Trho=expit(rho);
		"),
		statenames=c("S", "I", "C", "M"),
		paramnames=c("R0", "gamma", "i0", "theta", "rho"),
		zeronames=c("C", "M")
	)
}
