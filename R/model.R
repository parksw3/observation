sir <- function(t, y, param) {
	with(as.list(c(y, param)), {
		inf <- beta * S * I/N
		
		dS <- -inf
		dI <- inf - gamma * I
		dR <- gamma * I
		
		list(c(dS, dI, dR))
	})
}
