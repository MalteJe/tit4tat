# calculate_profits takes played actions (i.e. prices) as inputs and returns the profits for all players
calculate_profits <- function(a,       # logical vector of played actions (TRUE represents high price)
										n        # number of players
										) {
	if (min(a) == 1) {
		# collusive scenario: split high profits (2400) among all players
		rep(2400/n, n)
	} else (
		# competitive scenario: split low profits (1440) among players that played the low price
		(1440 / sum(!a)) * !a
	)
}

# examples of profits 
calculate_profits(c(T,T,T), 3)
calculate_profits(c(F,F,F), 3)
calculate_profits(c(T,T,F), 3)
calculate_profits(c(T,F,F), 3)
calculate_profits(c(T,T,T,T), 4)

