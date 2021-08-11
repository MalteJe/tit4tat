all_high <- c(T,T,T)
all_low <- c(F,F,F)
one_cheater <- c(T,T,F)
two_cheaters <- c(T,F,F)

calculate_profits <- function(p,n) {
	if (min(p) == 1) {
		rep(2400/n, n)
	} else (
		(1440 / sum(!p)) * !p
	)
}


calculate_profits(all_high, 3)
calculate_profits(all_low, 3)
calculate_profits(one_cheater)
calculate_profits(two_cheaters)

calculate_profits(c(F,F,F,F), 4)