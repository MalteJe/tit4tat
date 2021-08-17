calculate_profits <- function(a,n) {
	if (min(a) == 1) {
		rep(2400/n, n)
	} else (
		(1440 / sum(!a)) * !a
	)
}


calculate_profits(c(T,T,T), 3)
calculate_profits(c(F,F,F), 3)
calculate_profits(c(T,T,F), 3)
calculate_profits(c(T,F,F), 3)

calculate_profits(c(T,T,T,T), 4)




# select action: function to select action based on state set (both tit-for-tat and reinforcement learning agents)
select_action <- function(t4t, q, s, q_options, n, epsilon) {
	
	if(t4t == TRUE) {
		
		# tit-for-tat strategy
		prob <- sum(s[-1])/(n-1)  # probability based on previous prices
		a <- rbinom(n = 1, size = 1, prob = prob) # draw from binomial
	} else {
		
		# Q-learning agents
		
		explore <- rbinom(n = 1, size = 1, prob = epsilon) #determine whether to explore or to exploit
		
		if (explore) {
			a <- rbinom(n = 1, size = 1, prob = 0.5) # low or high price with 50% probability each
		} else {
			a <- nnet::which.is.max(q[q_options]) - 1L # choose action associated with a higher Q-value
		}
	}
	
	
	return(as.logical(a))
}

identify_options <- function(s, locater) {
	q_loc_low <- sum(locater[-1] * s) + 1
	return(c(q_loc_low, q_loc_low + locater[1]))
}


update_Q <- function(t4t, Q, q_loc, q_options, r, alpha, final_round) {
	if(t4t) {
		return(NULL)
	} else {
		
		if(final_round) {
			browser()
		}
		
		s2_value <- ifelse(final_round, 0L, max(Q[q_options]))
		
		
		
		Q[q_loc] <- Q[q_loc] + alpha * (r + s2_value - Q[q_loc])
		return(Q)
	}
}


update_Q_last_step <- function(t4t, Q, q_loc, r, alpha) {
	if(t4t) {
		return(NULL)
	} else {
		Q[q_loc] <- Q[q_loc] + alpha * (r - Q[q_loc]) # no upcoming state --> value = 0
		return(Q)
	}
}