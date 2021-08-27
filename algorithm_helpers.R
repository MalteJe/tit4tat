

# select_action  selects a single agent's action based on the current state set for both tit-for-tat and reinforcement learning algorithms
select_action <- function(t4t,          # logical: does the agent play tit-for-tat?
								  Q,            # Q-matrix of the agent
								  t4t_ignore,   # only relevant for tit-4-tat: which (own) action is ignored when calculating the probability of playing the high price
								  s,            # logical vector representing current state set 
								  q_options,    # vector to index which entries of the Q-matrix are playable
								  n,            # number of agents
								  epsilon       # exploration rate
								  ) {
	
	if(t4t == TRUE) {
		
		# tit-for-tat strategy
		prob <- sum(s[-t4t_ignore])/(n-1L)  # calculate probability based on previous prices of other players (excludes own action)
		a <- rbinom(n = 1L, size = 1L, prob = prob) # draw from binomial with calculated probability
	
		} else {
		
		# Q-learning agents
		explore <- rbinom(n = 1L, size = 1L, prob = epsilon) # determine whether to explore or to exploit based on exploration rate epsilon
		
		if (explore) {
			a <- rbinom(n = 1, size = 1, prob = 0.5)   # Exploration: low or high price with 50% probability each
		} else {
			a <- nnet::which.is.max(Q[q_options]) - 1L # Exploitation: out of available actions, choose the one associated with a higher Q-value
		}
	}
	
	
	return(as.logical(a))  # return action as logical (TRUE: high price, FALSE: low price)
}


# identify_options maps the available actions to the space set to identify the associated entries in the Q-matrix for all players.
identify_options <- function(s,       # logical vector representing current state set 
									  locater  # pre-specified locator to determine position in Q-matrix
									  ) {
	
	q_loc_low <- sum(locater[-1] * s) + 1          # locate position of low price option
	return(c(q_loc_low, q_loc_low + locater[1]))   # calculate position of high price option and return both
}

# update_Q updates the values in the Q-matrix of an agent based on the obtained reward and the upcoming state
update_Q <- function(t4t,         # logical: does the agent play tit-for-tat?
							Q,           # Q-matrix of the agent
							q_loc,       # position of selected action in Q-matrix
							q_options,   # position of upcoming available options in Q-matrix
							r,           # obtained reward
							alpha,       # learning rate parameter
							gamma,       # discount factor
							final_round  # logical indicating whether this is the final round
							) {
	
	
	if(t4t) {
		
		# if the agent plays tit-for-tat, don't do anything
		return(NULL)
	} else {
		
		# else, determine the value of the upcoming states. In the final round, it's 0. Before that, it's the maximum of both available options.
		s2_value <- ifelse(final_round, 0L, max(Q[q_options]))
		
		# update entry in Q-entry and return entire matrix
		Q[q_loc] <- Q[q_loc] + alpha * (r + gamma * s2_value - Q[q_loc])
		return(Q)
	}
}
