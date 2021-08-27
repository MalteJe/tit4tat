# simulate is the workhorse function that executes a single run from start to finish.

simulate <- function(n = 3L,                         # number of players
							t4t = c(T, rep(FALSE, n-1L)),   # tit-for-tat-agents, defaults to only the first player playing tit-for-tat
							Eps = 100L,                     # number of learning episodes
							Eps_final = 1L,                 # final episodes with learning/exploration disabled
							t_min = 20L,                    # maximum number of time steps per episode
							alpha = 0.1,                    # learning speed parameter
							beta = NULL,                    # decay in exploration rate
							gamma = 1L,                     # discount factor
							epsilon = 0.1,                  # constant eploration rate, overwritten when beta is provided
							r_adjust = 1440/n               # determine (negative) adjustment from profits to reward (equivalent to Nash solution)
) {
	
	# initialize Q-matrices (also creates Q-matrix for tit-4-tat-algorithm, but that will not be used)
	Q_init <- rep(0, 2^(n+1))
	Qs <- rep(list(Q_init), n)
	
	# vector to map state-action combination to the location of a Q-entry in Q-matrix
	locater <- 2^(n:0)
	
	# create matrix to store actions
	col_names <- list(NULL, c("ep", "t", str_c("a_", 1:n), str_c("r_", 1:n)))
	res <- matrix(data = NA, nrow = (Eps + Eps_final) * (t_min+5L) + 100L, ncol = 2 + n*2, dimnames = col_names) # creates more rows than needed in all likelihood
	
	# determine epsilon for the entire run
	if(!is.null(beta)) {
		
		# decaying exploration rate
		epsilon <- exp(-beta*1:nrow(res))
	} else {
		
		# constant exploration rate
		epsilon <- rep(epsilon, nrow(res))
	}
	
	
	# initialize episode counter and time step counter over all episodes
	ep <- 1L
	TT <- 0L
	
	while (ep <= Eps + Eps_final) {  # while episode counter is below concluded episodes, keep simulating
		
		# initialize states: tit-for-tat plays high price, others randomize with probability 0.5
		s <- map_lgl(t4t, .f = ~ifelse(. == TRUE, TRUE, as.logical(rbinom(n = 1L, size = 1L, prob = 0.5))))
		
		# identify initial positions of state sets in Q-matrices
		q_options <- identify_options(s, locater)
		
		# enter initial states in storage (without profits)
		res[TT + 1L,] <- c(ep, 1L, s, rep(NA, n))
		
		# initialize counter of the episode and logical indicator of whether to keep playing after a time step
		t <- 2L
		keep_playing <- TRUE
		
		while(keep_playing) {
			
			# select actions for all agents, see function 'select_action'
			a <- pmap_lgl(
				.l = list(
					t4t = t4t,
					Q = Qs,
					t4t_ignore = 1:n
				),
				.f = select_action,
				q_options = q_options,
				s = s,
				n = n,
				epsilon = epsilon[TT+t]
			)
			
			# retrieve position of Q-entry that will be adjusted in response to the actions played
			q_locs <- map_dbl(a, .f = ~q_options[. + 1])
			
			# retrieve reward (i.e. profit) from played actions and adjust to get reward relative to Nash solution
			pi <- calculate_profits(a,n)
			r <- pi - r_adjust
			
			# define upcoming state set as current actions
			s2 <- a
			
			# identify positions of upcoming state sets in Q-matrices
			q_options2 <- identify_options(s2, locater)
			
			
			# determine whether to keep playing if minimum number of episodes is over
			if (t >= t_min) {
				keep_playing <- rbinom(n = 1L, size = 1L, prob = 0.7)
			}
			
			
			# update entries of Q-matrices, see function 'update_q'
			Qs <- pmap(
				.l = list(
					t4t = t4t,
					Q = Qs,
					q_loc = q_locs,
					r = r),
				.f = update_Q,
				q_options = q_options2,
				alpha = alpha,
				gamma = gamma,
				final_round = !keep_playing
			)
			
			# store results in dedicated matrix
			res[TT + t,] <- c(ep, t, a, pi)
			
			
			# move to next stage
			s <- s2
			q_options <- q_options2
			t <- t+1
		}
	
		# adjust total time steps counter
		TT <- TT + t - 1           # -1 accounts for the fact that t is incremented even after the final round was played
		
		# if the final learning episode has concluded, adjust parameters for final episodes
		if (ep == Eps) {
			alpha <- 0L
			epsilon <- rep(0L, nrow(res))
			
		}
		
		# move to next episode
		ep <- ep + 1
		
			
	}
	
	# remove superfluous NA-rows
	
	res <- res %>%
		as_tibble() %>%
		drop_na(ep)
	
	return(list(res = res,Q = Qs))
}
