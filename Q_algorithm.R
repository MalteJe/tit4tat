library(nnet)
library(tidyverse)

source("environment.R")


set.seed(1)

simulate <- function(n = 3,                         # number of players
							t4t = c(T, rep(FALSE, n-1)),   # tit-for-tat-agents, defaults to only the first player playing tit-for-tat
							Eps = 3,                       # number of episodes
							t_min = 20,                    # maximum number of time steps per episode
							epsilon = 0.1,                 # eploration rate
							alpha = 0.2                    # learning speed parameter
) {
	
	# initialize Q-matrices with values from continuously playing one-shot solution (also creates Q-matrix for tit-4-tat-algorithm, but that will not be used)
	Q_init <- rep(0, 2^(n+1))
	Qs <- rep(list(Q_init), n)
	
	# vector to map state-action combination to location of Q-entry in Q-matrix
	locater <- 2^(n:0)
	
	# create matrix to store actions
	col_names <- list(NULL, c("ep", "t", str_c("a", 1:n), str_c("r", 1:n)))
	res <- matrix(data = NA, nrow = Eps * (t_min+5L) + 100L, ncol = 2 + n*2, dimnames = col_names) # creates more rows than needed in all likelihood
	
	
	# determine adjustment from profits to reward (equivalent to Nash solution)
	r_adjust <- 1440 / n
	
	# initialize episode counter and time step counter over all episodes
	ep <- 1L
	TT <- 0L
	
	while (ep <= Eps) {
		
		browser()
		
		# initialize states: tit-for-tat plays high price, others randomize
		s <- map_lgl(t4t, .f = ~ifelse(. == TRUE, TRUE, as.logical(rbinom(n = 1L, size = 1L, prob = 0.5))))
		
		# identify positions of state sets in Q-matrices
		q_options <- identify_options(s, locater)
		
		# enter initial states in storage (without profits)
		res[TT + 1L,] <- c(ep, 1L, s, rep(NA, n))
		
		# initialize counter of the episode and logical indicator to keep playing
		t <- 2L
		keep_playing <- TRUE
		
		while(keep_playing) {
			
			# select action of all agents
			a <- map2_lgl(.x = t4t,
							  .y = Qs,
							  .f = select_action,
							  q_options = q_options,
							  s = s,
							  n = n,
							  epsilon = epsilon)
			
			# retrieve position of Q-entry that will be adjusted in response to the actions played
			q_locs <- map_dbl(a, .f = ~q_options[. + 1])
			
			browser()
			
			# retrieve reward (i.e. profit) from played actions and adjust to get reward relative to Nash solution
			pi <- calculate_profits(a,n)
			
			r <- pi - r_adjust
			
			
			# define upcoming state set as current actions
			s2 <- a
			
			# identify positions of upcoming state sets in Q-matrices
			q_options2 <- identify_options(s2, locater)
			
			
			# determine whether to keep playing
			if (t >= 20) {
				
				browser()
				keep_playing <- rbinom(n = 1, size = 1, prob = 0.7)
			}
			
			
			# update entries of Q-matrices
			Qs <- pmap(
				.l = list(
					t4t = t4t,
					Q = Qs,
					q_loc = q_locs,
					r = r),
				.f = update_Q,
				q_options = q_options2,
				alpha = alpha,
				final_round = !keep_playing
			)
			
			# store results
			res[TT + t,] <- c(ep, t, a, pi)
			
			
			# move to next stage
			s <- s2
			q_options <- q_options2
			t <- t+1
		}
		
		browser()
	
		# adjust time step counter
		TT <- TT + t - 1           # -1 accounts for the fact that t is incremented even after final round was played
		
		# move to next episode
		ep <- ep + 1
			
	}
	
	return(res)
}

res <- simulate(Eps = 1000)


res %>%
	as_tibble() %>%
	filter(ep == 100, t < 20) %>%
	pivot_longer(cols = contains("r"), names_to = "r") %>%
	pivot_longer(cols = contains("a"), names_to = "a")
	ggplot(aes(x = t, ))

tail(res, 25)

head(res, 22)




