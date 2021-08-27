library(nnet)
library(tidyverse)
library(future.apply)

source("environment.R")
source("algorithm_helpers.R")
source("Q_algorithm.R")

n_runs <- 100

run <- function(run_id,
					 n = 3,
					 t4t = c(T, rep(FALSE, n-1L)),
					 Eps = 100L,
					 Eps_final = 1L,
					 t_min = 20L,
					 epsilon = 0.25,
					 alpha = 0.25,
					 gamma = 1L) {
	simulation <- simulate(Eps = Eps, Eps_final = Eps_final, n = n, t4t = t4t, t_min, epsilon, alpha, gamma)
	
	final_ep <- filter(simulation$res, ep == Eps + Eps_final) %>%
		pivot_longer(cols = c(-ep, -t), names_to = "metric_player") %>%
		separate(col = "metric_player", into = c("metric", "player"), sep = "_") %>%
		mutate(run_id = run_id, n = n, t4t = sum(t4t))
	
	return(list(final_ep = final_ep, Q = simulation$Q))
}



# n = 2, no t4t ---------------------------------------------------------------------

plan(cluster, workers = 4)
meta_res <- future_apply(X = matrix(1:n_runs),
								 MARGIN = 1,
								 FUN = run,
								 n = 2, t4t = c(F, F),
								 future.seed = 123) %>%
	transpose()


final_ep_n2 <- meta_res$final_ep %>%
	bind_rows()

final_sum_n2 <- final_ep_n2 %>%
	filter(metric == "a") %>%
	group_by(t, n, t4t) %>%
	summarize(coop_rate = mean(value), n_actions = n())

ggplot(final_sum_n2, aes(x = t, y = coop_rate)) +
	geom_line() +
	geom_point() +
	ylim(c(0,1)) +
	theme_bw()


# n = 2, with t4t ---------------------------------------------------------

plan(cluster, workers = 4)
meta_res <- future_apply(X = matrix(1:n_runs),
								 MARGIN = 1,
								 FUN = run,
								 n = 2, t4t = c(T, F),
								 future.seed = 123) %>%
	transpose()


final_ep_n2_t4t <- meta_res$final_ep %>%
	bind_rows()

final_sum_n2_t4t <- final_ep_n2_t4t %>%
	filter(metric == "a") %>%
	group_by(t, n, t4t) %>%
	summarize(coop_rate = mean(value), n_actions = n())

ggplot(final_sum_n2_t4t, aes(x = t, y = coop_rate)) +
	geom_line() +
	geom_point() +
	ylim(c(0,1)) +
	theme_bw()



# n = 3, no t4t ---------------------------------------------------------------------

plan(cluster, workers = 4)
meta_res <- future_apply(X = matrix(1:n_runs),
								 MARGIN = 1,
								 FUN = run,
								 n = 3, t4t = c(F, F, F),
								 future.seed = 123) %>%
	transpose()


final_ep_n3 <- meta_res$final_ep %>%
	bind_rows()

final_sum_n3 <- final_ep_n3 %>%
	filter(metric == "a") %>%
	group_by(t, n, t4t) %>%
	summarize(coop_rate = mean(value), n_actions = n())

ggplot(final_sum_n3, aes(x = t, y = coop_rate)) +
	geom_line() +
	geom_point() +
	ylim(c(0,1)) +
	theme_bw()


# n = 3, with t4t ---------------------------------------------------------

plan(cluster, workers = 4)
meta_res <- future_apply(X = matrix(1:n_runs),
								 MARGIN = 1,
								 FUN = run,
								 n = 3, t4t = c(T, F, F),
								 future.seed = 123) %>%
	transpose()


final_ep_n3_t4t <- meta_res$final_ep %>%
	bind_rows()

final_sum_n3_t4t <- final_ep_n3_t4t %>%
	filter(metric == "a") %>%
	group_by(t, n, t4t) %>%
	summarize(coop_rate = mean(value), n_actions = n())

ggplot(final_sum_n3_t4t, aes(x = t, y = coop_rate)) +
	geom_line() +
	geom_point() +
	ylim(c(0,1)) +
	theme_bw()


# n = 4, no t4t ---------------------------------------------------------------------

plan(cluster, workers = 4)
meta_res <- future_apply(X = matrix(1:n_runs),
								 MARGIN = 1,
								 FUN = run,
								 n = 4, t4t = c(F, F, F, F),
								 future.seed = 123) %>%
	transpose()


final_ep_n4 <- meta_res$final_ep %>%
	bind_rows()

final_sum_n4 <- final_ep_n4 %>%
	filter(metric == "a") %>%
	group_by(t, n, t4t) %>%
	summarize(coop_rate = mean(value), n_actions = n())

ggplot(final_sum_n4, aes(x = t, y = coop_rate)) +
	geom_line() +
	geom_point() +
	ylim(c(0,1)) +
	theme_bw()


# n = 4, with t4t ---------------------------------------------------------

plan(cluster, workers = 4)
meta_res <- future_apply(X = matrix(1:n_runs),
								 MARGIN = 1,
								 FUN = run,
								 n = 4, t4t = c(T, F, F, F),
								 future.seed = 123) %>%
	transpose()


final_ep_n4_t4t <- meta_res$final_ep %>%
	bind_rows()

final_sum_n4_t4t <- final_ep_n4_t4t %>%
	filter(metric == "a") %>%
	group_by(t, n, t4t) %>%
	summarize(coop_rate = mean(value), n_actions = n())

ggplot(final_sum_n4_t4t, aes(x = t, y = coop_rate)) +
	geom_line() +
	geom_point() +
	ylim(c(0,1)) +
	theme_bw()



# collect and save results ---------------------------------------------------------

ls(pattern = "^final_ep.*") %>%
	map_dfr(.f = get) %>%
	select(n, t4t, run_id, t, player, metric, value) %>%
	write_csv(file = "final_episodes.csv")

ls(pattern = "^final_sum.*") %>%
	map_dfr(.f = get) %>%
	mutate(t4t = ifelse(t4t == 0, "no t4t agent", "with t4t agent")) %>%
	write_csv(file = "final_episodes_aggregated.csv") %>%
	filter(t <= 20) %>%
	ggplot(aes(x = t, y = coop_rate)) +
	geom_line() +
	geom_point() +
	ylim(c(0,1)) +
	theme_bw() +
	facet_grid(t4t~n)
ggsave("cooperation_rates.png")
