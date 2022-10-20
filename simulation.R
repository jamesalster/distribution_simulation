
#### Simulation ####

#designed to model how many individuals are served in a period of time, given:
#multiple distributions
#people coming back more than once
#population turnover

#Basic time unit = distribution
#Codes: 0  = not present and not come, 3 = not present and left
#2 = came to distro, 1 = present but not at distro

#N = starting population
#t = number of tickets per distro (vectorised to allow changing values over time)
#d = number of distro (vectorised to allow changing values over time)
#n_arrive = number arriving between distros (vectorised to allow changing values over time)
#p_leave = probability of leaving between distros (vectorised to allow changing values over time)
#if p_leave = n_arrive / N, the population remains roughly constant
#p_increased = increased relative probability of coming to a distro, if have already come to a distro

library(tidyverse)

#### Global Variables ####

#max population (arbitrary, just set to very large)
#smaller = faster. But the simulation throws a sample() error if it can't add more people
max_N <- 20000L

#### Functions as part of simulation ####

#function to work out people leaving
leaving <- function(x, p_leave) {
  is_present <- (x == 1L | x == 2L)
  not_leaving <- (runif(length(x)) > p_leave)
  not_yet_arrived <- (x == 0L)
  x <- if_else(not_yet_arrived | (is_present & not_leaving), x, 3L) 
  x[x == 2L] <- 1L #make everyone present but not come ready for the next distribution
  x
}

#function to work out number of people arriving
arriving <- function(x, n_arrive) {
  not_yet_present <- which(x == 0L) #from pool of those not yet arrived
  x[not_yet_present[1:n_arrive]] <- 1L #assign next n_arrive as present
  x
}

#function to work out distro attendance
distro <- function(x, t, prob_vector) {
  present_ids <- which((x == 1L | x == 2L)) #only those present can come to distro
  x[sample(present_ids, t, prob = prob_vector[present_ids])] <- 2L
  x
}

#function to calculate a new day
iterate <- function(simul, distro_number, p_leave, n_arrive, t, p_increased) {
  #calculate probabiltiy vectors for distro()
  n_previous_distros <- rowSums(simul == 2L, na.rm = T)  #this is quite slow
  any_previous_distros <- if_else(n_previous_distros < 1L, 1, p_increased) 
  
  previous_day <- simul[,distro_number - 1L]
  leavers <- leaving(previous_day, p_leave)
  arrivals <- arriving(leavers, n_arrive)
  distro_population <- distro(arrivals, t, prob_vector = any_previous_distros) #or n_previous_distros
  simul[,distro_number] <- distro_population
  simul
}

#initialise a simulation instance
init_simul <- function(d, N) {
  simul <- matrix(nrow = max_N, ncol = d)
  initial_state <- c(rep(1L, N), rep(0L, max_N - N))
  simul[,1L] <- initial_state
  simul
}

#run simulation, iterating over the vectors of parameters to calculate values for a specific day
run_simulation <- function(simul, p_leave, n_arrive, t, d, p_increased) {
  for (i in 2:d) {
    simul <- iterate(simul,
                     distro_number = i,
                     p_leave = p_leave[i],
                     n_arrive = n_arrive[i],
                     t = t[i],
                     p_increased)
  }
  simul
}

#function to convert 3D arrays to lists of 2D arrays
array2list <- function(array) {
  n_dims <- dim(array)[3]
  map(1:n_dims, ~array[,,.])
}

#### 1-off Simulation #####

d <- 30
N <- 600
t <- rep(100, d)
n_arrive <- rep(30, d)
p_increased <- 1
p_leave <- n_arrive / N

simul <- init_simul(d = d, N = N) %>%
  run_simulation(t = t,
                  n_arrive = n_arrive,
                  p_increased = p_increased,
                 d = d,
                  p_leave = p_leave)

#### Visualise ####

#show visualisation of complete simulation instance
ever_present <- rowSums(simul != 0L) > 0L

pal1 <- c("#BBBBBB", "#D34A36", "#1A6172", "#D3B436")

as_tibble(simul, name_repair = "universal") %>%
  filter(ever_present) %>%
  #slice_sample(n =20) %>%
  mutate(personid = row_number()) %>%
  pivot_longer(-personid, names_to = "distro_day", values_to = "status") %>%
  mutate(distro_day = as.integer(str_remove(distro_day, "V")),
         status = fct_recode(factor(status), "not arrived" = "0",
                             "present not at distro" = "1",
                             "at distro" = "2",
                             "left" = "3")) %>%
  ggplot(aes(x = distro_day, y = personid, fill = status)) +
  geom_raster() +
  scale_y_continuous(trans = scales::reverse_trans()) +
  labs(title = "Simulation instance, complete",
       substitle = "pop = 600, tickets = 100, 30 arrivals per distro") +
  scale_fill_manual(values = pal1) +
  theme_minimal()

ggsave("graphs/complete_simulation_instance.svg", device = "svg")

#show number of people present by day
#currently just a random walk
ggplot(data = NULL, aes(y = colSums(simul == 1L | simul == 2L), x = 1:d)) +
  geom_line(size = 1) +
  labs(y = "Population", x = "Distribution",
       title = "People Present by Distribution")

ggsave("graphs/people_present_by_distribution.svg", device = "svg")

#number of times each person has come to a distro, as distribution
ever_present <- rowSums(simul != 0L) > 0L
times_come <- rowSums(simul == 2L)[ever_present] #exclude the people who 'never arrived'

ggplot(data = NULL, aes(x = times_come)) +
  geom_histogram(binwidth = 1, colour = "black") +
  labs(y = "number_of_people",
       title = "Distribution of number of times people came to a distribution")

mean(times_come)
#number of people actually served
sum(rowSums(simul == 2L) > 0)

#### effect of parameters on simulation outcome ####

##use mulitple simulations
parameter_grid <- expand_grid(n_tickets = c(60, 80, 100),
                              N = c(400, 700, 1000),
                              n_arrive = c(10, 30, 50),
                              n_distros = 30,
                              p_increased = c(1, 3)) %>%
  mutate(p_leave = n_arrive / N)

n_simuls <- 10

simulations <- parameter_grid %>%
  rowwise() %>%
  mutate(initial_simuls = list(init_simul(n_distros, N)),
         across(c(n_tickets, p_leave, n_arrive), ~list(rep(., n_distros))),
         #with multiple runs
         simul = list(replicate(n_simuls, run_simulation(initial_simuls, p_leave, n_arrive, n_tickets, n_distros, p_increased))))

tidied_simuls <- simulations %>%
  transmute(across(c(n_tickets, p_leave, n_arrive), ~ pluck(., 1)),
            n_distros, N, p_increased,
            simul = list(array2list(simul))) %>%
  unnest(simul)

#plot number of people vs number of tickets
pal <- c("#DF9A3E", "#9B492B", "#007F74")

tidied_simuls %>%
  rowwise() %>%
  mutate(n_people = sum(rowSums(simul == 2L) > 0)) %>%
  select(-simul) %>%
  group_by(n_tickets, p_leave, n_arrive, n_distros, N, p_increased) %>%
  summarise(mean_people = mean(n_people),
            sd_people = sd(n_people), .groups = "drop") %>%
  rename(population = N, increased_rel_prob = p_increased) %>%
  ggplot(aes(x = n_tickets, y = mean_people, colour = factor(n_arrive))) +
  geom_point() +
  geom_line() +
  geom_linerange(aes(ymin = mean_people - sd_people, ymax = mean_people + sd_people)) +
  scale_colour_manual(values = pal, name = "arrivals per distro") +
  facet_grid(increased_rel_prob~population, labeller = label_both) +
  labs(y = "people",
       title = "People served as a function of:\ntickets, population, arrivals, relative probability of returning to distro",
       subtitle = "Assuming roughly constant population. 30 distros.\n10 simulations per group (standard deviation shown).")

ggsave("graphs/people_served_multiple_simulations.svg", device = "svg")

#get number of times people come to distributions
times_at_distro <- function(simul) {
  ever_present <- rowSums(simul != 0L) > 0L
  rowSums(simul == 2L)[ever_present]
}

times_come <- tidied_simuls %>%
  rowwise() %>%
  mutate(times_come = list(times_at_distro(simul)))

times_come %>%
  select(-simul) %>%
  filter(n_tickets == 80) %>%
  group_by(n_tickets, p_leave, n_arrive, n_distros, N, p_increased) %>%
  slice_sample(n = 1) %>%
  ungroup() %>%
  unnest(times_come) %>%
  rename(population = N, increased_rel_prob = p_increased) %>%
  ggplot(aes(x = times_come, fill = factor(n_arrive))) +
  geom_histogram(binwidth = 1, position = "dodge") +
  facet_grid(increased_rel_prob~population, labeller = label_both) +
  scale_x_continuous(limits = c(NA, 7), breaks = scales::breaks_width(1)) +
  scale_fill_manual(values = pal, name = "arrivals per distro") +
  labs(title = "Number of times people present come to distro, 30 distros",
       subtitle = "1 simulation, 80 tickets per distro",
       y = "people")

ggsave("graphs/times_come_multiple_simulations.svg", device = "svg")
