library(tidyverse)
library(dslabs)
library(gridExtra)
take_poll(25)

p <- 0.45
N <- 1000
B <- 10000
x_hat <- replicate(B, {
  x <- sample(c(0,1), size = N, replace = TRUE, prob = c(1-p, p))
  mean(x)
})

p1 <- data.frame(x_hat = x_hat) %>%
  ggplot(aes(x_hat)) +
  geom_histogram(binwidth = 0.005, color="black")

p2 <- data.frame(x_hat = x_hat) %>%
  ggplot(aes(sample = x_hat)) +
  stat_qq(dparams = list(mean = mean(x_hat), sd = sd(x_hat))) +
  geom_abline() + ylab("x_hat") + xlab("Theoretical normal")

grid.arrange(p1, p2, nrow = 1)

#------------------------------------------------------------------------------

# Write a function called `take_sample` that takes `p` and `N` as arguements and returns the average value of a randomly sampled population.
take_sample <- function(p, N) {
  x <- sample(c(1,0), N, replace = TRUE, prob = c(p, 1-p))
  mean(x)
}

# Define `p` as the proportion of Democrats in the population being polled
p <- 0.45

# Define `N` as the number of people polled
N <- 100

# The variable `B` specifies the number of times we want the sample to be replicated
B <- 10000

# Use the `set.seed` function to make sure your answer matches the expected result after random sampling
set.seed(1)

# Create an objected called `errors` that replicates subtracting the result of the `take_sample` function from `p` for `B` replications
errors <- replicate(B, {
  p - take_sample(p, N)
})

# Calculate the mean of the errors. Print this value to the console.
mean(errors)
hist(errors)

# Calculate the standard deviation of `errors`
sqrt(mean(errors^2))

# Calculate the standard error
sqrt(p * (1-p) / N)

#------------------------------------------------------------------------------

data("polls_us_election_2016")
str(polls_us_election_2016)
polls <- polls_us_election_2016 %>% 
  filter(enddate >= "2016-10-31" & state == "U.S.")
nrow(polls)
N <- polls$samplesize[1]
X_hat <- polls$rawpoll_clinton[1] / 100
se_hat <- sqrt(X_hat * (1 - X_hat) / N)
z <- qnorm (1 - ((1 - 0.95) / 2))
ci <- c(X_hat - z*se_hat, X_hat + z*se_hat)

pollster_results <- polls %>%
  mutate(X_hat = rawpoll_clinton / 100, 
         se_hat = sqrt(X_hat * (1 - X_hat) / samplesize),
         lower = X_hat - qnorm(0.975) * se_hat,
         upper = X_hat + qnorm(0.975) * se_hat
        ) %>%
  select(pollster, enddate, X_hat, se_hat, lower, upper)

p <- 0.482
avg_hit <- pollster_results %>%
  mutate(hit = (p >= lower) & (p <= upper)) %>%
  summarise (avg = mean(hit))

#--------------

polls <- polls_us_election_2016 %>% 
  filter(enddate >= "2016-10-31" & state == "U.S.") %>%
  mutate(d_hat = (rawpoll_clinton - rawpoll_trump) / 100)

pollster_results <- polls %>%
  mutate(X_hat = (d_hat + 1) / 2, 
         se_hat = 2 * sqrt(X_hat * (1 - X_hat) / samplesize),
         lower = d_hat - qnorm(0.975) * se_hat,
         upper = d_hat + qnorm(0.975) * se_hat
        ) %>%
  select(pollster, enddate, d_hat, lower, upper)

d <- 0.021
avg_hit <- pollster_results %>%
  mutate(hit = (d >= lower) & (d <= upper)) %>%
  summarise (avg = mean(hit))

polls <- polls %>%
  mutate(error = d_hat - d)

polls %>% ggplot(aes(x = pollster, y = error)) +
  geom_point() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

polls %>% group_by(pollster) %>%
  filter(n() >= 5) %>%
  ggplot(aes(x = pollster, y = error)) +
  geom_point() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

#------------------------------------------------------------------------------

d <- 0.039
Ns <- c(1298, 533, 1342, 897, 774, 254, 812, 324, 1291, 1056, 2172, 516)
p <- (d + 1) / 2

confidence <- function(N) {
  x <- sample(c(0,1), size=N, replace=TRUE, prob=c(1-p, p))
  x_hat <- mean(x)
  se_hat <- sqrt(x_hat * (1 - x_hat) / N)
  2 * c(x_hat, x_hat - 2*se_hat, x_hat + 2*se_hat) - 1
}

confidence_intervals <- sapply(Ns, confidence)

polls <- data.frame(poll = 1:ncol(confidence_intervals), 
                   t(confidence_intervals),
                   sample_size = Ns)
names(polls) <- c("poll", "estimate", "low", "high", "sample_size")

d_hat <- polls %>% 
  summarize(avg = sum(estimate*sample_size) / sum(sample_size)) %>% 
  .$avg

z <- qnorm (1 - ((1 - 0.95) / 2))
p_hat <- (1 + d_hat) / 2
moe <- 2 * z * sqrt(p_hat * (1 - p_hat) / sum(polls$sample_size))

#------------------------------------------------------------------------------

prev <- 0.00025
N <- 100000
outcome <- sample(c("Disease","Healthy"), N, replace = TRUE, 
                  prob = c(prev, 1 - prev))
N_D <- sum(outcome == "Disease")
N_H <- sum(outcome == "Healthy")
accuracy <- 0.99
test <- vector("character", N)
test[outcome == "Disease"]  <- sample(c("+", "-"), N_D, replace = TRUE, 
                                      prob = c(accuracy, 1 - accuracy))
test[outcome == "Healthy"]  <- sample(c("-", "+"), N_H, replace = TRUE, 
                                      prob = c(accuracy, 1 - accuracy))
table(outcome, test)

#----------------------- ELECTION FORECASTING --------------------------------
results <- polls_us_election_2016 %>%
  filter(state!="U.S." & 
           !str_detect(state, "CD") & 
           enddate >="2016-10-31" & 
           (grade %in% c("A+","A","A-","B+") | is.na(grade))) %>%
  mutate(spread = rawpoll_clinton/100 - rawpoll_trump/100) %>%
  group_by(state) %>%
  summarize(avg = mean(spread), sd = sd(spread), n = n()) %>%
  mutate(state = as.character(state))

results <- left_join(results, results_us_election_2016, by = "state")

results <- results %>%
  mutate(sd = ifelse(is.na(sd), median(results$sd, na.rm = TRUE), sd))

mu <- 0
tau <- 0.02
results %>% mutate(sigma = sd / sqrt(n), 
                   B = sigma^2 / (sigma^2 + tau^2),
                   posterior_mean = B * mu + (1 - B) * avg,
                   posterior_se = sqrt(1/ (1/sigma^2 + 1/tau^2))) %>%
  ggplot(aes(avg, posterior_mean, size = n)) + geom_point() + 
  geom_abline(slope = 1, intercept = 0)

trytounderstand <- results %>% 
  mutate(sigma = sd/sqrt(n), 
    B = sigma^2 / (sigma^2 + tau^2),
    posterior_mean = B * mu + (1 - B) * avg,
    posterior_se = sqrt(1 / (1/sigma^2 + 1/tau^2)),
    simulated_result = rnorm(length(posterior_mean), posterior_mean, posterior_se),
    clinton = ifelse(simulated_result>0, electoral_votes, 0))

clinton_EV <- replicate(10000, {
  results %>% mutate(sigma = sd/sqrt(n), 
                     B = sigma^2 / (sigma^2 + tau^2),
                     posterior_mean = B * mu + (1 - B) * avg,
                     posterior_se = sqrt(1 / (1/sigma^2 + 1/tau^2)),
                     simulated_result = rnorm(length(posterior_mean), 
                                              posterior_mean, posterior_se),
                     clinton = ifelse(simulated_result>0, electoral_votes, 0)) %>% 
    summarize(clinton = sum(clinton)) %>% 
    pull(clinton) + 7
})

bias_sd <- 0.03
clinton_EV_2 <- replicate(1000, {
  results %>% mutate(sigma = sqrt(sd^2/n  + bias_sd^2),  
                     B = sigma^2 / (sigma^2 + tau^2),
                     posterior_mean = B*mu + (1-B)*avg,
                     posterior_se = sqrt( 1/ (1/sigma^2 + 1/tau^2)),
                     simulated_result = rnorm(length(posterior_mean), 
                                              posterior_mean, posterior_se),
                     clinton = ifelse(simulated_result>0, electoral_votes, 0)) %>% 
    summarize(clinton = sum(clinton) + 7) %>% 
    pull(clinton)
})

polls_us_election_2016 %>%
  filter(state == "U.S.") %>%
  group_by(pollster) %>%
  filter(n()>=10) %>%
  ungroup() %>%
  mutate(spread = rawpoll_clinton/100 - rawpoll_trump/100) %>%
  ggplot(aes(enddate, spread)) + 
  geom_smooth(method = "loess", span = 0.1) + 
  geom_point(aes(color=pollster), show.legend = FALSE, alpha=0.6) 

stepbystep <- polls_us_election_2016 %>%
  filter(state == "U.S." & enddate>="2016-07-01") %>%
  select(enddate, pollster, rawpoll_clinton, rawpoll_trump) %>%
  rename(Clinton = rawpoll_clinton, Trump = rawpoll_trump) %>%
  gather(candidate, percentage, -enddate, -pollster) %>% 
  mutate(candidate = factor(candidate, levels = c("Trump","Clinton"))) %>%
  group_by(pollster) %>%
  filter(n()>=10) %>%
  ungroup()  

stepbystep %>%
  ggplot(aes(enddate, percentage, color = candidate)) +  
  geom_point(show.legend = FALSE, alpha=0.4)  + 
  geom_smooth(method = "loess", span = 0.15) +
  scale_y_continuous(limits = c(30,50))

#------------------------------------------------------------------------------

data("research_funding_rates")
totals <- research_funding_rates %>% 
  select(-discipline) %>% 
  summarize_all(funs(sum)) %>%
  summarize(yes_men = awards_men, 
            no_men = applications_men - awards_men, 
            yes_women = awards_women, 
            no_women = applications_women - awards_women) 

