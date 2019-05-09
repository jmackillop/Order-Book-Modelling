# Plots histogram of exponential next to histogram of observed limit order interarrival times

# Simulate from exponential distribution with parameter the inverse of mean limit order interarrival time
#  (number of simulations) = (number of limit orders)
simulated_exponential <- rexp(length(limit_interarrival_times[[j]][[i]]),1/mean(limit_interarrival_times[[j]][[i]]))

# Plot the simulated exponential and the actual interarrival times
sim_data <- data.frame(simulated_exponential) # correct form for ggplot
lodata <- data.frame(limit_interarrival_times[[j]][[i]]) # correct form for ggplot

# Following 2 plots only used to find suitable y axis values
plota <- ggplot(sim_data, aes(x=sim_data[,1])) + #sets data to be used
  geom_histogram(breaks = seq(0, pretty(3*mean(limit_interarrival_times[[j]][[i]]))[2], length.out = 9))
plotb <- ggplot(lodata, aes(x=lodata[,1])) + #sets data to be used
  geom_histogram(breaks = seq(0, pretty(3*mean(limit_interarrival_times[[j]][[i]]))[2],length.out = 9))

# Take max y value to be larger of max bin height over both the plots
ylimit <- max(ggplot_build(plota)$data[[1]][["count"]], ggplot_build(plotb)$data[[1]][["count"]])

# Create the two plots
plot1 <- ggplot(sim_data, aes(x=sim_data[,1])) + #sets data to be used
  geom_histogram(breaks = seq(0, pretty(3*mean(limit_interarrival_times[[j]][[i]]))[2], length.out = 9), col = "black", fill = "blue", alpha = 0.5) + #sets x-axis and colours
  ylim(c(0, ylimit)) + #sets y-axis as max height of bins over both plots
  labs(x = "Simulated exponential interarrival times (s)", y = "Frequency") #sets x and y axis labels
plot2 <- ggplot(lodata, aes(x=lodata[,1])) + #sets data to be used
  geom_histogram(breaks = seq(0, pretty(3*mean(limit_interarrival_times[[j]][[i]]))[2], length.out = 9), col = "black", fill = "blue", alpha = 0.5) + #sets x-axis and colours
  ylim(c(0, ylimit)) + #sets y-axis as max height of bins over both plots
  labs(x = "Observed limit order interarrival times (s)", y = "Frequency") #sets x and y axis labels

# Plot next to each other
grid.arrange(plot1, plot2, ncol = 2)