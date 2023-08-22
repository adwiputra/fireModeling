# Plotting graph
library(ggplot2)
library(ggnetwork) # is necessary otherwise won't plot
# command to plot the graph # all graph, so will be a bit too long to finish
ggplot(fireGraph, aes(x = x, y = y, xend = xend, yend = yend)) + 
  geom_edges() +
  geom_nodes()
