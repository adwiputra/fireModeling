# Plotting graph
library(igraph)
library(tidyverse)
library(ggnetwork) # is necessary otherwise won't plot
library(GGally)
library(intergraph)

# Preprocessing
load("final_graphAnalysis.RData")
graphToPlot <- fireGraph_decompose[[13670]]

# Extract point names to join with the pointCategories

vertex_names <- igraph::vertex_attr(graphToPlot, "name")
vertex_df <- ignitionSpread_df %>% mutate(pointID = as.character(pointID)) %>%  right_join(data.frame(pointID = vertex_names)) %>% mutate(color = case_when(pointCategory=="spread" ~ "#823E6A",
                                                                                                                                                            TRUE ~ "#00E6A9"))
# assign color into vertex.attributes
graphToPlot <- set_vertex_attr(graphToPlot, "color", value = vertex_df$color)

# Plotting
png("./graphPlot.png",
    width = 500, height = 980, units = "px")
GephiForR::easyplot(graphToPlot,  layout = layout_with_lgl)
dev.off()
# command to plot the graph # all graph, so will be a bit too long to finish
ggp_graph <- ggplot(fireGraph_decompose[[13670]], aes(x = x, y = y, xend = xend, yend = yend)) + theme_blank() +
  geom_edges(arrow = arrow(length = unit(0.5, "lines"), type = "closed"), ) + 
  geom_nodes(colour = "steelblue") + theme(axis.line = element_blank(), axis.title = element_blank(), axis.text = element_blank())


#  diagnostic plots
# function to find the Mode
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}
Mode(verticesCounts)


# make bar plot for the different number of 
verticesNumber <- read.csv("D:/Documents/otherOpps/YSSP/projects/analysis/annualStats_ignitials.csv") %>% pivot_longer(!year, names_to = "variables", values_to = "count")
verticesNumber <- verticesNumber %>% mutate(variables = as.factor(variables)) %>% mutate(variables = case_when(variables == "allPoints_n" ~ "Total fire points",
                                                                                                               variables == "ignitial_n" ~ "Initial points",
                                                                                                               TRUE ~ variables))

par(mfrow = c(1, 2))
ggplot(verticesNumber %>% filter(variables != "pct_ignitial"), aes(x = variables, y = count, fill = as.character(year))) + geom_col(position = "dodge") + theme_classic() + theme(legend.title=element_text("Year")) + scale_fill_brewer(name = "year")
ggplot(verticesNumber %>% filter(variables == "pct_ignitial"), aes(x = "Initial proportion", y = count, fill = as.character(year))) + geom_col(position = "dodge") + theme_classic() + theme(legend.title=element_blank())
library(ggplot2)
# Basic density
df <- data.frame(vCount = verticesCounts)
p <- ggplot(df, aes(x=vCount)) + 
  geom_density() + theme_classic() + xlim(0, 100) + xlab("Vertice counts") + theme(axis.title = element_text(size = 16), axis.text = element_text(size = 13))
# p
# Add mean line
p+ geom_vline(aes(xintercept=mean(vCount)),
              color="blue", linetype="dashed", linewidth=1)
# Replaced by density curve
# boxplot(verticesCounts, subset = verticesCounts < 800)
# verticesCounts[verticesCounts < 100] %>% boxplot()


# playing around
testGraph <- fireGraph_decompose[[13670]]
btw <- degree(testGraph, mode = "in")
# want to display: the origin, the most in and out
btw_dum <- (btw - (btw-1))*3
btw_dum[btw == 0] <- 8
plot(testGraph, vertex.size = btw_dum, vertex.label = NA, edge.arrow.size = 0.2, edge.color = "gray", vertex.color = "steelblue", layout = layout_with_lgl)



