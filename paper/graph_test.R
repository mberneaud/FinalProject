# graph in ggplot2
library(ggplot2)
ggplot(MayorElection2, aes(x = L.VoteShareWinner, y = Reelection, 
                           colour = IncumbentSparkassenMember)) +
  geom_point(alpha = 0.5) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                       panel.background = element_blank()) + 
  xlab("Vote share in previous election") + 
  scale_colour_discrete(name = "", breaks = c(FALSE, TRUE),
                      labels = c("not in the Sparkassen Board", "Sparkassen baord member")) +
  scale_y_continuous(breaks=0:1)


# Same in plotly
library(plotly)
plot_ly(MayorElection2, x = L.VoteShareWinner, y = Reelection, 
        text = paste("Name winner:", NameCandidate1), 
        mode = "markers", color = SparkassenMember, colors = "Accent")


pl <- plot_ly(MayorElection2, x = L.VoteShareWinner, y = Reelection, 
        text = paste("Name winner:", NameCandidate1), 
        mode = "markers", group = IncumbentSparkassenMember)

subplot(pl)

# Graph which is used in the end
library(plotly)
x <- list(title = "Vote share in previous election")
y <- list(tickmode = "auto", nticks = 2)
p <- plot_ly(MayorElection2, x = L.VoteShareWinner, y = Reelection, 
             text = paste("Name winner:", NameCandidate1), 
             mode = "markers", group = SparkassenMember, colors = "Set1",
             opacity = 0.5) %>%  layout(xaxis = x, yaxis = y) 
p
