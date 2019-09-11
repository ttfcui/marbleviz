setwd("~/Misc/ML/matchup/")
library(tidyverse)

b <- read.csv("MLRivalries_IO.csv", check.names=FALSE)
colnames(b)[1] <- "teamX"
b <- b %>% mutate(teamX=str_to_title(teamX)) %>%
    mutate(teamX=factor(teamX,levels=rev(teamX))) %>%
    gather(teamY, matchup, -teamX) %>%
    mutate(teamY=str_to_title(teamY)) %>%
    mutate(teamY=factor(teamY, levels=unique(teamY)))


ggplot(data=b, aes(x=teamY, y=teamX, width=1,height=1, fill=matchup)) +
    geom_tile() + 
    scale_fill_distiller(na.value=NA, breaks=seq(0,10,1),palette="YlGnBu", direction=1) +
    theme_minimal() + labs(y="", x="") +
    guides(fill = guide_colorbar(title=NULL, direction="horizontal",
                                 barwidth=30, barheight=1)) +
    theme(text= element_text(family="Gill Sans"),
      plot.title= element_text(size=16),
      axis.text=element_text(size=9),
      axis.title=element_text(size=16),
      panel.grid.major.y = element_blank(),
      panel.grid.major.x = element_blank(),
      axis.text.x = element_text(angle=90, hjust=1),
      legend.position = "top",
      legend.justification = c(0, 1),
      plot.subtitle= element_text(size=9, lineheight=0.8),
      plot.caption = element_text(size=9, lineheight=0.8)) +
    labs(title = "Every Marble League Team Rivalry",
         subtitle = paste("...Ranked by the number of meaningful showdowns",
                          "between pairs of teams. \nThe more encounters there were,",
                          "the more history exists for a real rivalry."),
         caption = paste("Notes:\n \"Meaningful Showdowns\" are defined as two teams",
                         "facing each other in any semifinal or any final, or when",
                         "two teams place in the top four of a non-knockout event. \nCounts",
                         "summed over all 2016-9 ML events, excluding qualifiers and ML Showdown."))