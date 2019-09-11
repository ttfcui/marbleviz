library(tidyverse)
library(readxl)
out <- read_excel("~/Misc/ML/ML_histpts.xlsx")
outGraph <- select(out, TEAM, `2016ORIG`, `2017PTS`, `2018PTS`,
                   `2019PTS`, `TOTAL PTS`,  `AVG RANK`, `GOLDS`,
                   `SILVERS`, `BRONZES`, `TOTAL MEDALS`,
                   `TOTAL RANK`) %>%
    mutate(`TOTAL RANK` = rev(`TOTAL RANK`)) %>%
    gather(column, value, -TEAM, -`TOTAL RANK`) %>%
    mutate(value = floor(value), column=factor(column)) %>%
    mutate(value = ifelse(value == 0, NA, value))

graph1Column <- function(data, thirdcutoff, lab, colour="#ec8142",
                         subtitle="Bottom Text") {
    ggplot(data=data,
           aes(y=`TOTAL RANK`, x=column, width=1, height=1,
               label=value, fill=value)) +
        geom_tile() +  geom_text(colour="gray95", family="Gill Sans", size=5) +
        theme_minimal() + labs(y="", x=lab) +
        scale_x_discrete(position="top") +
        scale_y_continuous(breaks=unique(outGraph$`TOTAL RANK`),
                           labels=unique(str_to_title(outGraph$TEAM))) +
        scale_fill_gradient(na.value=NA, limits=c(NA, thirdcutoff),
                             low="gray40", high=colour) +
        theme(legend.position = "none",
              text= element_text(family="Gill Sans"),
              panel.background = element_rect(fill="gray80", color=NA),
              panel.grid.major.y = element_blank(),
              panel.grid.minor.y = element_blank(),
              panel.grid.major.x = element_blank(),
              axis.text.x = element_blank(),
              plot.title= element_text(size=16),
              axis.text=element_text(size=11),
              axis.title=element_text(size=13),
              plot.subtitle= element_text(size=9, lineheight=0.8),
              plot.caption = element_text(size=9, lineheight=0.8)) +
        labs(title = "Past Marble League Performances",
             subtitle = subtitle)
}

#2016 Pts column (ORIGINAL NOT CONVERTED)
thirdcutoff <- min(top_n(select(filter(outGraph, column=="2016ORIG"), value),
                         3))
graph1Column(filter(outGraph, column=="2016ORIG"), thirdcutoff,
             "'16 Points")

#2017 Pts column
thirdcutoff <- min(top_n(select(filter(outGraph, column=="2017PTS"), value),
                         3))
graph1Column(filter(outGraph, column=="2017PTS"), thirdcutoff,
             "'17 Points")

#2018 Pts column
thirdcutoff <- min(top_n(select(filter(outGraph, column=="2018PTS"), value),
                         3))
graph1Column(filter(outGraph, column=="2018PTS"), thirdcutoff,
             "'18 Points")

#2019 Pts column
thirdcutoff <- min(top_n(select(filter(outGraph, column=="2019PTS"), value),
                         3))
graph1Column(filter(outGraph, column=="2019PTS"), thirdcutoff,
             "'19 Points")

graph1Column(filter(outGraph, column %in% c("BRONZES", "SILVERS", "GOLDS",
                                            "TOTAL MEDALS")),
                    NA, "Golds  Silvers  Bronzes  Medal Count",
             colour="#ccc115")

graph1Column(filter(outGraph, column %in% c("GOLDS")),
             NA, "", colour="#ccc115")

graph1Column(filter(outGraph, column %in% c("SILVERS")),
             NA, "", colour="gray80")

graph1Column(filter(outGraph, column %in% c("BRONZES")),
             NA, "")

thirdcutoff <- min(top_n(select(filter(outGraph, column=="TOTAL PTS"), value),
                         3))
graph1Column(filter(outGraph, column=="TOTAL PTS"), thirdcutoff,
             "Total Points")

# Got too tired generalizing the function, so repasting this for AVGRANK
# YOU CAN EDIT SUBTITLES AND CAPTIONS HERE, BUT NOTE YOU'LL MISALIGN
# ROW LENGTH WITH EVERYTHING ELSE.
ggplot(data=filter(outGraph, column=="AVG RANK"),
       aes(y=`TOTAL RANK`, x=column, width=1, height=1,
           label=value, fill=28-value)) +
    geom_tile() +  geom_text(colour="gray95", family="Gill Sans", size=5) +
    theme_minimal() + labs(y="", x="Avg-Based Rank") +
    scale_x_discrete(position="top") +
    scale_y_continuous(breaks=unique(outGraph$`TOTAL RANK`),
                       labels=unique(str_to_title(outGraph$TEAM))) +
    scale_fill_gradient(na.value=NA, limits=c(NA, 25),
                        low="gray40", high="#ec8142") +
    theme(legend.position = "none",
          text= element_text(family="Gill Sans"),
          panel.background = element_rect(fill="gray80", color=NA),
          panel.grid.major.y = element_blank(),
          panel.grid.minor.y = element_blank(),
          panel.grid.major.x = element_blank(),
          axis.text.x = element_blank(),
          plot.title= element_text(size=16),
          axis.text=element_text(size=11),
          axis.title=element_text(size=13),
          plot.subtitle= element_text(size=9, lineheight=0.8),
          plot.caption = element_text(size=9, lineheight=0.8)) +
    labs(title = "Past Marble League Performances",
         subtitle = paste("Marble teams win the League by earning points across",
                          "different events.\nSome teams are consistently dominant",
                          "in a way you do not expect marbles to be."),
         caption = paste("Notes:\n Each team result, within one column, is coloured",
                         "according to where they place compared to all teams. Because",
                         "ML2016 used a different points system, the 'Total Points' column",
                         "converts 2016 results to the modern system.\nAverage-Based Rank",
                         "ranks the average number of points by teams over MLs, including",
                         "predicted points for teams that didn't qualify."))
