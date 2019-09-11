setwd("~/Misc/ML/funnels/")
library(tidyverse)

test <- read.csv("ML_funnels.csv")
test$TEAMID <- paste0(test$TEAM, test$RACE)
test$F13.33 <- 0.0

testSlice <- function(data, regex, highlights) {
    out <- data %>% filter(grepl(regex, RACE))
    out$TEAMID <- factor(out$TEAMID, levels=unique(out$TEAMID)[
        c((1:dim(out)[1])[-highlights], highlights)])
    out <- out %>%
        gather("funnel", time, -TEAM, -TEAMID, -MEMBER, -RACE) %>%
        mutate(funnel=as.numeric(sub("F", "", funnel)) - 1)
    out
}

graphLabels <- function(data, p) {
    p + geom_step() + geom_point(size=2.5, data=data %>% filter(funnel==0)) +
        scale_y_continuous(breaks=seq(0,12,2)) +
        scale_x_continuous(breaks=seq(0,150,30)) + expand_limits(x=c(0,150)) +
        xlab("Time Elapsed (s)") + ylab("Funnels Until Finish") +
        theme_minimal() +
        theme(legend.position = "none",
              text= element_text(family="Gill Sans"),
              panel.grid.major.y = element_blank(),
              panel.grid.minor.y = element_blank(),
              plot.title= element_text(size=16),
              axis.text=element_text(size=11),
              axis.title=element_text(size=13),
              plot.subtitle= element_text(size=9, lineheight=0.8),
              plot.caption = element_text(size=9, lineheight=0.8)) + 
        labs(title = "The Modern Funnel Race",
             subtitle = paste("A bunch of marbles spin down a tower of funnels;",
                              "the marble lasting the longest on the course wins. \nUsing",
                              "funnel drop times, we can plot the data and see the tactics",
                              "used by champion athletes."),
             caption = paste("Notes:\n This chart overlays all 41 funnel races recorded in 2019 with the 12-funnel tower,",
                             "covering ML and ML Showdown events."))
}

#test1 <- test %>% filter(grepl("^S19G", RACE))
#highlights1 <- c(4,10,1,6,2)
#test1$TEAM <- factor(test1$TEAM,
#                    levels=test1$TEAM[c((1:12)[-highlights1], highlights1)])
#out <- test1 %>%
#    gather("funnel", time, -TEAM, -MEMBER, -RACE) %>%
#    mutate(funnel=as.integer(sub("F", "", funnel)))

outSQ <- testSlice(test, "^S19G", c(4,10,1,6,2))
graphLabels(outSQ,
    ggplot(data=outSQ, aes(
        x=time, y=funnel, colour=TEAMID, size=TEAMID, alpha=TEAMID)) +
    scale_size_manual(values=c(rep(0.75,7), seq(1.25, 2.25, 0.25))) +
    scale_colour_manual(
        values=c(rep("slategray3", 7), "lawngreen", "gold", "red", "aquamarine3", "skyblue2")) +
    scale_alpha_manual(values=c(rep(0.25,7), rep(0.75,5)))
)

outAll <- testSlice(test, "^[A-Z]", c(4, 29, 14, 38, 32))
graphLabels(outAll,
    ggplot(data=outAll, aes(
        x=time, y=funnel, colour=TEAMID, size=TEAMID, alpha=TEAMID)) +
        scale_size_manual(values=c(rep(0.75,36), seq(1.25, 2.25, 0.25))) +
        scale_colour_manual(
            values=c(rep("slategray3", 36), "lawngreen", "deepskyblue3", "skyblue2", "firebrick", "maroon1")) +
        scale_alpha_manual(values=c(rep(0.25,36), rep(0.75, 5)))
)

# Get funnel-specific times
for (i in 16:5) {
    test[,i] = test[,i] - test[,i-1]
}
test %>% select(-TEAM, -MEMBER, -RACE, -TEAMID) %>% summarise_all(max)
test %>% select(-TEAM, -MEMBER, -RACE, -TEAMID) %>% summarise_all(min)
test