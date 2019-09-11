setwd("~/Misc/ML/streaks/")
library(tidyverse)
library(gridExtra)

streakCalc <- function(data, number) {
    if (as.integer(number) <= 2) stop("Streak must be > 1")
    startval <- 2+number
    out <- data.frame(row.names=1:dim(data)[1])
    out[,1:number] <- NA
    out <- cbind(select(data, team, year), out)
    for (i in startval:dim(data)[2]) {
        out[,i] <- rowSums(data[,(i-number+1):i])
        colnames(out)[i] <- paste0("e", i-2)
    }
    out %>%
        gather(event, score, -team, -year) %>%
        unite(events, year, event, sep="_") %>%
        spread(events, score)
}

streakSums <- function(data) {
    columns <- colnames(data)
    out <- apply(select(data, -team), 1, function(x) max(x, na.rm=TRUE))
    outwhere <- apply(select(data, -team), 1,
                      function(x) which.max(x))
    out2 <- apply(select(data, -team), 1, function(x) min(x, na.rm=TRUE))
    out2where <- apply(select(data, -team), 1,
                      function(x) which.min(x))
    list(max=cbind(select(data, team), out, which=columns[outwhere + 1]),
         min=cbind(select(data, team), out2, which=columns[out2where + 1]))
}

streaks <- read.csv("ML_teamstreaks.csv", stringsAsFactors = FALSE)
streaks <- streaks[complete.cases(streaks[,3:14]),]
rownames(streaks) <- NULL

# Print out the streaks
maxN=vector("list", 10)
minN=vector("list", 10)
for (n in c(3:12)) {
    sumsN <- streakSums(streakCalc(streaks, n)) 
    print(sumsN)
    maxN[[n-2]] <- sumsN$max %>% top_n(4, out) %>% mutate(streak=n)
    minN[[n-2]] <- sumsN$min %>% top_n(-4, out2) %>% mutate(streak=n)
}
maxN <- do.call(rbind.data.frame, maxN) %>% arrange(streak, desc(out))
minN <- do.call(rbind.data.frame, minN) %>% arrange(streak, out2)

graphstreaks <- data.frame(rbind(
    c(streaks[14,1:14]), c(streaks[62,c(1:2,6:17)]),
    c(streaks[53,c(1:2,5:15)], NA),
    c(streaks[40,c(1:2,12:14)], rep(NA, 9)),
    c(streaks[33,c(1:5)], rep(NA, 9)),
    c(streaks[59,c(1:2,6:17)]), c(streaks[13,1:14]),
    c(streaks[4,c(1:11)], rep(NA, 3)),
    c(streaks[12,c(1:2, 6:13)], rep(NA, 4)),
    c(streaks[51,c(1:2,16:18)], rep(NA, 9))
)) %>% unnest()

graphstreaks <- graphstreaks %>%
    mutate(grp=c(rep(1, 5), rep(2, 5)),
           eSum = rowSums(select(graphstreaks, -team, -year), na.rm = TRUE)) %>%
    mutate(team= paste0(team, "\n", eSum, " points in ", year)) %>%
    mutate(team=factor(team, levels=c(team[c(1,4,2,3,5,
                                             6,10,7,8,9)]))) %>%
    gather(event, score, -team, -year, -grp, -eSum) %>%
    mutate(event=as.numeric(sub("e", "", event)))

streakFig1 <- ggplot(data=graphstreaks %>% filter(grp==1),
    aes(x=team, y=event, width=0.5, height=1, label=score, fill=score)) +
    geom_tile() + geom_text(colour="white", family="Gill Sans", size=8) +
    labs(y="", x="") + expand_limits(x=c(0,5)) +
    scale_x_discrete(position="top") +
    scale_fill_gradient2(na.value=NA,
                         low="black", mid="grey45", high="#eee559") +
    theme_void() + 
    theme(legend.position = "none",
              text= element_text(family="Gill Sans"),
              plot.title= element_text(size=16),
              axis.text=element_text(size=11),
              axis.title=element_text(size=16),
              axis.text.y = element_blank(),
              axis.text.x = element_text(hjust=0.2),
              plot.subtitle= element_text(size=9, lineheight=0.8),
              plot.caption = element_text(size=9, lineheight=0.8)) +
    labs(title = "(In)famous Streaks in ML History",
         subtitle = paste("Which teams are known for record-breaking success? Which",
                          "teams are still ashamed of past collapses?"))

streakFig2 <- ggplot(data=graphstreaks %>% filter(grp==2),
       aes(x=team, y=rev(event), width=0.5, height=1, label=score, fill=score)) +
    geom_tile() + geom_text(colour="white", family="Gill Sans", size=8) +
    labs(y="", x="") + expand_limits(x=c(0,5)) +
    scale_x_discrete(position="top") +
    scale_fill_gradient2(na.value=NA,
                         low="black", mid="grey45", high="#ec8142") +
    theme_void() + 
    theme(legend.position = "none",
          text= element_text(family="Gill Sans"),
          plot.title= element_text(size=16),
          axis.text=element_text(size=11),
          axis.title=element_text(size=16),
          axis.text.y = element_blank(),
          axis.text.x = element_text(hjust=0.2),
          plot.subtitle= element_text(size=9, lineheight=0.8),
          plot.caption = element_text(size=9, lineheight=0.8)) +
    labs(caption = paste("Notes:\n Read top half streaks from the bottom up, and vice versa for",
                         "bottom half droughts. Ticks mark the first and final events in the",
                         "streak.\nEach cell records an event",
                         "with points received by the team labelled. Negative points are possible",
                         "due to disqualification."))

grid.arrange(streakFig1, streakFig2, ncol=1)
