library(tidyverse)
mlspd <- read.csv('./MLspd_cleaned.csv')

mloutcomes <- read.csv('./MLspd_outcomes.csv',
                       stringsAsFactors=FALSE, na.strings = "")
mlfinals <- mloutcomes %>%
    filter(type=="final") %>%
    mutate_at(colnames(mloutcomes)[-(1:2)],
              function(x) ifelse(grepl("Finalist|Semi", x), 1, 0))
mloutcomes <- rbind(mloutcomes %>% filter(type=="rank"), mlfinals)

gettotal <- function(data, discfact) {
    test <- select(data, -team, -member, -overallcount)
    test <- sapply(test, function(x) ifelse(is.na(x), -999, x))
    rowsort <- as.data.frame(t(apply(test, 1, function(x) sort(x, TRUE))))
    rowsort <- sapply(rowsort, function(x) ifelse(x == -999, NA, x))
    for (i in 1:dim(rowsort)[2]) {
        rowsort[,i] <- rowsort[,i]*discfact^(i-1)
    }
    data.frame(overalltot=rowSums(rowsort,na.rm=TRUE))
}

getscore <- function(data, discfact, teamdisc) {
    data <- data %>% mutate(relay17 = relay17*teamdisc,
                              winBiathlon = winBiathlon*teamdisc,
                              relay = relay*teamdisc)
    data <- cbind(data, gettotal(data, discfact))
    data <- data %>%
        mutate(overallscore=overalltot*
                   (1-discfact)/(1-(discfact)^overallcount)) %>%
        select(-overalltot)
    
    fastest.rank <- data %>% group_by(team) %>%
        summarise(max=max(overallscore, na.rm=TRUE)) %>%
        mutate(rank=dense_rank(max))
    data <- left_join(data, select(fastest.rank, -max))
    data
}

mergeoutcomes <- function(data, outcomefile, stat) {
    ml.long <- data %>%
        select(-c("overallcount", stat)) %>% 
        gather("race", "value", -team, -member, -overallscore) %>%
        filter(!is.na(value))
    
    mlreg <- outcomefile %>% filter(type==stat) %>% select(-("type")) %>%
        gather("race", stat, -team) %>%
        left_join(select(ml.long, team, member, race, overallscore),
                  by=c("team", "race"))
    mlreg
}

maxroutine <- function(vec, data, outcomefile) {
    discfact <- vec[1]
    teamdisc <- vec[2]
    dataout <- getscore(data, discfact, teamdisc)
    
    mlrankreg <- mergeoutcomes(dataout, outcomefile, "rank")
    regout <- lm(as.integer(stat) ~ overallscore,
                 data=mlrankreg %>% filter(abs(overallscore) < 1))
    print(summary(regout))
    anova(regout)["Residuals", "Sum Sq"]
    #regout$coefficients[1] + regout$coefficients[2]^2
}

#optim(c(0.7, 0.9), maxroutine,
#      data=mlspd, outcomefile=mloutcomes, method="Nelder-Mead")

#mlfinreg <- mloutcomes %>% filter(type=="final") %>% select(-("type")) %>%
#    gather("race", "final", -team) %>%
#    left_join(select(mlspd_long, team, member, race, overallscore),
#              by=c("team", "race"))

mlspd <- getscore(mlspd, 0.95, 0.525)
teammeans <- mlspd %>% 
             group_by(team) %>%
             mutate(wgtscore=
                        overallcount*overallscore/sum(overallcount)) %>%
             summarise(teammean=sum(wgtscore, na.rm=TRUE)) %>%
             mutate(rank2=dense_rank(teammean))
mlspd_merged <- left_join(mlspd, teammeans, by="team")
mlspd_merged <- mlspd_merged %>%
    mutate(ranktop = rank2 + .45, rankbottom = rank2 - .45) %>%
    mutate(overallscore = overallscore*100, teammean=teammean*100)

mlrankreg <- mergeoutcomes(mlspd_merged, mloutcomes, "rank")
qplot(x=overallscore, y=as.integer(stat),
      data=mlrankreg %>% filter(abs(overallscore) < 100)) +
    geom_smooth(method="lm")

write.csv(mlspd_merged %>%
          mutate(overallscore=round(overallscore), teammean=round(teammean)) %>% 
          select(team, teammean, member, overallscore, overallcount,
                 everything(), -rank, -ranktop, -rankbottom),
          "MLspd_indexes.csv")

#
colorguide <- c("#fc8d62","#fc8d62","#66c2a5","#a6d854","#8da0cb",
                "#8da0cb","#e78ac3","#66c2a5","#8da0cb","#a6d854",
                "#a6d854","#8da0cb","#66c2a5","#fc8d62","#66c2a5",
                "#e78ac3","#e78ac3","#e78ac3","#fc8d62","#a6d854",
                "#66c2a5","#e78ac3","#a6d854","#fc8d62","#8da0cb")
shapeguide <- c(2,1,2,17,1,
                2,16,1,17,18,
                2,18,17,17,16,
                1, 18, 17,16,16,
                18,2,1,18,16)
#
ggplot(aes(x=overallscore, y=rank2, color=team, shape=team),
      data=mlspd_merged %>% filter(overallcount > 1) %>%
          mutate(teammean=round(teammean))) + 
    geom_point(size=3) +
    geom_linerange(aes(x=teammean, ymin=rankbottom, ymax=ranktop),
                   size=1.75) +
    scale_y_continuous(
        breaks=unique(mlspd_merged$rank2),
        labels=unique(str_to_title(mlspd_merged$team))) +
    scale_color_manual(values=colorguide) +
    scale_shape_manual(values=shapeguide) +
    xlab("Speed Index Rating") + ylab("") +
    theme(legend.position = "none",
          text= element_text(family="Gill Sans"),
          panel.grid.major.y = element_blank(),
          panel.grid.minor.y = element_blank(),
          plot.title= element_text(size=16),
          axis.text=element_text(size=11),
          axis.title=element_text(size=16),
          plot.subtitle= element_text(size=9, lineheight=0.8),
          plot.caption = element_text(size=9, lineheight=0.8)) + 
    labs(title = "The Marble League Speed Index",
         subtitle = paste("Which marbles are the fastest?",
                          "Are there \"Fast\" and \"Slow\" ML Teams?\nFor",
                          "the first time, these questions can be answered",
                          "with an index aggregating marble performances in race events."),
         caption = paste("Notes:\n Individual Ratings are weighted averages of marble times",
                         "over any of 23 racing events.\n Some times in team events are imputed",
                         "based on assumed marble order. Marbles with only 1 race event recorded",
                         "are dropped from the graph.\n For more info on the",
                         "weighting process, see: https://pastebin.com/G7rZSszx ."))