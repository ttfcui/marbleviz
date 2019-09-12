setwd("~/Misc/ML/collision/")
library(tidyverse)
library(gridExtra)

test <- read.csv("collision_bymatch.csv",stringsAsFactors = FALSE)
ranks <- read.csv("collision_ranks.csv",stringsAsFactors = FALSE)
test <- test %>% 
             left_join(rename(ranks, Team1=Team, Rank1=Rank) %>%
                           group_by(EVENT, GROUP) %>%
                           mutate(Rank1=rev(Rank1)),
                       by=c("EVENT", "GROUP", "Team1")) %>%
             left_join(rename(ranks, Team2=Team, Rank2=Rank),
                       by=c("EVENT", "GROUP", "Team2")) %>%
             mutate(Rank1=Rank1-0.167, Rank2=Rank2-0.167)
testRevRank <- test %>% rename(Team1=Team2, Team2=Team1,
                               Score1=Score2, Score2=Score1,
                               Form2=Form1, Form1=Form2) %>%
                        mutate(Rank1=Rank1+0.33, Rank2=Rank2+0.33)

out <- rbind(test, testRevRank) %>% mutate(
    win=ifelse(Score1 >= Score2, 1, 0),
    tied=ifelse(Score1 == Score2, 1, 0))

matchOut <- function(data, winsize, losesize, axmin=1.5, axmax=4.5) {
    ggplot(data=data,
           aes(y=Rank1, x=Rank2, colour=Form1,
               shape=factor(win), size=factor(win))) +
        geom_point() + 
        scale_color_manual(values=c("Line" = "#8AE65C", "Wide" = "#0A4BCC",
                                    "Block" = "#FF012A", "Cross" = "#F5DC62",
                                    "Flank" = "#39E6CB", "Shield" = "#AA00FF",
                                    "Spear" = "gray70")) +
        scale_shape_manual(values= c("1" = 18, "0" = 19)) +
        scale_size_manual(values= c("1" = winsize, "0" = losesize)) +
        geom_text(aes(label=Score1), color="white", family="Gill Sans",
                  size=6) +
        theme_void() + expand_limits(x=c(axmin, axmax), y=c(axmin,axmax)) +
        theme(legend.position = "none",
              text= element_text(family="Gill Sans")) 
}

matchOut(out %>% filter(grepl("19$", EVENT) & GROUP=="A"), 16, 9)
matchOut(out %>% filter(grepl("19$", EVENT) & GROUP=="B"), 16, 9)
matchOut(out %>% filter(grepl("19$", EVENT) & GROUP=="C"), 16, 9)
matchOut(out %>% filter(grepl("19$", EVENT) & GROUP=="D"), 16, 9)
matchOut(out %>% filter(grepl("19$", EVENT) & GROUP=="KO1"), 20, 12)
matchOut(out %>% filter(grepl("19$", EVENT) & GROUP=="KO2"), 20, 12)
matchOut(out %>% filter(grepl("19$", EVENT) & GROUP=="FINAL"), 28, 15)