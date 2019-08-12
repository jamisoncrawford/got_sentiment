# GAME OF THRONES: DIALOGUE ANALYSIS & VISUALIZATION

#' DESCRIPTION: EDA & EDV of Game of Thrones Dialogue, S1-S7
#' DATE: 2019-08-11
#' R VERSION: 3.6.0
#' RSTUDIO VERSION: 1.2.133
#' OPERATING SYSTEM: Windows 10



#----------# CLEAR OBJECTS & SET WORKING DIRECTORY

rm(list = ls())

dir <- paste0("~/Coursera/Data Science Specializat", 
              "ion (Johns Hopkins)/Course 9 - Deve",
              "loping Data Products/Week 3 Project")

setwd(dir); rm(dir)



#----------# INSTALL & LOAD PACKAGES

if(!require(tidyr)){install.packages("tidyr")}
if(!require(readr)){install.packages("readr")}
if(!require(dplyr)){install.packages("dplyr")}
if(!require(scales)){install.packages("scales")}
if(!require(plotly)){install.packages("plotly")}
if(!require(koRpus)){install.packages("koRpus")}
if(!require(stringr)){install.packages("stringr")}
if(!require(tidytext)){install.packages("tidytext")}
if(!require(textdata)){install.packages("textdata")}

library(tidyr)
library(readr)
library(dplyr)
library(scales)
library(plotly)
library(koRpus)
library(stringr)
library(tidytext)
library(textdata)



#----------# READ IN PREPROCESSED DATA

url <- paste0("https://raw.githubuserconte",
              "nt.com/jamisoncrawford/got_",
              "writing/master/data/process",
              "ed/got_all_lines.csv")             # Assign URL

all <- read_csv(url, col_types = "ciiicl")        # Import; specify class

rm(url)                                           # Rm obsolete vars



#----------# ISOLATE INDIVIDUAL WORDS & TIDY DATASET

all <- all[complete.cases(all), ]                         # Remove incomplete data
dlg <- tibble(cap = all$cap)                              # Isolate captions in df

dlg$cap <- gsub(pattern = "[^[:alnum:][:space:]']", 
                replacement = "", x = dlg$cap)            # Remove punctuation

dlg <- as.data.frame(str_split(dlg$cap, " ", 
                               simplify = TRUE))          # Split: Individual words

dlg <- dlg %>% mutate_all(na_if,"")                       # Empty strings to NAs
all <- bind_cols(all, dlg)                                # Bind columns: "all"

rm(dlg)

brk <- all %>%
  group_by(name, season, episode) %>%                     # Group by season, episode
  mutate(bin = cut(seq_along(line), 10, 1:10),            # Cut 10 bins/episode
         bin = as.numeric(bin)) %>%                       # Coerce to numeric
  arrange(season, episode) %>%                            # Arrange ascending
  select(name:music, bin) %>%
  ungroup()

all <- all %>%
  gather(key = order, value = word, V1:V22) %>%           # Tidy data format
  mutate(order = str_replace_all(order, "V", ""),
         order = as.integer(order),                       # Format "word" as number
         word = tolower(word),                            # Words to lowercase
         word = str_replace_all(word, "'s", ""),
         word = str_replace_all(word, "s'", "s"),         # Remove possessives
         word = str_replace_all(word, "^'", ""),
         word = str_replace_all(word, "'$", "")) %>%      # Remove quote
  filter(!is.na(word)) %>%                                # Remove NAs
  arrange(season, episode, line, order) %>%               # Order desc. by "word"
  left_join(brk) %>%                                      # Merge bin labels
  select(name:episode, music, line:word, bin)             # Reorder vars

rm(brk)



#----------# IMPORT & MERGE LEXICONS

lex_nrc <- get_sentiments(lexicon = "nrc")
lex_bng <- get_sentiments(lexicon = "bing")
lex_afn <- get_sentiments(lexicon = "afinn")
lex_lrn <- get_sentiments(lexicon = "loughran")           # Import sent. lexicons

cnt <- data.frame(table(all$word)) %>%
  rename(count = Freq,
         word = Var1) %>%
  mutate(word = as.character(word)) %>%
  arrange(desc(count))                                    # Word count table: "cnt"

lx_afn <- all %>%
  left_join(lex_afn) %>%
  rename(afn = value)                                     # Join "afinn" lex: "afn"

lx_bng <- all %>%  
  left_join(lex_bng) %>%
  rename(bng = sentiment)                                 # Join "bing" lex: "bng"

lx_lrn <- all %>%   
  left_join(lex_lrn) %>%
  rename(lrn = sentiment)                                 # Join "loughran" lef: "lfn"
  
lx_nrc <- all %>%
  left_join(lex_nrc) %>%
  rename(nrc = sentiment)                                 # Join "nrc" lex: "nrc"

rm(lex_afn, lex_bng, lex_lrn, lex_nrc)                    # Rm obsolete vars

cc_afn <- lx_afn[which(complete.cases(lx_afn)), ]
cc_bng <- lx_bng[which(complete.cases(lx_bng)), ]
cc_lrn <- lx_lrn[which(complete.cases(lx_lrn)), ]
cc_nrc <- lx_nrc[which(complete.cases(lx_nrc)), ]                  # Rm unscored rows



# HOUSE & CHARACTER MENTION TABLES & PLOTS

house <- c("arryn", "baratheon", "greyjoy", "lannister", "martell", 
           "stark", "tully", "frey", "casterly", "gardener", "baratheon", 
           "bolton", "tyrell", "targaryen", "mormont", "reed", "glover", 
           "cerwyn", "manderly", "hornwood", "umber", "karstark", "forrester",
           "tarly", "redwyne", "florent", "hightower", "royce", "waynwood", 
           "blackwood", "bracken", "mallister", "stokeworth")

chars <- c("jon", "sansa", "arya", "bran", "edmure", "davos", "tormund", 
           "meera", "lyanna", "benjen", "cersei", "jaime", "tyrion", "qyburn",
           "gregor", "bronn", "tywin", "joffrey", "myrcella", "tommen", 
           "brienne", "podrick", "samwell", "gilly", "gendry", "baelish",  
           "sandor", "melisandre", "euron", "theon")

df_house <- cnt[which(str_detect(cnt$word,                         # House mentions
                                 paste(house, 
                                       collapse = "|^"))), ] %>% 
  mutate(word = str_replace_all(word, ".*s$", "")) %>%
  group_by(word) %>%
  summarize(count = sum(count)) %>%
  arrange(desc(count)) %>%
  filter(word != "",
         word != "lannistertyrell",
         count > 50) %>%                                          # Mentions > 50
  mutate(word = str_to_title(word)) %>%
  rename(House = word,
         Count = count) %>%
  ungroup()

df_house$House <- factor(x = df_house$House, 
                         levels = rev(df_house$House), 
                         labels = rev(df_house$House), 
                         ordered = TRUE)                          # Ordered factor

p_house <- ggplot(df_house, 
                  aes(x = House,
                      y = Count)) +
  geom_bar(stat = "identity", fill = "dodgerblue") +
  coord_flip() +
  labs(title = "Top 10 Most Mentioned Houses in Game of Thrones",
       subtitle = "Game of Thrones, Season 1-7",
       x = NULL,
       y = "Mentions") +
  theme_minimal()                                                 # Plot in ggplot2

ggplotly(p_house)                                                 # Convert to Plotly

df_chars <- cnt[which(str_detect(cnt$word,                        # Character mentions
                                 paste(chars, 
                                       collapse = "|^"))), ] %>% 
  mutate(word = str_replace_all(word, ".*s$", "")) %>%
  group_by(word) %>%
  summarize(count = sum(count)) %>%
  arrange(desc(count)) %>%
  filter(word != "",
         count > 20) %>%                                          # Mentions > 20
  mutate(word = str_to_title(word)) %>%
  rename(Character = word,
         Count = count) %>%
  ungroup()

df_chars$Character <- factor(x = df_chars$Character, 
                             levels = rev(df_chars$Character), 
                             labels = rev(df_chars$Character), 
                             ordered = TRUE)                      # Ordered factor

p_chars <- ggplot(df_chars, 
                  aes(x = Character,
                      y = Count)) +
  geom_bar(stat = "identity", fill = "dodgerblue") +
  coord_flip() +
  labs(title = "Top 20 Most Mentioned Characters in Game of Thrones",
       subtitle = "Game of Thrones, Season 1-7",
       x = NULL,
       y = "Mentions") +
  theme_minimal()                                                 # Plot in ggplot2

ggplotly(p_chars)                                                 # Convert to Plotly



# AFFINITY SCORES BY EPISODE, EACH SEASON

gp_afn <- cc_afn %>%
  group_by(name, season, episode, bin) %>%
  summarize(afn = sum(afn)) %>%
  arrange(season, episode, name, bin) %>%
  ungroup()                                                       # Group by ep. bins

gp_afn$season <- factor(x = gp_afn$season, 
                        levels = gp_afn$season, 
                        labels = paste("Season", 
                                       gp_afn$season, 
                                       sep = " "),                # Label factor
                        ordered = TRUE)                           # Ordered factor

gp_afn$name <- factor(x = gp_afn$name, 
                      levels = gp_afn$name, 
                      labels = paste(gp_afn$name), 
                      ordered = TRUE)                             # Ordered factor

gp_afn$mood <- NA

for (i in seq_along(gp_afn$mood)){
  if (gp_afn$afn[i] >= 0){
    gp_afn$mood[i] <- "Positive"
  } else if (gp_afn$afn[i] < 0){
    gp_afn$mood[i] <- "Negative"
  }
}

gp_afn$mood <- as.factor(gp_afn$mood)

gp_afn <- gp_afn %>%
  rename(Title = name,
         Season = season,
         `Episode Number` = episode,
         Progress = bin,
         Affinity = afn,
         Mood = mood)

gp_afn_s1 <- gp_afn %>% filter(Season == "Season 1")
gp_afn_s2 <- gp_afn %>% filter(Season == "Season 2")
gp_afn_s3 <- gp_afn %>% filter(Season == "Season 3")
gp_afn_s4 <- gp_afn %>% filter(Season == "Season 4")
gp_afn_s5 <- gp_afn %>% filter(Season == "Season 5")
gp_afn_s6 <- gp_afn %>% filter(Season == "Season 6")
gp_afn_s7 <- gp_afn %>% filter(Season == "Season 7")

p_afn_s1 <- ggplot(gp_afn_s1, 
                   aes(x = Progress, y = Affinity, fill = Mood)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  labs(title = "Game of Thrones affinity by episode, Season One",
       x = "Episode Progress",
       y = "Affinity",
       color = "Episode") +
  scale_x_continuous(breaks = c(1:10)) +
  facet_wrap(~ Title) +
  theme_classic()

ggplotly(hide_legend(p_afn_s1))

p_afn_s2 <- ggplot(gp_afn_s2, 
                   aes(x = Progress, y = Affinity, fill = Mood)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  labs(title = "Game of Thrones affinity by episode, Season Two",
       x = "Episode Progress",
       y = "Affinity",
       color = "Episode") +
  scale_x_continuous(breaks = c(1:10)) +
  facet_wrap(~ Title) +
  theme_classic()

ggplotly(hide_legend(p_afn_s2))

p_afn_s3 <- ggplot(gp_afn_s3, 
                   aes(x = Progress, y = Affinity, fill = Mood)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  labs(title = "Game of Thrones affinity by episode, Season Three",
       x = "Episode Progress",
       y = "Affinity",
       color = "Episode") +
  scale_x_continuous(breaks = c(1:10)) +
  facet_wrap(~ Title) +
  theme_classic()

ggplotly(hide_legend(p_afn_s3))

p_afn_s4 <- ggplot(gp_afn_s4, 
                   aes(x = Progress, y = Affinity, fill = Mood)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  labs(title = "Game of Thrones affinity by episode, Season Four",
       x = "Episode Progress",
       y = "Affinity",
       color = "Episode") +
  scale_x_continuous(breaks = c(1:10)) +
  facet_wrap(~ Title) +
  theme_classic()

ggplotly(hide_legend(p_afn_s4))

p_afn_s5 <- ggplot(gp_afn_s5, 
                   aes(x = Progress, y = Affinity, fill = Mood)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  labs(title = "Game of Thrones affinity by episode, Season Five",
       x = "Episode Progress",
       y = "Affinity",
       color = "Episode") +
  scale_x_continuous(breaks = c(1:10)) +
  facet_wrap(~ Title) +
  theme_classic()

ggplotly(hide_legend(p_afn_s5))

p_afn_s6 <- ggplot(gp_afn_s6, 
                   aes(x = Progress, y = Affinity, fill = Mood)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  labs(title = "Game of Thrones affinity by episode, Season Six",
       x = "Episode Progress",
       y = "Affinity",
       color = "Episode") +
  scale_x_continuous(breaks = c(1:10)) +
  facet_wrap(~ Title) +
  theme_classic()

ggplotly(hide_legend(p_afn_s6))

p_afn_s7 <- ggplot(gp_afn_s7, 
                   aes(x = Progress, y = Affinity, fill = Mood)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  labs(title = "Game of Thrones affinity by episode, Season Seven",
       x = "Episode Progress",
       y = "Affinity",
       color = "Episode") +
  scale_x_continuous(breaks = c(1:10)) +
  facet_wrap(~ Title) +
  theme_classic()

ggplotly(hide_legend(p_afn_s7))



# Affinity Scores, All Seasons

gp_afn_all <- cc_afn %>%
  group_by(season, episode, name) %>%
  summarize(afn = sum(afn)) %>%
  arrange(season) %>%
  ungroup() %>%
  rename(Season = season,
         Episode = episode,
         Title = name,
         Affinity = afn)

gp_afn_all$Order <- as.numeric(paste0(gp_afn_all$Season, ".", 
                                      gp_afn_all$Episode - 1))

gp_afn_all$Mood <- NA

for (i in seq_along(gp_afn_all$Mood)){
  if (gp_afn_all$Affinity[i] >= 0){
    gp_afn_all$Mood[i] <- "Positive"
  } else if (gp_afn_all$Affinity[i] < 0){
    gp_afn_all$Mood[i] <- "Negative"
  }
}

gp_afn_all$`Season & Episode` <- as.numeric(gp_afn_all$`Season & Episode`)

p_afn_all <- ggplot(gp_afn_all, 
       aes(x = Order, 
           y = Affinity, 
           fill = Mood, 
           label = Title, 
           text = paste0("Season ", Season, "; ", "Episode ", Episode))) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  labs(title = "Game of Thrones affinity over series",
       x = "Seasons",
       y = "Affinity",
       color = "Episode") +
  scale_x_continuous(breaks = c(1:7)) +
  theme_classic()

ggplotly(hide_legend(p_afn_all))



# MOST EMOTIONS IN SERIES

gp_nrc_all <- data.frame(table(cc_nrc$nrc)) %>%
  rename(Emotion = Var1,
         Frequency = Freq) %>%
  filter(Emotion != "positive",
         Emotion != "negative") %>%
  mutate(Emotion = str_to_title(Emotion)) %>%
  arrange(desc(Frequency))

gp_nrc_all$Emotion <- factor(gp_nrc_all$Emotion, 
                             levels = rev(gp_nrc_all$Emotion), 
                             labels = rev(gp_nrc_all$Emotion), 
                             ordered = TRUE)

p_gp_nrc_all <- ggplot(gp_nrc_all, aes(x = Emotion, y = Frequency)) +
  geom_bar(stat = "identity", fill = "dodgerblue") +
  coord_flip() +
  labs(title = "Game of Thrones: Most common emotions in series") +
  scale_y_continuous(breaks = c(0, 3000, 6000, 9000),
                     labels = c("0", "3,000", "6,000", "9,000")) +
  theme_classic()

ggplotly(p_gp_nrc_all)



# EMOTIONS BY SEASON

nrc_s1 <- data.frame(table(cc_nrc[cc_nrc$season == 1, "nrc"])) %>% mutate(season = 1)
nrc_s2 <- data.frame(table(cc_nrc[cc_nrc$season == 2, "nrc"])) %>% mutate(season = 2)
nrc_s3 <- data.frame(table(cc_nrc[cc_nrc$season == 3, "nrc"])) %>% mutate(season = 3)
nrc_s4 <- data.frame(table(cc_nrc[cc_nrc$season == 4, "nrc"])) %>% mutate(season = 4)
nrc_s5 <- data.frame(table(cc_nrc[cc_nrc$season == 5, "nrc"])) %>% mutate(season = 5)
nrc_s6 <- data.frame(table(cc_nrc[cc_nrc$season == 6, "nrc"])) %>% mutate(season = 6)
nrc_s7 <- data.frame(table(cc_nrc[cc_nrc$season == 7, "nrc"])) %>% mutate(season = 7)

nrc_all <- bind_rows(nrc_s1, nrc_s2, nrc_s3, nrc_s4, nrc_s5, nrc_s6, nrc_s7) %>%
  rename(Emotion = Var1, Frequency = Freq, Season = season) %>%
  select(Season, Emotion, Frequency) %>%
  mutate(Emotion = str_to_title(Emotion)) %>%
  filter(Emotion != "Positive",
         Emotion != "Negative") %>%
  arrange(Season, desc(Frequency))

nrc_all$Emotion <- factor(nrc_all$Emotion, 
                          levels = rev(nrc_all$Emotion), 
                          labels = rev(nrc_all$Emotion), 
                          ordered = TRUE)

nrc_all$Season <- factor(nrc_all$Season, 
                         levels = nrc_all$Season, 
                         labels = paste("Season", nrc_all$Season, sep = " "), 
                         ordered = TRUE)

p_nrc_all <- ggplot(nrc_all, aes(x = Emotion, y = Frequency, fill = Season)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  coord_flip() +
  theme_classic() +
  facet_wrap(~Season) +
  scale_y_continuous(breaks = c(0, 500, 1000, 1500),
                     labels = c("0", "500", "1,000", "1,500")) +
  labs(title = "Game of Thrones: Most common emotions by season")

ggplotly(hide_legend(p_nrc_all))