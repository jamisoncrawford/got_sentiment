# GAME OF THRONES: DIALOGUE ANALYSIS & VISUALIZATION

        #' DESCRIPTION: EDA & EDV of Game of Thrones Dialogue, S1-S7
        #' DATE: 2019-08-09
        #' R VERSION: 3.6.0
        #' RSTUDIO VERSION: 1.2.133
        #' OPERATING SYSTEM: Windows 10



#----------# SET WORKING DIRECTORY

dir <-paste0("~/Coursera/Data Science Specializat", 
             "ion (Johns Hopkins)/Course 9 - Deve",
             "loping Data Products/Week 3 Project")

setwd(dir); rm(dir)



#----------# INSTALL & LOAD PACKAGES

if(!require(readr)){install.packages("readr")}
if(!require(dplyr)){install.packages("dplyr")}
if(!require(koRpus)){install.packages("koRpus")}
if(!require(stringr)){install.packages("stringr")}
if(!require(jsonlite)){install.packages("jsonlite")}

library(readr)
library(dplyr)
library(koRpus)
library(stringr)
library(jsonlite)



#----------# READ JSON FILES; CONVERT TO LISTS OF EQUAL LENGTH (ELEMENT-WISE)

url1 <- paste0("https://raw.githubusercontent.com/jamiso",
               "ncrawford/got_writing/master/data/season")

url2 <- ".json"                                 # Assign parsed URL strings

nums <- 1:7                                     # Assign sequence for seasons
null <- 8:10                                    # Assign sequence for missing

seas <- sapply(paste0(url1, nums, url2),
               simplifyDataFrame = FALSE, 
               fromJSON)                        # Import all seasons into list

for (i in seq_along(nums)){
    assign(paste0("s", i), seas[[i]])
}                                               # Assign seasons to new lists

s4[[11]] <- NULL                                # Remove empty list elements

for (i in null){
    s7[[i]] <- NA
}                                               # Add empty list elements

rm(seas, null, url1, url2)



#----------# COERCE LISTS TO DATA FRAMES, TRANSPOSE

for (i in seq_along(nums)){                     # Coerce to dataframe, transpose
    assign(paste0("s", i), 
           data.frame(eval(parse(text = paste0("s", i)))));
    assign(paste0("s", i), 
           t(eval(parse(text = paste0("s", i)))));
    assign(paste0("s", i), 
           data.frame(eval(parse(text = paste0("s", i)))))
}

all <- rbind(s1, s2, s3, s4, s5, s6, s7)        # Bind all seasons into 1 object

rm(s1, s2, s3, s4, s5, s6, s7, i, nums)



#----------# ORGANIZE & PARSE VARIABLES

colnames(all)[1] <- "cap"               # Rename variable: "cap"
all$episodes <- rownames(all)           # Assign rownames to variable: "episodes"
rownames(all) <- NULL                   # Remove rownames

all <- all %>%
    mutate(cap = as.character(cap),                                     # Non-categorical 
           season = str_extract_all(episodes, "S[0-9]{2}"),
           season = as.integer(str_extract_all(season, "[0-9]{2}")),    # Extract: "season"
           episode = str_extract_all(episodes, "E[0-9]{2}"),
           episode = as.integer(str_extract_all(episode, "[0-9]{2}")),  # Extract: "episode"
           line = as.integer(str_extract_all(episodes, "[0-9]{1,4}$")), # Extract: "line" (#)
           name = str_extract_all(episodes, "E[0-9]{2}.*srt"),
           name = str_replace_all(name, "^E[0-9]{2}\\.|\\.srt$", ""),
           name = str_replace_all(name, "\\.", " ")) %>%                # Extract: (ep.) "name"
    select(name, season, episode, line, cap) %>%                        # Rearrange vars
    arrange(season, episode, line)                                      # Order values



#----------# REMOVE ITALICS &  IN-CAPTION METADATA

all <- all %>% mutate(cap = str_replace_all(cap, "<i>|</i>", ""))   # Rm italics

ind <- which(str_detect(string = all$cap, pattern = "<.*>"))        # Rm metadata
all[ind, "cap"] <- NA

all <- all %>% 
    mutate(cap = str_replace_all(cap, "\\(.*\\)", ""),           # Rm contextual
           cap = str_replace_all(cap, "[A-Z]{2}.*: ", ""),       # Rm speaker caps
           cap = str_replace_all(cap, " ???2| ???3", ""),            # Rm anon distinctions
           cap = str_replace_all(cap, "^- ", ""),                # Rm interruptions
           cap = str_replace_all(cap, "\\.\\.\\. - ", ". "),     # Rm pause-interrupt
           cap = str_replace_all(cap, "--", "..."),              # Replace pauses
           cap = str_replace_all(cap, "- Man: ", ""),            # Replace anon interrupt
           cap = str_replace_all(cap, "^Man: |^Woman: ", ""),    # Replace anon start
           cap = str_replace_all(cap, " - ", " "),               # Rm mid-line interrupt
           cap = str_replace(cap, "^ *[A-Z]{1}[a-z]*: ", ""))    # Rm sentcase speaker start

spkr <- paste0(" Grey Worm:|Hot Pie: |Gold cloak: |B",
               "ran's voice: |Rhaegar and Lyanna: |T",
               "he Hound: |All: |Davos: |Tyrion: |Ja",
               "ime: |Henk: |Sansa: |Jon: |Tormund: ",
               "|Bron: |Sam: |Randyll: ")                        # Compile unique speakers

all <- all %>% mutate(cap = str_replace(cap, spkr, ""))          # Rm mid-line speakers
all <- all %>% mutate(cap = str_replace(cap, "^- *", ""))        # Rm start interrupt
all <- all %>% mutate(cap = str_trim(cap, side = "both"))        # Trim whitespace
all <- all %>% mutate(cap = str_replace(cap, " {2,}", " "))      # Rm 2+ spaces

ind <- which(str_detect(all$cap, pattern = "??? ???"))               # Rm empty music strings
all$cap[ind] <- NA         

all$music <- FALSE
ind <- which(str_detect(all$cap, "???"))      # Index musical notes
all$music[ind] <- TRUE                      # Assign as "music"
ind <- which(str_detect(all$cap, "#"))      # Index musical notes 
all$music[ind] <- TRUE                      # Assign as "music"

all <- all %>% 
    mutate(cap = str_replace_all(cap, 
                                 "For all men must die", 
                                 "For all men must die..."),        # Unique music replace
           cap = str_replace_all(cap, "^??? |??? ", ""),                # Rm lead music notes
           cap = str_replace_all(cap, "\\.\\.\\. ???$| ???$", "..."),   # Rm trail/mid notes
           cap = str_replace_all(cap, ">", ""))                     # Rm stray ">"

for (i in seq_along(all$cap)){
    if (str_detect(all$cap[i], "^# ") & !str_detect(all$cap[i], "\\.\\.\\.$|!$")){
        all$cap[i] <- paste0(all$cap[i], "...");
        all$cap[i] <- str_replace_all(all$cap[i], "^# | #", "")
    } else if (str_detect(all$cap[i], "^# ")){
        all$cap[i] <- str_replace_all(all$cap[i], "^# ", "")
    }
}                                                                   # Format music with "#"

ind <- which(str_detect(all$cap, '"'))
all$cap[ind] <- gsub('\"', "'", all$cap[ind], fixed = TRUE)         # Replace " with '

rm(i, ind, spkr)

all <- select(all, name:episode, music, line:cap)                   # Rearrange variables



#----------# COMBINE SENTENCES

all$ln_beg <- all$line               # Initialize variables: beg & end "line"
all$ln_end <- all$line

all$cap1 <- all$cap                 # Initialize variables: Connected captions
all$cap2 <- NA
all$cap3 <- NA

for (i in 3:44888){
    if (grepl("[a-z]{1}$|,$", all$cap1[i]) &                # Line doesn't end
        !grepl("[a-z]{1}$|,$", all$cap1[i + 1]) &           # Next line ends
        !grepl("[a-z]{1}$|,$", all$cap1[i - 1])){           # Previous line ends
            all$cap2[i] <- all$cap1[i + 1]
            all$cap1[i + 1] <- NA
            all$ln_end[i] <- all$ln_beg[i + 1]
            all$ln_beg[i + 1] <- NA
            all$ln_end[i + 1] <- NA
    } else if (grepl("[a-z]{1}$|,$", all$cap1[i]) &         # Line doesn't end
               grepl("[a-z]{1}$|,$", all$cap1[i + 1]) &     # Next line doesn't end
               !grepl("[a-z]{1}$|,$", all$cap1[i + 2]) &    # Next line ends
               !grepl("[a-z]{1}$|,$", all$cap1[i - 1])){    # Previous line ends
            all$cap2[i] <- all$cap1[i + 1]
            all$cap3[i] <- all$cap1[i + 2]                  # Move lines to same row 
            all$cap1[c(i + 1, i + 2)] <- NA                 # Remove moved captions
            all$ln_end[i] <- all$ln_beg[i + 2]              # Update end "line"
            all$ln_beg[c(i + 1, i + 2)] <- NA
            all$ln_end[c(i + 1, i + 2)] <- NA               # Remove obs. "line" vals
    }
}

all[which(is.na(all$cap1)), "cap1"] <- ""
all[which(is.na(all$cap2)), "cap2"] <- ""
all[which(is.na(all$cap3)), "cap3"] <- ""                   # Converts NAs to ""

all <- all %>%
    mutate(sent = paste(cap1, cap2, cap3, sep = " "),       # Paste complete sentences
           sent = str_trim(sent, "both"),                   # Trim whitespace
           sent = str_replace(sent, " {2,}", " ")) %>%      # Remove 2+ spaces
    select(-cap1:-cap3)                                     # Rm cap1-3 vars

all[which(all$sent == ""), "sent"] <- NA                    # Empty strings to NA

all <- all[complete.cases(all), ]                           # Remove partial "cap" rows

rm(i)



#----------# PASTE SENTENCES & GROUP BY SEASON, EPISODE

grp <- all %>%
    group_by(season, episode, name) %>%
    summarize(dialogue = paste0(sent, collapse = " "))



#----------# WRITE TO TEXT FILES (.CSV)

write_csv(x = all, "got_all_lines.csv")
write_csv(x = grp, "got_episodes_merged.csv")