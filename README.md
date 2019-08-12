# Game of Thrones Dialogue Analysis

## Overview

This work organizes and analyzes dialogue across seven seasons of the hit HBO series, [Game of Thrones](https://en.wikipedia.org/wiki/Game_of_Thrones), based on the book series, [A Song of Ice and Fire](https://en.wikipedia.org/wiki/A_Song_of_Ice_and_Fire) by [George R. R. Martin](https://en.wikipedia.org/wiki/George_R._R._Martin).

## Methods

This approach uses a bag-of-words model for both frequency counts and sentiment analysis for each word in the series' dialogue. The [AFINN](https://github.com/fnielsen/afinn) and [NRC](https://saifmohammad.com/WebPages/NRC-Emotion-Lexicon.htm) sentiment lexicons to evaluate each words with positive, negative, or more specific emotional connotations.

## Data Sources

Data were retrieved from Kaggle's [Game of Thrones Subtitles](https://www.kaggle.com/gunnvant/game-of-thrones-srt) dataset and are available in JSON format. Documentation describes the dataset as "every line from every season" (Saini, Gunnvant 2018).

Notably, the raw data are not provided in the correct order, but are still organized by episode and season (ibid.).

## Works Cited

**Finn, Arup (2015).** "AFINN Sentiment Lexicon". *GitHub*. Retrieved on 11 August 2019 from [here](https://github.com/fnielsen/afinn). 

**Saif, Mohammad (2013).** "NRC Word-Emotion Association Lexicon". Retrieved on 11 August 2019 from [here](https://saifmohammad.com/WebPages/NRC-Emotion-Lexicon.htm).

**Saini, Gunnvant (2018).** "Game of Thrones Subtitles". *Kaggle*. Retrieved on 9 August 2019 from [here](https://www.kaggle.com/gunnvant/game-of-thrones-srt).
