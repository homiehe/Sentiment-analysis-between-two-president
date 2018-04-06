library(tidytext)
library(tidyverse)
library(dplyr)
library(stringr)
library(ggplot2)
library(wordcloud2)

multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {  
  library(grid)  
  
  # Make a list from the ... arguments and plotlist  
  plots <- c(list(...), plotlist)  
  
  numPlots = length(plots)  
  
  # If layout is NULL, then use 'cols' to determine layout  
  if (is.null(layout)) {  
    # Make the panel  
    # ncol: Number of columns of plots  
    # nrow: Number of rows needed, calculated from # of cols  
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),  
                     ncol = cols, nrow = ceiling(numPlots/cols))  
  }  
  
  if (numPlots==1) {  
    print(plots[[1]])  
    
  } else {  
    # Set up the page  
    grid.newpage()  
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))  
    
    # Make each plot, in the correct location  
    for (i in 1:numPlots) {  
      # Get the i,j matrix positions of the regions that contain this subplot  
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))  
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,  
                                      layout.pos.col = matchidx$col))  
    }  
  }  
}  
# 1. word analysis

# trump text
#####
trump <- scan("trump first state of the union.txt", "")
trump
trump_df <- data_frame(line = 1:5141, text = trump)
trump_df

trump_text <- trump_df %>%
  unnest_tokens(word, text)

data(stop_words)

trump_text1 <- trump_text %>%
  anti_join(stop_words)

trump_text2 <- trump_text1 %>%
  count(word, sort = TRUE) 

trump_text2

View(trump_text2)

trump_text3 <- trump_text2[c(-1,-7,-1294:-1301),]

wordcloud2(trump_text3)
#####



# obama text
#####
obama<-scan("obama first state of the union.txt", "")
obama
obama_df <- data_frame(line = 1:5858, text = obama)
obama_df

obama_text <- obama_df %>%
  unnest_tokens(word, text)

data(stop_words)

obama_text1 <- obama_text %>%
  anti_join(stop_words)

obama_text2 <- obama_text1 %>%
  count(word, sort = TRUE) 

View(obama_text2)

obama_text3 <- obama_text2[-1,]

wordcloud2(obama_text3)
#####

# 1.1 word frequency

# word frequency of trump
p1<-trump_text3 %>%
  filter(n > 7) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) +
  geom_col() +
  xlab("Trump") +
  coord_flip()

p1
#word frequency of obama 
p2<-obama_text3 %>%
  filter(n > 7) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) +
  geom_col() +
  xlab("Obama") +
  coord_flip()

p2
# compare the word frequency of those two president.
multiplot(p1,p2)

# comment: The graph shows that both president willing to use words like America, people, country and tonight. 
#          However, Obama use more specific words like health, eduaction, jobs, energy than Trump.  
#
#          Also, we see more diversity in Obama's speech than Trump, from a higher density in Obama's word
#          frequency.
#
#          This also shows the change in what matters to American people , from education, health and crsis 
#          in Obama time to immigration and workers in Trump time. 
          

# 1.2 repetitive rate analysis between trump and obama

frequency1 <- bind_rows(mutate(trump_text3, president = "trump"),
                       mutate(obama_text3, president = "obama")) %>% 
  count(word , president) %>%
  group_by(president) %>%
  mutate(proportion = nn / sum(nn)) %>% 
  select(-nn) %>% 
  spread(key = president, value = proportion)

frequency1

frequency1[is.na(frequency1)] <- 0

# make the comparison between first state of the union of obama and trump 

ggplot(frequency1, aes(x = obama, y = trump)) +
  geom_abline(color = "gray40", lty = 2) +
  geom_jitter(alpha = 0.1, size = 2.5, width = 0.3, height = 0.3) +
  geom_text(aes(label = word), check_overlap = TRUE, vjust = 1.5) +
  scale_x_log10(labels = percent_format()) +
  scale_y_log10(labels = percent_format()) +
  scale_color_gradient(limits = c(0, 0.001), low = "darkslategray4", high = "gray75") +
  theme(legend.position = "none")

# we dont get much useful information here.

## 2 . sentiment analysis

# create the whole table for president.
president <- rep('obama',2231)
obama_text_s1 <- cbind(president,obama_text1)

president <- rep('trump',2305)
trump_text_s1 <- cbind(president,trump_text1)

obama_text_s1
trump_text_s1

total_text <- rbind(obama_text_s1,trump_text_s1) %>% rename(index=line) %>% as.tibble()

total_text

# create the sentiment table for both president

# 2.1 positive & negative analysis
president_sentiment <- total_text %>%
  inner_join(get_sentiments("bing")) %>%
  count(president, index = index %/% 40, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative)

ggplot(president_sentiment, aes(index, sentiment, fill = president)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ president, ncol = 2, scales = "free_x")

# From the above graph, we can see that Trump use more postive words than Obama, especially in the end of his
# speech, with its value raise to 7.5, a number that Obama never reached.

# 2.2 nrc analysis
president_sentiment_nrc <- total_text %>%
  inner_join(get_sentiments("nrc")) %>%
  count(president, index = index %/% 40, sentiment) %>%
  spread(sentiment, n, fill = 0) 

# 2.2.1 joy analysis

ggplot(president_sentiment_nrc, aes(index, joy, fill = president)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ president, ncol = 2, scales = "free_x")

#From the graph above, we can see that Trump shows much more joy than Obama in the speech, especially in the 
#bottom half

# 2.2.2 anger analysis

ggplot(president_sentiment_nrc, aes(index, anger, fill = president)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ president, ncol = 2, scales = "free_x")

#From the graph above, we can see that Trump shows more anrgy than Obama in the speech, from the half of the 
#speech, while both of them calm down in the end.

# 2.2.3 anticipation analysis

ggplot(president_sentiment_nrc, aes(index, anticipation, fill = president)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ president, ncol = 2, scales = "free_x")

#Obama and Trump's anticipation seems similar in their speech.