#This is my week 8 in class work r Script.
## we'll be going over working with Text today

install.packages('tidytext')
install.packages('wordcloud2')
install.packages('janeaustenr')
install.packages('stopwords')

#Libraries to load


library(here)
library(tidyverse)
library(tidytext)
library(wordcloud2)
library(janeaustenr)


words<-"This is a string"
words

## [1] "This is a string"
# You can also have several strings in a vector.

words_vector<-c("Apples", "Bananas","Oranges")
words_vector

paste("High temp", "Low pH")
## [1] "High temp Low pH"

#Add a dash in between the words

paste("High temp", "Low pH", sep = "-")
## [1] "High temp-Low pH"

#Remove the space in between the words

paste0("High temp", "Low pH")

shapes <- c("Square", "Circle", "Triangle")
paste("My favorite shape is a", shapes)

two_cities <- c("best", "worst")
paste("It was the", two_cities, "of times.")

shapes # vector of shapes

str_length(shapes) # how many letters are in each word?

seq_data<-c("ATCCCGTC")
str_sub(seq_data, start = 2, end = 4) # extract the 2nd to 4th AA

str_sub(seq_data, start = 3, end = 3) <- "A" # add an A in the 3rd position
seq_data

str_dup(seq_data, times = c(2, 3)) # times is the number of times to duplicate

badtreatments<-c("High", " High", "High ", "Low", "Low")
badtreatments

str_trim(badtreatments) # this removes both

str_trim(badtreatments, side = "left") # this removes left

str_pad(badtreatments, 5, side = "right") # add a white space to the right side

str_pad(badtreatments, 5, side = "right", pad = "1") # add a 1 to the right side

x<-"I love R!"
str_to_upper(x)

str_to_lower(x)

str_to_title(x)

data<-c("AAA", "TATA", "CTAG", "GCTT")
# find all the strings with an A
str_view(data, pattern = "A")

str_detect(data, pattern = "A")

str_detect(data, pattern = "AT")

str_locate(data, pattern = "AT")

vals<-c("a.b", "b.c","c.d")

#string, pattern, replace
str_replace(vals, "\\.", " ")

vals<-c("a.b.c", "b.c.d","c.d.e")
#string, pattern, replace
str_replace(vals, "\\.", " ")
#str_replace only replaces the first instance. Let's try str_replace_all()
#string, pattern, replace
str_replace_all(vals, "\\.", " ")

val2<-c("test 123", "test 456", "test")
str_subset(val2, "\\d")


str_count(val2, "[aeiou]")

str_count(val2, "[0-9]")

strings<-c("550-153-7578",
           "banana",
           "435.114.7586",
           "home: 672-442-6739")

phone <- "([2-9][0-9]{2})[- .]([0-9]{3})[- .]([0-9]{4})"

# Which strings contain phone numbers?
str_detect(strings, phone)

# subset only the strings with phone numbers
test<-str_subset(strings, phone)
test
str_replace_all(test, "\\.|\\-| |[a-z]|:", "-") 

str_replace(test, ".*?([2-9][0-9]{2})[- .]([0-9]{3})[- .]([0-9]{4}).*", "\\1-\\2-\\3")

# explore it
head(austen_books())

tail(austen_books())

original_books <- austen_books() %>% # get all of Jane Austen's books
  group_by(book) %>%
  mutate(line = row_number(), # find every line
         chapter = cumsum(str_detect(text, regex("^chapter [\\divxlc]", # count the chapters (starts with the word chapter followed by a digit or roman numeral)
                                                 ignore_case = TRUE)))) %>% #ignore lower or uppercase
  ungroup() # ungroup it so we have a dataframe again
# don't try to view the entire thing... its >73000 lines...
head(original_books)

tidy_books <- original_books %>%
  unnest_tokens(output = word, input = text) # add a column named word, with the input as the text column
head(tidy_books) # there are now >725,000 rows. Don't view the entire thing!

#see an example of all the stopwords
head(get_stopwords())

cleaned_books <- tidy_books %>%
  anti_join(get_stopwords()) # dataframe without the stopwords
head(cleaned_books)

cleaned_books %>%
  count(word, sort = TRUE)

sent_word_counts <- tidy_books %>%
  inner_join(get_sentiments()) %>% # only keep pos or negative words
  count(word, sentiment, sort = TRUE) # count them
head(sent_word_counts)[1:3,]

sent_word_counts %>%
  filter(n > 150) %>% # take only if there are over 150 instances of it
  mutate(n = ifelse(sentiment == "negative", -n, n)) %>% # add a column where if the word is negative make the count negative
  mutate(word = reorder(word, n)) %>% # sort it so it gows from largest to smallest
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col() +
  coord_flip() +
  labs(y = "Contribution to sentiment")

words<-cleaned_books %>%
  count(word) %>% # count all the words
  arrange(desc(n))%>% # sort the words
  slice(1:100) #take the top 100
wordcloud2(words, shape = 'triangle', size=0.3) # make a wordcloud out of the top 100 words

