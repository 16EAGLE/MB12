# reading line by line
poll <- readLines("poll.md")
typeof(poll) # find out mode or type
dim(poll)
length(poll)

# index by element
poll[1]
poll

# filter / exclude empty elements
poll <- poll[poll != ""]

# convert this vector into data frame
df <- as.data.frame(matrix(poll, byrow = T, ncol = 4))
df
dim(df)
colnames(df) <- c("name", "exp", "lang", "os")
colnames(df)

View(df)

# clean names
df$name <- gsub("#", "", df$name)
df$name <- trimws(df$name)

# clean os
df$os <- gsub("[*]", "", df$os)
df$os <- trimws(df$os)

# clean exp
df$exp <- gsub("[*]", "", df$exp)
df$exp <- trimws(df$exp)
df$exp[df$exp == "0-5"] <- "2.5"
df$exp <- as.numeric(df$exp)

# clean lang
df$lang <- gsub("[*]", "", df$lang)

# this is dangerous, since hard-coded: if the file changes, code breaks
df$lang[4] <- "Python, JavaScript"
df$lang[11] <- "R-Studio"

df$lang <- trimws(df$lang)
df$lang[df$lang == "no one"] <- "None"
df$lang <- gsub("a little bit of ", "", df$lang)
# make everything lowe case
df$lang <- tolower(df$lang)

# Task 1: 
# how can we make a vector from df$lang that contains each
# language mentioned as a single element? e.g.
# python, r, javascript, rstudio, r, python, java
df$lang
lang <- gsub(" ", "", unlist(strsplit(df$lang, ", ")))
lang_count <- table(lang)
names(lang_count)


# Task 2:
# install the package wordcloud, load it
# and if you can, try to make a wordcloud from out language vector from task 1
#wordcloud::wordcloud(words = lang, min.freq = 1, scale = c(3, 0.7))
wordcloud::wordcloud(names(lang_count), freq = lang_count, min.freq = 1)

