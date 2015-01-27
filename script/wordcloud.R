# Menghitung frequensi per kata
text = readLines("pidato-presiden.txt")
text = unlist(strsplit(text, "\\W+"))
text = tolower(text)
text = data.frame(table(text))

# Menghapus stop-words
stopw = readLines("stopwords-id.txt")
text = text[!is.element(text$text, stopw),]

# Generate word-cloud
wordcloud(text$text, text$Freq, random.order=FALSE, rot.per=0.25, colors=brewer.pal(8, "Dark2"))
