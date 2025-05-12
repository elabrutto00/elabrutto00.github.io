################################################################################
#
# textminingproject.R
#
################################################################################
# External Functions
################################################################################
library(readtext)
library(tm)                #Text mining
library(smallstuff)
library(phm)               #Phrase mining
library(data.table)
library(magrittr)          #The pipe
library(wordcloud)
library(wordcloud2)
library(tidytext)
library(ggplot2)
library(dplyr)
library(gt)  
library(gtExtras)
library(RColorBrewer)
################################################################################
# Internal Functions
################################################################################
################################################################################
# Save the environment
################################################################################
parSave=par(no.readonly = TRUE)
#par(parSave)
################################################################################
# Processing
################################################################################

#What can we gather from our documents with regards to ADHD in adults?

#Code will be organized by numbers, like a homework assignment

#1
#a
#loading our documents into R
pm = getPubMed("../tmData/pubmed-adhdadults-set.txt")

#b
#creating a corpus
co = pm %>% DFSource %>% VCorpus

#c
#checking out first 5 document contents
lapply(co, content) %>% head(5)

#d
#looking at meta info
lapply(co, meta) %>% head(5)
#the meta info for our docs include important things such as author, datetimestamp,
#ID(as well as a PMID), and a title

#2
#a
#creating a phrasedoc of our corpus
pd = phraseDoc(co)
pdm = pd %>% as.matrix(ids = F)

#b
(originalfp = freqPhrases(pd, n = 20))
#here are the 20 most frequent phrases. top phrase is "95% CI" which is 
#interesting, we can infer a strong statistical overlay on our corpus

#we will now input some more parameters, getting rid of some phrases that are too
#general

#c
pd = removePhrases(pd, c("adhd", "attention deficit hyperactivity disorder",
                         "attention-deficit/hyperactivity disorder",
                         "attention-deficit hyperactivity disorder"))

(newfp = freqPhrases(pd, n = 30))
#these results are more interesting. in order to gather info related to adults 
#with adhd, we will look further into some results specifying adulthood

#d
#barplot of top 5 phrases
graph1 = newfp[1:6, 1]
barplot(graph1, col = 2:7, xlab = "Phrases", ylab="Frequency",
        main="6 Most Frequent Phrases")

#d
adult = "adolescents and adults with adhd"
(gd = getDocs(pd, adult, ids=F))

#e
getPhrases(pd, c(146, 276, 500, 509), ids = F)
#doc 276 has some interesting phrases like "genetic liability etc. lets look at this one

meta(co[[276]])
#title is "Neurological and psychiatric disorders among autistic adults: 
#a population  healthcare record study."

content(co[[276]])
#"All  psychiatric conditions examined were more common amongst adults with autism 
#after  adjusting for age, sex and deprivation. Prevalence of attention-deficit  
#hyperactivity disorder (7.00%)..."



#doc 146 has 4 instances of the phrase "adolescents with adhd" lets look at that

meta(co[[146]])
#title is "Psychiatric Comorbidities Associated With Keratoconus."

content(co[[146]])
#"OBJECTIVE: To assess the risk for keratoconus associated  with psychiatric 
#comorbidities in adolescents and adults.", "In a large cohort  of adolescents 
#and adults, ADHD was associated with a diagnosis of keratoconus in  male patients, 
#even after adjusting for possible confounders. Although a  causative effect could not 
#be ascribed, these findings support further  investigation into the potential value 
#of education regarding eye rubbing in this  population."



#both of these docs are about comorbidities in adults with adhd. lets see if this
#is a popular theme in our corpus

#f
(gd2 = getDocs(pd, "mental health comorbidity", ids = F))
#these docs have the phrase "mental health comorbidity" lets see their titles

#creating a new corpus with these docs
idx=as.numeric(colnames(gd2))
(co2=co[idx])


#g
#their titles
lapply(co2,meta,"title") %>% unlist %>% unname

#3
#a
#term document matrix
tdm=TermDocumentMatrix(co2, control=list(removePunctuation=T,
                                        removeNumbers=T,
                                        bounds=list(local=c(3,Inf)), # at least 3 times in each docs
                                        stopwords=T)) %>%
  as.matrix


sums=rowSums(tdm)
head(sort(sums,decr=T))
table(sums)
myCol=c("black",          "blueviolet",     "brown1",         "brown4",
        "chartreuse2",    "darkgoldenrod4", "darkgreen",      "deeppink3",
        "green4",         "mediumblue",     "mediumorchid3",  "mediumpurple",
        "mediumpurple4",  "mediumseagreen", "olivedrab",      "orange2",
        "palevioletred4", "red3",           "royalblue4",     "violet",
        "yellow4",        "red" )

#b
# Sentiment words
#Data table for sums
dt=data.table(word=names(sums),freq=sums)
#Store the nrc lexicon in a data table
nrc=as.data.table(get_sentiments("nrc"))
#Create data tables for certain sentiments
coFear=dt[nrc,on=.(word),nomatch=NULL][sentiment=='fear']
coJoy=dt[nrc,on=.(word),nomatch=NULL][sentiment=='joy']
coPos=dt[nrc,on=.(word),nomatch=NULL][sentiment=='positive']
coNeg=dt[nrc,on=.(word),nomatch=NULL][sentiment=='negative']

#c
par(mfrow=c(2,2))

#Primary word clouds
#All fear words
set.seed(101L)
wordcloud(coFear$word,coFear$freq,min.freq=1,colors=myCol,random.order=F)
title("Fear Words",cex.main=2)
#All joy words
set.seed(101L)
wordcloud(coJoy$word,coJoy$freq,min.freq=1,colors=myCol,random.order=F)
title("Joy Words",cex.main=2)
#All positive words
set.seed(101L)
wordcloud(coPos$word,coPos$freq,min.freq=1,colors=myCol,random.order=F,scale=c(3,.4))
title("Positive Words",cex.main=2)
#All negative words
set.seed(101L)
wordcloud(coNeg$word,coNeg$freq,min.freq=1,colors=myCol,random.order=F)
title("Negative Words",cex.main=2)
par(parSave)



combined_df <- rbind(coFear, coJoy, coPos, coNeg)

# View(combined_df)

count_data <- combined_df %>%
  group_by(sentiment) %>%
  summarise(TotalFrequency = sum(freq)) %>%
  arrange(desc(TotalFrequency)) 

#d
# View(count_data)

# Heat Map 
gt(count_data) %>%
  # Add a title and subtitle
  tab_header(
    title = 'Word Frequencies by Sentiment'
  ) %>%
  # Add a footnote (to denote the source of the data)
  tab_footnote(
    footnote = 'Source: Pubmed'
  ) %>%
  # Customization 
  tab_options(
    heading.title.font.size = 22, 
    footnotes.font.size = 12
  ) %>%
  # Change the theme of the visualization
  gt_theme_538() %>%
  # Add a heatmap to the average rating column
  data_color(
    columns = 'TotalFrequency', 
    palette = 'Blues', 
    method = 'numeric',
    quantiles = 4
  ) %>%
  # Style the word "Negative" to be bolded
  tab_style(
    style = list(cell_text(weight = 'bold')),
    locations = cells_body(
      columns = "sentiment", 
      rows = sentiment == 'negative'
    )
  )

#4
#a
#phrase doc for more specific corpus
pd2 = phraseDoc(co2)
pdm2 = pd2 %>% as.matrix(ids = F)

#b
#creating a wordcloud
sums2 = rowSums(pdm2)

set.seed(123L)
wc = wordcloud(names(sums2),sums2,min.freq=2,colors=myCol,
               random.order=F,scale=c(2.5,.2))
title(main = "ADHD Comorbidities")


par(parSave)
