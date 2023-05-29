# A Song of Reviews and Q&As
## Investigating the Role of Q&A Sections on Review Sentiment on Goodreads

User-generated content is becoming increasingly important in the buying process of consumers. In this study, the role of a Q&A section on Goodreads on review sentiment of books is studied. Based on literature, the following three hypotheses were formulated:

**H1**  *The presence of a Q&A section has a positive effect on the average sentiment of reviews.*\
**H2**	*The effect of the presence of a Q&A section on the average sentiment of reviews increases (decreases) as the number of answers on the featured question goes up (down).*\
**H3**	*The effect of the presence of a Q&A section on the average sentiment of reviews increases (decreases) as the number of likes on the featured question goes up (down).*

![](https://images.theconversation.com/files/45159/original/rptgtpxd-1396254731.jpg?ixlib=rb-1.1.0&q=45&auto=format&w=754&h=502&fit=crop)\
Source: [The 50 great books on education](https://theconversation.com/the-50-great-books-on-education-24934) by Dennis Hayes, 2014

The study uses data from Goodreads and includes data on questions, reviews, and related information. Using fixed effects regression models and a difference-in-difference analysis, the effect of having a Q&A section present is estimated. In the difference-in-difference analysis, reviews on Goodreads were considered as the treatment group while Amazon reviews were part of the control group.

The fixed effect regression models showed a negative relationship between having a Q&A and the average sentiment of reviews and difference-in-difference analysis suggested that an increase in question likes, lead to more positive reviews on Goodreads.

## Running instructions
### Necessities

R and R Studio [Installation Guide.](https://tilburgsciencehub.com/building-blocks/configure-your-computer/statistics-and-computation/r/)\
For required R packages, source code files where lines starting with `library` indicate the required packages.\
Various datasets with raw data. Contact [Shrabastee Banerjee](https://github.com/shrabasteebanerjee) for access to the following raw data files:

1) `2864_goodreads_com_book_full.csv`
2) `2864_web_archive_org_one_year_level.csv`
3) `qa_subset_amazon_text_merged.RDS`
4) `qa_subset_goodreads_text_merged.RDS`
5) `overlap_titles_amazon_gr.txt`


### Running the code
Clone the repository to your device\
Set working directory to the structure of your directory by using the `setwd()` function in R Studio\
Run the source code files for data preparation (located in the `src/dataprep` folder) in the following order:

1) `gr_questions_cleaning.R`
2) `gr_reviews_cleaning.R`
3) `am_reviews_cleaning.R`
4) `preparing_gr_and_am.R`
5) `gr_sentiment.R`
7) `am_sentiment.R`

Then, run the source code files for the analyses (located in the `src/analysis` folder) in the following order:

1) `summary_statistics.R`
2) `regression.R`

### Shortcut
If the cleaning and sentiment measurement processes are not of interest, unzip `sentiment.zip` located in the `gen/dataprep` folder to gain access to the sentiment files used for the analyses in `regression.R`. 
