# The Game of Books
## Investigating the Role of Q&A Sections on Review Sentiment on Goodreads

User-generated content is becoming increasingly important in the buying process of consumers. In this study, the role of a Q&A section on Goodreads on review sentiment of books is studied. Using fixed effects regression models and a difference-in-difference analysis, the effect of having a Q&A section present is estimented. 

## Running instructions
### Necessities

R and R Studio [Installation Guide.](https://tilburgsciencehub.com/building-blocks/configure-your-computer/statistics-and-computation/r/)\
For required R packages, source code files where lines starting with `library` indicate the required packages.
Various datasets on Goodreads and Amazon data. Contact [Shrabastee Banerjee](https://github.com/shrabasteebanerjee) for access to the following files:

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

Then, run the source code files for the analyses in the following order:

1) `summary_statistics.R`
2) `regression.R`
