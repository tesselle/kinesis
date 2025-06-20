Sys.setenv(LANGUAGE = "en") # Force locale

library("shiny")
using("tinysnapshot")

link_info_year <- kinesis:::info_article(author = "Filzmoser et al.", year = "2009")
expect_snapshot_print(link_info_year, label = "info_article_year")

link_info_doi <- kinesis:::info_article(author = "Filzmoser et al.", year = "2009",
                                        doi = "10.1016/j.scitotenv.2009.08.008")
expect_snapshot_print(link_info_doi, label = "info_article_doi")
