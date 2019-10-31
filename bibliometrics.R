library(readxl)
library(tidyr)
library(stringr)

setwd("~/GitHub/data")
Bibliografia <- read_excel("Bibliografia.xlsx")

get_author_list = function(x){
  authors = strsplit(Bibliografia$Autores, '.,', fixed = TRUE)
  authors = lapply(authors, str_remove_all, pattern = "[& ]")
  return(authors)
}

ref_summary = function(x){
  articles = nrow(x)
  sources = length(unique(x$Revista))
  period = paste(min(x$Año, na.rm = TRUE), '-', max(x$Año))
  citations = mean(x$Citas, na.rm = TRUE)
  authors = get_author_list(Bibliografia)
  num_authors = length(unique(unlist(authors)))
  
  print(paste('Number of articles:', articles))
  print(paste('Number of sources:', sources))
  print(paste('Time period:', period))
  print(paste('Average citations:', citations))
  print(paste('Number authors:', num_authors))
  return(list(articles, sources, period, citations))
}

sum= ref_summary(Bibliografia)
authors = get_author_list(Bibliografia)

length(unique(unlist(authors)))
