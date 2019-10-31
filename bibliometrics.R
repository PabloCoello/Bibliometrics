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
  appear_authors = length(unlist(authors))
  
  year = x %>% 
    group_by(Año) %>% 
    summarize(count = n())
  
  print(paste('Number of articles:', articles))
  print(paste('Number of sources:', sources))
  print(paste('Time period:', period))
  print(paste('Average citations:', citations))
  print(paste('Number authors:', num_authors))
  print(paste('Authors appearances:', appear_authors))

  return(list(articles, sources, period, citations, num_authors,
              appear_authors, year))
}

get_most_citated_publications = function(x, n){
  cit = sort(x$Citas, decreasing = TRUE)
  cit = cit[c(1:n)]
  ref = x[which(x$Citas %in% cit),]
  print(ref[,2])
  return(ref)
}

sum= ref_summary(Bibliografia)
authors = get_author_list(Bibliografia)

#Plot publicaciones por año
plot(sum[[7]][which(sum[[7]]>0),])

get_most_citated_publications(Bibliografia, n=10)

