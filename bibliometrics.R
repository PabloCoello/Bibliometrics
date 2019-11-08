library(readxl)
library(tidyr)
library(stringr)

setwd("~/GitHub/data")
Bibliografia <- read_excel("Bibliografia.xlsx")

get_author_list = function(x){
  authors = strsplit(Bibliografia$Autores, '.,', fixed = TRUE)
  authors = lapply(authors, str_remove_all, pattern = "[& ]")
  for (i in 1:length(authors)){
    for (j in 1:length(authors[[i]])){
      if (substring(authors[[i]][j], nchar(authors[[i]][j])) != '.'){
        authors[[i]][j] = paste(authors[[i]][j], '.', sep = '')
      }else{}
    }
  }
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

get_most_citated_authors = function(x, n){
  author = unique(unlist(get_author_list(x)))
  author_citations = array(length(author))
  for (i in 1:length(author)){
    pos = array(nrow(x))
    for (j in 1:nrow(x)){
      if (any(author[i] %in% authors[[j]])){
        pos[j] = j
      }else{
        pos[j] = NA
      }
    }
    pos = pos[!is.na(pos)]
    author_citations[i] = sum(x$Citas[pos])
  }
  df = cbind.data.frame(author, author_citations)
  df = arrange(df, desc(author_citations))
  print(df[1:n,])
  return(df)
}

sum= ref_summary(Bibliografia)
authors = get_author_list(Bibliografia)

#Plot publicaciones por año
plot(sum[[7]][which(sum[[7]]>0),])

  cit = get_most_citated_publications(Bibliografia, n=10)
auth = get_most_citated_authors(Bibliografia, n=10)
