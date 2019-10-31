library(readxl)

setwd("~/GitHub/data")
Bibliografia <- read_excel("Bibliografia.xlsx")

ref_summary = function(x){
  articles = nrow(x)
  sources = length(unique(x$Revista))
  period = paste(min(x$Año), '-', max(x$Año))
  print(paste('Number of articles:', articles))
  print(paste('Number of sources:', sources))
  print(paste('Time period:', period))
  return(list(articles, sources, period))
}

sum= ref_summary(Bibliografia)
