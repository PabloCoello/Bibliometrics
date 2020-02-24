if(!require(readxl)){install.packages("readxl");library(readxl)}
if(!require(dplyr)){install.packages("dplyr");library(dplyr)}
if(!require(tidyr)){install.packages("tidyr");library(tidyr)}
if(!require(stringr)){install.packages("stringr");library(stringr)}
if(!require(bibliometrix)){install.packages("bibliometrix");library(bibliometrix)} # https://github.com/massimoaria/bibliometrix
if(!require(quanteda)){install.packages("quanteda");library(quanteda)}
if(!require(ggplot2)){install.packages("ggplot2");library(ggplot2)}
if(!require(lubridate)){install.packages("lubridate");library(lubridate)}
if(!require(topicmodels)){install.packages("topicmodels");library(topicmodels)}
if(!require(devtools)){install.packages("devtools");library(devtools)}
if(!require(RgScholar)){install_github('akshaynagpal/rgscholar');library(RgScholar)} # https://github.com/akshaynagpal/RgScholar
if(!require(scholar)){install.packages("scholar");library(scholar)} # https://www.rdocumentation.org/packages/scholar/versions/0.1.7
if(!require(rscopus)){install.packages("rscopus");library(rscopus)} # https://github.com/muschellij2/rscopus // https://www.rdocumentation.org/packages/rscopus/versions/0.6.6

## Revisar http://htmlpreview.github.io/?https://github.com/massimoaria/bibliometrix/master/vignettes/bibliometrix-vignette.html
# La idea es empezar cargando una primera revisión inicial hecha a mano con el código siguiente:

setwd("~/GitHub/data/bibliometrics")


bib <- readFiles("scopus.bib")
bib <- convert2df(bib, dbsource = "scopus", format = "bibtex")


results <- biblioAnalysis(bib, sep = ";")
S<- summary(object=results,k=20,pause=FALSE)

plot(x = results, k = 10, pause = FALSE)

NetMatrix <- biblioNetwork(bib, analysis = "coupling", network = "authors", sep = ";")
net=networkPlot(NetMatrix, normalize = "salton", weighted=T, n = 20, labelsize=0.5,curved=TRUE,Title = "A Co-citation Network of Authors", type = "kamada", size=TRUE,remove.multiple=TRUE)

CR <- citations(M, field = "article", sep = ";")
cbind(CR$Cited[1:10])

CR <- citations(M, field = "author", sep = ";")
cbind(CR$Cited[1:10])

# Con scholar se busca información de los Autores

#RgScholar. Se usa para buscar términos clave de la revisión original
query <- google_Scholar("heart rate") # HTTP error 429 -> too many requests

# rscopus (desde la USC)
options("elsevier_api_key" = '688f324bf0bbe4273de2fd6ef18593b3')
res = author_df(last_name = "Muschelli", first_name = "John", verbose = FALSE, general = FALSE)
names(res)

df <- scopus_search('multicriteria', api_key = '688f324bf0bbe4273de2fd6ef18593b3', count = 200,
                    view = c("COMPLETE"), start = 0, verbose = TRUE,
                    max_count = 200,
                    http = "https://api.elsevier.com/content/search/scopus",
                    headers = NULL, wait_time = 0)

ent = gen_entries_to_df(df$entries)
ent_df <- ent$df
af_df <- ent$affiliation
auth_df <- ent$author
cit_df <- entries_to_citation_df(df$entries)

citations = citation_retrieval(doi = ent_df$`prism:doi`)
citations = parse_citation_retrieval(citations)


# Métodos para plantilla:
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
x11()
plot(sum[[7]][which(sum[[7]]>0),])
table = sum[[7]]
table

cit = get_most_citated_publications(Bibliografia, n=10)
auth = get_most_citated_authors(Bibliografia, n=10)


```{r Scopus query, eval=FALSE, include=FALSE}
# Doc en https://dev.elsevier.com/documentation/FullTextEntilementAPI.wadl
# Api limits https://dev.elsevier.com/api_key_settings.html
df <- generic_elsevier_api(query = NULL, type = c("search", "article",
                                                  "entitlement", "recommendation", "object", "fragment", "abstract",
                                                  "affiliation", "embase", "author", "serial", "nonserial", "subject",
                                                  "holdings", "citation-count", "citations", "metadata", "ev",
                                                  "ev_records", "analytics"), search_type = c("affiliation", "author",
                                                                                              "scopus", "scidir", "scidir-object", "sciencedirect", "plumx"),
                           api_key = NULL, headers = NULL, content_type = c("content",
                                                                            "feedback"), root_http = "https://api.elsevier.com", http_end = NULL,
                           verbose = TRUE, api_key_error = TRUE, ...)
df <- scopus_search(query, api_key = NULL, count = 200,
                    view = c("STANDARD", "COMPLETE"), start = 0, verbose = TRUE,
                    max_count = 20000,
                    http = "https://api.elsevier.com/content/search/scopus",
                    headers = NULL, wait_time = 0, ...)
```