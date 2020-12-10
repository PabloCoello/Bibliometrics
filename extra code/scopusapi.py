from elsapy.elsclient import ElsClient
from elsapy.elsprofile import ElsAuthor, ElsAffil
from elsapy.elsdoc import FullDoc, AbsDoc
from elsapy.elssearch import ElsSearch
import scholarly

client = ElsClient("688f324bf0bbe4273de2fd6ef18593b3")
keywords = 'SFA + DEA'

def get_search(keywords, client):
    doc_srch = ElsSearch(keywords,'scopus')
    doc_srch.execute(client, get_all = True)
    print ("doc_srch has", len(doc_srch.results), "results.")
    df = doc_srch.results_df
    return(df)

df = get_search(keywords, client)

search_query = scholarly.search_keyword('SFA')

def get_citated_by_list(df):
    pub = []
    for i in range(len(df)):
        search_query = scholarly.search_author(df['dc:creator'][i])
        author = next(search_query).fill()
        pub.append(author.publications[author.publications == df['dc:title'][i]].fill())
    return(pub)

citations = get_citated_by_list(df)


search_query = scholarly.search_pubs_query(df['dc:title'][0])
doc = next(search_query).fill()
search_query.get_citedby()


# Retrieve the author's data, fill-in, and print
search_query = scholarly.search_author(df['dc:creator'][0])
author = next(search_query).fill()
print(author)

# Print the titles of the author's publications
print([pub.bib['title'] for pub in author.publications])

# Take a closer look at the first publication
pub = author.publications[author.publications == df['dc:title'][0]].fill()
print(pub)

# Which papers cited that publication?
print([citation.bib['title'] for citation in pub.get_citedby()])