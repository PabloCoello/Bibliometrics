from elsapy.elsclient import ElsClient
from elsapy.elsprofile import ElsAuthor, ElsAffil
from elsapy.elsdoc import FullDoc, AbsDoc
from elsapy.elssearch import ElsSearch

client = ElsClient("688f324bf0bbe4273de2fd6ef18593b3")
print(dir(client))

doc_srch = ElsSearch("AFFIL(dartmouth) AND AUTHOR-NAME(lewis) AND PUBYEAR > 2011",'scopus')
doc_srch.execute(client, get_all = True)
print ("doc_srch has", len(doc_srch.results), "results.")


aff_srch = ElsSearch('affil(ourense)','affiliation')
aff_srch.execute(client)
print ("aff_srch has", len(aff_srch.results), "results.")