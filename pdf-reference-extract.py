import re
import PyPDF2

with open("paper_example.pdf", mode="rb") as file:
    pdf_reader = PyPDF2.PdfFileReader(file)
    text = []
    for i in range(pdf_reader.numPages):
        page = pdf_reader.getPage(i)
        text.append(page.extractText())


print(text)