from PyPDF2 import PdfReader, PdfWriter

def make_filename(filename):
    return filename.replace('/','-') + ".pdf"

def extract_filename(page):
    text = page.extractText()
    lines = text.split()
    filename = next(filter(lambda line: line.startswith("BL-"), lines))
    return make_filename(filename)

def split(filename):
    reader = PdfReader(filename)
    for n, page in enumerate(reader.pages):
        writer = PdfWriter()
        writer.add_page(page)
        to = extract_filename(page)
        with open(to, "wb") as fp:
            writer.write(fp)
    
