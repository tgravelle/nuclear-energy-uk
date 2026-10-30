rm(list=ls())
getwd()

library(tinytex)

latexmk("./manuscript/main.tex", clean = TRUE, pdf_file = "./manuscript/SMRs in the UK-draft.pdf")

latexmk("./ep-submission/title_page.tex", clean = TRUE, pdf_file = "./ep-submission/title_page.pdf")
latexmk("./ep-submission/highlights.tex", clean = TRUE, pdf_file = "./ep-submission/highlights.pdf")
latexmk("./ep-submission/manuscript_anon.tex", clean = TRUE, pdf_file = "./ep-submission/manuscript_anon.pdf")
latexmk("./ep-submission/appendix_tables.tex", clean = TRUE, pdf_file = "./ep-submission/appendix_tables.pdf")

