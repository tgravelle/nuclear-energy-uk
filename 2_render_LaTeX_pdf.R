rm(list=ls())
getwd()

library(tinytex)

latexmk("./manuscript/main.tex", clean = TRUE, pdf_file = "./manuscript/SMNRs in the UK-draft.pdf")