rm(list=ls())
getwd()

library(tinytex)

latexmk("./manuscript/main.tex", clean = TRUE, pdf_file = "./manuscript/SMRs in the UK-draft.pdf")
