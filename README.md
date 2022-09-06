# Quantification
Stata and R code for ITN Quantification project, multi-country

# Scripts
Stata scripts are 01, 02, 03 - these create the datasets with indicative populations, distribute the nets per different scenarios, decay them. The decay script in 02 works within script 01.

R scripts 04, 05, 06:
- 04 is full of exploratory maps, but also has the tables used in the current draft of the paper.

- 05 produces the full set of geofaceted graphs for each scenario result; it is Supplemental File 1. Takes a while to run. Some graph titles are cut off and I need to fix the size of that giant logo!

- 06 creates the maps of where school distribution is feasible based on % population that are currently-attending primary school students from DHS. This relies on a Stata file that is not included here... 

# Paper
Paper (quant_pword) is written with R Markdown with MS Word output. Much of the script content in 04-06 has been transferred into this file. Tables are built with flextable to facilitate Word output. A Word template file (drdanholmes-ref-word.docx) serves to guide the styles used for the paper (size, font, captions, line spacing, etc). Bibliography file is located in quant_paper/quantbibfile.bib, using bibtex.

# Underlying data
Underlying data is not kept in this repo, mainly due to my existing file structures and workflows. Please contact me if you're interested in working with data from this project, however.
