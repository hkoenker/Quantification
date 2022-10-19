# Quantification
Stata and R code for ITN Quantification project, multi-country

# Scripts

Scripts are numbered and include both R and Stata scripts (#sorrynotsorry). 

- 01 - R Script - creates a file used to convert nets-per-capita to ITN access in later scripts

- 02 - Stata Do File - loads country retention times and creates hypothetical populations of 10million, with population growth over several years, then "distributes" ITNs in six different strategies. Each strategy is comprised of numerous iterations using different quantification factors. Each iteration is saved as a .dta file.

- 03 - Stata Do File - used within 02 to decay the ITN crops according to the Chitnis function.

- 04 - Stata Do File - appends all the iteration .dta files together and then runs code to identify the 'best' quantifier for each ITN strategy scenario for each country (i.e. lowest population x X% quantifier for continuous distribution strategies; highest population/Y quantifier for the mass campaign scenarios).

- 05 - R Markdown file for the manuscript, output to Microsoft Word, pulling from files created above and other files in the data folder to produce all the analysis and plots for the paper, including supplemental tables and figures, but not Supplemental File 1 or Supplemental File 2. Tables are built with flextable to facilitate Word output. A Word template file (drdanholmes-ref-word.docx) serves to guide the styles used for the paper (size, font, captions, line spacing, etc). Bibliography file is located in quant_paper/quantbibfile.bib, using bibtex.

- 06 - R Markdown file that produces the full set of geofaceted graphs for each scenario result; it is Supplemental File 1. Takes a while to run. It also saves copies of the plots as pdfs.

- 07 - R Markdown file that produces the full set of frontier plots (total person-years of ITN access vs total nets required, for each of scenarios, showing only the iteration within each scenario that provides the most person-years of ITN access for the fewest ITNs). It also saves copies of the plots as pdfs.


# Underlying data
Underlying data is not kept in this repo, mainly due to my existing file structures and workflows. Please contact me if you're interested in working with data from this project, however.
