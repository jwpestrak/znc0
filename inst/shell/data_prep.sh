# This file contains Bash commands used outside of the R environment

# This command was used to split yoochoose-clicks.dta into separate files,
# each containing  lines only for a specific Session_ID value.
# stackoverflow.com/questions/9951393/split-large-csv-text-file-based-on-column-value
awk -F',' ' { print >> ($1".csv"") } ' yoochoose-clicks.dat
