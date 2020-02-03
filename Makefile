# Search path
VPATH = scripts data data-raw docs eda reports

# Processed data files
DATA = acquisitions.rds all_cetaceans_data.rds

# EDA studies
EDA = progress-report-1.md

# Reports
REPORTS =

# All targets
all : $(DATA) $(EDA) $(REPORTS)

# Data dependencies
acquisitions.rds: acquisitions.csv
all_cetaceans_data.rds : all_cetaceans_data.csv

# EDA study and report dependencies
progress-report-1.md : acquisitions.rds all_cetaceans_data.rds

# Pattern rules
%.rds : %.R
	Rscript $<
%.md : %.Rmd
	Rscript -e 'rmarkdown::render(input = "$<", output_options = list(html_preview = FALSE))'
