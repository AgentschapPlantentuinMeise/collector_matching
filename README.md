# collector_matching

This R workflow extracts recordedBy strings from a Darwin Core archive and attempts to automatically match them to person items in Wikidata using a rule-based approach.

# Requirements

R 4.3 and Ruby 2.7 or higher need to be installed for the workflow to run. Dependencies should install automatically. 

# Data
Extract the contents of the Darwin Core archive to the data/gbif sets folder.

# Configuration
Edit the config.ini file to adjust parameters of the workflow. The following parameters are currently available:

`cores` : Set the number of cores to use for parallel processing. Set a low number such as 2 if you're not sure. You can also set a low number with a + and then the workflow will estimate the number of cores available on your system. Note that this does not check for availability of computational resources, so be conservative if you're not sure on availability.
`dwc_folder` : The folder where the occurrence.txt file can be found from the darwin core archive to process.
`columns` : A file listing the columns to extract from the Darwin Core source material. Don't edit this.
`wikifile` : Destination of where a cache of Wikidata metadata can be found, or should be saved.
`rmode` : Mode for the match validation. Can be "first", "best" or "all".
`output` : Format in which the matches should be exported. Can be "dwc_attribution" for the (unratified) Darwin Core Agents Attribution Extension, "fst" for a fast binary file format and "quickstatements" to quickly publish all unambiguous matches into Wikidata.
`institution_id` : When using the "quickstatements" export, a Wikidata item ID for the institution to link to should be specified.

# Run

To run the workflow, run the `run.R` script using either Rscript or an R IDE such as Rstudio.
