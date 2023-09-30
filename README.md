# collector_matching

This R workflow extracts recordedBy (or other) strings from a Darwin Core archive and attempts to automatically match them to person items in Wikidata using a rule-based approach. The workflow is designed in a modular and configurable manner, so that it can be adapted for other data models or use cases if desired. It also makes use of parralel processing to compensate for the fairly slow rule-based approach.

# Requirements

[R](https://www.r-project.org/) 4.3 and [Ruby](https://www.ruby-lang.org) 2.7 or higher need to be installed for the workflow to run. Dependencies within these languages should install automatically. 

# Data
Extract the contents of the Darwin Core archive to be processed to the `data/gbif sets` folder. Different paths can be specified in the `config.ini` file if needed.

# Configuration
Edit the config.ini file to adjust parameters of the workflow. The following parameters are currently available:

`cores` : Set the number of cores to use for parallel processing. Set a low number such as 2 if you're not sure. You can also set a low number with a + and then the workflow will estimate the number of cores available on your system. Note that this does not check for availability of computational resources, so be conservative if you're not sure on availability.

`dwc_folder` : The folder where the occurrence.txt file can be found from the darwin core archive to process.

`dwc_property` : The property (column name) in the occurrence.txt file from where the strings will be taken.

`columns` : A file listing the columns to extract from the Darwin Core source material.

`wikifile` : Destination of where a cache of Wikidata label data can be found, or should be saved.

`rmode` : Mode for the match validation. Can be "best" to only return the highest scored match, "cut" to return up to a certain number of matches or "all" to return all possible matches.

`output` : Format in which the matches should be exported. Can be "dwc_attribution" for the (unratified) [Darwin Core Agents Attribution Extension](https://github.com/tdwg/attribution), "fst" for a fast binary file format and "quickstatements" to quickly publish all unambiguous matches into Wikidata using the [Quickstatements](https://quickstatements.toolforge.org) tool.

`institution_id` : When using the "quickstatements" export, a Wikidata item ID for the institution to link to should be specified.

`data_type` : Which data format is expected for the source data to match. Currently works for "DwC-A" to use a Darwin Core Archive, but can be extended for other data formats.

# Run

To run the workflow, run the `run.R` script using either Rscript or an R IDE such as Rstudio.
