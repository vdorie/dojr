---
title: "Data Repository Requirements"
date: "December 20, 2016"
output: pdf_document
---

# Overview

The DoJ currently produces a number of datasets in different, legacy file formats. The age of these formats has made it difficult to keep analyses up-to-date as new information is made available and has complicated converting the data into more modern formats: much of the data are collected monthly but are transmitted to other organizations as large single-year data-dumps, and while numerous attempts to read these files have been made, errors are common and hard to track back to their source.

Both CJSC and Open Justice have expressed an interest in having more flexible access to the data, but their organizational directives imply different goals. CJSC is primarily interested in data warehousing, so that for them it is imperative that the stored data are identical to the original. Open Justice would like to perform analysis and publish subsets or aggregate counts, so that correcting coding errors and adding structure to the data are likely to be important. Consequently, the data repository must be able to satisfy the following basic requirements:

1. Incorporate new data as it is published and existing data when it is changed
2. Store the legacy data files essentially verbatim
3. Produce structured versions of the data
4. Centralize storage of and access to all the datasets
5. Permit access to the data by the stakeholders, including CJSC and Open Justice publications for the web and derivative data products
6. Accommodate past and future format changes within datasets

# Technical Requirements

## Storage Solution

While not strictly required, a relational database may be helpful when creating multiple views of what are essentially tabular data. Some datasets that we would like to store in the future (e.g. gun sales) consist of multiple tables and links between them using identifiers. While every SQL database can create a replica of a flat file using a `character(N)` type, the vendors differ in the ease in which files can be read into those structures. They also differ in their ability to handle illegal characters, and null-bytes appear with some regularity in the flat files. Adding structure to flat-file columns may also be more or less difficult in various databases, as converting strings to typed data in the presence of exceptions generally requires writing one's own functions with extensions to the SQL standard or doing the transformation in another setting. Finally, security and access need to be considered as it would be ideal to be able to carry out operations within an un-privileged process and an un-privileged database connection, hopefully on the same server as the database itself. The requirements are thus:

1. Store tabular data and support queries that join across tables
2. Store flat-files verbatim
3. (Optional) convert flat-file columns to typed data within the database
4. Support as many operations as possible in a un-privileged setting

In the long-term scalability may be a consideration as we might conceivably have numerous analysts simultaneously downloading gigabytes of raw information with end-users making requests for summary statistics derived from the database on the fly. Updates should be far rarer than reads.

## Programming Language

No real requirements. Although a scripting language may be preferred for the ease of development, presumably costly data manipulation operations will be performed in a more-optimized setting.

## Server

The server needs to be able to:

1. Run the database software
2. Store multiple copies of the data, at a minimum the raw data and an imported version
3. Run import scripts
4. (Optional) schedule import scripts - ideally adding the new data files to a folder will be sufficient to start the update process
5. Accept connections from a reasonably secure source

As most database solutions permit secure connections over encrypted protocols, it would be ideal to be able to connect to the server without taking the additional steps of connecting to a private network. This implies a static IP (or dynamically resolved IP) with an open port.

# Data Integrity

Deviations in the structured data from the original should only arise due to known coding errors. In light of this, a post-hoc analysis should be conducted after importation and conversion as a means of quality control, comparing the values of the columns in each.

# Additional Considerations

In addition to the minimal versions of the data documented above (unchanged and structured) as users it would be beneficial to create and store additional views, including:

1. A "columnar" view corresponding to the raw files but with the capacity to refer to columns by name and not position (e.g. define `record_id` as a `character(2)` with contents `SUBSTRING(rawRecord, 54, 2)`)
2. A "clean" view of the typed version with as many corrections to coding errors as possible - to construct this it will likely involve analyzing why the rows of the raw data failed to convert
3. Anonymized public use files and de-identified scientific use files

# Open Questions

Our solution should accomodate reasonable answers to the following questions:

1. How does CJSC want to access the data?
2. In what format does CJSC want the data?
3. Will the virtual server be sufficient for CJSC?
4. How do we feed things to the web team?
5. What are the steps to get raw files into the database?
6. What is the web team's ideal workflow for getting data? How do we make new files available for download?
7. Do we need notifications about data changes?
8. What is the scope of people who are directly trying to access the database?
9. Will the data access for external researchers be handled within DOJ's network or through an external server?
10. How are we cleaning the data? How does cleaning fit into workflow? Should it run automatically? Should only flag for new data? Do we need cleaning permissions? We need a column in the info table saying that the data is cleaned or awaiting. 
11. If we want to identify suspect data - do we have a table of columns for data flagging and fixing that refers to unique IDs and columns in the data table itself? 
12. What are the rules for preserving partial data? For dates, should we split them into their own columns?
13. Automatically producing a research dataset is easy, but we should check it. 
14. Automatically updated tables should come with documentation about what was done with the new data

Guiding principles:

1. Preserve the original, raw data as much as possible
2. Document all data transformations and losses of information
3. For each potential problem in a dataset, develop a rule for what gets flagged.
4. Sealed records get a flag. 
5. 

Do we install a scripting environment on Windows? Should we install Cygwin?



