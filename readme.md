
# The AUTOTYP database

Balthasar Bickel, Johanna Nichols, Taras Zakharko, Alena Witzlack-Makarevich, Kristine Hildebrandt, Michael Rießler, Lennart Bierkandt, Fernando Zúñiga and John B. Lowe


---

**Release version 0.1.1**

---


**Table of Contents**

- [Data files](#data-files)
    - [General design](#general-design)
    - [Overview of the individual modules](#overview-of-the-individual-modules-in-alphabetical-order)
- [Coverage](#coverage)
- [Metadata files](#metadata-files)
- [Bibliographical references](#bibliographical-references)
- [Naming conventions](#naming-conventions)
- [File formats and how to download and access the data](#file-formats-and-how-to-download-and-access-the-data)
- [Error reports, feature requests, pull requests](#error-reports-feature-requests-pull-requests)
- [License](#license)
- [Citation](#citation)
- [FAQ](#faq)
- [References](#references)



# Data files

These files are stored as `csv` tables in the directory `data/`.

## General design
AUTOTYP differs from traditional typological databases in that in most cases, data is entered in a fairly raw format (comparable to reference grammar descriptions) and need to be aggregated and reshaped for most analytical purposes. For example, we do not enter alignment statements ('S=A≠P' or 'nominative-accusative alignment', etc) but enter individual case markers with the roles they cover and the conditions under which they occur. Alignment statements can then be ***derived*** (i.e. aggregated and/or reshaped) from the data using scripts. The raw data supports a variety of such derivations (apart from alignment statements, one might be interested in whether or not there is a split in case marking, or how many cases can code the same generalized semantic argument role etc.). As a result, AUTOTYP usually contains several alternative derivations from the same raw data. However, the current release (version 0.1.1) includes only tables that we have already derived in earlier research, and the few raw tables that can be used off the shelf. The raw data has a more complex database structure and will be released later, together with scripts for making your own aggregations as well as for exploring and mapping the data. (But of course, it will always be possible to use the data tables without scripts, e.g. for exploring, looking things up in particular languages, copying data to your own spreadsheet, etc.)

Another way in which AUTOTYP differs from traditional databases is that most variables were not predefined but were developed in a technique we call ***autotypologizing*** (Bickel & Nichols 2002): the values of the variables (features, categories, types) and their definitions are constantly revised and expanded during data collection until they stabilize. For example, instead of surveying the presence/absence of a predefined category like 'aspect', we develop a list of categories as we encounter them in our survey work and equate them (or not) to cross-linguistically stable types on the basis of an evolving analysis. The result is a list of categories and definitions and this can then be queried later as to which category occurs where. The outputs of such queries are published here, with the definitions stored in the metadata. 

AUTOTYP has been developed for over 20 years, in a series of loosely related projects. Each project resulted in one or more database modules. Because the projects were carried out with specific purposes in mind and at different times, the variables do not necessarily form a tightly integrated and internally consistent system. The variables sometimes assume different basic notions, reflecting different research questions or a different stage in our theoretical research. For example, some modules make reference to an open list of semantic roles (e.g. the *locus* module), while other modules (e.g. the *grammatical relations* module) makes reference to a Dowty-style approach with a fixed roster of generalized roles that we adopted at a later point.

Another perennial concern of typological databases is empty cells (blanks, or "NA"s in R parlance). In the current release, we do not distinguish between different types of empty values. That is, an empty value can mean 'logically impossible to fill' (e.g. fusion of case marker when there is no case marker), or 'we don't know', or 'nobody knows'. We hope to improve this in future releases.

Finally, a note on the nature and quality of our data. We sometimes deliberately deviate from the analysis provided in reference grammars because we find our analyses are better supported by the data in the grammar or text collections. Because of this and because the analyses evolve slowly together with autotypologizing variables, simple reliability tests were not really feasible during the development of AUTOTYP. However, during the process, all analyses and definitions were extensively discussed in project teams until we reached consensus. Also, in several areas, such as in work on agreement and case morphology or on NP structure, data was collected in independent projects and was then extensively tested for consistency, discussing and resolving any mismatches in the analyses. (Our data publication pipeline also includes a series of formal consistency checks in the form of scripts.)


## Overview of the individual modules (in alphabetical order)

- **`Agreement`**: Various aspects of verb agreement. Entries are based on the *Locus* and *Synthesis* modules, with additional coding by hand.

- **`Alienability`**: Various aspects of possessive classification. Expanded version of Nichols and Bickel's contributions on possession in the [World Atlas of Language Structure](http://wals.info). Note: this module is likely to undergo substantial revision in conjunction with the *NP structure* module. (Some information is duplicated in the two modules, but was collected independently and cross-checked.)

- **`Alignment`**: Alignment of generalized semantic roles. Coded at the level of individual case and agreement (sub)systems. (This represents aggregations of a raw table on grammatical relations, which will be released later.)

- **`Alignment_per_language`:** Alignment of generalized semantic roles. Aggregated at the language level. Based on the information in the *Alignment* module.

- **`Alignment_case_splits`:** Splitting of the case marking of A and/or P by referential or parts of speech category conditions. Based on the information in the *Alignment* module.

- **`Clause_linkage`**: A multivariate typology of clause linkage constructions. Contains only few languages (see the medata for counts), but detailed coding.

- **`Clause_word_order`**: Basic and alternative word orders at the clause level.

- **`Clusivity`**: Various aspects of how inclusive vs. exclusive distinctions are made (if any).

- **`Gender`**: Various aspects of gender distinctions and their reflexes.

- **`Grammatical_markers`**: Various formal and semantic properties of individual grammatical markers. The coding of fusion and exponence is an expanded versions of Bickel & Nichols's contributions to the [World Atlas of Language Structure](http://wals.info).

- **`GR_per_language`**: Aggregated information on grammatical relations (GRs), specifically on the presence and nature of case marking and verb agreement.

- **`Locus_per_language`**: Various per-language aggregations of the locus information in the *Locus per (macro/micro)relation* modules. 

- **`Locus_per_macrorelation`**: Locus of marking (head vs. dependent marking and various special cases) coded at the level of broad macrorelations, such as S, A, P, attributes etc., under default conditions. Aggregated version of the *Locus per microrelation* module.

- **`Locus_per_microrelation`**: Locus of marking (head vs. dependent marking and various special cases) coded at a fine-grained level, tracking language-internal variation in detail. Expanded version of Nichols and Bickel's chapters on Locus in the [World Atlas of Language Structure](http://wals.info).

- **`Markers_per_language`**: Various properties of exemplary markers (a Negation, a Tense, a Case, and a Noun Plural marker) in each language. Aggregated from the *Grammatical markers* module.

- **`Morpheme_types`**: A multivariate typology of words, affixes and clitics focusing on the host restrictions and on the phonological and grammatical behavior of morphemes. Not many languages (see the metadata for counts).

- **`Morphology_per_language`**: Various aggregated properties of morphology, based on the *Grammatical markers*, *Locus*, and *Synthesis* modules.

- **`NP_per_language`**: Various per-language aggregations of the data in the *NP structure* module, with a particular (but non-exclusive) focus on adjectival attribution constructions.

- **`NP_structure`**: Various aspects of noun phrases, focusing on their marking and (formal or semantic) constraints on head and dependents. Each entry is an NP construction type with a distinct morphosyntax and/or distinct constraints. Note: this module is likely to undergo substantial revision in conjunction with the *Alienability* module. (Some information is doubled in the two modules, but collected independently and cross-checked.)

- **`NP_structure_presence`**: Reshaped version of some variables of the *NP structure* module, tracking the presence in each NP type of specific morphosyntactic properties and of constraints on what can be head and dependent.

- **`NP_word_order`**: Basic and alternative word orders at the noun phrase level.

- **`Numeral_classifiers`**: Presence and number of numeral classifiers.

- **`Register`**: This modules tracks information on genealogical, geographical and other information. It was previously released as  Nichols et al. (2013). There are three subparts:

    - *IDs*: apart from our own unique language identifier (`LID`), the database is matched to [Glottolog](http://glottolog.org) and [ISO](http://www-01.sil.org/iso639-3/) codes.

   - *Genealogical information*: The genealogy part is based on the state of the art in each family.  We stay away from geographical groupings, widely mentioned but unproven groupings, or similar hypothetical groupings. (In the few cases where we felt it necessary to include a label for a residual grouping that is not a clade, such as Western Malayo-Polynesian, we have included 'non-clade' in the name of the group.) Thus, our genealogy is similar to the classifications of Campbell & Poser (2008) or [Glottolog](http://glottolog.org), but does not attempt to include languages or families for which there is classificatory information but little or no typological information (e.g. Beothuk, Cayuse) and does not include languages with typological information available that happen not to be in our database. The genealogical database uses two classificatory levels (and corresponding database fields) that are cross-linguistically comparable: `Language` and `Stock`, as defined in the metadata. Between these two levels, the database provides various nested convenience levels that reflect the current state of the art in subgrouping research. They are not comparable across families (e.g. what is called  a major branch can reflect a primary split in one but later splits in another family). Also, they are not necessarily complete. 

   - *Geographical information*: The geography database contains information on the geographical location of languages and a small-scale and a large-scale classification of languages into areas. The area classifications are based on our assumptions about contact events in history, informed by current knowledge of the historical, genetic, anthropological, and archeological record. We try to keep our definition of areas free of linguistic information in order to avoid circularity in areal linguistics research (Bickel & Nichols 2006). The individual definitions are given in the metadata and illustrated in static [area](figures/areas.jpg) and [continent](figures/continents.jpg) pictures. For closer inspection, we recommend loading the files `figures/autotyp.areas.kml` and `figures/autotyp.continents.kml` into [Google Earth](https://www.google.com/earth/).
  
   - *Other information*: This contains information on the genesis (creole vs. regular) and modality (spoken vs. signed) of languages, and on the main subsistence of their speakers.

- **`Rhythm_per_language`**: A rough classification of languages into phonological rhythm types, based on Schiering et al. (2012).

- **`Synthesis`**: Various aspects of how maximally inflected synthetic verb forms are structured in a language, including exhaustive listings of the inflectional categories that can be marked separately (non-cumulatively) and overtly. Expanded version of Bickel and Nichols' chapter on synthesis in the [World Atlas of Language Structure](http://wals.info).

- **`Valence_classes`**: Valence classes as distinguished by case assignment, agreement and other syntactic patterns

- **`Valence_classes_per_language`**: Presence (i.e. mentioning) of specific semantic types in the valence classes that the language distinguishes by case assignment, agreement and other syntactic patterns. Aggregated from the *Valence class* module.

- **`VAgreement`**: Presence of macrorole (A, P, POSS) marking, i.e., agreement, in maximally inflected synthetic verb forms, aggregated from the *Synthesis* module (but only when we are relatively confident that our survey is complete).

- **`VInfl_categories`**: Presence (vs. absence) of separatively and overtly marked inflectional categories on maximally inflected synthetic verb forms. The range of categories comes from the *Synthesis* module (and is defined there) but the aggregation is only produced when we are relatively confident that our survey is complete.
	
- **`VInfl_counts_per_position`**: Counts of separatively (i.e. non-cumulatively) marked inflectional categories that are realized before, after, inside etc. the maximally inflected synthetic verb form. Aggregated from the *Synthesis* module.


- **`VInfl_macrocategories`**: Presence (vs. absence) of separatively and overtly marked inflectional categories in maximally inflected synthetic verb forms, aggregated into broad macro-categories. The range of macro-categories comes from the *Synthesis* module (and is defined there) but the aggregation is only produced when we are relatively confident that our survey is complete.

- **Position of verb-inflectional categories**:  6 different ways of reporting the morphological positions of all separatively (i.e. non-cumulatively) marked verb-inflectional categories encountered in the *Synthesis* module (which covers maximally inflected synthetic verb forms):

   - **`VInfl_cat_postposed`**: presence of at least some exponents of the category after the phonological host
	
   - **`VInfl_cat_positions4`**: four-way aggregation of the position types into prae, post, simul/in, split
	
   - **`VInfl_cat_positions`**: maximally resolved distinctions of positions
   
   - **`VInfl_cat_positions5`**: five-way aggregation of the position types (in, post, prae, simul vs. split)
	
   - **`VInfl_cat_preposed`**: presence of at least some exponents of the category before the phonological host
   
   - **`VInfl_cat_multiexponence`**: presence of at least some multiple/simultaneous realization of the category at issue


- **Position of verb-inflectional macrocategories**: 6 different ways of reporting the morphological positions of all separatively (i.e. non-cumulatively) marked verb-inflectional macrocategories encountered in the *Synthesis* module (which covers maximally inflected synthetic verb forms):

   - **`VInfl_macrocat_postposed`**: presence of at least some exponents of the macrocategory after the phonological host
	
   - **`VInfl_macrocat_position4`**: four-way aggregation of the position types into prae, post, simul/in, split
	
   - **`VInfl_macrocat_position`**: maximally resolved distinctions of positions
   
   - **`VInfl_macrocat_position5`**: five-way aggregation of the position types (in, post, prae, simul vs. split)
	
   - **`VInfl_macrocat_preposed`**: presence of at least some exponents of the macrocategory before the phonological host
   
   - **`VInfl_macrocat_multiexponence`**: presence of at least some multiple/simultaneous realization of the macrocategory at issue

- **Position of verbal macrorole markers**: 6 different ways of reporting the morphological positions of macrorole (A, P, POSS) marking, based on the *Synthesis* module. Note that because the *Synthesis* module targets maximally inflected synthetic verbs, it privileges coding of transitively inflected verbs, and so S is normally not covered.

   - **`VAgr_postposed`**: presence of at least some exponents of the role marker after the phonological host
	
   - **`VAgr_position4`**: four-way aggregation of the position types into prae, post, simul/in, split
	
   - **`VAgr_position`**: maximally resolved distinctions of positions
   
   - **`VAgr_position5`**: five-way aggregation of the position types (in, post, prae, simul vs. split)
	
   - **`VAgr_preposed`**: presence of at least some exponents of the macrorole before the phonological host
   
   - **`VAgr_multiexponence`**:presence of at least some multiple/simultaneous realization of the macrorole at issue


- **`Word_domains`**: Strings of morphs are coded for phonological and grammatical cohesion; cohesion patterns come with explicit descriptions and morphs are categorized in a multivariate typology. Not many languages (see the metadata for counts).


# Coverage

The present release includes over 1000 variables distributed over 50 modules (tables) with a total of about 4.5 million typological datapoints. The following histogram shows the data coverage across variables:

![Data coverage across variable](figures/autotyp-data-histogram.png?raw=true)

The following map shows how the data are distributed over the world. Points are sized in proportion to the number of entries for each language:

![Data coverage across languages](figures/data-coverage-map.png?raw=true)


# Metadata files
Each data file is associated with a metadata file that explains the coding decisions and definitions of each variable in the data file. The metadata files are stored in [`YAML`](https://en.wikipedia.org/wiki/YAML) format in the directory `metadata/`. A tabular overview is also available in the file [`metadata_overview.csv`](metadata_overview.csv).

The following fields are provided for each variable:

* **`Variable`**: the name of the variable. For naming conventions, see below.

* **`Description`**: free-text variable definition

* **`SetUp`**: whether the variable codes data at the level of language or at the level of individual parts (subsystems, constructions, marker etc.). Possible values:
	- `single entry per language` = for a given language, this variable has at most one value
	- `single aggregated entry per language` = for a given language, this variable has at most one value, and this value is an aggregated result that was computed from more fine-grained data
	- `multiple entries per language` = for a given language, this variable can have multiple values

* **`DataEntry`**: whether the data is inputted `by hand` or `derived` (i.e. aggregated and/or reshaped) by scripts from information in other variables (raw data or other aggregations)

* **`VariableType`**: This field tracks the type of variable:
	- `data` = entries are observations that can be meaningfully used for quantitative analysis 
	- `register` = entries are statements about a language or a construction, i.e. geographical, genealogical information, labels and identifiers
	- `condition` = entries specify the condition or domain within which an observation is made
	- `details` = entries specify details about some data points, e.g. listing individual predicates of valence classes or individual nouns in constraints on head classes 
	- `quality` = entries that specify something about the source or quality of the data
    
* **`DataType`**: whether the data are of type `categorical`, `ratio`, `count` or `logical`

* **`VariantOf`** (optional): This field tracks some of the logical dependencies between variables: it tracks whether two variables are different aggregations of the same underlying variable, e.g. the order of V and O and the order of A and O are two variant aggregations of the six-way variable that codes the order of A, O, and V. What the current release does not track is more complex relationships, e.g. when a variable aggregates partial data from different other variables or   even different modules. (To some extent, variables with DataEntry `derived` are candidates for such cases, but not all `derived` variables are variants of another variable because they might be the sole aggregation of some raw data and the raw data is not (yet) available for typological analysis or not suitable for it.)

* **`N.levels`**: number of distinct levels (unique values, types) of the variable

* **`N.entries`**: number of entries in the variable, excluding all blank cells ('NA's) in the database

* **`N.languages`**: number of different languages described by this particular variable, excluding all blank cells ("NA"s) in the database

* **`N.missing`**: number of missing entries for the variable

* **`Levels`** (optional): definitions of individual values for certain categorical variables. Often, levels are aggregated (or binned) versions of more fine grained levels in another variable. In this case, the definition is a comma-separated concatenated list of individual definitions. If the definitions are complex, they are first wrapped in square brackets for the purposes of readability. Example: `prae: '[before the phonological host], [before the phonological host by default, but around the host under exceptional conditions (typically lexical)]'`. This means that the level `prae` is an aggregation of two options, one where a marker is always before the host and one where the marker is before the host only with some stems but is circumfixed with other stems.

* **`Notes`** (optional): Further explanations


# Bibliographical references
The sources for all entries can be retrieved from the `.bib` file (see [Formats](#file-formats-and-how-to-download-and-access-the-data) via the language identifier (`LID`) which comes with every entry. There may be gaps, in which case the references listed in [Glottolog](http://glottolog.org) are likely to correspond to what we relied on.

# Naming conventions
<!-- (see public.release.paper/autotyp_data_curation_bestpractices_rev.pages) -->
The names of variables are transparent on their own (we hope), but there are a few conventional suffixes that might help:

- `.Presence`: indicates that the variable is binary and codes the presence (`TRUE`) vs. absence (`FALSE`) of a certain feature (value, property, trait, construction, or marker).
- `.binned` followed by a number *N*: indicates that the variable is a binned or (re-)categorized version of the variable that it is a variant of (see metadata field `Variant_Of`), with the number *N* indicating into how many bins the variable was (re-)categorized. (If *N*=2, this is coded as `.Presence`)
- `.n`: indicates that the variable represents a count of something
- `.prop`: indicates that the variable represents a proportion of something
- `.v1`, `.v2` etc.: separates slightly different versions of aggregating or binning the same underlying variable (as given in `Variant_Of`). Note that the numbering is arbitrary, i.e. it is not the case that version 2 is better than version 1; it just captures a slightly different aspect of a multidimensional pattern.


# File formats and how to download and access the data
The database can be downloaded by clicking on the "Clone or Download" button in the upper right corner of this page. This retrieves directories with files in `.csv` format for data, `.yaml` format for metadata, and `.bib` format for bibliographical references. In addition, <!-- the genealogical data are also available in `.nex` and --> the geographical is available data in `.kml` format. Finally, we also provide the entire dataset as list in `R`'s `.rds` format. There, each list component represents a single variable, stored as a column in a data frame, together with the corresponding LID. (Note that the list only contains entries of type `data` and does not track any conditions on the data. Use with caution!).

Here are suggestions for how to read the files:

- `.csv`: any spreadsheet application; or load into [R](https://www.r-project.org) with `read.csv("table_name.csv", na.strings = "")`
- `.yaml`: any plain text editor; or load into [R](https://www.r-project.org) with `library(yaml); yaml.load_file("table_name.yaml")`
- `.bib`: any plain text or `bibtex` editor (e.g. [BibDesk](http://bibdesk.sourceforge.net); or load into a bibliography manager like [Zotero](https://www.zotero.org))
<!-- - `.nex`: a tree drawing program like [FigTree](http://tree.bio.ed.ac.uk/software/figtree/); or load into [R](https://www.r-project.org) with `library(ape); read.nexus('tree_name.nex')` -->
- `.kml`: load into [Google Earth](https://www.google.com/earth/)
- `.rds`: load into [R](https://www.r-project.org) with `readRDS("autotyp_all_data_list.RDS")`

You can switch between different versions of the dababase by clicking on the `branch` menu on the top of the GitHub page and selecting a version from the tags tab. 


# Error reports, feature requests, pull requests

All databases have errors and we welcome **error reports**. However, experience tells us that most error reports are spurious because they do not arise from a genuine empirical issue but from a misunderstanding of how variables are defined. So, before making a report, please first carefully read the definition of the variables and any references mentioned in the definition. In some cases, definitions may be unclear or ambiguous, and in these cases, we are grateful for reports as well. When submitting an error report, please indicate whether it concerns a variable with `DataEntry` as `derived` or `by hand` (see Metadata). This helps us track the source of the error.

We keep adding alternative derivations (aggregations or reshapings) of the data. If you would like to see such a derivation added, please submit a **feature request**. 

Both error reports and feature requests can be submitted online using the [GitHub issue tracker for the database](https://github.com/autotyp/autotyp-data/issues). Please note that you will require a (free of charge) GitHub account to submit an issue.

# License

<a rel="license" href="http://creativecommons.org/licenses/by/4.0/"><img alt="Creative Commons License" style="border-width:0" src="https://i.creativecommons.org/l/by/4.0/88x31.png" /></a><br />This work is licensed under a <a rel="license" href="http://creativecommons.org/licenses/by/4.0/">Creative Commons Attribution 4.0 International License</a>.

# Citation

Bickel, Balthasar, Johanna Nichols, Taras Zakharko, Alena Witzlack-Makarevich, Kristine Hildebrandt, Michael Rießler, Lennart Bierkandt, Fernando Zúñiga & John B. Lowe. 2017. *The AUTOTYP typological databases*. Version 0.1.1 [https://github.com/autotyp/autotyp-data/tree/0.1.1](https://github.com/autotyp/autotyp-data/tree/0.1.1)

Please make sure to always include the version number with your citation. This ensures that results can be always reproduced even after the database has been updated. The GitHub website tracks all versions of the database that were ever published (you can switch between them by clicking on the branch menu on the top of this page and selecting an appropriate tag). AUTOTYP uses [semantic versioning ](https://semver.org/spec/v2.0.0.html). 

# FAQ

**Q**: Why isn't the database available via a [CLLD](http://clld.org) web interface? That would be so much more convenient!

**A**: We currently lack the resources to implement this and instead put all our efforts into developing the next release versions. However, since AUTOTYP is released under a [CC-BY](http://creativecommons.org/licenses/by/4.0/) license, everybody is free to implement a pipeline that takes each AUTOTYP release and makes it available through a [CLLD](http://clld.org) interface --- as long as the source is fully credited. 


**Q**: What if I want to read the data directly into R from the GitHub website?

**A**: Click on the "Raw" button and then use the URL of this page as the argument of `read.csv(<insert URL here>, na.strings = "")`
More generally, the following URL fits all content and will give you the most recent version of the file:
`https://raw.githubusercontent.com/autotyp/autotyp-data/0.1.1/...`

We recommend downloading the database for offline use (and noting down the version number), in the interest of replicability. You will find the version number in the readme file and also in the `VERSION` file in the database folder. 


**Q**: Can I quickly produce maps of specific data?

**A**: We provide a set of `R` tools to work with the data, including a mapping function. These tools are available for now in a simple script (`R/autotyp.utilities.R`), but will at some point be published as a stand-alone `R` package.


**Q**: What do the version numbers mean?

**A**: We use [semantic versioning system v2.0.0](https://semver.org/spec/v2.0.0.html). Under this system, the version always consists of three numbers, separated by dots. The first number (major version) describes substantial releases with significant changes. The second number (minor version) signals potentially incompatible changes (such as addition/removal of modules or changes of structure within individual modules). The third number (patch version) tracks small updates and fixes, such as maintenance data or metadata edits.   

---
# References

Bickel, Balthasar & Johanna Nichols. 2002. Autotypologizing databases and their use in fieldwork. In Peter Austin, Helen Dry & Peter Wittenburg (eds.), *Proceedings of the International LREC Workshop on Resources and Tools in Field Linguistics, Las Palmas,* 26-27 May 2002. Nijmegen: MPI for Psycholinguistics [[download](http://www.autotyp.uzh.ch/download/canary.pdf)].

Bickel, Balthasar & Johanna Nichols. 2006. Oceania, the pacific rim, and the theory of linguistic areas. *Proc. Berkeley Linguistics Society* 32. 3–15.

Campbell, Lyle & William J. Poser. 2008. *Language classification: History and method*. Cambridge: Cambridge University Press.

Nichols, Johanna, Alena Witzlack-Makarevich & Balthasar Bickel. 2013. The AUTOTYP genealogy and geography database: 2013 release. Electronic database [[download](http://www.autotyp.uzh.ch/download/release_2013/autotyp-release_2013.pdf)].

Schiering, René, Kristine Hildebrandt & Balthasar Bickel. 2012. Stress-timed = word-based? Testing a hypothesis in Prosodic Typology. *Language Typology and Universals* 65. 157–168.
