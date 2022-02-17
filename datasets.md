# Overview of the available datasets

## Module `Register`

This module tracks information on genealogical, geographical and other aspects of language
classification. It was previously released as [Nichols et al. (2013)](https://www.autotyp.uzh.ch/download/release_2013/autotyp-release_2013.pdf).
There are three main data components:

- IDs: apart from our own unique language identifier (LID), the database is matched to Glottolog
and ISO codes.

- Genealogical information: The genealogy part is based on the state of the art in each family.
We stay away from geographical groupings, widely mentioned but unproven groupings, or similar
hypothetical groupings. (In the few cases where we felt it necessary to include a label for a
residual grouping that is not a clade, such as Western Malayo-Polynesian, we have included
'non-clade' in the name of the group.) Thus, our genealogy is similar to the classifications
of Campbell & Poser (2008) or Glottolog, but does not attempt to include languages or families
for which there is classificatory information but little or no typological information (e.g.
Beothuk, Cayuse) and does not include languages with typological information available that
happen not to be in our database. The genealogical database uses two classificatory levels (and
corresponding database fields) that are cross-linguistically comparable: Language and Stock, as
defined in the metadata. Between these two levels, the database provides various nested
convenience levels that reflect the current state of the art in subgrouping research. They are
not comparable across families (e.g. what is called a major branch can reflect a primary split
in one but later splits in another family). Also, they are not necessarily complete.

- Geographical information: The geography database contains information on the geographical
location of languages and a small-scale and a large-scale classification of languages into
areas. The area classifications are based on our assumptions about contact events in history,
informed by current knowledge of the historical, genetic, anthropological, and archeological
record. We try to keep our definition of areas free of linguistic information in order to avoid
circularity in areal linguistics research (Bickel & Nichols 2006). The individual definitions
are given in the metadata and illustrated in static area and continent pictures.

- Other information: This contains information on the genesis (creole vs. regular) and modality
(spoken vs. signed) of languages, and on the main subsistence of their speakers.

## Module `Definitions`

This module contains the definitions of common concepts and types used within AUTOTYP. *Caution*: 
these values on their own are not typological variables and can represent different stages of 
database development - do not rely on them to establish typologies, use data in the specific 
datasets instead. Currently available datasets are:


- **`Areas`** (primary)

    Linguistic areas

- **`Category`** (primary)

    Morphological category

- **`Continents`** (primary)

    Language continents

- **`Diathesis`** (primary)

    Diathesis

- **`Flexivity`** (primary)

    Formative flexivity types: allomorphy of affixes depending on grammatical
    categories or lexical choice (see Bickel & Nichols 2007 in Language
    typology and syntactic description, ed. T. Shopen, Cambridge:
    Cambridge University Press)

- **`HostRestriction`** (primary)

    Type of categorical restriction on possible hosts in terms of parts
    of speech

- **`LocusOfMarking`** (primary)

    Locus of marking, as defined in Bickel & Nichols 2007 (in Language
    typology and syntactic description, ed. T. Shopen, Cambridge: Cambridge
    University Press)

- **`MorphemeType`** (primary)

    Morpheme type (as defined in Bickel & Nichols 2007 in Language typology
    and syntactic description, ed. T. Shopen, Cambridge: Cambridge University
    Press)

- **`PartOfSpeech`** (primary)

    Part of speech (detailed referential type)

- **`PhonologicalFusion`** (primary)

    Phonological fusion of grammatical marker, as defined in Bickel & Nichols
    2007 (in Language typology and syntactic description, ed. T. Shopen,
    Cambridge: Cambridge University Press)

- **`Placement`** (primary)

    Placement of a grammatical marker within a phrasal domain

- **`Polarity`** (primary)

    Polarity

- **`Position`** (primary)

    Position of grammatical marker with regard to its phonological host

- **`PositionalBehavior`** (primary)

    Positional behavior of a grammatical marker in its phrase, as defined in
    Bickel & Nichols 2007 (in Language typology and syntactic description,
    ed. T. Shopen, Cambridge: Cambridge University Press)

- **`SemanticClass`** (primary)

    Semantic class

- **`SemanticRole`** (primary)

    Generalized semantic roles

- **`SourceOfMarking`** (primary)

    Source of grammatical marker (agreement vs. assignment or any
    combination thereof)

- **`SyntacticDomain`** (primary)

    Syntactic domain

- **`SyntacticRole`** (primary)

    Syntactic role

- **`TAM`** (primary)

    TAM categories


## Module `Categories`

This module describes selected grammatical categories. Currently available datasets are:

- **`Alienability`** (primary)

    Various aspects of possessive classification. Expanded version of Nichols and Bickel's
    contributions on possession in the *World Atlas of Language Structure*.
    
    Note: this module is likely to undergo substantial revision in conjunction with the NP
    structure module. (Some information is duplicated in the two modules, but was collected
    independently and cross-checked.)

- **`Clusivity`** (primary)

    Various aspects of how inclusive vs. exclusive distinctions are made (if any).

- **`Gender`** (primary)

    Various aspects of gender distinctions and their reflexes.

- **`NumeralClassifiers`** (primary)

    Presence and number of numeral classifiers.


## Module `Clause`

This module describes selected aspects of clause syntax, currently clause linkage and clause word 
order:



## Module `NP`

This module describes selected properties of NPs across languages. It currently consists of a 
single dataset:

- **`NPStructure`** (primary)

    Various aspects of noun phrases, focusing on their marking and (formal or semantic) constraints
    on head and dependents. Each entry is an NP construction type with a distinct morphosyntax and/or
    distinct constraints.

- **`NPStructurePresence`** (derived)

    Per-language presence of NP properties

## Module `Morphology`

This module contains information about verb morphology and grammatical markers. Currently available 
datasets are:

- **`GrammaticalMarkers`** (primary)

    Various formal and semantic properties of individual grammatical markers. The coding of fusion
    and exponence is an expanded versions of Bickel & Nichols's contributions to the World Atlas of
    Language Structure.

- **`LocusOfMarkingPerMicrorelation`** (primary)

    Locus of marking (head vs. dependent marking and various special cases) coded at a fine-grained
    level, tracking language-internal variation in detail. Expanded version of Nichols and Bickel's
    chapters on Locus in the World Atlas of Language Structure.

- **`MorphemeClasses`** (primary)

    A multivariate typology of words, affixes and clitics focusing on the host restrictions and on
    the phonological and grammatical behavior of morphemes.

- **`VerbSynthesis`** (primary)

    Various aspects of how verbs forms can be structured in a language, such
    as presence of incorporation and inflectional categories (including agreement)
    expressed on maximally inflected verbs etc. These are high-level
    per-language aggregations, not individual verb forms.
    
    *Caution*: the fields `IsVerbAgreementSurveyComplete` and `IsVerbInflectionSurveyComplete` specify
    whether the data in `VerbAgreement` and `VerbInflectionCategories` is exhaustively coded.
    Do not rely on entries where these flags are FALSE.

- **`DefaultLocusOfMarkingPerMacrorelation`** (derived)

    Default locus of marking, aggregated per language and macrorelation

## Module `GrammaticalRelation`

This module contains information about grammatical relations and valence frames. Currently available 
datasets are:

- **`GrammaticalRelationCoverage`** (primary)

    Presence and coding status of various types of grammatical relations. See GrammaticalRelations
    (and the underlaying raw table GrammaticalRelationsRaw) for grammatical relation data itself.

- **`GrammaticalRelationsRaw`** (primary)

    Detailed coding of grammatical relations as selectors on generalized semantic roles, conditioned
    by various parameters (https://www.autotyp.uzh.ch/projects/grhandbook/GR_Quest.pdf).
    
    *Caution*: this is a raw dataset that may contain incomplete or erroneous entries. The flag
    `IsSelectorSurveyComplete` denotes entries where data input is completed and checked. Also, refer
    to `GrammaticalRelationsCoverage` for the coding status of specific phenomena. For most users, we
    recommend to use the table GrammaticalRelations which has been cleaned up and pre-processed
    (see `aggregations/Alignment.R` for the algorithm).

- **`PredicateClasses`** (primary)

    Predicate classes as distinguished by case assignment, agreement and
    other syntactic patterns. Classes with IDs #1, #2 and #3 are
    language-independent default (open) classes for intransitive, monotransitive
    and ditransitive predicates, respectively.

- **`GrammaticalRelations`** (derived)

    Detailed coding of grammatical relations as selectors of generalized semantic roles,
    conditioned by various parameters (https://www.autotyp.uzh.ch/projects/grhandbook/GR_Quest.pdf).
    
    Note: this is a processed and validated dataset, generated from complete data in
    `GrammaticalRelationsRaw` (`IsSelectorSurveyComplete==TRUE`). We currently include GR data on
    case marking and agreement only, with default diathesis and semantic/syntactic domain condition.

- **`Alignment`** (derived)

      Computed alignments for case and agreement (syntactic and per-marker)

Note: `GrammaticalRelations` contains a processed and validated subset of data from 
`GrammaticalRelationsRaw`, more suitable for direct usage. It currently includes GR data on case 
marking and agreement only, with default diathesis and semantic/syntactic domain condition. Users
are welcome to inspect the scripts in `aggregation-scripts\Alignment.R` and use it to produce 
their own aggregations. 

## Module `Word`

This module describes selected aspects of wordhood across languages. It currently consists of a 
single dataset:

- **`WordDomains`** (primary)

    Strings of morphs are coded for phonological and grammatical cohesion; cohesion patterns come
    with explicit descriptions and morphs are categorized in a multivariate typology.  See our
    [2009 project report](http://www.autotyp.uzh.ch/download/finalreport_words.pdf) for details.

## Module `PerLanguageSummaries`

This module contains various per-language aggregations using the data from all the other modules. 
Currently available aggregations are:

- **`AlignmentForDefaultPredicatesPerLanguage`** (derived)

    Per-language summary of morphosyntactic alignments (default predicate class only)

- **`GrammaticalMarkersPerLanguage`** (derived)

    Grammatical marker exemplars properties (based on the data from `GrammaticalMarkers`),
    aggregated per language

- **`LocusOfMarkingPerLanguage`** (derived)

    Locus of marking, aggregated per language

- **`MorphologyPerLanguage`** (derived)

    Per-language summary of morphological properties

- **`NPStructurePerLanguage`** (derived)

    Per-language aggregations of NP properties

- **`PredicateClassesSemanticsPerLanguage`** (derived)

    Per-language summaries of predicate classes with distinct morphosyntactic behavior

- **`VerbInflectionCategoriesAggregatedPresence`** (primary)

    Per-language summaries of presence of inflectional categories

- **`VerbInflectionAndAgreementCountsByPosition`** (derived)

    Per-language number of inflection and agreement markers on the verb

- **`VerbInflectionCategoriesAggregatedByMarkerPosition`** (derived)

    Marker position (from `GrammaticalMarkers::MarkerPosition`), reshaped and aggregated per language and inflection category

- **`VerbInflectionCategoriesAggregatedByMarkerPositionBinned4`** (derived)

    Marker position (from `GrammaticalMarkers::MarkerPositionBinned4`), reshaped and aggregated per language and inflection category

- **`VerbInflectionCategoriesAggregatedByMarkerPositionBinned5`** (derived)

    Marker position (from `GrammaticalMarkers::MarkerPositionBinned5`), reshaped and aggregated per language and inflection category

- **`VerbInflectionCategoriesAggregatedByMarkerHasPreposedExponent`** (derived)

    Marker position (from `GrammaticalMarkers::MarkerHasPreposedExponent`), reshaped and aggregated per language and inflection category

- **`VerbInflectionCategoriesAggregatedByMarkerHasPostposedExponent`** (derived)

    Marker position (from `GrammaticalMarkers::MarkerHasPostposedExponent`), reshaped and aggregated per language and inflection category

- **`VerbInflectionCategoriesAggregatedByMarkerHasMultipleExponents`** (derived)

    Marker position (from `GrammaticalMarkers::MarkerHasMultipleExponents`), reshaped and aggregated per language and inflection category

- **`VerbInflectionMacrocategoriesAggregatedByMarkerPosition`** (derived)

    Marker position (from `GrammaticalMarkers::MarkerPosition`), reshaped and aggregated per language and inflection category (binned into broad types)

- **`VerbInflectionMacrocategoriesAggregatedByMarkerPositionBinned4`** (derived)

    Marker position (from `GrammaticalMarkers::MarkerPositionBinned4`), reshaped and aggregated per language and inflection category (binned into broad types)

- **`VerbInflectionMacrocategoriesAggregatedByMarkerPositionBinned5`** (derived)

    Marker position (from `GrammaticalMarkers::MarkerPositionBinned5`), reshaped and aggregated per language and inflection category (binned into broad types)

- **`VerbInflectionMacrocategoriesAggregatedByMarkerHasPreposedExponent`** (derived)

    Marker position (from `GrammaticalMarkers::MarkerHasPreposedExponent`), reshaped and aggregated per language and inflection category (binned into broad types)

- **`VerbInflectionMacrocategoriesAggregatedByMarkerHasPostposedExponent`** (derived)

    Marker position (from `GrammaticalMarkers::MarkerHasPostposedExponent`), reshaped and aggregated per language and inflection category (binned into broad types)

- **`VerbInflectionMacrocategoriesAggregatedByMarkerHasMultipleExponents`** (derived)

    Marker position (from `GrammaticalMarkers::MarkerHasMultipleExponents`), reshaped and aggregated per language and inflection category (binned into broad types)

- **`VerbAgreementAggregatedByMarkerPosition`** (derived)

    Marker position (from `GrammaticalMarkers::MarkerPosition`), reshaped and aggregated per language and agreement microrelation

- **`VerbAgreementAggregatedByMarkerPositionBinned4`** (derived)

    Marker position (from `GrammaticalMarkers::MarkerPositionBinned4`), reshaped and aggregated per language and agreement microrelation

- **`VerbAgreementAggregatedByMarkerPositionBinned5`** (derived)

    Marker position (from `GrammaticalMarkers::MarkerPositionBinned5`), reshaped and aggregated per language and agreement microrelation

- **`VerbAgreementAggregatedByMarkerHasPreposedExponent`** (derived)

    Marker position (from `GrammaticalMarkers::MarkerHasPreposedExponent`), reshaped and aggregated per language and agreement microrelation

- **`VerbAgreementAggregatedByMarkerHasPostposedExponent`** (derived)

    Marker position (from `GrammaticalMarkers::MarkerHasPostposedExponent`), reshaped and aggregated per language and agreement microrelation

- **`VerbAgreementAggregatedByMarkerHasMultipleExponents`** (derived)

    Marker position (from `GrammaticalMarkers::MarkerHasMultipleExponents`), reshaped and aggregated per language and agreement microrelation
