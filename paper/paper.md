---
title: 'LinguiPhyR: A Package for Linguistic Phylogenetic Analysis in R'
tags:
  - R
  - R Shiny
  - phylogenetics
  - historical linguistics
  - indo-european
authors:
  - name: Marc E. Canby
    orcid: 0000-0002-8420-9658
    affiliation: 1 # (Multiple affiliations must be quoted)
affiliations:
 - name: University of Illinois at Urbana-Champaign, USA
   index: 1
date: 14 November 2023
bibliography: paper.bib

---


<!---# Optional fields if submitting to a AAS journal too, see this blog post:
# https://blog.joss.theoj.org/2018/12/a-new-collaboration-with-aas-publishing
aas-doi: 10.3847/xxxxx <- update this with the DOI from AAS once you know it.
aas-journal: Astrophysical Journal <- The name of the AAS journal.--->

# Introduction

Phylogenetic methods have become commonplace in historical linguistics research. However, many current 
research activities in the area are undertaken by statisticians rather than linguists, which is largely 
(and understandably) due to the highly mathematical and computational nature of the work. This paper aims to bridge the gap 
between linguistic and statistical research by introducing LinguiPhyR, an R package that provides a graphical 
user interface (GUI) to aid in the phylogenetic analysis of linguistic data. As such, very little computational 
or statistical expertise is required by the user. A linguist may simply upload a dataset, select 
optimization criteria, and visualize the phylogenies found by the search algorithm. Alternatively, one may upload 
trees of interest to be analyzed given the dataset. Several tools 
for tree analysis are provided: a user may examine what characters are responsible for particular splits 
in the tree, see the characters that are incompatible on the tree, annotate internal nodes of the tree with 
reconstructed states, and even see a relative chronology of state changes. 

We note that, at present, our software focuses on parsimony-based tree estimation and analyses.
We make this choice because such an approach is easily interpretable by linguists: the best tree is simply
the tree that minimizes the number of state changes. Many popular methods for phylogenetic estimation, such as maximum likelihood and 
Bayesian inference, are less easily interpretable, as they are highly parametric and rely on a likelihood function that may be unrealistic
or obscure to linguists. Parsimony analyses, on the other hand, make it easy to see the effect of each character in the dataset on
tree search. Other concerns about fully parametric approaches have been raised as well, such as the suggestion that non-parametric methods like parsimony are more accurate [@barbancondiachronica2013; @tutorialNicholsWarnow; @holmes2003statistics]. 
Nonetheless, future work will include the incorporation of other search algorithms and analytical methods into LinguiPhyR.

# Statement of need

Given the recent explosion of new linguistic phylogenetic datasets [@heggartyetal; @Tresoldi2023; @herce2023short; @jager2018global], new tools for their analyses are called for. Many linguists
want to perform parsimony analyses of their dataset, and our software makes it easy to do so with little effort.
In this work,
we provide an easy-to-use tool for phylogenetic analysis that emphasizes *interpretability*, allowing linguists to understand
why trees are returned for a particular dataset *or* what evidence a new dataset has for existing trees suggested by the community.
<!---Currently,
the de-facto standard for phylogenetic analysis is Bayesian inference, which, despite efforts to reduce barrier to entry,
requires reasonable mathematical maturity to understand and operates largely as a black-box.---> 

The primary goals of LinguiPhyR are to

1. Make phylogenetics accessible to linguists by requiring *no* coding or writing of configuration files. While these are useful skills,
we believe phylogenetics can only be useful to historical linguistics if considerable analysis is given to the
results of phylogenetic algorithms by linguists. Over-emphasis on technical ability often hinders this work.

2. Make it easy to find and visualize trees for a new linguistic dataset. One simply has to upload the dataset and select optimization criteria (or use the
default settings). Trees are then displayed in the app and can be downloaded for inclusion in other work.

3. Provide a comprehensive set of (parsimony-based) analysis tools. These focus on the following questions: why are particular trees being suggested for
the dataset? What evidence does a dataset contain for other trees proposed by the community? What is the effect of particular coding
decisions in the dataset on the understanding of a tree?
<!---5. Provide a platform for phylogenetic visualization. Discussion with many linguists has pointed to the need for an interactive
framework to demonstrate and analyze trees, often in presentation settings or tutorials.--->

<!---4. Make linguistic phylogenetic analysis reproducible. Not only can one download the PAUP\* configuration file, a researcher can download
the trees . Conclusions drawn on the basis of the software can 
thus be easily analyzed by other researchers.--->
<!---The introduction of an easy-to-use interface aids in this goal because a researcher can 
reproduce work simply by uploading the same dataset and running optimization with the same settings.---> 


Our work is not the only attempt to make phylogenetic methods accessible and interpretable to linguists, nor is it the only GUI for this purpose.
For example, PAUP\* [@swofford2002phylogenetic] provides a GUI containing a comprehensive set of parsimony-based tools for phylogenetics, although it does
require writing Nexus configuration files and is not specifically aimed at linguists. Tools specific to Bayesian linguistic phylogenetics 
include BEASTling [@maurits2017beastling], which is a wrapper for BEAST [@bouckaert2014beast], and Traitlab [@kelly2023traitlab]. A useful tutorial in
R for linguistic phylogenetics is @IndoEuropeanphylogeneticswithR.

# LinguiPhyR: Linguistic Phylogenetic Analysis in R

The following sections describe each page of the app: Data Upload, Tree Search, and Analysis.
Throughout the subsequent discussion, many terms familiar to historical linguists are used (e.g. *clade*, *cognate*, and *regular sound change*); 
we suggest @ringeska for further reading. Similarly, we recommend @warnow2017computational for further discussion on language common in
the phylogenetics literature, such as *character*, *polymorphism*, and *parsimony*.

## Data Upload

The user first uploads a dataset of linguistic characters, which encode certain properties about languages that are likely
to be relevant to the branching structure of the underlying tree. The characters should
be specified in a spreadsheet and uploaded as a CSV file. An example of the data format is shown below:

| id   | feature | weight | chartype | HI | AR | GK | AL | TB | VE | AV | OC | LI | $\cdots$
| ----------- | ----------- | ----------- | ----------- | ----------- |----------- |----------- |----------- |----------- |----------- |----------- |----------- |----------- |-----------
| c1      | P1       | 50|standard|4 | 1 | 1 | 1 | 1 | 1 | 1 | 1 | 1  |$\cdots$
| c26   | M3        |50|standard|1 | 2 | 2 | 3 | 2 | 2 | 2 | 2 | 4 |$\cdots$
| c50 | bird |1|standard|1 | 2 | 3 | 4 | 5 | 6 | 6 | 7 | 8 |$\cdots$

Table: Example dataset specification, excerpted from the screened Indo-European dataset of @ringe2002indo.

<!---, and the columns represent attested languages (the leaves of the tree). --->
Each row represents a character. The first four columns
specify special character information: a unique character ID, the character name ("feature"), the weight of the character (optional,
to be used in parsimony analyses), and the character type (which can be *standard*, *irreversible*, or *custom*). The remaining columns
contain the character states for each attested language (i.e. the leaves of the tree). 

Two languages should be given the same state for a character *if and only if* the languages' realization of that character
could be from a common genetic source (and not, for example, from borrowing). For lexical data, characters typically represent particular 
semantic slots (such as "bird" in the table above), and
languages should share a state if their words for that meaning are cognate $-$ that is, the words are derived from a common ancestor via regular sound change.
However, if a linguist can demonstrate that two languages share the same cognate due to borrowing or some other non-genetic source, then
the languages should be given different states for that character. 

Such cognate judgements are critically important to the results of phylogenetic estimation. A haphazard or automated
data representation will not yield meaningful trees; hence, it is important to have well-trained linguists judge relevant material and select characters
that actually represent potentially shared innovations. An abundance of literature discusses good methodology for doing this [@ringe2002indo; @tutorialNicholsWarnow].

Our data format also supports *polymorphic* character states: these are instances where a language exhibits more than one state for a character.
In the context of lexical data, this would mean that a language manifests two cognate classes for the same semantic slot.
Such examples are denoted by separating the states with a */* (e.g. *1/2*) in the dataset.

Finally, we note that our software permits *multi-state* characters, not just binary traits. Binary traits are particularly common in 
likelihood-based phylogenetic estimation, because most likelihood models require a pre-specified state space (e.g. $0$ and $1$). Non-parametric methods like parsimony
do not have this assumption, and, in fact, it is not advisable to treat multi-state characters as a set of binary traits because 
the estimation algorithms consider the traits independent (when they are not) [@rexova2003cladistic; @tutorialNicholsWarnow; @warnow2017computational]. For example, a lexical character denoting "bird" may have
states $1$, $2$, and $3$, each representing a different cognate class observed in attested languages. Treating this as binary would create three traits,
referring to whether or not the languages exhibit each of these cognate classes in the "bird" meaning. Unless there is a reason to make this binary conversion
(e.g. because it is necessary to run likelihood algorithms), we suggest to leave the data in the underlying multi-state form.

The app then presents some statistics about the dataset, and one can perform some simple analyses:

* **Parsimony Uninformative Characters:** The characters that are not *parsimony informative* are displayed. These characters
will have no effect on parsimony-based tree estimation because they can be fit equally well to any tree (see @warnow2017computational for a discussion). This 
is especially helpful to a linguist, who may not be thinking about the consequences of character codings to 
the parsimony algorithm when coding individual characters. This thus allows a linguist to carefully consider coding choices.

* **Character-level Statistics:** Various information about each character is displayed, such as the number of
languages having polymorphic states for that character and whether or not
the character is parsimony-informative (among others). The dataset may be sorted by these metrics.

* **Clade Analysis:** The user may select a subset of languages and analyze what characters provide support for 
such a clade (a clade is a subset of languages separated from all other languages by an edge in the tree).
This is computed in the strictest sense: a character only supports a hypothetical clade if the languages
in the clade all share the same state, and all other languages share a different state.

<!---The first five columns are special columns that provide information about each character; the remaining columns are 
language states. The five special columns are:
* **id:** The unique identifier for a character (typically short)
* **feature:** The name of the character (typically a longer description)
* **weight optional, default 1:** The (integer) weight the character should be given in phylogenetic analyses
* **chartype optional, default 'standard':** The character type. This may be *standard*, *irreversible*, or *custom*,
the last of which requires a special declaration format described in the app itself.--->


## Tree Search

Then, the user may proceed to the second page of the app, which conducts a search for the optimal tree(s) given the 
dataset. We use PAUP\* [@swofford2002phylogenetic] to perform tree search, a well-established package in the biological community for running parsimony 
and other phylogenetic analyses. The user may specify various optimization criteria in the app without
having to write configuration files by hand, which is a big barrier to entry for many linguists. Nonetheless, users may download these 
configuration files from the app and modify them as needed.

## Analysis

Finally, one may use the dataset to analyze trees. These trees can be either the result of a PAUP\* tree search, or 
specific trees of interest uploaded by the user. This latter option is especially helpful for determining the support that a
dataset exhibits for various trees accepted by the community. Strict and majority consensus trees for the trees returned by PAUP\*
are displayed as well. The primary analyses that can be performed on a tree are the following:

1. **Tree Score:** Each tree is scored using various metrics, including *parsimony*, *compatibility*, *total edge support*, and
*minimum edge support*. Hence, the trees can be ranked according to these options.

2. **Character annotations:** The user may select any character and see the most parsimonious annotation(s) of that character's
states across the tree (including reconstructed states at internal nodes). This is convenient for studying a character's behavior, and can help a linguist
interpret the consequences of particular character codings on phylogeny estimation.

3. **Incompatible characters:** This reports the characters that are not compatible on a tree. This is useful
for considering how plausible various trees are: if the set of characters that a tree is not compatible on seems unrealistic, 
a linguist may wish to discard the tree in favor of other options.

4. **Enforcing characters:** This reports the characters that enforce, or support, each edge in the tree. Thus, one may analyze evidence for 
and against various clades.

5. **Relative chronology:** This reports a relative chronology of state changes *across* characters. This is calculated by first
determining the most parsimonious state transitions for each character, and then ordering these transitions based on
the edges they occur on from the root of the tree to a specified clade. This type of relative chronology may seem unusual to the
typical historical linguist, but its results can be illuminating.

\autoref{fig:example} depicts an example tree analysis in LinguiPhyR.

![Analysis page of LinguiphyR.\label{fig:example}](figure.png)

# Conclusions

We present LinguiPhyR, a useful tool for analyzing phylogenetic datasets and trees without the need to code. Even for experienced 
programmers, LinguiPhyR can quickly enable analysis on a new linguistic dataset or provide a starting place
for finding new trees. In our app, we especially emphasize 
parsimony-based interpretability
by providing useful visualizations and tools to see the impact of certain coding decisions on tree estimation. Future work will include the incorporation
of other inference methods (such as distance-based and quartet approaches), as well as more advanced analytical tools, such as bootstrap 
analysis.

<!---# Citations

Citations to entries in paper.bib should be in
[rMarkdown](http://rmarkdown.rstudio.com/authoring_bibliographies_and_citations.html)
format.

If you want to cite a software repository URL (e.g. something on GitHub without a preferred
citation) then you can do it with the example BibTeX entry below for @fidgit.

For a quick reference, the following citation commands can be used:
- `@author:2001`  ->  "Author et al. (2001)"
- `[@author:2001]` -> "(Author et al., 2001)"
- `[@author1:2001; @author2:2001]` -> "(Author1 et al., 2001; Author2 et al., 2002)"

# Figures

Figures can be included like this:
![Caption for example figure.\label{fig:example}](figure.png)
and referenced from text using \autoref{fig:example}.

Figure sizes can be customized by adding an optional second parameter:
![Caption for example figure.](figure.png){ width=20% }--->

# Acknowledgements

The author would like to acknowledge Thomas Olander, Matthew Scarborough,
Simon Poulsen, Anders Jørgensen, Stefanos Baziotis, and Tandy Warnow, who have all provided invaluable feedback throughout
the project.

This research was supported by the research project Connecting the Dots: Reconfiguring the Indo-European Family Tree (2019–2024), financed by the Independent Research Fund Denmark (project number 9037-00086B).

# References
