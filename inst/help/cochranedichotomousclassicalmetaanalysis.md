Classical Meta-Analysis of Cochrane Dichotomous Outcomes
===

The Cochrane module allow the user to re-analyze meta-analyses from the Cochrane Database of Systematics Reviews, combine multiple analyses, and extend them with their own data. Please, see the help files of the Classical Meta-Analysis in the Meta-Analysis module for information on the individual analysis options.

### Database
---
#### Select systematic reviews based on
- Topics: Show all systematic reviews that correspond to a given topic.
- Keywords: Show all systematic reviews that contain a given keyword (only first 1000 keywords are shown).
  - Search: Filter keywords according to a search. Only complete matches are used by default, to use partial matches pre-pend the search term by "_". E.g., to filter all keywords including "cre" search with "_cre".
- Search titles: Show all meta-analyses which title contains at least one of the specified search terms (including partial matches). Use commas (,) to separate multiple search terms or add minus (-) in front of a search term to omit corresponding matches. E.g., to search for all meta-analysis titles including terms child but excluding term adult, search with "child, -adult"

#### Analyze Data
- Individually: Analyze each meta-analysis separetly.
- Pooled: Combine primary studies across meta-analyses.

#### Plot
- Effect sizes: Visualize effect sizes of the primary studies as a histogram.
- Sample sizes: Visualize sample sizes of the primary studies as a histogram.
- Display density: Add density of to the histogram.
- Display rug marks: Add rug marks to the histograms.

#### Add estimates
Add additional primary studies to analyses (they will be highligted with blue color in the Forest plot).
You will need to specify the Effect Size and Standard Error or the Confidence Interval.
