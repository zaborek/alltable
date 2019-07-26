# alltable
A table generating package in R that does it all.

to use:

args:
(1) dataset

(2) group variable within dataset (optional, if not given just use all observations)

(3) group labels for group variable

(4) data frame of columns:

-variables to include in output table (must be named vars)

-variable labels (must be named varlabs)

-numeric, percentage, categorical specs for each variable (must be named type)

-test type for each variable to compare across groups (optional, if included must be named test)


return:

@outmat
character matrix with number of columns equal to number of groups, plus one for p-value. Each row denotes a variable or a category of a categorical variable.

numeric variables present the mean (or median if specified), SD, and range.
Percentage vars present number=1 and percent (eg 50 (25%))
categorical vars present a row for each unique category as percentage vars

all stat tests default to kruskall-wallis for numerical, and uncorrected chi-square for categorical.

@p-value dataframe
a dataframe with two columns: variable name and numeric p-value.


=====
notes
=====

-pretty p-values

-rounding digits

-correct for multiple testing?

