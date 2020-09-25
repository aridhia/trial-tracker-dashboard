# Report Generation

## Requirements

**R Packages**

- `knitr`
- `brew`
- `tinytex`

**Installing LaTeX**

LaTeX is required to compile PDF reports from Rnw files. A complete installation of this can be setup
or the `tinytex` package can be used to create a minimal installation

```{r}
tinytex::install_tinytex()
```

R may need to be restarted after installing.

## Report Templates

The `brew` package provides syntax for creating template reports.
The .brew files are converted to .Rnw files by the `brew` function so the complete
workflow from a .brew file to a pdf is:

.brew -> .Rnw -> .tex -> .pdf

This workflow can be wrapped up into one function similar to the `knit2pdf` funtion in `knitr` 



## Generating Reports

A simple .brew template is included in the repository which requires titles and outputs objects of the same length.
This can be tested from the global environment with the brew -> .Rnw -> .tex -> .pdf workflow.

Utility functions are provided in reporting/report_template.Rnw which bundle up the required steps
and keep evaluation within the function environment. It is recommended that these are used for
use within larger solutions to avoid confusion with objects in different environments.

## Examples

```{r}
source("reporting/reporting_utils.R")

# A basic example with dummy outputs
gen_report_old(titles = c("Section 1", "Section 2", "Section 3"), outputs = list(1, 2, 3))

# Plot outputs
library(ggplot2)
p1 <- ggplot(iris, aes(x = Sepal.Length, y = Petal.Length, color = Species, fill = Species)) + geom_point()
p2 <- ggplot(iris, aes(x = Sepal.Length)) + geom_histogram()
p3 <- ggplot(iris, aes(x = Sepal.Length, color = Species)) + geom_density()

gen_report_old(titles = c("Scatterplot", "Histogram", "Density Plot"), outputs = list(p1, p2, p3), types = c("plot", "plot", "plot"))

# Table outputs
library(knitr)

t1 <- kable(head(iris), format = "latex")
t2 <- kable(tail(iris), format = "latex")
t3 <- kable(iris[51:60, ], format = "latex")
gen_report_old(titles = c("Head", "Tail", "Middle"), outputs = list(t1, t2, t3))

```






