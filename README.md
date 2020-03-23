# ncov-outputs-example

This is a stripped down version of the outputs from analysis of 2019-nCoV - it is an [`orderly`](https://github.com/vimc/orderly) project.  The directories are:

* `src`: create new outputs here
* `archive`: versioned results of running your output
* `data`: copies of data used in the outputs

## Quickstart

### Really quick

Clone the repository onto your computer

```
git clone https://github.com/ncov-ic/ncov-outputs-example
```

Open the Rstudio project from within `ncov-outputs`

### Orderly documentation

There is some basic documentation here: https://vimc.github.io/orderly/articles/orderly.html but these are still quite VIMC specific.  We will improve them!  Please post questions/complaints/suggestions for improvements to the slack or to [the ncov-outputs issue tracker](https://github.com/ncov-ic/ncov-outputs/issues).

### Package installation

You need a few packages to compile outputs:

```
# install.packages("drat") # (install this if you don't have it already)
drat:::add("ncov-ic")
install.packages(c("orderly", "vaultr", "orderlyweb"))
```

`orderly` and `vaultr` are on CRAN but you will need `orderlyweb` if you want to pull reports from the server.

### Create a new output skeleton

```r
orderly::orderly_new("my_output_name")
```

The name is important as it cannot be changed after outputs have been run.  Consider a good prefix.

### Run your output locally

```r
orderly::orderly_run("my_output_name")
```

if you want your R objects to persist in the session later then add `envir = .RGlobalEnv`; see `?orderly_run` for more details.
