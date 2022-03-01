# CONTAM - factorialization using JSON

#### how to run
```
app.R
```

#### helpful links
* [JSON formatting](https://jsonformatter.curiousconcept.com/#)

# to add a new category to filter on

1. add tabPanel line to app.R

1. add source(methods_XX) to app.R

1. make a new methods_XX.R file (by copying the filter one -- make sure to change the names of each new function) -- Look at CONTAM usermanual to see what each section of the prj means

1. make XX.json, a json rendering of the base objects -- JSON NAMES CANNOT HAVE "_"

1. add a clause to get_json_choices in `custom_function.R`

1. update section_parser.R to include the section you need (see contam_sections.txt)

1. need to updated server functions to include each section as you build it
(# <<<<*****>>>>>) ==> there are 5 places

checkpoints
1. make the XX.json elements show up when you load the page <<
2. make the "Base_species" read_parser show up << have to update section_parser.R


need to  make the thing that makes it with every combination across tabsets

#<>> WHAT IF YOU ONLY WANT TO DO ONE TYPE OF SWAP IN??

# notes on making an exe
* [this link](https://foretodata.com/how-to-make-a-standalone-desktop-application-with-shiny-and-electron-on-windows/) with a few modification
* in this `R-windows`, have to use the `install_packages_cm.R` command
* have to delete the electron process from the task manager
* upadate `gitignore`, and `package.json` with `package-cm`
