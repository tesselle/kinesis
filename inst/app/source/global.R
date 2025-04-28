## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## Title:   Compositional Data Analysis
## Authors: Nicolas Frerebeau, Université Bordeaux Montaigne (France)
## Contact: nicolas.frerebeau@u-bordeaux-montainge.fr
##++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

## Set shiny settings ==========================================================
options(shiny.maxRequestSize = 30*1024^2)

## Set future strategy =========================================================
future::plan(
  strategy = future::multisession,
  workers = getOption("kinesis.workers") %||% 1
)
