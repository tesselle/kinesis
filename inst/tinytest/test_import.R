Sys.setenv(LANGUAGE = "en") # Force locale
library("shiny")

# Import =======================================================================
path <- "fake.csv"
fake <- read.csv(path)

testServer(import_server, {
  session$setInputs(header = TRUE, sep = ",",
                    dec = ".", quote = "\"'", rownames = FALSE,
                    na.strings = "NA", skip = 0, comment = "#")

  session$setInputs(file = list(datapath = "path.txt"), go = 1)
  expect_null(data$values)

  session$setInputs(file = list(datapath = path), go = NULL)
  expect_null(data$values)

  session$setInputs(file = list(datapath = path), go = 1)
  expect_equal(data$values, fake)
})

testServer(kinesis:::import_server, args = list(demo = "compiegne"), {
  session$setInputs("demo" = 1)
  expect_equal(dim(data$values), c(5L, 16L))
})
