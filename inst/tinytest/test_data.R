library("shiny")
using("tinysnapshot")

# Create sample data
df <- data.frame(x = c(1, 2, 3), y = c("A", "B", "C"))

# read_table() handles all input types
path_csv <- tempfile()
path_csv2 <- tempfile()
path_tsv <- tempfile()
write.csv(df, path_csv, row.names = FALSE)
write.csv2(df, path_csv2, row.names = FALSE)
write.table(df, path_tsv, sep = "\t", row.names = FALSE)

expect_equal(kinesis:::read_table(path_csv, sep = ",", dec = "."), df)
expect_equal(kinesis:::read_table(path_csv2, sep = ";", dec = ","), df)
expect_equal(kinesis:::read_table(path_tsv, sep = "\t"), df)

# Filter data
filter_html <- mapply(
  FUN = kinesis:::make_filter, x = df, var = colnames(df),
  MoreArgs = list(session = list(ns = shiny::NS("test")), num = TRUE, char = TRUE)
)
expect_snapshot_print(as.character(filter_html), label = "make_filter")
expect_equal(kinesis:::filter_var(x = df$x, val = c(1.5, 2.5)), c(FALSE, TRUE, FALSE))
expect_equal(kinesis:::filter_var(x = df$y, val = "B"), c(FALSE, TRUE, FALSE))

# Import =======================================================================
path <- system.file("tinytest", "fake.csv", package = "arkhe")
fake <- read.csv(path)

testServer(import_server, {
  session$setInputs(file = list(datapath = path), header = TRUE, sep = ",",
                    dec = ".", quote = "\"'", rownames = FALSE,
                    na.strings = "NA", skip = 0, comment = "#")
  expect_equal(file(), path)
  expect_equal(data(), fake)
})

# Prepare ======================================================================
x <- reactiveVal(fake)

testServer(prepare_server, args = list(x = x), {
  # remove_whitespace = FALSE
  # remove_zero_row = FALSE
  # remove_zero_column = FALSE
  # remove_constant_column = FALSE
  # all = FALSE
  # zero_as_NA = FALSE
  # remove = "none"

  session$setInputs(select = colnames(fake))
  dataset <- session$getReturned()
  expect_equal(dataset(), fake)

  session$setInputs(select = colnames(fake))
  session$setInputs(remove_zero_row = TRUE, all = FALSE)
  expect_equal(data_filter(), arkhe::remove_zero(fake, margin = 1, all = FALSE))

  session$setInputs(remove_zero_row = FALSE)
  session$setInputs(remove_zero_column = TRUE, all = FALSE)
  expect_equal(data_filter(), arkhe::remove_zero(fake, margin = 2, all = FALSE))

  session$setInputs(remove_zero_column = FALSE)
  session$setInputs(zero_as_NA = TRUE)
  expect_equal(data_filter(), arkhe::replace_zero(fake, value = NA))

  session$setInputs(zero_as_NA = FALSE)
  session$setInputs(remove = "zero")
  expect_equal(data_filter(), arkhe::replace_NA(fake, value = 0))

  session$setInputs(remove = "col")
  expect_equal(data_filter(), arkhe::remove_NA(fake, margin = 2))

  session$setInputs(remove = "row")
  expect_equal(data_filter(), arkhe::remove_NA(fake, margin = 1))

  session$setInputs(remove = "none")
  session$setInputs(
    filter_doi = fake$doi,
    filter_color = fake$color,
    filter_height = c(0.06, 0.79)
  )
  sub <- !is.na(fake$height) & fake$height >= 0.06 & fake$height <= 0.79
  expect_equal(data_filter(), fake[sub, ])
})
