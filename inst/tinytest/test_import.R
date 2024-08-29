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
