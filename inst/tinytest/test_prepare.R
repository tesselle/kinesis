library("shiny")
using("tinysnapshot")

path <- system.file("tinytest", "fake.csv", package = "arkhe")
fake <- read.csv(path)
x <- reactiveVal(fake)

# remove_whitespace = FALSE
# remove_zero_row = FALSE
# remove_zero_column = FALSE
# remove_constant_column = FALSE
# all = FALSE
# zero_as_NA = FALSE
# remove = "none"

# Prepare ======================================================================
# TODO
# testServer(prepare_server, args = list(x = x, id = "foo"), {
#
# })

# Select =======================================================================
testServer(kinesis:::select_server, args = list(x = x), {
  session$setInputs(select = colnames(fake))
  dataset <- session$getReturned()
  expect_equal(dataset(), fake)

  session$setInputs(select = c("doi", "color", "height"))
  dataset <- session$getReturned()
  expect_equal(dataset(), fake[, c("doi", "color", "height")])
})

# Clean ========================================================================
testServer(kinesis:::clean_server, args = list(x = x), {
  dataset <- session$getReturned()
  expect_equal(dataset(), fake)

  session$setInputs(remove_zero_row = TRUE, all = FALSE)
  expect_equal(dataset(), arkhe::remove_zero(fake, margin = 1, all = FALSE))

  session$setInputs(remove_zero_row = FALSE)
  session$setInputs(remove_zero_column = TRUE, all = FALSE)
  expect_equal(dataset(), arkhe::remove_zero(fake, margin = 2, all = FALSE))
})

# Missing ======================================================================
testServer(kinesis:::missing_server, args = list(x = x), {
  dataset <- session$getReturned()
  expect_equal(dataset(), fake)

  session$setInputs(zero_as_NA = FALSE)
  session$setInputs(remove = "zero")
  expect_equal(dataset(), arkhe::replace_NA(fake, value = 0))

  session$setInputs(remove = "col")
  expect_equal(dataset(), arkhe::remove_NA(fake, margin = 2))

  session$setInputs(remove = "row")
  expect_equal(dataset(), arkhe::remove_NA(fake, margin = 1))

  session$setInputs(remove = "none")
})

# Filter =======================================================================
testServer(kinesis:::filter_server, args = list(x = x), {
  expect_equal(filter(), TRUE)

  session$setInputs(
    doi = fake$doi,
    color = fake$color,
    height = c(0.06, 0.79)
  )
  sub <- !is.na(fake$height) & fake$height >= 0.06 & fake$height <= 0.79
  expect_equal(filter(), sub)

  session$setInputs(
    doi = fake$doi,
    color = c("LightCoral", "DarkSlateBlue", "OrangeRed"),
    height = c(0.06, 0.79)
  )
  sub <- !is.na(fake$height) & fake$height >= 0.06 & fake$height <= 0.79 &
    fake$color %in% c("LightCoral", "DarkSlateBlue", "OrangeRed")
  expect_equal(filter(), sub)
})

# Create sample data
df <- data.frame(x = c(1, 2, 3), y = c("A", "B", "C"))

# Filter data
filter_html <- mapply(
  FUN = kinesis:::filter_build, x = df, id = c("num", "char"), var = colnames(df),
  MoreArgs = list(num = TRUE, char = TRUE)
)
expect_snapshot_print(as.character(filter_html), label = "filter_build")
expect_equal(kinesis:::filter_var(x = df$x, val = c(1.5, 2.5)), c(FALSE, TRUE, FALSE))
expect_equal(kinesis:::filter_var(x = df$y, val = "B"), c(FALSE, TRUE, FALSE))
