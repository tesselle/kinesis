library("shiny")
using("tinysnapshot")

fake <- data.frame(
  group = rep(c("A", "B", "C"), each = 3),
  Ca = c(7.72, 0, 3.11, 7.19, 7.41, 5, 0, 1, 4.51),
  Fe = c(6.12, 5.88, 5.12, 0, 6.02, 0, 0, 5.28, 5.72),
  Na = c(0.97, 1.59, 0, 0.86, 0.76, 0.51, 0.75, 0.52, 0.56)
)
imp <- nexus::replace_zero(nexus::as_composition(fake, groups = 1),
                           value = c(0.02, 0.1, 0.01) / 100, delta = 2/3)
x <- reactiveVal(fake)

testServer(coda_server, args = list(x = x), {
  session$setInputs(groups = "", condense = "group")
  expect_equal(NROW(coda()), 3)

  session$setInputs(groups = "", condense = "", delta = 2/3,
                    limit_Ca = 0, limit_Fe = 0, limit_Na = 0)
  expect_error(valid(), "Compositional data must not contain zeros")

  session$setInputs(groups = "group", delta = 2/3,
                    limit_Ca = 0.02, limit_Fe = 0.1, limit_Na = 0.01)
  dataset <- session$getReturned()
  expect_equal(dataset(), imp)
})
