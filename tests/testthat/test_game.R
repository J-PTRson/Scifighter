test_that("get stats works", {
  expect(length(get_stats("hero"))==5, failure_message = "Returning dimensions of get_stats are off. Check .json!")
})

test_that("attack works", {
  expect(attack("hero","punch")==1, failure_message = "Returning dimensions of get_stats are off. Check .json!")
})
