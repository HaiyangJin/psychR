test_that("reliability for subtraction works", {
  raw_main <- c(2, 3, 2.5, 5, 3, 4, 2, 1, 3, 1)
  raw_base <- c(2, 5, 2, 1, 3, 5, 3, 2, 4, 1)
  expect_equal(rel_corrected(.9, .8, raw_main, raw_base)$reliability, 0.77845994)
})

test_that("reliability for subtraction works with 'subt'", {
  raw_main <- c(2, 3, 2.5, 5, 3, 4, 2, 1, 3, 1)
  raw_base <- c(2, 5, 2, 1, 3, 5, 3, 2, 4, 1)
  expect_equal(rel_corrected(.9, .8, raw_main, raw_base, 'subt')$reliability, 0.77845994)
})

test_that("reliability for regression works", {
  raw_main <- c(2, 3, 2.5, 5, 3, 4, 2, 1, 3, 1)
  raw_base <- c(2, 5, 2, 1, 3, 5, 3, 2, 4, 1)
  expect_equal(rel_corrected(.9, .8, raw_main, raw_base, 'regression')$reliability, 0.8725551)
})

test_that("reliability for regression works", {
  raw_main <- c(2, 3, 2.5, 5, 3, 4, 2, 1, 3, 1)
  raw_base <- c(2, 5, 2, 1, 3, 5, 3, 2, 4, 1)
  expect_equal(rel_corrected(.9, .8, raw_main, raw_base, 'reg')$reliability, 0.8725551)
})
