source("help-functionality_check.R")
context("Name standardization")




# Test: standarize_name() -------------------------------------------------

test_that("standardize_name", {
  initialize_database()
  create_test_database()
  expect_error({
    standardize_name("tst", dbname = tdbn)
  },
  NA)

  expect_error({
    standardize_name("tst2", dbname = tdbn)
  },
  NA)

  expect_error({
    standardize_name("tst3", dbname = tdbn)
  },
  NA)
})


# Test: match_names() -----------------------------------------------------

test_that("match_name", {
  expect_equal({
    match_names(
      "test",
      data.frame(
        id = 1,
        name = "test",
        depth = 1,
        id_tmp = 1
      ),
      return_match_scores = FALSE,
      clean_a = FALSE,
      clean_b = FALSE
    )
  },
  setNames(1, "test")
  )

  names_b_data_frame <- create_names_b_data()

  expect_equal({
    match_names(
      "testtesttesttest",
      names_b_data_frame,
      return_match_scores = FALSE,
      clean_a = FALSE,
      clean_b = FALSE
    )
  },
  setNames(as.integer(NA),"testtesttesttest")
  )

  expect_error({
    match_names(
      "test",
      names_b_data_frame,
      return_match_scores = FALSE,
      clean_a = FALSE,
      clean_b = FALSE
    )
  },
  NA
  )

  expect_equal({
    match_names(
      "baltimore",
      names_b_data_frame,
      return_match_scores = FALSE,
      clean_a = FALSE,
      clean_b = FALSE
    )
  },
  setNames(5, "baltimore")
  )

  expect_equal({
    match_names(
      "taznania",
      names_b_data_frame,
      return_match_scores = FALSE,
      clean_a = FALSE,
      clean_b = FALSE
    )
  },
  setNames(7, "tanzania")
  )

  expect_equal({
    match_names(
      "bar",
      names_b_data_frame,
      return_match_scores = FALSE,
      clean_a = FALSE,
      clean_b = FALSE
    )
  },
  setNames(as.integer(NA), "bar")
  )

  expect_equal({
    match_names(
      "ficshun",
      names_b_data_frame,
      return_match_scores = FALSE,
      clean_a = FALSE,
      clean_b = FALSE
    )
  },
  setNames(12, "phiction")
  )
})
