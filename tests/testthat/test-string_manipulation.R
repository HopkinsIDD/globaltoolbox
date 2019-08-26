source("help-functionality_check.R")
context("String manipulation")

test_that("standardize string", {
  expect_equal({
    results <- fuzzr::fuzz_function(
      standardize_string,
      "string",
      tests = fuzzr::test_char()
    )
    all(is.na(as.data.frame(results)$errors))
  },
  TRUE
  )

  expect_equal({
    results <- fuzzr::fuzz_function(
      standardize_string,
      "string",
      tests = fuzzr::test_all()
    )
    unique(as.data.frame(results)$result_classes)
  },
  "list"
  )

  expect_equal({
    standardize_string("A")
  },
    list("a")
  )


  expect_equal({
    standardize_string("A-a")
  },
    list("aa")
  )

  expect_equal({
    standardize_string("A|a")
  },
    list(c("a", "a"))
  )

  expect_equal({
    standardize_string(".!@#$%^&*()`~?/>.<,'\"\\")
  },
    list("")
  )

  ## Unicde stuff goes here
  expect_equal({
      standardize_string("brûlée")
  },
  list("brulee")
  )

  expect_equal({
      standardize_string("鬼")
  },
  list("gui")
  )

  expect_equal({
      standardize_string("φάντασμα")
  },
  list("phantasma")
  )

  expect_equal({
      standardize_string("ผี")
  },
  list("phi")
  )

  expect_equal({
      standardize_string("призрак")
  },
  list("prizrak")
  )

  expect_equal({
      standardize_string("شبح")
  },
  list("shbh")
  )
})
