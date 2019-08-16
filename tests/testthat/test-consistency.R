source("help-functionality_check.R")

context("Tree Consistency")

test_that("detect subtree match", {
  initialize_database()
  create_test_database()
  expect_equal({
    detect_subtree_match("::tst","::tst2",dbname=tdbn)
  },
  1)
})

test_that("detect all subtree matches", {
  initialize_database()
  create_test_database()
  expect_equal({
    detect_all_subtree_matches("",dbname=tdbn)$match_distribution
  },
  1
  )

})
