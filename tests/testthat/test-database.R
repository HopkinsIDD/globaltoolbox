# Test that the package correctly identifies country names

source("help-functionality_check.R")

context("Database Low Level Interface")

test_that("default_database_filename", {
  expect_error({
    default_database_filename()
  },
  NA
  )
  expect_equal({
    class(default_database_filename())
  },
  "character"
  )
})

test_that("Database Reset",{
  expect_error(create_database(tdbn), NA)
  expect_error(reset_database(tdbn), NA)
  expect_equal(file.exists(tdbn), TRUE)
  expect_error(reset_database(tdbn), NA)
  expect_equal(file.exists(tdbn), TRUE)
})

test_that("Database Creation", {
  skip_on_cran()
  expect_error(create_database(tdbn), NA)
  expect_error(create_database(tdbn), NA)
})

test_that("Add Location", {
  initialize_database()
  expect_error({
    database_add_location(
      name = "TST",
      readable_name = "Test",
      metadata = list("test" = 1),
      tdbn)
  },
  NA
  )
  expect_error({
    database_add_location(
      name = "TST2",
      readable_name = "Test",
      metadata = list("test" = 1, test2 = "foo"),
      tdbn)
  },
  NA
  )
  expect_error({
    database_add_location(
      name = "TST3",
      readable_name = "Test",
      metadata = list("test" = "John's Data"),
      tdbn
    )
  },
  NA
  )
  # Can't have multiple standard locations with the same name
  expect_error({
    database_add_location(
      name = "TST",
      readable_name = "Test",
      metadata = list("test" = 1)
    )
  }
  )
  # Can't have standard location with no metadata
  expect_error({
    database_add_location(
      name = 'TST',
      readable_name='Test',
      metadata = NULL
    )
  })

  # Custom database

  initialize_database()
  expect_error({
      database_add_location(
          name = "TST",
          readable_name = "Test",
          metadata = list("test" = 1),
         dbname = tdbn
      )
  },
  NA
  )
  expect_error({
      database_add_location(name = "TST", readable_name = "Test", dbname = fdbn)
  })
})

test_that("Add Hierarchy", {
  initialize_database()
  create_locations()
  expect_error(database_add_hierarchy(1, 3, 1, tdbn), NA)
  expect_error(database_add_hierarchy(1, 3, 1, tdbn))
})

test_that("Add Alias", {
  initialize_database()
  create_locations()
  expect_error(database_add_location_alias(1, "Alias", tdbn), NA)
  expect_error(database_add_location_alias(1, "Alias", tdbn))
  expect_error(database_add_location_alias(1, "Test", tdbn), NA)
  expect_error(database_add_location_alias(1, "Test", tdbn))
  expect_error(database_add_location_alias(3, "Alias", tdbn), NA)
})

test_that("Add Geometry", {
  initialize_database()
  create_locations()
  skip_if(!require(sf), "Geometry operations require the sf package")
  skip_if(
    !require(lubridate),
    "Geometry operations require the lubridate package"
  )
  expect_error({
    database_add_location_geometry(
      1,
      lubridate::now() - lubridate::years(1),
      lubridate::now(),
      st_sfc(st_point(c(0, 1))),
      tdbn
    )
  },
  NA
  )
  expect_error({
    database_add_location_geometry(
      1,
      lubridate::now() - lubridate::years(1),
      lubridate::now(),
      st_sfc(st_point(c(1, 1))),
      tdbn
    )
  })
  expect_error({
    database_add_location_geometry(
      1,
      lubridate::now(),
      lubridate::now() + lubridate::years(1),
      st_sfc(st_point(c(1, 1))),
      tdbn
    )
  },
  NA
  )
  expect_error({
    database_add_location_geometry(
      1,
      lubridate::now() - lubridate::years(1),
      lubridate::now() + lubridate::years(1),
      st_sfc(st_point(c(1, 1))),
      tdbn
    )
  })
  expect_error({
      database_add_location_geometry(
        2,
        lubridate::now() - lubridate::years(1),
        lubridate::now() + lubridate::years(1),
        st_sfc(st_point(c(1, 1))),
        tdbn
      )
    },
    NA
  )
})

test_that("Retrieve ID", {
  initialize_database()
  create_locations()
  expect_equal(get_database_id_from_name("", dbname = tdbn), 1)
  expect_equal(get_database_id_from_name("::tst", dbname = tdbn), 2)
  expect_equal(get_database_id_from_name("::tst2", dbname = tdbn), 3)
  expect_equal(get_database_id_from_name("::tst::tst", dbname = tdbn), 4)
  expect_equal(
    get_database_id_from_name(
      c("", "::tst", "::tst::tst", "::tst2"),
      dbname = tdbn
    ),
    setNames(
      c(1, 2, 4, 3),
      c("", "::tst", "::tst::tst", "::tst2")
    )
  )
})

test_that("Merge Locations", {
  initialize_database()
  create_test_database()
  expect_error({
    database_merge_locations(
      "::tst",
      "::tst2",
      dbname = tdbn
    )
  },
    NA
  )

  expect_equal({
      is.na(get_database_id_from_name("::tst2", dbname = tdbn))
  },
  FALSE)

  expect_error({
      is.na(get_database_id_from_name("::tst", dbname = tdbn))
  },
  "Ambiguous location ::tst has 0 location_ids")

  expect_equal({
      initialize_database()
      create_test_database()
      tst_aliases <- get_all_aliases("::tst", dbname = tdbn)
      tst2_aliases <- get_all_aliases("::tst2", dbname = tdbn)
      database_merge_locations(
          "::tst",
          "::tst2",
          dbname = tdbn
      )
      new_tst2_aliases <- get_all_aliases("::tst2", dbname = tdbn)
      isTRUE(all.equal(
          sort(unique(c(tst_aliases$alias, tst2_aliases$alias))),
          sort(unique(new_tst2_aliases$alias))
      ))
  },
  TRUE)

  expect_equal({
      initialize_database()
      create_test_database()
      tst_metadata <- get_location_metadata("::tst", dbname = tdbn,aliases=FALSE)
      tst2_metadata <- get_location_metadata("::tst2", dbname = tdbn,aliases=FALSE)
      database_merge_locations(
          "::tst",
          "::tst2",
          dbname = tdbn
      )
      new_tst2_metadata <- get_location_metadata("::tst2", dbname = tdbn,aliases=FALSE)
      lhs = cbind(tst_metadata[,-(1:8),drop=FALSE], tst2_metadata[,-(1:8),drop=FALSE])
      rhs <- new_tst2_metadata[,-(1:8),drop=FALSE]
      all(names(rhs) %in% names(lhs))
  },
  TRUE)

  expect_equal({
      initialize_database()
      create_test_database()
      tst_metadata <- get_location_metadata("::tst", dbname = tdbn,aliases=FALSE)
      tst2_metadata <- get_location_metadata("::tst2", dbname = tdbn,aliases=FALSE)
      database_merge_locations(
          "::tst",
          "::tst2",
          dbname = tdbn
      )
      new_tst2_metadata <- get_location_metadata("::tst2", dbname = tdbn,aliases=FALSE)
      lhs = cbind(tst_metadata[,-(1:8),drop=FALSE], tst2_metadata[,-(1:8),drop=FALSE])
      rhs <- new_tst2_metadata[,-(1:8),drop=FALSE]
      toggle <- TRUE
      for(name in names(rhs)){
        toggle <- toggle && all(rhs[[name]] %in% lhs[names(lhs) == name])
      }
      return(toggle)
  },
  TRUE)

  expect_equal({
      initialize_database()
      create_test_database()
      tst_children <- get_location_metadata(source="::tst", dbname = tdbn,aliases=FALSE)
      tst2_children <- get_location_metadata(source="::tst2", dbname = tdbn,aliases=FALSE)
      database_merge_locations(
          "::tst",
          "::tst2",
          dbname = tdbn
      )
      new_tst2_children <- get_location_metadata(source="::tst2", dbname = tdbn,aliases=FALSE)
      new_tst2_children$id %in% c(tst_children$id,tst2_children$id)
  },
  TRUE)
})
