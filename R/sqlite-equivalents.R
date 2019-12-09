#' @export
#' @param dbname The name of the database to connect to
remove_sqlite_database <- function(dbname = default_database_filename(),...){
  con <- DBI::dbConnect(drv = RPostgres::Postgres(), dbname = dbname,...)

  tryCatch({
    DBI::dbClearResult(
      DBI::dbSendQuery(
        con,
      "drop schema sqlite cascade"
      )
    )
  },
  error = function(e){
    RSQLite::dbDisconnect(con)
    stop(e$message)
  })
}



#' @export
#' @param dbname The name of the database to connect to
create_sqlite_database <- function(dbname = default_database_filename(),...){
  con <- DBI::dbConnect(drv = RPostgres::Postgres(), dbname = dbname,...)

  tryCatch({
    DBI::dbClearResult(
      DBI::dbSendQuery(
        con,
      "create schema sqlite"
      )
    )
  },
  error = function(e){
    RSQLite::dbDisconnect(con)
    stop(e$message)
  })

  tryCatch({
    DBI::dbClearResult(
      DBI::dbSendQuery(
        con,
        "CREATE TABLE sqlite.locations as (
          SELECT
            id,
            id || '_' || name as name,
            readable_name,
            '{}' as metadata
          FROM
            location_geometries
        );"
      )
    )
  },
  error = function(e){
    RSQLite::dbDisconnect(con)
    stop(e$message)
  })

  tryCatch({
    DBI::dbClearResult(
      DBI::dbSendQuery(
        con,
        "CREATE TABLE sqlite.location_hierarchy as (
          SELECT
            links.ancestor as parent_id,
            links.descendant as descendant_id,
            count(*) as depth
          FROM ((location_hierarchy links
            LEFT JOIN location_hierarchy lhs ON
              ((
                (links.ancestor = lhs.ancestor) AND
                (links.time_left <= lhs.time_right) AND
                (links.time_right >= lhs.time_left)
              ))
            )
            LEFT JOIN location_hierarchy rhs ON (
              (
                (lhs.descendant = rhs.ancestor) AND
                (links.descendant = rhs.descendant) AND
                (links.time_left <= rhs.time_right) AND
                (links.time_right >= rhs.time_left) AND
                (lhs.time_left <= rhs.time_right) AND
                (lhs.time_right >= rhs.time_left)
              )
            )
          )
          WHERE (
            (rhs.ancestor is NOT NULL) OR
            (lhs.descendant = links.descendant)
          )
          GROUP BY
            links.ancestor,
            links.descendant,
            links.time_left,
            links.time_right
        );"
      )
    )
  },
  error = function(e){
    RSQLite::dbDisconnect(con)
    stop(e$message)
  })

  tryCatch({
    DBI::dbClearResult(
      DBI::dbSendQuery(
        con,
        "CREATE TABLE sqlite.location_geometries as (
          SELECT
            id,
            id as location_id,
            time_left,
            time_right,
            ST_AsText(inner_geometry) as geometry
          FROM location_geometries
        );"
      )
    )
  },
  error = function(e){
    RSQLite::dbDisconnect(con)
    stop(e$message)
  })

  tryCatch({
    DBI::dbClearResult(
      DBI::dbSendQuery(
        con,
        "CREATE TABLE sqlite.location_aliases as (
          SELECT
            alias,
            geometry_id as location_id
          FROM
            location_aliases
        );"
      )
    )
  },
  error = function(e){
    RSQLite::dbDisconnect(con)
    stop(e$message)
  })
}


###  tryCatch({
###    DBI::dbClearResult(
###      DBI::dbSendQuery(
###        con,
###        "CREATE MATERIALIZED VIEW location_hierarchy as (
###        SELECT
###          ancestor,
###          descendant,
###          time_left,
###          time_right
###        FROM
###          pre_location_hierarchy
###        WHERE
###        (
###          st_area(st_difference(descendant_geometry, ancestor_geometry)) /
###          st_area(descendant_geometry)
###        ) < (0.1)::double precision
###      )"
###      )
###    )
###  },
###  error = function(e){
###    RSQLite::dbDisconnect(con)
###    stop(e$message)
###  })
