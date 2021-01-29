export_acns <- function(
  .data,
  date = attr(.data, "date"),
  dir = "V:/EPI DATA ANALYTICS TEAM/Case Assignment/Daily Data",
  force = FALSE
) {

  title <- paste0("positive_tests_", date)
  file <- path_create(dir, title, ext = "xlsx")

  wb <- openxlsx::createWorkbook(title = title)
  openxlsx::addWorksheet(wb, "positive_tests")

  openxlsx::writeDataTable(
    wb,
    sheet = "positive_tests",
    x = .data,
    bandedRows = FALSE
  )

  openxlsx::protectWorkbook(
    wb,
    password = Sys.getenv("excel_pwd"),
    lockStructure = TRUE
  )

  openxlsx::protectWorksheet(
    wb,
    "positive_tests",
    password = Sys.getenv("excel_pwd"),
    lockSelectingUnlockedCells = FALSE,
    lockSorting = FALSE,
    lockAutoFilter = FALSE,
    lockSelectingLockedCells = FALSE,
    lockFormattingCells = TRUE,
    lockFormattingColumns = FALSE,
    lockFormattingRows = TRUE,
    lockInsertingColumns = TRUE,
    lockInsertingRows = TRUE,
    lockInsertingHyperlinks = TRUE,
    lockDeletingColumns = TRUE,
    lockDeletingRows = TRUE,
    lockPivotTables = TRUE,
    lockObjects = TRUE,
    lockScenarios = TRUE
  )

  openxlsx::addStyle(
    wb,
    "positive_tests",
    style = openxlsx::createStyle(locked = FALSE),
    rows = vctrs::vec_seq_along(.data) + 1L,
    cols = match("DECEASED", colnames(.data)),
    gridExpand = TRUE
  )

  saved <- openxlsx::saveWorkbook(
    wb,
    file = file,
    overwrite = force,
    returnValue = TRUE
  )

  if (rlang::is_condition(saved)) {
    rlang::abort(
      paste(
        "Export was not successful. if you are trying to overwrite",
        "an existing file, ensure that the file is closed",
        "and set `force = TRUE`."
      )
    )
  }
}
