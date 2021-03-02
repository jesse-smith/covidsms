#' Install dialr and System Dependencies
#'
#' `install_dialr()` installs dialr and the latest JDK release (if not already
#' installed)
#'
#' @param inst_java Should Java be installed if not already?
#'
#' @param java_dir Directory to install OpenJDK
#'
#' @return `TRUE` if successful; will error otherwise
#'
#' @export
install_dialr <- function(
  inst_java = !detect_java(dialr = TRUE),
  java_dir = "~"
) {
  # Check that remotes is installed
  coviData::assert(
    rlang::is_installed("remotes"),
    message = paste(
      "The remotes package must be installed to use `install_dialr()`;",
      "install using `install.packages('remotes')`."
    )
  )

  # Install Java if needed
  if (inst_java) install_java(dir = java_dir, check_java = FALSE)

  # Install dialr
  remotes::install_github(
    "socialresearchcentre/dialr",
    INSTALL_opts = "--no-multiarch"
  )

  TRUE
}

#' Install OpenJDK for Windows
#'
#' `install_java()` downloads and makes available the current release version
#' of OpenJDK, which is required for the dialr package. It sets the `JAVA_HOME`
#' variable and adds the JDK "bin" folder to the user `PATH`.
#'
#' @param dir Directory to save the JDK folder
#'
#' @param check_java Should the function attempt to detect Java prior to
#'   installation?
#'
#' @return The path to the OpenJDK directory (invisibly)
#'
#' @keywords internal
#'
#' @export
install_java <- function(dir = "~", check_java = TRUE) {
  # Check that Java is not already installed (if `check_java = TRUE`)
  coviData::assert(
    if (check_java) !detect_java(quiet = TRUE) else TRUE,
    message = "Java appears to already be installed; aborting `install_java()`."
  )

  # Check that OS is windows
  coviData::assert(
    Sys.info()[["sysname"]] == "Windows",
    message = "`install_java()` currently only works on Windows"
  )

  # Check xml2
  coviData::assert(
    rlang::is_installed("xml2"),
    message = paste(
      "The xml2 package must be installed to use `install_java()`;",
      "install using `install.packages('xml2')`."
    )
  )

  # Get URL to current Windows binary
  page   <- xml2::read_html("https://jdk.java.net/15/")
  anchor <- xml2::xml_find_all(page, "//blockquote//table//td//a")
  links  <- xml2::xml_attr(anchor, "href")
  url    <- links[grepl("(?i)win", links) & !grepl("(?i)sha256", links)]

  # Clean `dir`
  dir <- coviData::path_create(dir)

  # Create ZIP directory for JDK download
  zip_dir <- fs::file_temp(pattern = ".jdk_temp_")
  fs::dir_create(zip_dir)
  on.exit(fs::dir_delete(zip_dir), add = TRUE)

  # Download JDK
  zip_path <- coviData::path_create(zip_dir, "openjdk", ext = "zip")
  utils::download.file(url, destfile = zip_path, mode = "wb")

  # Extract and rename JDK directory
  jdk_dir <- zip_path %>%
    utils::unzip(list = TRUE) %>%
    dplyr::pull("Name") %>%
    stringr::str_extract("[^/]+(?=[/])") %>%
    vctrs::vec_unique()
  jdk_path <- coviData::path_create(dir, jdk_dir)
  coviData::assert(
    !fs::dir_exists(jdk_path),
    message = paste("A JDK directory already exists at", jdk_path)
  )
  utils::unzip(zip_path, exdir = dir)

  # Set JAVA_HOME permanently and in the current session
  j_home        <- gsub("/", "\\\\", path.expand(jdk_path))
  set_java_home <- paste0('setx JAVA_HOME ', '"', j_home, '"')
  system(set_java_home)
  Sys.setenv(JAVA_HOME = j_home)

  # Add JAVA_HOME/bin to path
  sys_path <- Sys.getenv("PATH")
  p_end    <- if (!grepl(";$", sys_path)) "" else ";"
  new_path <- paste0(sys_path, p_end, j_home, "\\bin;")
  set_path <- paste0('setx Path ', '"', new_path, '"')
  system(set_path)
  Sys.setenv(Path = new_path)

  # Return path to JDK download
  invisible(j_home)
}

#' Test that the dialr Package Works
#'
#' dialr depends on the libphonenumber Java library and requires a working
#' Java Development Kit installation with environment variables set
#' appropriately. This function checks that dialr works and attempts to provide
#' informative messages about why it may not.
#'
#' @param quiet Should warnings be shown if dialr does not work? These will only
#'   be shown once per session.
#'
#' @return A boolean indicating whether dialr is functioning properly
#'
#' @export
test_dialr <- function(quiet = FALSE) {
  # Test that dialr works; if it does, don't perform specific checks
  sink <- try(
    utils::capture.output({test <- dialr::phone("9012222000", region = "US")}),
    silent = TRUE
  )
  passed <- !rlang::inherits_any(test, "try-error")
  if (passed || quiet) return(passed)

  # Test that dialr is installed
  if (!rlang::is_installed("dialr")) {
    rlang::warn(
      "dialr will not be used to parse phone numbers b/c it is not installed",
      .frequency = "once",
      .frequency_id = "dialr_not_installed"
    )
    return(FALSE)
  }

  # Check JAVA_HOME and PATH environment variables
  detect_java(dialr = TRUE)

  # If failed for another reason, print and return `FALSE`
  if (!passed) {
    msg <- stringr::str_replace(
      as.vector(test),
      "^Error",
      replacement = "Warning (converted from error)"
    )
    rlang::warn(msg, .frequency = "once", .frequency_id = "dialr_failed_other")
  }
  passed
}

#' Detect Java Install Using Environment Variables
#'
#' `detect_java()` attempts to check that the JAVA_HOME variable is set and that
#' the PATH includes a JDK bin. If either of these things is not true, it
#' returns `FALSE`.
#'
#' @param dialr Should warning messages be formatted for
#'   \code{\link[covidsms:test_dialr]{test_dialr()}}?
#'
#' @param quiet Should warning messages be displayed at all?
#'
#' @return A boolean indicating whether Java is available
#'
#' @keywords internal
#'
#' @export
detect_java <- function(dialr = FALSE, quiet = FALSE) {
  # Check JAVA_HOME and PATH environment variables
  j_home <- !rlang::is_empty(Sys.getenv("JAVA_HOME"))
  j_path <- stringr::str_detect(Sys.getenv("PATH"), "jdk[^;]*bin")

  # Just return boolean if `quiet = TRUE`
  if (quiet || (j_home && j_path)) return(j_home && j_path)

  # Issue warnings if `quiet = FALSE`
  if (!j_home) {
    rlang::warn(
      paste(
        if (dialr) "dialr will not be used to parse phone numbers b/c" else "",
        if (dialr) "the `JAVA_HOME` environment variable is not set." else "",
        "The `JAVA_HOME` environment variable must contain the path",
        "to a Java Development Kit installation."
      ),
      .frequency = "once",
      .frequency_id = "JAVA_HOME_not_set"
    )
    FALSE
  } else if (!j_path) {
    rlang::warn(
      paste(
        if (dialr) "dialr will not be used to parse phone numbers b/c" else "",
        if (dialr) "the `PATH` environment variable" else "",
        if (dialr) "does not have a JDK bin." else "",
        "The `PATH` environment variable must contain the path",
        "to a Java Development Kit installation's bin folder."
      ),
      .frequency = "once",
      .frequency_id = "path_missing_jdk_bin"
    )
    FALSE
  } else {
    TRUE
  }
}
