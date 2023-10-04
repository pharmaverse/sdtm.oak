# Set renv profile base on R version.
.get_dependencies <- function(project_dir) {
  dependencies <- renv::dependencies("DESCRIPTION", dev = TRUE)$Package

  c(
    project_dir,
    dependencies,
    "devtools",
    "staged.dependencies",
    "styler"
  ) |>
    unique() |>
    renv:::renv_package_dependencies() |>
    names()
}


options(renv.snapshot.filter = .get_dependencies)

if (Sys.getenv("GITHUB_ACTIONS") != "") {
  options(repos = c(CRAN = "https://packagemanager.posit.co/cran/latest"))
  Sys.setenv("RENV_AUTOLOADER_ENABLED" = FALSE)
}
Sys.setenv("RENV_CONFIG_SANDBOX_ENABLED" = FALSE)
Sys.setenv("RENV_CONFIG_AUTO_SNAPSHOT" = FALSE)

if (!Sys.getenv("RENV_AUTOLOADER_ENABLED") %in% c("false", "FALSE")) {
  .renv_profile <- paste(R.version$major, substr(R.version$minor, 1, 1), sep = ".")
  if (!file.exists("./renv/profile")) {
    if (.renv_profile %in% c("4.1", "4.2", "4.3")) {
      message("Set renv profile to `", .renv_profile, "`")
      Sys.setenv("RENV_PROFILE" = .renv_profile)
    } else {
      message("This repository do not contains the renv profile for your R version.")
    }
  } else {
    message(
      "Using renv profile from `renv/profile` file.\n",
      "The `", readLines("./renv/profile"), "` profile will be used."
    )
  }
}

source("renv/activate.R")
