tf <- tempfile(fileext = ".md")
# Default branch name can change between repos so we check first
ctv_file_address <- gh::gh(
  "/repos/{owner}/{repo}/contents/{path}",
  owner = "cran-task-views",
  repo = "Epidemiology",
  path = "Epidemiology"
)
download.file(
  ctv_file_address$download_url,
  tf
)

ctv_pkgs <- ctv::read.ctv(tf) |> 
  purrr::pluck("packagelist", "name") 

cran_epidemiology_packages <- ctv_pkgs |>
  purrr::map(pkgsearch::cran_package_history)

cran_epidemiology_packages <- cran_epidemiology_packages |> 
  dplyr::select(
    Package, 
    Version,
    `Authors@R`,
    Author,
    Maintainer
  )

rownames(cran_epidemiology_packages) <- NULL

usethis::use_data(cran_epidemiology_packages, overwrite = TRUE, version = 3)
