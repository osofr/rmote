
#' Serve rmarkdown html report as a webpage
#'
#' @param dir directory that contains html report(s) that should be served
#' @param file.name name of the html file (report) that should be served
#' @param \ldots  additional parameters
#' @export
serve_rmd_html <- function(dir, file.name, ...) {
  if(is_rmote_on() && file.exists(file.path(dir, file.name))) {
    message("serving html page through rmote")
    res <- try({
      if (!file.exists(file.path(dir, file.name))) stop("...")
      readLines(file.path(dir, file.name))
    }, silent = TRUE)

    if(!inherits(res, "try-error")) {
      server_dir <- get_server_dir()
      # move R.css over
      # file.path(R.home('doc'), 'html', 'R.css')
      if(!file.exists(file.path(server_dir, "R.css")))
        file.copy(file.path(system.file(package = "rmote"), "R.css"), server_dir)

      ii <- get_output_index()
      writeLines(c("<!-- DISABLE-SERVR-WEBSOCKET -->", res),
        file.path(server_dir, get_output_file(ii)))
      write_index(ii)

      if(is_history_on()) {
        message("making thumbnail")
        fbase <- file.path(server_dir, "thumbs")
        if(!file.exists(fbase))
          dir.create(fbase)
        nf <- file.path(fbase, gsub("html$", "png", get_output_file(ii)))
        opts <- list(filename = nf, width = 300, height = 150)
        if(capabilities("cairo"))
          opts$type <- "cairo-png"
        do.call(png, opts)
        getFromNamespace("print.trellis", "lattice")(text_plot(paste("help:", topic)))
        dev.off()
      }
      return()
    }
  } else {
    stop("the rmote server has not been started or the file doesn't exist")
  }
}
