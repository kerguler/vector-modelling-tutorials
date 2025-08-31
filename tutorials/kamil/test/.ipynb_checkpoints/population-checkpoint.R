# population.R
# R wrapper for a PopJSON-compiled shared library

PopulationModel <- setRefClass("PopulationModel",
  fields = list(
    lib = "character",
    dll = "ANY",
    numpop = "integer",
    numpar = "integer",
    numint = "integer",
    numenv = "integer",
    stoch = "integer",
    param = "numeric",
    parmin = "numeric",
    parmax = "numeric",
    popnames = "character",
    parnames = "character",
    intnames = "character",
    envnames = "character",
    popids = "list",
    parids = "list",
    intids = "list",
    envids = "list"
  ),

  methods = list(

    initialize = function(libpath) {
      lib <<- libpath
      dll <<- dyn.load(lib)

      res <- .C("init",
                no = as.integer(0),
                np = as.integer(0),
                ni = as.integer(0),
                ne = as.integer(0),
                st = as.integer(0))

      numpop <<- res$no
      numpar <<- res$np
      numint <<- res$ni
      numenv <<- res$ne
      stoch  <<- res$st

      total <- numpop + numpar + numint + numenv
      namebuf <- rep("", total)
      param_buf <- numeric(numpar)
      parmin_buf <- numeric(numpar)
      parmax_buf <- numeric(numpar)

      res_names <- .C("parnames",
                      names = as.character(namebuf),
                      param = as.double(param_buf),
                      parmin = as.double(parmin_buf),
                      parmax = as.double(parmax_buf))

      allnames <- res_names$names
      popnames <<- allnames[1:numpop]
      parnames <<- allnames[(numpop + 1):(numpop + numpar)]
      intnames <<- allnames[(numpop + numpar + 1):(numpop + numpar + numint)]
      envnames <<- allnames[(numpop + numpar + numint + 1):total]

      param <<- res_names$param
      parmin <<- res_names$parmin
      parmax <<- res_names$parmax

      popids <<- setNames(as.list(seq_along(popnames)), popnames)
      parids <<- setNames(as.list(seq_along(parnames)), parnames)
      intids <<- setNames(as.list(seq_along(intnames)), intnames)
      envids <<- setNames(as.list(seq_along(envnames)), envnames)

      message("Loaded model with: ",
              numpop, " pops, ",
              numpar, " pars, ",
              numint, " ints, ",
              numenv, " envs.")
    },

    sim = function(ftime, 
                   envir = list(), 
                   pr = numeric(0), 
                   prs = list(),
                   y0 = list(), 
                   rep = 1, 
                   file0 = " ", 
                   file1 = " ", 
                   boil = FALSE) {
      rep <- as.integer(rep)
      rdim <- if (rep >= 0) rep else -rep
      ftime <- as.integer(ftime)
      file0 <- as.character(file0)
      file1 <- as.character(file1)

      if (length(prs) == 0) prs <- list(pr)
      nsets <- length(prs)

      y0vec <- sapply(popnames, function(nm) if (nm %in% names(y0)) y0[[nm]] else 0.0)

      if (numenv > 0) {
        envirvec <- unlist(lapply(envnames, function(nm) {
          v <- if (nm %in% names(envir)) envir[[nm]] else numeric(ftime)
          c(length(v), v)
        }))
      } else {
        envirvec <- numeric(0)
      }

      success <- 0L
      ret_list <- list()
      iret_list <- list()

      for (pidx in seq_len(nsets)) {
        prvec <- if (length(prs[[pidx]]) > 0) as.numeric(prs[[pidx]]) else param

        tryCatch({
          res <- .C("sim",
                    tf = ftime,
                    rep = rep,
                    envir = as.double(envirvec),
                    pr = as.double(prvec),
                    y0 = as.double(y0vec),
                    file_from = file0,
                    file_to = file1,
                    ret = double(rdim * ftime * numpop),
                    iret = double(rdim * (ftime - 1) * numint),
                    success = as.integer(1))

          success <- success + 1L
          ret_list[[success]] <- res$ret
          if (numint > 0) iret_list[[success]] <- res$iret

        }, error = function(e) {
          message(sprintf("Simulation %d failed: %s", pidx, e$message))
        })
      }

      if (success == 0L) stop("No successful simulations.")

      if (success == 1) {
        ret_mat <- array(ret_list[[1]], dim = c(numpop, ftime, rdim))
        ret_mat <- aperm(ret_mat, c(3, 2, 1))  # Now [rdim, ftime, numpop]
      } else {
        tmp <- lapply(ret_list, function(x) {
          a <- array(x, dim = c(numpop, ftime, rdim))
          aperm(a, c(3, 2, 1))  # [rdim, ftime, numpop]
        })
        ret_mat <- array(unlist(tmp), dim = c(success, rdim, ftime, numpop))
      }

      iret_mat <- if (numint > 0) {
        if (success == 1) {
          iret_mat <- array(iret_list[[1]], dim = c(numint, ftime - 1, rdim))
          iret_mat <- aperm(iret_mat, c(3, 2, 1))  # [rdim, tdim-1, numint]
        } else {
          tmp <- lapply(iret_list, function(x) {
            a <- array(x, dim = c(numint, ftime - 1, rdim))
            aperm(a, c(3, 2, 1))  # [rdim, tdim-1, numint]
          })
          iret_mat <- array(unlist(tmp), dim = c(success, rdim, ftime - 1, numint))
        }
      } else NULL

      ret_out <- if (boil) apply(ret_mat, if (success == 1) c(2, 3) else c(2, 3, 4), mean) else ret_mat
      iret_out <- if (!is.null(iret_mat) && boil) apply(iret_mat, if (success == 1) c(2, 3) else c(2, 3, 4), mean) else iret_mat

      list(
        success = success,
        ret = ret_out,
        iret = iret_out
      )
    },

    define_function = function(name, numpar) {
      #
      # ERROR: This is a template, not functional at the moment.
      #
      fname <- paste0("define_", name)
      tryCatch({
        function(...) {
          args <- list(...)
          stopifnot(length(args) == numpar)
          inputs <- lapply(args, as.double)
          all_args <- c(inputs, list(result = as.double(0)))
          result <- do.call(".C", c(list(fname), all_args))
          result$result
        }
      }, error = function(e) {
        warning(sprintf("Function '%s' not found.", fname))
        return(NULL)
      })
    },

    destroy = function() {
      dyn.unload(lib)
    }
  )
)
