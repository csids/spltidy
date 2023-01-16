#' Saves (serializes) an encrypted object to disk
#' @param x The object to serialize
#' @param filepath The filepath (must end in '.cs.encrypted' or '.cs')
#' @param nthreads Number of threads to use. Default 1.
#' @param public_key_path Path to public key
#' @export
save_cs <- function(x, filepath, nthreads = 1, public_key_path = Sys.getenv("ENCRYPTR_ID_RSA_PUB")){
  UseMethod("save_cs", x)
}

#' @method save_cs default
#' @export
save_cs.default  <- function(x, filepath, nthreads = 1, public_key_path = Sys.getenv("ENCRYPTR_ID_RSA_PUB")){
  if(!(stringr::str_detect(filepath, ".cs.encrypted$") | stringr::str_detect(filepath, ".cs$"))){
    stop("filepath must end with '.cs.encrypted' or '.cs'")
  }

  if(is.null(public_key_path)){
    public_key_path <- system.file("cssave/id_rsa.pub", package="cstidy")
  } else if(public_key_path==""){
    public_key_path <- system.file("cssave/id_rsa.pub", package="cstidy")
  }

  if(stringr::str_detect(filepath, ".cs$")){
    qs::qsave(
      x = x,
      file = filepath,
      nthreads = nthreads
    )
    message("Saved to *UN*encrypted ", filepath)

  } else {
    tmp <- tempfile()
    on.exit(unlink(tmp))
    qs::qsave(
      x = x,
      file = tmp,
      nthreads = nthreads
    )

    openssl::encrypt_envelope(tmp, public_key_path) %>%
      saveRDS(file = filepath, compress = FALSE)

    message("Saved to ", filepath, ", ENCRYPTED with public key ", public_key_path)
  }
}

#' @method save_cs csfmt_rts_data_v1
#' @export
save_cs.csfmt_rts_data_v1  <- function(x, filepath, nthreads = 1, public_key_path = Sys.getenv("ENCRYPTR_ID_RSA_PUB")){
  x <- as.data.frame(x)
  attr(x, "cs_class") <- "csfmt_rts_data_v1"

  # call save_cs.default
  NextMethod()
}

#' Reads an object in an encrypted file serialized to disk
#' @param filepath The filepath (must end in '.cs.encrypted' or '.cs')
#' @param private_key_path Path to private key
#' @export
read_cs <- function(filepath, private_key_path = Sys.getenv("ENCRYPTR_ID_RSA")){
  if(!(stringr::str_detect(filepath, ".cs.encrypted$") | stringr::str_detect(filepath, ".cs$"))){
    stop("filepath must end with '.cs.encrypted' or '.cs'")
  }

  if(stringr::str_detect(filepath, ".cs$")){
    retval <- qs::qread(filepath)
  } else if (stringr::str_detect(filepath, ".cs.encrypted$")){
    tmp <- tempfile()
    on.exit(unlink(tmp))

    .crypt = readRDS(filepath)
    zz = file(tmp, "wb")
    openssl::decrypt_envelope(
      .crypt$data,
      .crypt$iv,
      .crypt$session,
      key = private_key_path,
      password = NULL
    ) %>%
      writeBin(zz)
    close(zz)

    retval <- qs::qread(tmp)
    retval
  }

  if(!is.null(attr(retval, "cs_class"))){
    if(attr(retval, "cs_class") == "csfmt_rts_data_v1"){
      setDT(retval)
      set_csfmt_rts_data_v1(retval)
    }
  }
  return(retval)
}
