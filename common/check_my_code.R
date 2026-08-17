## ================= CODE DIFF CHECKER (in-memory version) =================
## Compares a reference code block (saved as a string/variable) against
## your manually-typed version, line by line — pinpoints exactly where a
## typed line diverges, including subtle typos that wouldn't error but
## would silently change behavior (== vs =, a misspelled argument, etc.)

lcs_align <- function(a, b) {
  n <- length(a); m <- length(b)
  dp <- matrix(0L, n + 1, m + 1)
  for (i in seq_len(n)) {
    for (j in seq_len(m)) {
      if (a[i] == b[j]) {
        dp[i + 1, j + 1] <- dp[i, j] + 1L
      } else {
        dp[i + 1, j + 1] <- max(dp[i, j + 1], dp[i + 1, j])
      }
    }
  }
  
  i <- n; j <- m
  ops <- list()
  while (i > 0 && j > 0) {
    if (a[i] == b[j]) {
      ops[[length(ops) + 1]] <- list(type = "match", a = i, b = j)
      i <- i - 1; j <- j - 1
    } else if (dp[i, j + 1] >= dp[i + 1, j]) {
      ops[[length(ops) + 1]] <- list(type = "remove", a = i, b = NA)
      i <- i - 1
    } else {
      ops[[length(ops) + 1]] <- list(type = "add", a = NA, b = j)
      j <- j - 1
    }
  }
  while (i > 0) { ops[[length(ops) + 1]] <- list(type = "remove", a = i, b = NA); i <- i - 1 }
  while (j > 0) { ops[[length(ops) + 1]] <- list(type = "add", a = NA, b = j); j <- j - 1 }
  
  rev(ops)
}

highlight_diff <- function(ref_line, my_line) {
  ref_chars <- strsplit(ref_line, "")[[1]]
  my_chars  <- strsplit(my_line, "")[[1]]
  
  max_common <- min(length(ref_chars), length(my_chars))
  prefix_len <- 0
  while (prefix_len < max_common && identical(ref_chars[prefix_len + 1], my_chars[prefix_len + 1])) {
    prefix_len <- prefix_len + 1
  }
  
  suffix_len <- 0
  while (suffix_len < (max_common - prefix_len) &&
         identical(ref_chars[length(ref_chars) - suffix_len], my_chars[length(my_chars) - suffix_len])) {
    suffix_len <- suffix_len + 1
  }
  
  ref_mid_idx <- seq_len(max(0, length(ref_chars) - suffix_len - prefix_len)) + prefix_len
  my_mid_idx  <- seq_len(max(0, length(my_chars) - suffix_len - prefix_len)) + prefix_len
  
  list(
    ref_mid = paste(ref_chars[ref_mid_idx], collapse = ""),
    my_mid  = paste(my_chars[my_mid_idx], collapse = "")
  )
}

## Normalizes input to a character vector of lines, whether you pass a
## single multi-line string (e.g. from r"(...)" ) or an already-split vector.
as_lines <- function(x) {
  if (length(x) == 1 && grepl("\n", x, fixed = TRUE)) {
    strsplit(x, "\n")[[1]]
  } else {
    x
  }
}

## ---- Main entry point — pass two strings/vectors, no files needed ----
compare_code <- function(reference, mine, ignore_whitespace = TRUE) {
  ref  <- as_lines(reference)
  mine <- as_lines(mine)
  
  ref_cmp  <- if (ignore_whitespace) trimws(ref)  else ref
  mine_cmp <- if (ignore_whitespace) trimws(mine) else mine
  
  ops <- lcs_align(ref_cmp, mine_cmp)
  
  cat("=====================================================\n")
  cat("Comparing your code against the reference\n")
  cat("=====================================================\n\n")
  
  n_match <- sum(vapply(ops, function(x) x$type == "match", logical(1)))
  n_diff  <- length(ops) - n_match
  
  i <- 1
  while (i <= length(ops)) {
    op <- ops[[i]]
    
    if (op$type == "match") {
      i <- i + 1
      next
    }
    
    if (op$type == "remove" && i < length(ops) && ops[[i + 1]]$type == "add") {
      ref_line <- ref[op$a]
      my_line  <- mine[ops[[i + 1]]$b]
      d <- highlight_diff(trimws(ref_line), trimws(my_line))
      
      cat("Line ", op$a, " (yours: line ", ops[[i + 1]]$b, ") DIFFERS:\n", sep = "")
      cat("  reference: ", ref_line, "\n", sep = "")
      cat("  yours:     ", my_line,  "\n", sep = "")
      cat("  mismatch:  \"", d$ref_mid, "\"  vs  \"", d$my_mid, "\"\n\n", sep = "")
      
      i <- i + 2
    } else if (op$type == "remove") {
      cat("Line ", op$a, " is in the reference but MISSING from your code:\n", sep = "")
      cat("  reference: ", ref[op$a], "\n\n", sep = "")
      i <- i + 1
    } else {
      cat("Extra line in your code, not in reference (your line ", op$b, "):\n", sep = "")
      cat("  yours: ", mine[op$b], "\n\n", sep = "")
      i <- i + 1
    }
  }
  
  cat("=====================================================\n")
  cat(n_match, "lines match exactly.", n_diff, "lines differ.\n")
  cat(if (n_diff == 0) "Your code matches the reference exactly.\n" 
      else "Review the differences above before running your script.\n")
  cat("=====================================================\n")
  
  invisible(list(ops = ops, n_match = n_match, n_diff = n_diff))
}


run_my_code <- function(code, keep_file = FALSE) {
  code_lines <- as_lines(code)
  
  temp_path <- tempfile(fileext = ".R")
  writeLines(code_lines, temp_path)
  
  message("Running as: ", temp_path)
  
  result <- tryCatch({
    source(temp_path, echo = TRUE)
  }, error = function(e) {
    message("Error while running your code: ", conditionMessage(e))
    NULL
  })
  
  if (!keep_file) file.remove(temp_path)
  
  invisible(result)
}


