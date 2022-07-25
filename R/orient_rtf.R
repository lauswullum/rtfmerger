

#' rtf_text
#' 
#' This orients a RTF table, in the form of a vector 
#' where each element is a line from an RTF table, from 
#' landscape to portrait or the portrait to landscape
#'
#' @param rtf_text A vector of character lines from an RTF.
#'
#' @return A character vector containing lines from RTF
#' @export
#'
#' @examples
#' rtflines = readLines("rtf_file.rtf")
#' turned_rtf = orient_rtf(rtflines, landscape)
#' writeLines(turned_rtf, "turned_rtf.rtf")
#'
orient_rtf = function(rtf_text, landscape = TRUE) {
  if ((is.character(rtf_text) != TRUE) | all(rtf_text == "")) {
    print("Input must be character vector and non-empty")
  }
   
  # Find index for start of orientation tag
  # This is the "\paperw" tag
  start_ind = which(grepl("paperw", rtf_text, fixed = TRUE))
  
  if (landscape) {
    for (ind in start_indices) {
      rtf_text[c(ind, ind+2)] = 
        c("\\paperw15840\\paperh12240",
          "\\margl1440\\margr1440\\margt1440\\margb1440\\headery720\\footery720")
    }
  } else {
    for (ind in start_indices) {
      rtf_text[c(ind, ind+2)] = 
        c("\\paperw12240\\paperh15840",
          "\\margl1800\\margr1440\\margt2520\\margb1800\\headery2520\\footery1449")
    }
  }
  rtf_text
}

