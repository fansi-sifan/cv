# This file contains all the code needed to parse and print various sections of your CV
# from data. Feel free to tweak it as you desire!


#' Create a CV_Printer object.
#'
#' @param data_location Path of the spreadsheets holding all your data. This can be
#'   either a URL to a google sheet with multiple sheets containing the four
#'   data types or a path to a folder containing four `.csv`s with the neccesary
#'   data.
#' @param pdf_location What location will the PDF of this CV be hosted at?
#' @param html_location What location will the HTML version of this CV be hosted at?
#' @param source_location Where is the code to build your CV hosted?
#' @param pdf_mode Is the output being rendered into a pdf? Aka do links need
#'   to be stripped?
#' @param language zh or en
#' @param sheet_is_publicly_readable If you're using google sheets for data,
#'   is the sheet publicly available? (Makes authorization easier.)
#' @return A new `CV_Printer` object.
create_CV_object <-  function(data_location,
                              language,
                              sheet_is_publicly_readable = TRUE) {

  cv <- list(
    language = language,
    links = c()
  )

  is_google_sheets_location <- stringr::str_detect(data_location, "docs\\.google\\.com")

  if(is_google_sheets_location){
    if(sheet_is_publicly_readable){
      # This tells google sheets to not try and authenticate. Note that this will only
      # work if your sheet has sharing set to "anyone with link can view"
      googlesheets4::gs4_deauth()
    } else {
      # My info is in a public sheet so there's no need to do authentication but if you want
      # to use a private sheet, then this is the way you need to do it.
      # designate project-specific cache so we can render Rmd without problems
      options(gargle_oauth_cache = ".secrets")
    }

    cv$entries_data <- googlesheets4::read_sheet(data_location, sheet = "entries", skip = 1) %>%
      # Google sheets loves to turn columns into list ones if there are different types
      dplyr::mutate_if(is.list, purrr::map_chr, as.character) 

    cv$skills        <- googlesheets4::read_sheet(data_location, sheet = "language_skills", skip = 1)
    cv$text_blocks   <- googlesheets4::read_sheet(data_location, sheet = "text_blocks", skip = 1)
    cv$contact_info  <- googlesheets4::read_sheet(data_location, sheet = "contact_info", skip = 1)
  } else {
    # Want to go old-school with csvs?
    cv$entries_data <- readr::read_csv(paste0(data_location, "entries.csv"))
    cv$skills       <- readr::read_csv(paste0(data_location, "language_skills.csv"))
    cv$text_blocks  <- readr::read_csv(paste0(data_location, "text_blocks.csv"))
    cv$contact_info <- readr::read_csv(paste0(data_location, "contact_info.csv"), skip = 1)
  }


  # This year is assigned to the end date of "current" events to make sure they get sorted later.
  future_year <- lubridate::year(lubridate::ymd(Sys.Date())) + 10

  # Clean up entries dataframe to format we need it for printing
  cv$entries_data %<>%
    dplyr::filter(in_resume) %>% 
    dplyr::filter(language == cv$language) %>% 
    # tidyr::pivot_wider(names_from = "col",names_prefix = "description_", values_from = "description") %>% 
    # tidyr::unite(
    #   tidyr::starts_with('description'),
    #   col = "description_bullets",
    #   sep = "\n- ",
    #   na.rm = TRUE
    # ) %>%
    # dplyr::mutate(
    #   description_bullets = paste0("- ", description_bullets),
    #   end = ifelse(is.na(end), "Current", end),
    #   end_num = ifelse(tolower(end) %in% c("current", "now", ""), future_year, end),
    #   timeline = ifelse(is.na(start) | start == end,
    #                     end,
    #                     glue::glue('{end} - {start}'))
    # ) %>%
    # dplyr::arrange(desc(end_num)) %>%
    dplyr::mutate_all(~ ifelse(is.na(.), 'N/A', .))

  cv
}


# Remove links from a text block and add to internal list
sanitize_links <- function(cv, text){
  if(cv$pdf_mode){
    link_titles <- stringr::str_extract_all(text, '(?<=\\[).+?(?=\\])')[[1]]
    link_destinations <- stringr::str_extract_all(text, '(?<=\\().+?(?=\\))')[[1]]

    n_links <- length(cv$links)
    n_new_links <- length(link_titles)

    if(n_new_links > 0){
      # add links to links array
      cv$links <- c(cv$links, link_destinations)

      # Build map of link destination to superscript
      link_superscript_mappings <- purrr::set_names(
        paste0("<sup>", (1:n_new_links) + n_links, "</sup>"),
        paste0("(", link_destinations, ")")
      )

      # Replace the link destination and remove square brackets for title
      text <- text %>%
        stringr::str_replace_all(stringr::fixed(link_superscript_mappings)) %>%
        stringr::str_replace_all('\\[(.+?)\\]', "\\1")
    }
  }

  list(cv = cv, text = text)
}

