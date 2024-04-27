# Load required libraries
library(httr)
library(jsonlite)

tiktok_account_info <- function(dataframe) {

  dataframe <- summary_accounts

  #verifico che sia presente l'API relativa a TIKTOK
  if (Sys.getenv("TIKTOK_API_KEY") != "") {

    get_tiktok_account_info <- function(account_name) {
      # TikTok API endpoint for getting account info
      api_url <- paste0("https://api.tiktok.com/node/share/user/@", account_name)

      # Make GET request to TikTok API
      response <- GET(api_url)

      # Check if request was successful
      if (http_status(response)$category == "Success") {
        # Parse JSON response
        account_info <- content(response, "text") %>%
          fromJSON(flatten = TRUE)

        return(account_info)
      } else {
        cat("Error: Unable to fetch account information.")
        return(NULL)
      }
    }

    account_name <- "username"
    account_info <- get_tiktok_account_info(account_name)
    print(account_info)

    for (i in 1:nrow(dataframe)) {
      i=1
      account_name <- dataframe[i, "account_id"]
      account_info <- get_tiktok_account_info(account_name)

      # Append information to dataframe
      if (!is.null(account_info)) {
        dataframe[i, paste0(names(account_info), "_TikTok")] <- unlist(account_info)
      }
    }
  }

  return(dataframe)
}
