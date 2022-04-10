getGithubDF <- function() {
  library(jsonlite)
  library(httpuv)
  library(httr)
  
  oauth_endpoints("github")
  myapp <- oauth_app(appname = "Test_OAuth",
                     key = "e62878364d658f4dda27",
                     secret = "825be6551bdd407fe5ded3cc2bfce017af6234e7")
  # Get OAuth credentials
  github_token <- oauth2.0_token(oauth_endpoints("github"), myapp)
  # Use API
  gtoken <- config(token = github_token)
  req <- GET("https://api.github.com/users/jtleek/repos", gtoken)
  
  stop_for_status(req)
  
  json1 <- content(req)
  
  gitDF <- jsonlite::fromJSON(jsonlite::toJSON(json1))
  
  gitDF[gitDF$full_name == "jtleek/datasharing", "created_at"]
}