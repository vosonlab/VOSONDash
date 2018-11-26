#' VOSON Dashboard apiKeysServer
#'
#' Very simple storage and loading of api keys. 
#'

#### values ----------------------------------------------------------------------------------------------------------- #

# named list of app api keys
api_keys <- NULL

# set api keys file to R working directory voson_keys.rds for now
path <- paste0(getwd(), "/voson_keys.rds", sep = "")
key_status <- "" # text description of last action

#### events ----------------------------------------------------------------------------------------------------------- #

observeEvent(input$keys_save_button, {
  writeKeysFile()
})

observeEvent(input$keys_load_button, {
  readKeysFile()
})

observeEvent(input$keys_twitter_populate_button, {
  populateTwitterKeys()
})

observeEvent(input$keys_youtube_populate_button, {
  populateYoutubeKeys()
})

#### output ----------------------------------------------------------------------------------------------------------- #

output$keys_file_output <- renderText({
  setKeyStatus()
})

#### reactives -------------------------------------------------------------------------------------------------------- #

setKeyStatus <- reactive({
  input$keys_save_button
  input$keys_load_button
  input$keys_twitter_populate_button
  input$keys_youtube_populate_button
  
  return(key_status)
})

#### functions -------------------------------------------------------------------------------------------------------- #

# save input field values to api_keys list and then save object as rds
writeKeysFile <- function() {
  status <- ""
  
  api_keys <<- list(
    twitter_api_key = input$keys_twitter_api_key_input,
    twitter_api_secret = input$keys_twitter_api_secret_input,
    twitter_access_token = input$keys_twitter_access_token_input,
    twitter_access_token_secret = input$keys_twitter_access_token_secret_input,
    youtube_api_key = input$keys_youtube_api_key_input
  )
  
  saveRDS(api_keys, path)
  
  status <- paste0("* wrote keys to ", path, ".\n", sep = "")
  key_status <<- status
}

# read api_keys object from rds file and update input fields with values
readKeysFile <- function() {
  status <- ""
  
  if (file.exists(path)) {
    status <- paste0("* file ", path, " exists.\n", sep = "")
    key_status <<- status
    
    api_keys <<- readRDS(file = path)
    
  } else {
    status <- paste0("* file ", path, " not found.\n", sep = "")
    key_status <<- status
    
    return(NULL)
  }
  
  updateTextInput(session, "keys_twitter_api_key_input", label = NULL, value = api_keys$twitter_api_key)
  updateTextInput(session, "keys_twitter_api_secret_input", label = NULL, value = api_keys$twitter_api_secret)
  updateTextInput(session, "keys_twitter_access_token_input", label = NULL, value = api_keys$twitter_access_token)
  updateTextInput(session, "keys_twitter_access_token_secret_input", label = NULL, value = api_keys$twitter_access_token_secret)
  updateTextInput(session, "keys_youtube_api_key_input", label = NULL, value = api_keys$youtube_api_key)
  
  status <- paste0(status, "* read keys from ", path, ". (", length(api_keys), ").\n", sep = "")
  key_status <<- status
}

# copy keys input field values to twitter section api keys fields
populateTwitterKeys <- function() {
  updateTextInput(session, "twitter_api_key_input", label = NULL, value = input$keys_twitter_api_key_input)
  updateTextInput(session, "twitter_api_secret_input", label = NULL, value = input$keys_twitter_api_secret_input)
  updateTextInput(session, "twitter_access_token_input", label = NULL, value = input$keys_twitter_access_token_input)
  updateTextInput(session, "twitter_access_token_secret_input", label = NULL, value = input$keys_twitter_access_token_secret_input)
  
  key_status <<- paste0("* populated twitter keys.\n", sep = "")
}

# copy keys input field values to youtube section api key field
populateYoutubeKeys <- function() {
  updateTextInput(session, "youtube_api_key_input", label = NULL, value = input$keys_youtube_api_key_input)
  
  key_status <<- paste0("* populated youtube keys.\n", sep = "")
}