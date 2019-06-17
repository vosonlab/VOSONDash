#' VOSON Dashboard apiKeysServer
#'
#' Very simple storage and loading of api keys. 
#'

#### values ----------------------------------------------------------------------------------------------------------- #

# named list of app api keys
api_keys <- NULL
key_status <- "" # text description of last action

check_keys_startup <- TRUE

#### events ----------------------------------------------------------------------------------------------------------- #

observeEvent(check_keys_startup, {
  isolate({
    if (file.exists(g_api_keys_path)) {
      api_keys <- readRDS(file = g_api_keys_path)
      
      load_and_use_keys <- api_keys$load_and_use_keys
      
      if (load_and_use_keys) {
        readKeysFile()
        
        updateTextInput(session, "twitter_app_name_input", label = NULL, value = api_keys$twitter_app_name)
        updateTextInput(session, "twitter_api_key_input", label = NULL, value = api_keys$twitter_api_key)
        updateTextInput(session, "twitter_api_secret_input", label = NULL, value = api_keys$twitter_api_secret)
        updateTextInput(session, "twitter_access_token_input", label = NULL, value = api_keys$twitter_access_token)
        updateTextInput(session, "twitter_access_token_secret_input", label = NULL, value = api_keys$twitter_access_token_secret)
        updateTextInput(session, "youtube_api_key_input", label = NULL, value = api_keys$youtube_api_key)
        
        key_status <<- paste0("* loaded and populated api keys.\n", sep = "")
      }
    }
  })
}, once = TRUE)

observeEvent(saveButtonStatus(), {
  if (saveButtonStatus()) {
    shinyjs::enable("keys_save_button")
  } else {
    shinyjs::disable("keys_save_button")
  }
})

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

saveButtonStatus <- reactive({
  key_values <- c(input$keys_twitter_app_name_input,
                  input$keys_twitter_api_key_input,
                  input$keys_twitter_api_secret_input,
                  input$keys_twitter_access_token_input,
                  input$keys_twitter_access_token_secret_input,
                  input$keys_youtube_api_key_input)
  
  check_keys <- sapply(key_values, isNullOrEmpty)
  
  if (any(check_keys != TRUE)) { 
    return(TRUE)
  }
  return(FALSE)
})

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
    load_and_use_keys = input$load_and_use_keys_check,
    twitter_app_name = input$keys_twitter_app_name_input,
    twitter_api_key = input$keys_twitter_api_key_input,
    twitter_api_secret = input$keys_twitter_api_secret_input,
    twitter_access_token = input$keys_twitter_access_token_input,
    twitter_access_token_secret = input$keys_twitter_access_token_secret_input,
    youtube_api_key = input$keys_youtube_api_key_input
  )
  
  saveRDS(api_keys, g_api_keys_path)
  
  status <- paste0("* wrote keys to ", g_api_keys_path, ".\n", sep = "")
  key_status <<- status
}

# read api_keys object from rds file and update input fields with values
readKeysFile <- function() {
  status <- ""
  
  if (file.exists(g_api_keys_path)) {
    status <- paste0("* file ", g_api_keys_path, " exists.\n", sep = "")
    key_status <<- status
    
    api_keys <<- readRDS(file = g_api_keys_path)
    
  } else {
    status <- paste0("* file ", g_api_keys_path, " not found.\n", sep = "")
    key_status <<- status
    
    return(NULL)
  }
  
  updateCheckboxInput(session, "load_and_use_keys_check", label = NULL, value = api_keys$load_and_use_keys)
  
  updateTextInput(session, "keys_twitter_app_name_input", label = NULL, value = api_keys$twitter_app_name)
  updateTextInput(session, "keys_twitter_api_key_input", label = NULL, value = api_keys$twitter_api_key)
  updateTextInput(session, "keys_twitter_api_secret_input", label = NULL, value = api_keys$twitter_api_secret)
  updateTextInput(session, "keys_twitter_access_token_input", label = NULL, value = api_keys$twitter_access_token)
  updateTextInput(session, "keys_twitter_access_token_secret_input", label = NULL, value = api_keys$twitter_access_token_secret)
  updateTextInput(session, "keys_youtube_api_key_input", label = NULL, value = api_keys$youtube_api_key)
  
  status <- paste0(status, "* read keys from ", g_api_keys_path, ". (", length(api_keys), ").\n", sep = "")
  key_status <<- status
}

# copy keys input field values to twitter section api keys fields
populateTwitterKeys <- function() {
  updateTextInput(session, "twitter_app_name_input", label = NULL, value = input$keys_twitter_app_name_input)
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
