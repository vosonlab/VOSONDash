#' VOSON Dashboard apiKeysServer
#'
#' Very simple storage and loading of api keys. 
#'

#### values ---------------------------------------------------------------------------------------------------------- #

# named list of app api keys
api_keys <- NULL
check_creds_startup <- TRUE

creds_rv <- reactiveValues(
  file_tokens = list(),
  tokens = list(), 
  created_token = NULL,
  selected_token_id = NULL,
  use_token = NULL,
  msg_log = c()
)

#### events ---------------------------------------------------------------------------------------------------------- #

observeEvent(check_creds_startup, {
  if (isLocal) {
    
  isolate({
    if (file.exists(u_api_keys_path)) {
      api_keys <- readRDS(file = u_api_keys_path)
      
      load_and_use_keys <- api_keys$load_and_use_keys
      
      if (load_and_use_keys) {
        readKeysFile()
        
        updateTextInput(session, "twitter_bearer_name_input", label = NULL, value = api_keys$twitter_bearer_name)
        updateTextInput(session, "twitter_bearer_token_input", label = NULL, value = api_keys$twitter_bearer_token)
        
        updateTextInput(session, "twitter_app_name_input", label = NULL, value = api_keys$twitter_app_name)
        updateTextInput(session, "twitter_api_key_input", label = NULL, value = api_keys$twitter_api_key)
        updateTextInput(session, "twitter_api_secret_input", label = NULL, value = api_keys$twitter_api_secret)
        updateTextInput(session, "twitter_access_token_input", label = NULL, value = api_keys$twitter_access_token)
        updateTextInput(session, "twitter_access_token_secret_input", label = NULL, 
                        value = api_keys$twitter_access_token_secret)
        updateTextInput(session, "youtube_api_key_input", label = NULL, value = api_keys$youtube_api_key)
        
        creds_rv$msg_log <- logMessage(creds_rv$msg_log, "loaded and populated api keys")
      }
    }
    
    if (file.exists(u_api_tokens_path)) {
      creds_rv$file_tokens <- readRDS(file = u_api_tokens_path)
      creds_rv$tokens <- isolate(creds_rv$file_tokens)
      creds_rv$msg_log <- logMessage(creds_rv$msg_log, paste("loaded tokens from file", u_api_tokens_path))
      ids <- getTokenIds()
      updateSelectInput(session, "twitter_token_select", label = NULL, choices = ids, selected = ids[length(ids)])
    } else {
      creds_rv$msg_log <- logMessage(creds_rv$msg_log, paste("no tokens file found", u_api_tokens_path))
    }    
  })
    
  } # end isLocal
}, once = TRUE)

observeEvent(input$create_bearer_auth_token, {
  bearerToken <- input$keys_twitter_bearer_token_input
  
  tryCatch({
    creds_rv$created_token <- VOSONDash::createTwitterBearerToken(input$keys_twitter_bearer_name_input, bearerToken)
  }, error = function(err) {
    creds_rv$msg_log <- logMessage(creds_rv$msg_log, paste("token creation error:", err))
    creds_rv$created_token <- NULL
  }, warning = function(w) {
    creds_rv$msg_log <- logMessage(creds_rv$msg_log, paste("token creation warning:", w))
  })    
})

observeEvent(input$rtweet_auth_check, {
  if (input$rtweet_auth_check) {
    shinyjs::disable("keys_twitter_app_name_input")
    shinyjs::disable("keys_twitter_api_key_input")
    shinyjs::disable("keys_twitter_api_secret_input")
    shinyjs::disable("keys_twitter_access_token_input")
    shinyjs::disable("keys_twitter_access_token_secret_input")
    shinyjs::disable("create_app_token")
  } else {
    shinyjs::enable("keys_twitter_app_name_input")
    shinyjs::enable("keys_twitter_api_key_input")
    shinyjs::enable("keys_twitter_api_secret_input")
    shinyjs::enable("keys_twitter_access_token_input")
    shinyjs::enable("keys_twitter_access_token_secret_input")
    shinyjs::enable("create_app_token")
  }
})

observeEvent(input$create_app_token, {
  # not caught if httpuv aborted as it as ends shiny session
  tryCatch({
    keys <- list(apiKey = input$keys_twitter_api_key_input, 
                 apiSecret = input$keys_twitter_api_secret_input,
                 accessToken = input$keys_twitter_access_token_input, 
                 accessTokenSecret = input$keys_twitter_access_token_secret_input)
    
    creds_rv$created_token <- VOSONDash::createTwitterDevToken(input$keys_twitter_app_name_input, keys)
  }, error = function(err) {
    creds_rv$msg_log <- logMessage(creds_rv$msg_log, paste("token creation error:", err))
    creds_rv$created_token <- NULL
  }, warning = function(w) {
    creds_rv$msg_log <- logMessage(creds_rv$msg_log, paste("token creation warning:", w))
  })    
})

observeEvent(input$create_web_auth_token, {
  # not caught if httpuv aborted as it as ends shiny session
  tryCatch({
    if (input$rtweet_auth_check == TRUE) {
      cred <- list(socialmedia = "twitter", token = NULL, auth = NULL)
      class(cred) <- append(class(cred), c("credential", "twitter"))
      
      cred$auth <- rtweet::rtweet_user()
      cred$type <- "user"
      cred$name <- "rstats2twitter"
      cred$created <- format(Sys.time(), "%Y-%m-%d_%H-%M-%S")
      
      creds_rv$created_token <- cred
    } else {
      keys <- list(apiKey = input$keys_twitter_api_key_input, 
                   apiSecret = input$keys_twitter_api_secret_input)
      
      creds_rv$created_token <- VOSONDash::createTwitterWebToken(input$keys_twitter_app_name_input, keys)
    }
  }, error = function(err) {
    creds_rv$msg_log <- logMessage(creds_rv$msg_log, paste("token creation error:", err))
    creds_rv$created_token <- NULL
  }, warning = function(w) {
    creds_rv$msg_log <- logMessage(creds_rv$msg_log, paste("token creation warning:", w))
  })    
})

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

observeEvent(saveTokensButtonStatus(), {
  if (saveTokensButtonStatus()) {
    shinyjs::enable("tokens_save_button")
  } else {
    shinyjs::disable("tokens_save_button")
  }
})

observeEvent(input$tokens_save_button, {
  if (isLocal) {
    # save tokens
    saveRDS(creds_rv$tokens, u_api_tokens_path)
    creds_rv$file_tokens <- creds_rv$tokens
    creds_rv$msg_log <- logMessage(creds_rv$msg_log, paste0("saved tokens to file (", length(creds_rv$tokens), ")"))
    shinyjs::removeClass("tokens_save_button", "btn-success")
  }
})

observeEvent(creds_rv$tokens, {
  if (!identical(creds_rv$tokens, creds_rv$file_tokens)) {
    shinyjs::addClass("tokens_save_button", "btn-success")
  }
})

observeEvent(input$tokens_load_button, {
  if (file.exists(u_api_tokens_path)) {
    creds_rv$file_tokens <- readRDS(file = u_api_tokens_path)
    creds_rv$tokens <- isolate(creds_rv$file_tokens)
    creds_rv$msg_log <- logMessage(creds_rv$msg_log, paste("loaded tokens from file", u_api_tokens_path))
    ids <- getTokenIds()
    updateSelectInput(session, "twitter_token_select", label = NULL, choices = ids, selected = ids[length(ids)])
  } else {
    creds_rv$msg_log <- logMessage(creds_rv$msg_log, paste("no tokens file found", u_api_tokens_path))
  }  
})

observeEvent(creds_rv$created_token, {
  if (is.null(creds_rv$created_token)) {
    creds_rv$msg_log <- logMessage(isolate(creds_rv$msg_log), "created invalid token")
    shinyjs::disable("save_token")
  } else {
    creds_rv$msg_log <- logMessage(isolate(creds_rv$msg_log), paste("created token",
                                                                    creds_rv$created_token$name))
    shinyjs::enable("save_token")
  }  
}, ignoreInit = TRUE)

observeEvent(input$save_token, {
  token <- creds_rv$created_token
  
  if (!is.null(token)) {
    token_id <- createTokenId(token)
    creds_rv$tokens[[token_id]] <- token
    
    creds_rv$msg_log <- logMessage(creds_rv$msg_log, "saved token to list")
    
    creds_rv$selected_token_id <- token_id
  } else {
    creds_rv$msg_log <- logMessage(creds_rv$msg_log, "unable to save token to list")
  }
})

observeEvent(input$twitter_token_select, {
  if (input$twitter_token_select != "None") {
    shinyjs::enable("use_selected_token")
    shinyjs::enable("delete_selected_token")
  } else {
    shinyjs::disable("use_selected_token")
    shinyjs::disable("delete_selected_token")
  }
})

observeEvent(input$use_selected_token, {
  if (input$twitter_token_select == "None") {
    creds_rv$use_token <- NULL
  } else {
    creds_rv$use_token <- creds_rv$tokens[[input$twitter_token_select]]   
    creds_rv$msg_log <- logMessage(creds_rv$msg_log, paste("using token", input$twitter_token_select))
  }
})

output$use_selected_token_toggle <- reactive({
  if (!is.null(creds_rv$use_token)) {
    token <- createTokenId(creds_rv$use_token)
    output$selected_token_msg <- renderUI({
      tags$div(style = "padding:12px;",
        tags$i(class = "fa-solid fa-circle-check"),
        tags$span(token)
      )
    })
    return(TRUE)
  }
  output$selected_token_msg <- renderText("")
  return(FALSE)
})
outputOptions(output, "use_selected_token_toggle", suspendWhenHidden = FALSE)

observeEvent(input$delete_selected_token, {
  if (input$twitter_token_select %in% names(creds_rv$tokens)) {
    
    creds_rv$tokens <- creds_rv$tokens[!grepl(input$twitter_token_select, names(creds_rv$tokens), fixed = TRUE)]
    
    creds_rv$msg_log <- logMessage(creds_rv$msg_log, paste("deleted", input$twitter_token_select))

    if (!is.null(creds_rv$use_token)) {
      if (input$twitter_token_select == createTokenId(creds_rv$use_token)) {
        creds_rv$use_token <- NULL
      }
    }
    
    creds_rv$selected_token_id <- "None"
  }
})

observeEvent(input$keys_youtube_populate_button, {
  populateYoutubeKeys()
})

observeEvent(input$twitter_token_select, {
  creds_rv$selected_token_id <- input$twitter_token_select 
})

observeEvent(input$web_auth_check, {
  if (input$web_auth_check) {
    shinyjs::enable("create_web_auth_token")
  } else {
    shinyjs::disable("create_web_auth_token")
  }
})

observe({
  updateSelectInput(session, "twitter_token_select", label = NULL, choices = getTokenIds(),
                    selected = creds_rv$selected_token_id)
})

#### output ---------------------------------------------------------------------------------------------------------- #

output$api_keys_log_output <- renderText({
  paste0(creds_rv$msg_log, collapse = '\n')
})

output$save_token_output <- renderText({
  output <- c()
  token <- creds_rv$created_token
  if (is.null(token)) {
    output <- append(output, "Empty or invalid token.")
  } else {
    key <- ifelse(token$type %in% c("dev", "user"), token$auth$app$key,
                  ifelse(token$type %in% c("bearer"), token$auth$token, ""))
    if (nchar(key) >= 32) key <- paste0(substr(key, 1, 4), "...", substr(key, nchar(key) - 4, nchar(key)))
                                        
    output <- c(paste("token:", token$name),
                paste("social media:", token$socialmedia), 
                paste("key:", key),
                paste("type:", token$type),
                paste("created:", token$created))
  }
  paste0(output, collapse = '\n')
})

output$user_keys_path <- renderText({
  u_api_keys_path
})

output$user_tokens_path <- renderText({
  u_api_tokens_path
})

#### reactives ------------------------------------------------------------------------------------------------------- #

getTokenIds <- reactive({
  token_list <- "None"
  if (length(creds_rv$tokens) > 0) {
    for (id in names(creds_rv$tokens)) {
      token_list <- append(token_list, id)
    }            
  }
  return(token_list)
})

saveButtonStatus <- reactive({
  if (!isLocal) { return(FALSE) }
  
  key_values <- c(input$keys_twitter_bearer_name_input,
                  input$keys_twitter_bearer_token_input,
                  input$keys_twitter_app_name_input,
                  input$keys_twitter_api_key_input,
                  input$keys_twitter_api_secret_input,
                  input$keys_twitter_access_token_input,
                  input$keys_twitter_access_token_secret_input,
                  input$keys_youtube_api_key_input)
  
  check_keys <- sapply(key_values, VOSONDash::isNullOrEmpty)
  
  if (any(check_keys != TRUE)) { return(TRUE) }
  
  return(FALSE)
})

saveTokensButtonStatus <- reactive({
  if (!isLocal) { return(FALSE) }
  if (length(creds_rv$tokens) > 0) { return(TRUE) }
  return(FALSE)
})

#### functions ------------------------------------------------------------------------------------------------------- #

# save input field values to api_keys list and then save object as rds
writeKeysFile <- function() {
  if (isLocal) {
    status <- ""
    
    api_keys <<- list(
      load_and_use_keys = input$load_and_use_keys_check,
      twitter_bearer_name = input$keys_twitter_bearer_name_input,
      twitter_bearer_token = input$keys_twitter_bearer_token_input,
      twitter_app_name = input$keys_twitter_app_name_input,
      twitter_api_key = input$keys_twitter_api_key_input,
      twitter_api_secret = input$keys_twitter_api_secret_input,
      twitter_access_token = input$keys_twitter_access_token_input,
      twitter_access_token_secret = input$keys_twitter_access_token_secret_input,
      youtube_api_key = input$keys_youtube_api_key_input
    )
    
    saveRDS(api_keys, u_api_keys_path)
    creds_rv$msg_log <<- logMessage(creds_rv$msg_log, paste("wrote keys to", u_api_keys_path))
  
  } # end isLocal
}

# read api_keys object from rds file and update input fields with values
readKeysFile <- function() {
  status <- ""
  
  if (file.exists(u_api_keys_path)) {
    creds_rv$msg_log <<- logMessage(creds_rv$msg_log, paste("file", u_api_keys_path, "exists"))
    
    api_keys <<- readRDS(file = u_api_keys_path)
    
  } else {
    creds_rv$msg_log <<- logMessage(creds_rv$msg_log, paste("file", u_api_keys_path, "not found"))
    
    return(NULL)
  }
  
  updateCheckboxInput(session, "load_and_use_keys_check", label = NULL, value = api_keys$load_and_use_keys)
  
  updateTextInput(session, "keys_twitter_bearer_name_input", label = NULL, value = api_keys$twitter_bearer_name)
  updateTextInput(session, "keys_twitter_bearer_token_input", label = NULL, value = api_keys$twitter_bearer_token)
  
  updateTextInput(session, "keys_twitter_app_name_input", label = NULL, value = api_keys$twitter_app_name)
  updateTextInput(session, "keys_twitter_api_key_input", label = NULL, value = api_keys$twitter_api_key)
  updateTextInput(session, "keys_twitter_api_secret_input", label = NULL, value = api_keys$twitter_api_secret)
  updateTextInput(session, "keys_twitter_access_token_input", label = NULL, value = api_keys$twitter_access_token)
  updateTextInput(session, "keys_twitter_access_token_secret_input", label = NULL,
                  value = api_keys$twitter_access_token_secret)
  updateTextInput(session, "keys_youtube_api_key_input", label = NULL, value = api_keys$youtube_api_key)
  
  creds_rv$msg_log <<- logMessage(creds_rv$msg_log, paste0("read keys from ", u_api_keys_path, 
                                                           " (", length(api_keys), " values)"))
}

# copy keys input field values to youtube section api key field
populateYoutubeKeys <- function() {
  updateTextInput(session, "youtube_api_key_input", label = NULL, value = input$keys_youtube_api_key_input)
  
  creds_rv$msg_log <<- logMessage(creds_rv$msg_log, "populated youtube api keys")
}
