

setClass("kiteconnect",
         representation(
           api_key = "character",
           api_secret = "character",
           request_token = "character",
           access_token = "character",
           debug = "logical",
           micro_cache = "logical",
           session_hook = "function",
           timeout = "numeric",
           proxies = "character",
           root = "character",
           login = "character",
           routes = "list",
           details = "list"
         )
)

is_connection_object <- function(object){
  return(class(object) == "kiteconnect")
}
is_valid_api_key <- function(object){
  if(!is_connection_object(object))return(FALSE)
  return(length(object@api_key) > 0)
}
is_valid_api_secret <- function(object){
  if(!is_connection_object(object))return(FALSE)
  return(length(object@api_secret) > 0)
}
is_valid_request_token <- function(object){
  if(!is_connection_object(object))return(FALSE)
  return(length(object@request_token) > 0)
}
is_valid_access_token <- function(object){
  if(!is_connection_object(object))return(FALSE)
  return(length(object@access_token) > 0)
}
is_valid_session_hook <- function(object){
  if(!is_connection_object(object))return(FALSE)
  return(deparse(object@session_hook)[2]=="NULL")
}
is_valid_root <- function(object){
  if(!is_connection_object(object))return(FALSE)
  return(length(object@root) > 0)
}
is_valid_login <- function(object){
  if(!is_connection_object(object))return(FALSE)
  return(length(object@login) > 0)
}
is_valid_routes <- function(object){
  if(!is_connection_object(object))return(FALSE)
  return(length(object@routes) > 0)
}
is_valid_details <- function(object){
  if(!is_connection_object(object))return(FALSE)
  return(length(object@details) > 0)
}
is_valid_timeout <- function(object){
  if(!is_connection_object(object))return(FALSE)
  return(length(object@timeout) > 0)
}
is_valid_connection <- function(object){
  if(!is_connection_object(object))return(FALSE)
  return(
    is_valid_api_key(object) &
      is_valid_api_secret(object) &
      is_valid_root(object) &
      is_valid_routes(object)
  )
}
is_logged_connection <- function(object){
  if(!is_connection_object(object))return(FALSE)
  return(
    is_valid_connection(object) &
      is_valid_request_token(object)
  )
}
is_api_connected <- function(object){
  if(!is_connection_object(object))return(FALSE)
  return(
    is_logged_connection(object) &
      is_valid_access_token(object)
  )
}

#'Function to set the request token in the kite connect object.
#'@description Function to set request token (obtained from a successful
#'login flow) in the kite connect object.
#'@param object an S4 object of type kite connect.
#'@param request_token a string containing the request token from login flow.
#'@details  This function allows to set the request token of the kite
#'connect object. The request token is obtained from a successful login flow.
#'After the login flow, the user is redirected to a pre-specified URL. The
#'request token is posted alongwith as a query parameter.
#'@return Returns the object with request token field set.
#'@seealso \code{\link[kiteconnect3]{create_connection_object}}
#'@export
set_request_token <- function(object,request_token){
  if(!is_connection_object(object)){
    message("Invalid kite connect object")
  }
  if(any(class(request_token)=="character")){
    object@request_token <- request_token
  }else{
    message("Invalid request token")
  }
  return(object)
}

#'Function to set the access token in the kite connect object.
#'@description This function sets the access token (obtained by sending a
#'hash of api key, request token and api secret) inside the kite connect
#'object.
#'@param object a kite connect object.
#'@param access_token Access token.
#'@return Kite connect object with access token set.
#'@seealso \code{\link[kiteconnect3]{create_connection_object}}
#'@seealso \code{\link[kiteconnect3]{fetch_access_token}}
#'@export
set_access_token <- function(object,access_token){
  if(!is_connection_object(object)){
    message("Invalid kite connect object")
  }
  if(any(class(access_token)=="character")){
    object@access_token <- access_token
  }else{
    message("Invalid access token")
  }
  return(object)
}

set_session_hook <- function(object,f){
  if(!is_connection_object(object)){
    stop(InvalidConnectionObjectException)
  }
  if(any(class(f)=="function")){
    object@session_hook <- f
  }else{
    stop(InvalidInputException)
  }
  return(object)
}
set_proxies <- function(object, proxies){
  if(!is_connection_object(object)){
    stop(InvalidConnectionObjectException)
  }
  if(any(class(proxies)=="character")){
    object@proxies <- proxies
  }else{
    stop(InvalidInputException)
  }
  return(object)
}

set_timeout <- function(object, timeout){
  if(!is_connection_object(object)){
    stop(InvalidConnectionObjectException)
  }
  if(any(class(timeout)=="numeric")){
    object@timeout <- round(timeout)
  }else{
    stop(InvalidInputException)
  }
  return(object)
}
set_debug <- function(object,debug){
  if(!is_connection_object(object)){
    stop(InvalidConnectionObjectException)
  }
  if(any(class(debug)=="logical")){
    object@debug <- debug
  }else{
    stop(InvalidInputException)
  }
  return(object)
}
set_micro_cache <- function(object, micro_cache){
  if(!is_connection_object(object)){
    stop(InvalidConnectionObjectException)
  }
  if(any(class(micro_cache)=="logical")){
    object@micro_cache <- micro_cache
  }else{
    stop(InvalidInputException)
  }
  return(object)
}
set_details <- function(object, data){
  if(!is_connection_object(object)){
    stop(InvalidConnectionObjectException)
  }
  if(class(data)=="list"){
    object@details <- data
  }else{
    stop(InvalidInputException)
  }
  return(object)
}

#'Function to get the login URL.
#'@description Function to get the login URL (the login entry + api_key) from
#'the kite object.
#'@param object An object of kite connect type.
#'@return Returns the login URL.
#'@export
get_login_url <- function(object){
  if(!is_valid_connection(object)){
    message("Invalid kite connect object")
    return(NULL)
  }
  return(paste0(object@login,"?v=3&api_key=",object@api_key))
}

get_api_endpoint <- function(object,endpoint,route_params=list()){
  if(!is_valid_connection(object)){
    message("Invalid kite connect object")
    return(NULL)
  }
  url <- paste0(object@root,glue::glue(object@routes[[endpoint]],
                                 .envir=create_route_env(route_params)))
  return(url)
}

#'Create a kite connect connection object.
#'@description This function returns an object of kite connect type, given the
#'input parametrs.
#'@param params a list mapping object property name to values. See details
#'for more.
#'@details This function creates an S4 object from the input parameters. The
#'required parameters are api_key, api_secret. If root (the api root), routes
#'(the list of api endpoints) and login (the login url) is supplied, they are
#'used. Otherwise the internally defined values are used. Only a single
#'instance of this class per `api_key` should be initialized.
#'Parameters should be a named list of all inputs to create a connection.
#'The important attributes are: api_key( character) - your api key;
#'api_secret(character) - your api secret (both of these are available on
#'the developer page in your kite app, and must be supplied during object
#'creation); request_token(character) - should be set after a successful
#'login flow; access_token(character) - obtained using fetch_access_token
#'function (which also set the attribute `details`). Some attributes like
#'root (api root URL), login (kite api specific login URL) and routes (list
#'of api end points) are optional and recommended to use the default values.
#'Other attributes which are not implemented are: debug(logical); micro_cache
#'(logical); session_hook(function); timeout(numeric) and proxies(character).
#'@return S4 object for kite connect which can be used in further api calls.
#'@export
create_connection_object <- function(params){
  object = methods::new("kiteconnect")

  tryCatch({
    object@api_key = params[["api_key"]]
    object@api_secret = params[["api_secret"]]
    object@root = ifelse(is.null(params[["root"]]),api_root,
                         params[["root"]])
    if(is.null(params[["routes"]])){
      object@routes = api_routes
    } else{
      object@routes = params[["routes"]]
    }
    object@login = ifelse(is.null(params[["login"]]),api_login,
                          params[["login"]])
  }, error=function(e){
    message(e$message)
    return(NULL)
  })

  if(!is_valid_connection(object)){
    message("Failed to create kite connect object")
    return(NULL)
  }

  object@debug <- ifelse(is.null(params[["debug"]]),FALSE,params[["debug"]])
  object@micro_cache <- ifelse(is.null(params[["micro_cache"]]),FALSE,params[["micro_cache"]])
  object@timeout <- ifelse(is.null(params[["timeout"]]),7,params[["timeout"]])
  if(!is.null(params[["proxies"]])){
    object@proxies <- params[["proxies"]]
  }
  return(object)
}

#'Fetch access token given the api key, secret and a request token.
#'@description This function returns an access token given the inputs, that
#'can be used for subsequent api calls.
#'@param object An object of type kite connect, must have request token set.
#'@usage fetch_access_token(object)
#'@details This function generate the `access_token` by exchanging
#'`request_token`.The `request_token` obtained after the login flow. the
#'`access_token` required for all subsequent requests. The object passed in
#'this call must already have `request_token` set (along with api_key and
#'api_secret). A successful call also set the user data within the object.
#'@return A string containing the access token.
#'@seealso \code{\link[kiteconnect3]{set_request_token}}
#'@export
fetch_access_token <- function(object){
  access_token <- NULL
  if(!is_logged_connection(object)){
    message("Invalid kite connect object or request token not set")
    return(NULL)
  }
  mash <- paste0(iconv(object@api_key,"UTF-8"),iconv(object@request_token,"UTF-8"),
                 iconv(object@api_secret,"UTF-8"))
  checksum <- digest::digest(mash,algo = "sha256", serialize = FALSE)
  post_body <- list(
    "api_key"= object@api_key,
    "request_token"= object@request_token,
    "checksum" = checksum
  )
  tryCatch({
    r <- httr::POST(get_api_endpoint(object,"api.token"),body=post_body,
                    httr::add_headers("X-Kite-Version"=`X-Kite-Version`),
                    encode = "form")
    response <- httr::content(r)
    access_token <- response$data$access_token
  }, error=function(e){
    message(e$message)
    return(NULL)
  })

  if(r$status_code != 200){
    r <- httr::content(r)
    message(paste0(r$error_type,": ",r$message))
    return(NULL)
  }

  if(class(access_token)!="character"){
    message("Invalid access token received")
    return(NULL)
  }
  object = set_access_token(object,access_token)
  object = set_details(object,response$data)
  return(object)
}

refresh_access_token <- function(object){
  access_token <- NULL
  if(!is_logged_connection(object)){
    stop(InvalidConnectionObjectorTokenException)
  }
  mash <- paste0(iconv(object@api_key,"UTF-8"),iconv(object@request_token,"UTF-8"),
                 iconv(object@api_secret,"UTF-8"))
  checksum <- digest::digest(mash,algo = "sha256", serialize = FALSE)
  post_body <- list(
    "api_key"= object@api_key,
    "refresh_token"= object@request_token,
    "checksum" = checksum
  )
  tryCatch({
    r <- httr::POST(get_api_endpoint(object,"api.token.renew"),body=post_body,
                    httr::add_headers("X-Kite-Version"=`X-Kite-Version`),
                    encode = "form")
    response <- httr::content(r)
    access_token <- response$data$access_token
  }, error=function(e){
    message(e$message)
    stop(InvalidAccessTokenException)
  })
  if(class(access_token)!="character"){
    stop(InvalidAccessTokenException)
  }
  object = set_access_token(object,access_token)
  object = set_details(object,response$data)
  return(object)
}

#'Function to end an api session.
#'@description This function invalidates and rest the access token.
#'@param object of type kite connect, must have a valid access token set.
#'@usage kill_connection(object)
#'@details This function sends a request to kite api to invalidate the
#'session and then reset the request token and access token within the object.
#'@return An object of type kite connect, with tokens reset.
#'@export
kill_connection <- function(object){
  if(!is_logged_connection(object)){
    message("Invalid kite connect object")
    return(object)
  }

  del_body <- list(
    "api_key"= object@api_key,
    "access_token"= object@access_token
  )

  tryCatch({
    r <- rest_api_call(object,"DELETE","api.token.invalidate",list(),del_body)
  }, error=function(e){
    message(e$message)
  })

  object@access_token <- character(0)
  object@request_token <- character(0)
}

