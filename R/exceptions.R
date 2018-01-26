
condition <- function(subclass, message, call = sys.call(-1), code = 500,
                      ...) {
  structure(
    class = c(subclass, "condition"),
    list(message = message, call = call, code=code),
    ...
  )
}

# HTTP/ API exceptions
GeneralException <- condition(c("GeneralException", "error"),
                              "Unknown exception",code=500)
HttpException <- condition(c("HttpException", "error"),
                           "Http exception",code=500)
UnknwonHttpException <- condition(c("HttpException", "error"),
                                  "Unknown http method",code=500)
TokenException <- condition(c("TokenException", "error"),
                            "Expired or invalid token",code=403)
PermissionException <- condition(c("PermissionException", "error"),
                                 "Permission denied",code=403)
NotFoundException <- condition(c("PermissionException", "error"),
                               "Page not found",code=404)
OrderException <- condition(c("OrderException", "error"),
                            "Something went wrong with the order",
                            code=500)
InputException <- condition(c("InputException", "error"),
                            "Invalid user input",code=400)
DataException <- condition(c("DataException", "error"),
                           "Bad data received from server",code=502)
GatewayException <- condition(c("NetworkException", "error"),
                              "Bad gateway error",code=502)
NetworkException <- condition(c("NetworkException", "error"),
                              "Network error",code=503)
NetworkTimeOutException <- condition(c("NetworkException", "error"),
                                     "Network time-out error",code=504)
TooManyRequestsException <- condition(c("TooManyRequestsException", "error"),
                                      "Too many requests",code=429)
KiteException <- condition(c("KiteException", "error"),
                           "Red pills, check inputs",code=500)

# Authentication Exception
InvalidRequestTokenException = condition(c("InvalidRequestTokenException", "error"),
                                         "Invalid request token",code=999)
InvalidAccessTokenException = condition(c("InvalidAccessTokenException", "error"),
                                        "Invalid access token",code=999)
NotConnectedToAPIException = condition(c("NotConnectedToAPIException", "error"),
                                       "Not connected to api",code=999)

# Program Exception
InvalidConnectionObjectException = condition(c("InvalidConnectionObjectException", "error"),
                                             "Not a valid connection object",code=999)
InvalidConnectionObjectorTokenException = condition(c("InvalidConnectionObjectException", "error"),
                                                    "Not a valid connection object or request token",code=999)
InvalidInputException = condition(c("InvalidInputException", "error"),
                                  "Not a valid input",code=999)
InvalidParamsInputException = condition(c("InvalidInputException","error"),
                                        "required params missing:api_key, api_secret, root, login and routes",
                                        code=999)
NoDataException <- condition(c("NoDataException", "error"),
                             "No data available",code=999)
ConnectionResetException <- condition(c("ConnectionResetException", "error"),
                                      "Failed to reset connection",code=999)


kite_error_msg = "Take the red pill with Kite"
is_kite_error <- function(x){
  return(length(grep(kite_error_msg,x))>0)
}
