

rest_api_call <- function(object,method,endpoint,route_params,method_params){
  if(!is_api_connected(object)){
    stop(NotConnectedToAPIException)
  }

  url <- get_api_endpoint(object,endpoint,route_params)

  #old method of authorization, now moved to headers
  #method_params[["api_key"]] <- object@api_key
  #method_params[["access_token"]] <- object@access_token
  authorization=paste0("token ",object@api_key,":",object@access_token)

  r <- NULL
  method <- tolower(method)

  #print(paste("querying",url))
  #print(paste("method:",method))
  #print(route_params)
  #print(method_params)

  tryCatch({
    if(method=="get"){
      r <- httr::GET(url,query = method_params,
                     httr::add_headers("X-Kite-Version"=`X-Kite-Version`,
                                 "Authorization"=authorization))
    } else if(method=="post"){
      r <- httr::POST(url,body = method_params, encode = "form",
                      httr::add_headers("X-Kite-Version"=`X-Kite-Version`,
                                  "Authorization"=authorization))
    } else if(method=="put"){
      r <- httr::PUT(url,body = method_params, encode = "form",
                     httr::add_headers("X-Kite-Version"=`X-Kite-Version`,
                                 "Authorization"=authorization))
    } else if(method=="delete"){
      r <- httr::DELETE(url,body = method_params, encode = "form",
                        httr::add_headers("X-Kite-Version"=`X-Kite-Version`,
                                    "Authorization"=authorization))
    } else{
      stop(UnknwonHttpException)
    }
  }, error=function(e){
    message(e$message)
    stop(HttpException)
  })


  if(r$status_code != 200){
    r <- httr::content(r)
    message(paste0(r$error_type,": ",r$message))
    return(NULL)
  }

  if(!(r$headers$`content-type` %in% VALID_CONTENT_TYPE)){
    stop(DataException)
  }

  r <- suppressMessages(httr::content(r))

  if(any(class(r)=="data.frame") && NROW(r)>0){
    return(r)
  }

  if(any(class(r)=="data.frame") && NROW(r)<1){
    stop(NoDataException)
  }

  if(r$status != STATUS_OK){
    stop(GeneralException)
  }

  if(length(r$data)>0){
    if(is_kite_error(toString(r$data[[1]]))){
      stop(KiteException)
    }
  }

  if(length(r$data)==0){
    stop(NoDataException)
  }

  return(r)
}

#'Function to get the details of logged on user.
#'@description Gets the details of the user.
#'@usage get_profile(object)
#'@param object An object of type kite connect with valid api_key and
#'access_token.
#'@return Returns a list with user details, if successful.
#'@export
get_profile <- function(object){
  route_params = list()
  method_params = list()
  r <- NULL

  tryCatch({r <- rest_api_call(object,"GET","user.profile",
                               route_params,method_params)
  }, error=function(e){
    message(e$message)
  })

  if(!is.null(r))r <- r$data
  return(r)
}

#'Function to get the details of margins for the logged on user.
#'@description Gets the margin details of the user.
#'@param object An object of type kite connect with valid api_key and
#'access_token.
#'@param segment Either `equity` or `commodity`.
#'@details Returns a list with margin details. If no segment is specified.
#'returns for both the segments, else only for the one specified.
#'@return Returns a list with margin details, if successful.
#'@export
get_margins <- function(object,segment=NULL){
  route_params = list(segment=segment)
  method_params = list()
  endpoint <- ifelse(length(route_params)==0,"user.margins",
                     "user.margins.segment")
  r <- NULL

  tryCatch({r <- rest_api_call(object,"GET",endpoint,
                               route_params,method_params)
  }, error=function(e){
    message(e$message)
  })

  if(!is.null(r))r <- r$data
  return(r)
}

#'Function to get the order details.
#'@description Gets the order details of the user for the day.
#'@param object An object of type kite connect with valid api_key and
#'access_token.
#'@return Returns a dataframe with order details, if successful.
#'@export
get_orders <- function(object){
  route_params = list()
  method_params = list()
  r <- NULL

  tryCatch({r <- rest_api_call(object,"GET","orders",
                               route_params,method_params)
  }, error=function(e){
    message(e$message)
  })

  if(!is.null(r))r <- list_to_df(r$data)
  return(r)
}

#'Function to get the trade details.
#'@description Gets the order details of the user for the day.
#'@usage get_trades(object)
#'@param object An object of type kite connect with valid api_key and
#'access_token.
#'@return Returns a dataframe with trade details, if successful.
#'@export
get_trades <- function(object){
  route_params = list()
  method_params = list()
  r <- NULL

  tryCatch({r <- rest_api_call(object,"GET","trades",
                               route_params,method_params)
  }, error=function(e){
    message(e$message)
  })

  if(!is.null(r))r <- list_to_df(r$data)
  return(r)
}

#'Function to get the position details.
#'@description Gets the position details of the user for the day and net.
#'@usage get_positions(object)
#'@param object An object of type kite connect with valid api_key and
#'access_token.
#'@return Returns a list of two dataframe with trade details, if successful,
#' one for net (`net`) positions and another for the day(`day`).
#'@export
get_positions <- function(object){
  route_params = list()
  method_params = list()
  r <- NULL

  tryCatch({r <- rest_api_call(object,"GET","portfolio.positions",
                               route_params,method_params)
  }, error=function(e){
    message(e$message)
  })

  if(!is.null(r)){
    net <- list_to_df(r$data$net)
    day <- list_to_df(r$data$day)
    r <- list(net=net,day=day)
  }

  return(r)
}

#'Function to get the holdings details.
#'@description Gets the holdings details of the user for the day.
#'@usage get_holdings(object)
#'@param object An object of type kite connect with valid api_key and
#'access_token.
#'@return Returns a list with holdings details, if successful.
#'@export
get_holdings <- function(object){
  route_params = list()
  method_params = list()
  r <- NULL

  tryCatch({r <- rest_api_call(object,"GET","portfolio.holdings",
                               route_params,method_params)
  }, error=function(e){
    message(e$message)
  })

  if(!is.null(r))r <- r$data
  return(r)
}

#'Function to get the order history.
#'@description Gets the order history for a given order.
#'@usage order_history(object, order_id)
#'@param object An object of type kite connect with valid api_key and
#'access_token.
#'@param order_id ID of the order to fetch the history for.
#'@return Returns a dataframe with order history, if successful.
#'@seealso \code{\link[kiteconnect3]{order_trades}}
#'@export
order_history <- function(object,order_id){
  route_params = list(order_id=order_id)
  method_params = list()
  r <- NULL

  tryCatch({r <- rest_api_call(object,"GET","orders.info",
                               route_params,method_params)
  }, error=function(e){
    message(e$message)
  })

  if(!is.null(r))r <- list_to_df(r$data)
  return(r)
}

#'Function to get the trade history.
#'@description Gets the trade history for a given order.
#'@usage order_trades(object, order_id)
#'@param object An object of type kite connect with valid api_key and
#'access_token.
#'@param order_id ID of the order to fetch the history for.
#'@return Returns a dataframe with trade history, if successful.
#'@seealso \code{\link[kiteconnect3]{order_history}}
#'@export
order_trades <- function(object,order_id){
  route_params = list(order_id=order_id)
  method_params = list()
  r <- NULL

  tryCatch({r <- rest_api_call(object,"GET","orders.trades",
                               route_params,method_params)
  }, error=function(e){
    message(e$message)
  })

  if(!is.null(r))r <- list_to_df(r$data)
  return(r)
}

#'Function to place a trade.
#'@description Function to place a trade with specified details.
#'@param object An object of type kite connect with valid api_key and
#'access_token.
#'@param exchange Exchange for the order, e.g. NSE, NFO, CDS etc.
#'@param tradingsymbol Trading Symbol of the instrument.
#'@param transaction_type BUY or SELL.
#'@param quantity Quantity of the order.
#'@param order_type Order type, e.g. MARKET, LIMIT, SL or SL-M.
#'@param variety Order variety, e.g. regular, bo, co or amo.
#'@param product Product type, e.g. MIS, NRML, CNC, BO or CO.
#'@param price Price if required (e.g. for limit orders).
#'@param validity Validity period of the order (DAY/IOC), defaults to DAY.
#'@param disclosed_quantity Quantity to disclosed publicly.
#'@param trigger_price Trigger price if required (e.g. for stop loss orders).
#'@param squareoff Price difference for profit booking (bracker orders).
#'@param stoploss Price difference for loss booking (bracket orders).
#'@param trailing_stoploss Trailing stop loss (for bracket orders).
#'@param tag Optional string to tag an order (alphanumeric, max 8 chars).
#'@details This function sends an order with input details for execution.
#'Please note not all parameters are relevant for all types of orders. This
#'function (as with rest of the package) does no error checks and post the
#'trade request as is. It is left to the function calling this method to
#'carry out necessary error checks. If the order is successfully posted, an
#'order ID will be returned. A successfully posted order does not mean it is
#'a valid order, and orders with erroneous user input can be immedeately
#'cancelled. Therefore it is good practice to check the order details once
#'an order ID is received from this call.
#'@return Returns an order ID (string), if successful.
#'@seealso \code{\link[kiteconnect3]{order_history}}
#'@seealso \code{\link[kiteconnect3]{modify_order}}
#'@seealso \code{\link[kiteconnect3]{cancel_order}}
#'@export
place_order <- function(object,exchange,tradingsymbol,transaction_type,quantity,
                        order_type=ORDER_TYPE_MARKET,variety=VARIETY_REGULAR,
                        product=PRODUCT_NRML,price=NULL,validity=NULL,
                        disclosed_quantity=NULL,trigger_price=NULL,
                        squareoff=NULL,stoploss=NULL,trailing_stoploss=NULL,
                        tag=NULL){
  params <- as.list(environment(), all=TRUE)
  params[["object"]] <- NULL
  keys <- names(params)

  for(key in keys){
    if(is.null(params[[key]])){
      params[[key]] <- NULL
    }
  }

  route_params = list(variety=variety)
  method_params = params
  r <- NULL

  tryCatch({r <- rest_api_call(object,"POST","orders.place",
                               route_params,method_params)
  }, error=function(e){
    message(e$message)
  })
  if(!is.null(r)) r <- r$data[["order_id"]]
  return(r)
}

#'Function to modify an exiting order.
#'@description Function to modify an exiting order with specified details.
#'@param object An object of type kite connect with valid api_key and
#'access_token.
#'@param variety Order variety, e.g. regular, bo, co or amo.
#'@param order_id Order ID for the order to modify.
#'@param parent_order_id Required for bracket orders (cannot be modified).
#'@param quantity New quantity.
#'@param price New Price (e.g. for limit orders).
#'@param order_type Order type, e.g. MARKET, LIMIT, SL or SL-M.
#'@param trigger_price Trigger price if required (e.g. for stop loss orders).
#'@param validity Validity period of the order (DAY/IOC).
#'@param disclosed_quantity Quantity to disclosed publicly.
#'@details This function sends a modification request for an existing order,
#'assuming it is not already executed. Please note: not all parameters are
#'relevant for all types of orders. No error checks is carried out, It is
#'left to the function calling this method to carry out necessary error
#'checks.If successful, this will return the order ID (should be same as the
#'one passed). A successfully placed request does not mean it is
#'a valid order, and orders with erroneous user input can be immedeately
#'cancelled. Therefore it is good practice to check the order details once
#'an order ID is received from this call.
#'@return Returns an order ID (string), if successful.
#'@seealso \code{\link[kiteconnect3]{order_history}}
#'@seealso \code{\link[kiteconnect3]{place_order}}
#'@seealso \code{\link[kiteconnect3]{cancel_order}}
#'@export
modify_order <- function(object,variety,order_id,parent_order_id=NULL,quantity=NULL,
                         price=NULL,order_type=NULL,trigger_price=NULL,
                         validity=NULL,disclosed_quantity=NULL){
  params <- as.list(environment(), all=TRUE)
  params[["object"]] <- NULL
  keys <- names(params)

  for(key in keys){
    if(is.null(params[[key]])){
      params[[key]] <- NULL
    }
  }

  route_params = list(variety=variety,order_id=order_id)
  method_params = params
  r <- NULL

  tryCatch({r <- rest_api_call(object,"PUT","orders.modify",
                               route_params,method_params)
  }, error=function(e){
    message(e$message)
  })
  if(!is.null(r)) r <- r$data[["order_id"]]
  return(r)
}

#'Function to cancel an exiting order.
#'@description Function to cancel an exiting order with specified ID.
#'@param object An object of type kite connect with valid api_key and
#'access_token.
#'@param variety Order variety, e.g. regular, bo, co or amo.
#'@param order_id Order ID for the order to modify.
#'@param parent_order_id Required for bracket orders (cannot be modified).
#'@details This places a request to cancel an existing order, if it is not
#'already executed. Please note: no error checks are done for the input
#'parameters. Sanity check is left to the user of this function. If
#'successful, this will return the order ID (should be same as the
#'one passed). A successfully placed request does not mean it is
#'a valid order, and orders with erroneous user input can be immedeately
#'cancelled. Therefore it is good practice to check the order details once
#'an order ID is received from this call.
#'@return Returns an order ID (string), if successful.
#'@seealso \code{\link[kiteconnect3]{order_history}}
#'@seealso \code{\link[kiteconnect3]{place_order}}
#'@seealso \code{\link[kiteconnect3]{modify_order}}
#'@export
cancel_order <- function(object,variety, order_id, parent_order_id=NULL){
  params <- as.list(environment(), all=TRUE)
  params[["object"]] <- NULL
  keys <- names(params)

  for(key in keys){
    if(is.null(params[[key]])){
      params[[key]] <- NULL
    }
  }

  route_params = list(variety=variety,order_id=order_id)
  method_params = params
  r <- NULL

  tryCatch({r <- rest_api_call(object,"DELETE","orders.cancel",
                               route_params,method_params)
  }, error=function(e){
    message(e$message)
  })
  if(!is.null(r)) r <- r$data[["order_id"]]
  return(r)
}

convert_position <- function(object,exchange,tradingsymbol,transaction_type,
                             position_type,quantity,old_product,new_product){
  params <- as.list(environment(), all=TRUE)
  params[["object"]] <- NULL
  keys <- names(params)

  for(key in keys){
    if(is.null(params[[key]])){
      params[[key]] <- NULL
    }
  }

  route_params = list()
  method_params = params
  r <- NULL

  tryCatch({r <- rest_api_call(object,"PUT","portfolio.positions.convert",
                               route_params,method_params)
  }, error=function(e){
    message(e$message)
  })

  if(!is.null(r)) r <- r$data
  return(r)
}

#'Function to obtain list of all instruments.
#'@description Function to obtain a list of tradable instruments.
#'@param object An object of type kite connect with valid api_key and
#'access_token.
#'@param exchange Exchange for the instruments list. If not specified, returns
#'list of instruments across all exchanges.
#'@details This function returns list of instruments trading on a given
#'exchange or across all exchanges. Ideally one should not be required to
#'call this function more than once a day (and save a local copy).
#'@return Returns a dataframe of the instrument lists.
#'@export
get_instruments <- function(object, exchange=NULL){
  params <- list()
  end_point <- "market.instruments.all"
  if(!is.null(exchange)){
    params <- list(exchange=exchange)
    end_point <- "market.instruments"
  }

  route_params = params
  method_params = list()
  r <- NULL

  tryCatch({r <- rest_api_call(object,"GET",end_point,
                               route_params,method_params)
  }, error=function(e){
    message(e$message)
  })

  return(r)
}

#'Function to obtain current quote for a list of instruments.
#'@description Function to obtain quotes and market depths.
#'@param object An object of type kite connect with valid api_key and
#'access_token.
#'@param instruments A list of instruments. See details below.
#'@details This function accepts a list of instruments (as exchange:symbol
#'pair, e.g. NFO:NIFTY18JANFUT) and returns current market quotes, including
#' last traded price, OHLC prices and market depth (up to 5 best bids and
#' offers). Please note: this function is meant to be called as a one-off
#' way to get the quotes. One should not call this function too many times.
#' The api possibly has a rate limit and too many requests will block the
#' user. For continous data, websocket is more appropriate choice. Check the
#' Kite Connect online documentation for more details.
#' @return a list of quote and market depth data, with row names as
#' instruments.
#' @seealso \code{\link[kiteconnect3]{get_ohlc}}
#' @seealso \code{\link[kiteconnect3]{get_ltp}}
#' @seealso \code{\link[kiteconnect3]{get_historical_price}}
#' @export
get_quote <- function(object,instruments){

  instruments <- lapply(instruments, function(x){x})
  names(instruments) <- rep("i",length(instruments))

  route_params = list()
  method_params = instruments
  r <- NULL

  tryCatch({r <- rest_api_call(object,"GET","market.quote",
                               route_params,method_params)
  }, error=function(e){
    message(e$message)
  })

  if(!is.null(r)){
    r <- r$data
  }
  return(r)
}

#'Function to obtain current OHLC prices for a list of instruments.
#'@description Function to obtain open/high/low/close prices.
#'@param object An object of type kite connect with valid api_key and
#'access_token.
#'@param instruments A list of instruments. See details below.
#'@details This function accepts a list of instruments (as exchange:symbol
#'pair, e.g. NFO:NIFTY18JANFUT) and returns current OHLC prices. Please note:
#' this function is meant to be called as a one-off way to get the quotes.
#' One should not call this function too many times. The api possibly has a
#' rate limit and too many requests will block the user. For continous data,
#' websocket is more appropriate choice. Check the Kite Connect online
#' documentation for more details.
#' @return a dataframe with OHLC data, with instruments as row names.
#' @seealso \code{\link[kiteconnect3]{get_quote}}
#' @seealso \code{\link[kiteconnect3]{get_ltp}}
#' @seealso \code{\link[kiteconnect3]{get_historical_price}}
#' @export
get_ohlc <- function(object,instruments){
  instruments <- lapply(instruments, function(x){x})
  names(instruments) <- rep("i",length(instruments))

  route_params = list()
  method_params = instruments
  r <- NULL

  tryCatch({r <- rest_api_call(object,"GET","market.quote.ohlc",
                               route_params,method_params)
  }, error=function(e){
    message(e$message)
  })

  if(!is.null(r))r <- list_to_df(r$data)
  return(r)
}

#'Function to obtain last traded prices for a list of instruments.
#'@description Function to obtain last traded prices.
#'@param object An object of type kite connect with valid api_key and
#'access_token.
#'@param instruments A list of instruments. See details below.
#'@details This function accepts a list of instruments (as exchange:symbol
#'pair, e.g. NFO:NIFTY18JANFUT) and returns last traded prices. Please note:
#' this function is meant to be called as a one-off way to get the quotes.
#' One should not call this function too many times. The api possibly has a
#' rate limit and too many requests will block the user. For continous data,
#' websocket is more appropriate choice. Check the Kite Connect online
#' documentation for more details.
#' @return a dataframe with last price data, with instruments as row names.
#' @seealso \code{\link[kiteconnect3]{get_quote}}
#' @seealso \code{\link[kiteconnect3]{get_ohlc}}
#' @seealso \code{\link[kiteconnect3]{get_historical_price}}
#' @export
get_ltp <- function(object, instruments){
  instruments <- lapply(instruments, function(x){x})
  names(instruments) <- rep("i",length(instruments))

  route_params = list()
  method_params = instruments
  r <- NULL

  tryCatch({r <- rest_api_call(object,"GET","market.quote.ltp",
                               route_params,method_params)
  }, error=function(e){
    message(e$message)
  })

  if(!is.null(r))r <- list_to_df(r$data)
  return(r)
}

#'Function to obtain current historical prices for a instrument.
#'@description Function to obtain historical prices with given interval.
#'@param object An object of type kite connect with valid api_key and
#'access_token.
#'@param instrument_token An instrument_token. See details below.
#'@param from Starting time for history.
#'@param to Ending time for history.
#'@param interval Interval definition.
#'@param continuous True/ False for fetching continuous data (e.g. futures).
#'@details This function returns OHLC price history of a given instrument.
#'The instrument token is the one obtained with a call to get_instruments.
#'The valid intervals are `minute`,`day`,`3minute`,`5minute`,`10minute`,
#'`15minute`,`30minute` and `60minute`. The parameters `from` and `to` should
#'be in YYYY-MM-DD HH:MM:SS format (no timezone adjustment is applied).
#'Please note: this function is meant to be called as a one-off way to get
#'the quotes.One should not call this function too many times. The api
#'possibly has a rate limit and too many requests will block the user. For
#'continous data, websocket is more appropriate choice. Check the Kite Connect online
#' documentation for more details.
#' @return an xts object with OHLC data.
#' @seealso \code{\link[kiteconnect3]{get_instruments}}
#' @seealso \code{\link[kiteconnect3]{get_quote}}
#' @seealso \code{\link[kiteconnect3]{get_ohlc}}
#' @seealso \code{\link[kiteconnect3]{get_ltp}}
#' @export
get_historical_price <- function(object, instrument_token, from, to,
                                 interval, continuous=0){

  route_params = list(instrument_token=instrument_token,interval=interval)
  method_params = list(from=from,to=to,continuous=continuous)
  r <- NULL

  tryCatch({r <- rest_api_call(object,"GET","market.historical",
                               route_params,method_params)
  }, error=function(e){
    message(e$message)
  })

  x <- NULL
  tryCatch({
    x <- convert_to_xts(r$data$candles)
  }, error=function(e){
    message(e$message)
  })

  return(x)
}
get_trigger_range <- function(object,exchange,tradingsymbol,transaction_type){
  route_params = list(exchange=exchange,tradingsymbol=tradingsymbol)
  method_params = list(transaction_type=transaction_type)
  r <- NULL

  tryCatch({r <- rest_api_call(object,"GET","market.trigger_range",
                               route_params,method_params)
  }, error=function(e){
    message(e$message)
  })
  return(r)
}
