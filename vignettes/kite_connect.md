[Kite Connect](https://kite.trade/docs/connect/v3/) is a set of
REST-like HTTP APIs from [Zerodha](https://zerodha.com/) that expose
many capabilities required to build a complete stock market investment
and trading platform. It lets you execute orders in real time (equities,
commodities, mutual funds), manage user portfolios, stream live market
data over WebSockets, and more. This R package wraps a select set of
functionaliies of the version 3 (latest) of this API. This package
allows one to do the following:

-   Place, modify and cancel orders
-   Query specific order details, as well query holdings, positions,
    available margins and other related portfolio management operations.
-   Obtain market quotes as well as historical data

The parts of the API that this package does not implement:

-   Websocket streaming (may be in future!).
-   Mutual fund related functionalities (may be in future!).

Requirements
------------

The use of the API is based on Kite Connect app based authentication,
using `api_key` and `api_secret`. This requires subscription to the API
and registering an app. After an app is registered, the `api_key` and
the `api_secret` will be available on the [app
page](https://developers.kite.trade/apps). In addition, to use the
historical data option, one needs an additional subscription to the
historical data API.

Workflow
--------

The workflow starts with the creation of a kite connect object and
completing a successful login flow. To create a kite connect object the
following may be used. The login url then may be obtained using the
`get_login_url` function.

    require(kiteconnect3)
    kite = create_connection_object(list(api_key="xxxx",api_secret="yyyy"))
    login_url = get_login_url(kite)
    print(login_url)

Navigating to this login url and completing a successful login will
generate a `request_token`. A successful login comes back with the
`request_token` as a URL query parameter to the redirect URL registered
on the [developer console](https://developers.kite.trade/apps/) for that
`api_key`. It is not necessary to have a service running at the
redirection URL to capture the `request_token`. In fact, individual
developers can simply specify the local host IP (127.0.0.1) (or anything
for that matter) as the redirect URL. After manually navigating and
entering the login credentials, the browser will be redirected with the
`request_token` in the URL as a query parameter, which can simply be
copied from the browser address bar. This `request_token` may be used
for subsequent handshake with the API.

API Handshake
-------------

Once the `request_token` is available, the `set_request_token` function
can be used to update the kite connect object and then call the
`fetch_access_token` to complete the API handshake and obtain an
`access_token`. A successful handshake will set the `access_token` slot
inside the kite connect object and return the object itself. From this
point onward the handshake is complete and the API functions are
available to the user.

    kite = set_request_token(kite,request_token)
    kite = fetch_access_token(kite)

Example of Use
--------------

The following displays the call signatures for some useful functions.
For details on each function, see help (e.g. `?place_order`).

    # portfolio functions
    orders <- get_orders(kite)
    trades <- get_trades(kite)
    positions <- get_positions(kite)
    holdings <- get_holdings(kite)
    order_hist <- order_history(kite,orders$order_id[1])
    specific_trades <- order_trades(kite,orders$order_id[1])
    margins <- get_margins(kite,"equity")
    # trade functions
    order_id <- place_order(kite,"NFO","NIFTY18MARFUT","SELL",75)
    order_id <- modify_order(kite,"regular",order_id,quantity = 150)
    order_id <- cancel_order(kite,"regular",order_id)
    # market data functions
    instruments <- get_instruments(kite,"NFO")
    q <- get_quote(kite,c("NFO:NIFTY18FEBFUT"))
    ohlc <- get_ohlc(kite,c("NFO:NIFTY18FEBFUT","NFO:NIFTY18MARFUT"))
    ltp <- get_ltp(kite,c("NFO:NIFTY18FEBFUT","NFO:NIFTY18MARFUT"))

Important Points to Note
------------------------

Please note, this package is intended to provide low level
functionalities of the API. Most of the functions do minimum error
checks and input validations. As a result, output can be different from
what you expected. If you are using this package, be aware of this and
use necessary error handling and validation and test this works as per
your requirement. Also use some functions with cautions - for example
the market data functions (e.g. `get_quote`). The API possibly has a API
call rate restrictions and polling for market data too frequently will
probably ban your IP and/ or `api_key`. It is best to use the websocket
streaming functionality (not implemented here, may be in future!) if you
need continuous market update. Also, querying about the status of an
order is most efficiently done by using a
[postback](https://kite.trade/docs/connect/v3/postbacks/) or using the
websocket route. Also check the licesing.

Welcome to systematic trading in R with Kite Connect. In case you are
interested to learn more about systematic and quantitative trading from
the domain experts, you can check out
[QuantInsti](https://www.quantinsti.com/) or their online platform
[Quantra](https://quantra.quantinsti.com/).

> "Do or do not, there is no try"
