

api_root = "https://api.kite.trade"
api_login = "https://kite.trade/connect/login"

api_routes <- list()
api_routes[["parameters"]] = "/parameters"
api_routes[["api.token"]] = "/session/token"
api_routes[["api.token.invalidate"]] = "/session/token"
api_routes[["api.token.renew"]] = "/session/refresh_token"
api_routes[["user.profile"]] = "/user/profile"
api_routes[["user.margins"]] = "/user/margins"
api_routes[["user.margins.segment"]] = "/user/margins/{segment}"

api_routes[["orders"]] = "/orders"
api_routes[["trades"]] = "/trades"

api_routes[["orders.info"]] = "/orders/{order_id}"
api_routes[["orders.place"]] = "/orders/{variety}"
api_routes[["orders.modify"]] = "/orders/{variety}/{order_id}"
api_routes[["orders.cancel"]] = "/orders/{variety}/{order_id}"
api_routes[["orders.trades"]] = "/orders/{order_id}/trades"

api_routes[["portfolio.positions"]] = "/portfolio/positions"
api_routes[["portfolio.holdings"]] = "/portfolio/holdings"
api_routes[["portfolio.positions.convert"]] = "/portfolio/positions"

api_routes[["market.instruments.all"]] = "/instruments"
api_routes[["market.instruments"]] = "/instruments/{exchange}"
api_routes[["market.margins"]] = "/margins/{segment}"
api_routes[["market.quote"]] = "/quote"
api_routes[["market.quote.ohlc"]] = "/quote/ohlc"
api_routes[["market.quote.ltp"]] = "/quote/ltp"
api_routes[["market.historical"]] = "/instruments/historical/{instrument_token}/{interval}"
api_routes[["market.trigger_range"]] = "/instruments/{exchange}/{tradingsymbol}/trigger_range"

api_routes[["mf.orders"]] = "/mf/orders"
api_routes[["mf.order.info"]] = "/mf/orders/{order_id}"
api_routes[["mf.order.place"]] = "/mf/orders"
api_routes[["mf.order.cancel"]] = "/mf/orders/{order_id}"
api_routes[["mf.sips"]] = "/mf/sips"
api_routes[["mf.sip.info"]] = "/mf/sips/{sip_id}"
api_routes[["mf.sip.place"]] = "/mf/sips"
api_routes[["mf.sip.modify"]] = "/mf/sips/{sip_id}"
api_routes[["mf.sip.cancel"]] = "/mf/sips/{sip_id}"
api_routes[["mf.holdings"]] = "/mf/holdings"
api_routes[["mf.instruments"]] = "/mf/instruments"


