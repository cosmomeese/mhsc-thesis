cat('sourcing default keys')
sourceDir <- 'fitbit'
source(paste(sourceDir,"/","fitbitAPIKeys.R",
             sep=""))
email <- "XYZ@gmail.com"

scope <- c("sleep", "activity", "heartrate", "location", "nutrition", "profile", "settings", "social", "weight")
  
content_type <- httr::content_type("application/x-www-form-urlencoded")

request <- "https://api.fitbit.com/oauth2/token"
authorize <- "https://www.fitbit.com/oauth2/authorize"
access <- "https://api.fitbit.com/oauth2/token"
endpoint <- httr::oauth_endpoint(request, authorize, access)
header <- httr::add_headers(Authorization=paste0("Basic ", base64enc::base64encode(charToRaw(paste0(KEY, ":", SECRET)))))
myapp <- httr::oauth_app(appname="Medly Research",
					   key=KEY,
					   secret=SECRET,
					   redirect_uri=CALLBACK)
  
  
  
token <- httr::oauth2.0_token(endpoint,
							myapp,
							scope=scope,
							use_basic_auth=TRUE,
							config_init=c(header, content_type),
							cache=FALSE,
							auth_page_query_params=list(email=email,
														prompt="login"))
														
