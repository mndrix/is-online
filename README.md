This small web service listens on port 15000 for HTTP GET requests on the /ping
path.  It's intended to be used with
[httpmon for Android](https://market.android.com/details?id=org.jtb.httpmon&hl=en)
which should ping that URL every 5 minutes.  If the service isn't pinged
on schedule, it runs an external script with the argument "offline".
Once regular pings return, it runs the script with the argument "online".

I originally wrote it to automatically change how Google Voice text messages
are delivered based on whether or not my phone has a network connection.  (In
many rural areas, phone service is available but data is not).
