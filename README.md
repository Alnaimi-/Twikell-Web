# Overview

This is a Haskell API running on the Scotty web framework. The API runs on localhost:3000 and provides for basic HTTP requests to retrieve & add users/tweets. This data is retrieved and parsed from the public Twitter API.

Technologies used:

* `Scotty` for routing
* `Configurator` for configuration
* `Aeson` for JSON encoding/decoding
* MySQL server
* `mysql` and `mysql-simple` for communication with DB
* `resource-pool` for DB connections pool
* `wai-extra` for basic authentication
* `Warp` as webserver

## Authentication
User: hamza
Password: abc123

To Insert tweets from a certain Twitter user try:

curl -u hamza:abc123 -X POST -d 'name=katyperry' http://localhost:3000/admin/addtweets 