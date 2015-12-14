# Overview

This is a Haskell API running on the Scotty web framework. The API runs on localhost:3000 and provides for basic HTTP requests to retrieve & add users/tweets. This data is retrieved and parsed from the public Twitter API.

Technologies used:

* `Scotty` for routing
* `Configurator` for configuration
* `Aeson` for JSON encoding/decoding
*  MySQL server
* `mysql` and `mysql-simple` for communication with DB
* `resource-pool` for DB connections pool
* `wai-extra` for basic authentication
* `Warp` as webserver

## Authentication
User: hamza
Password: abc123

-------------

## Using the framework

### Tweets

To view all tweets in the DB try:
- http://localhost:3000/tweets
- curl -X GET http://localhost:3000/tweets
-- PS no authentication is required here.

To view a certain tweet in DB try:
- http://localhost:3000/tweets?id=X
- curl -X GET -d 'id=X' http://localhost:3000/tweets
-- where X is the id of a tweet in the db.

To Insert tweets into the DB try:
- curl -u hamza:abc123 -X POST -d 'name=X' http://localhost:3000/admin/tweet
-- Where X is the screen_name of a Twitter user. I.e. katyperry.

To delete tweets from the DB try:
- curl -u hamza:abc123 -X DELETE -d 'id=X' http://localhost:3000/admin/tweet
-- Where X is the id of a Tweet in the db.

### Users

To View a user from the DB try:
- http://localhost:3000/user?name=X
- curl -X GET -d 'name=X' http://localhost:3000/user 
-- Where X is the screen_name of a Twitter user. I.e. katyperry.

