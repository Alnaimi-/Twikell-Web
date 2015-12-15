# Overview

This is a Haskell API running on the Scotty web framework. The API runs on localhost:3000 and provides for basic HTTP requests to retrieve & add users/tweets. This data is retrieved and parsed from the public Twitter API.

Technologies used:

* `Scotty` for routing
* `Configurator` for configuration
* `Aeson` for JSON encoding/decoding
* `MySQL` server
* `mysql` and `mysql-simple` for communication with DB
* `resource-pool` for DB connections pool
* `wai-extra` for basic authentication
* `Warp` as webserver
* `OAuth` client for Twitter authentication

## Authentication
User: *hamza*
Password: *abc123*

## Using the project

### Tweets

**To view all tweets in the DB try:**
- `http://localhost:3000/tweet`
- `curl -X GET http://localhost:3000/tweet`
  - *PS no authentication is required here.*

**To view tweets belonging to a certain user:**
- `http://localhost:3000/tweet/X`
- `curl -X GET http://localhost:3000/tweet/X`
  - *where x is the screen_user in the db.*

**To view a certain tweet in based on ID try:**
- `http://localhost:3000/tweetid/X`
- `curl -X GET http://localhost:3000/tweetid/X`
  - *where X is the ID of a tweet in the db.*

**To populate the DB with tweets from a certain user:**
- `curl -u hamza:abc123 -X POST http://localhost:3000/admin/tweet/X`
  - *Where X is the screen_name of a Twitter user. I.e. katyperry.*

**To delete certain tweets from the DB try:**
- `curl -u hamza:abc123 -X DELETE -d 'id=X' http://localhost:3000/admin/tweet`
  - *Where X is the id of a Tweet in the db.*

### Users

**To list all users in the DB try:**
- `http://localhost:3000/user`
- `curl -X GET http://localhost:3000/user`
  - *PS no authentication is required here.*

**To View a user in the DB try:**
- `http://localhost:3000/user/X`
- `curl -X GET http://localhost:3000/user/X` 
  - *Where X is the screen_name of a Twitter user. I.e. katyperry.*

**To Update a user in the DB try:**
- `curl -u hamza:abc123 -H "Content-Type: application/json" -X PUT -d '{"screen_name":"X","name":"Ben Steer","profile_image_url":"updated.com","location":"London","followers_count":1}' http://localhost:3000/admin/user`
  - *Where X is the screen_name of a Twitter user. I.e. katyperry.*

**To populate the DB with users:**
- `curl -u hamza:abc123 -X POST http://localhost:3000/admin/user/X,Y,Z,...,N)`
  - *Where X,Y,Z are screen_name's of Twitter users separated by commas. PS. number of N must be less than 100.*
  - *This also works with a single user, i.e. /X where X can be TheEllenShow*

**To search for Users in Twitter:**
- `http://localhost:3000/search/X
- `curl -X GET http://localhost:3000/search/X`
  - *Where X is the search query to match a Twitter user. I.e. katy.*
  - *Note that this does not populate the local side DB. No need was found for this.*

