# Overview

This is a Haskell API running on the Scotty web framework. The API runs on localhost:3000 and provides for basic HTTP requests to retrieve & add users/tweets. This data is retrieved and parsed from the public Twitter API.

The framework is currently hosted and running on Amazon's EC2 server.

For haddock style documentation, open up the *index.html* file located inside the *haddock* folder.

Technologies used:

* `Aeson` for JSON encoding/decoding
* `Configurator` for configuration
* `mysql` and `mysql-simple` for DB access
* `OAuth` client for Twitter authentication
* `resource-pool` for DB connections pool
* `Scotty` for routing
* `wai-extra` for basic authentication
* `Warp` as webserver

## Configuring the project remotely

To ssh to the EC2 server you must first download the *twikell-public.pem* key pair for authentication. Once it's downloaded CD into the folder and run in terminal:

1. `chmod 600 twikell-public.pem`
2. `ssh -v -i twikell-public.pem ubuntu@ec2-54-194-118-199.eu-west-1.compute.amazonaws.com`
3. All required modules/libraries and DB already setup. Enjoy.

If you ever need to copy new files over to the EC2 server you can do so by *scp*

`scp -i twikell-public.pem -r . ubuntu@ec2-54-194-118-199.eu-west-1.compute.amazonaws.com:twikell-web`

Which adds the current directory recursively to the *twikell-web* folder at the remote side.

## Configuring the project locally

Make sure that both the haskell-framework and cabal is set up
before proceeding.

1. First you need to install both MySQL-Client and MySQL-Server
  `sudo apt-get install mysql-client mysql-server`
2. Make sure that libpcre is installed
  `sudo apt-get install libpcre3 libpcre3-dev`
3. Setup all dependent modules and libraries
  `cabal install --only-dependencies`
4. Finally run the mysql dump to populate the database.

Any cabal problems; you're on your own.

*PS. Something that helped personally was:* `rm -rf ~/.ghc ~/.cabal` *and reinstall both. All jokes aside though... dont. Have fun!*

## Using the framework

CD into the source folder, run GHCI and load the main:

1. `cd /path-to-twikell-web/src`
2. `ghci`
3. `:l Main.hs` 
4. `main`

Or compile and run from the main folder.

**For accessing the remote EC2 server, replace all occurences of localhost with 54.194.118.199. Remember to first run the server remotely by following the steps depicted under *Configuring the project remotely*.**

### Authentication

*Some routes that perform POST, DELETE and UPDATE methods require authentication.*

User: *hamza*
Password: *abc123*

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
  - *This also adds a user to the DB in case he doesn't exist.*

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

