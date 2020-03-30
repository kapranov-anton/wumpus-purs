#!/bin/bash -xe
spago bundle-app
parcel build index.html
docker build -t wumpus-frontend .
heroku container:login
heroku container:push web -a wumpus-frontend
heroku container:release web -a wumpus-frontend
heroku open -a wumpus-frontend
