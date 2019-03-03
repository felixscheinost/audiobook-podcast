#!/bin/sh
cd static/js
yarn dev
cd ../../
stack build --flag audiobook-podcast:dev --ghc-options -DDEVELOPMENT && stack exec -- audiobook-podcast
