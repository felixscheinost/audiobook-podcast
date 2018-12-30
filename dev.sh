#!/bin/sh
stack build --flag audiobook-podcast:dev --ghc-options -DDEVELOPMENT && stack exec -- audiobook-podcast
