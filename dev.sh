#!/bin/sh
stack build --flag calibre-audiobook-podcast:dev --ghc-options -DDEVELOPMENT && stack exec -- calibre-audiobook-podcast
