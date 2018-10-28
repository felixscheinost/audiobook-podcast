#!/bin/sh
stack build --ghc-options -DDEVELOPMENT && stack exec -- calibre-audiobook-podcast
