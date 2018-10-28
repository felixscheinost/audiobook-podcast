#!/bin/sh
stack --work-dir .stack-work-profile build --profile --ghc-options -DDEVELOPMENT && stack --work-dir .stack-work-profile exec -- calibre-audiobook-podcast
