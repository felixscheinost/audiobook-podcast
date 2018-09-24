{-# LANGUAGE OverloadedStrings #-}

module Views (homeView) where

import           Data.Monoid                 (mempty)
import           Data.Text.Lazy              (toStrict)
import           Prelude                     hiding (div, head, id)
import           Text.Blaze.Html             (Html, toHtml)
import           Text.Blaze.Html5            (Html, a, body, button,
                                              dataAttribute, div, docTypeHtml,
                                              form, h1, h2, head, input, li,
                                              link, meta, p, script, style,
                                              title, ul, (!))
import Text.Blaze.Internal (preEscapedText)
import Text.Blaze.Html.Renderer.Text (renderHtml)
import           Text.Blaze.Html5.Attributes (charset, class_, content, href,
                                              httpEquiv, id, media, name,
                                              placeholder, rel, src, type_)
import           Web.Scotty                  (ActionM, html)



blaze :: Html -> ActionM ()
blaze = html . renderHtml

layout :: Html -> Html -> Html
layout t b = docTypeHtml $ do
    preEscapedText "<!--[if lt IE 7]>      <html class='no-js lt-ie9 lt-ie8 lt-ie7'> <![endif]-->"
    preEscapedText "<!--[if IE 7]>         <html class='no-js lt-ie9 lt-ie8'/> <![endif]-->"
    preEscapedText "<!--[if IE 8]>         <html class='no-js lt-ie9'> <![endif]-->"
    preEscapedText "<!--[if gt IE 8]><!--> <html class='no-js'> <!--<![endif]-->"
    head $ do
        title t
        meta ! charset "utf-8"
        meta ! httpEquiv "X-UA-Compatible" ! content "IE=edge,chrome=1"
        meta ! name "description" ! content "Inspire Text"
        meta ! name "viewport" ! content "width=device-width"
        link ! href "/css/bootstrap.min.css" ! rel  "stylesheet" ! media "screen"
        -- style $ pet $ toStrict layoutCss
    body $ do
        b
        script ! src "/js/bootstrap.bundle.min.js" $ mempty

homeView :: ActionM ()
homeView = blaze $ layout "home" $ do
    h1 "Test"
    
