{-# LANGUAGE OverloadedStrings #-}

module Views
  ( homeView
  ) where

import           Data.Foldable                 (forM_)
import           Data.Monoid                   (mempty)
import           Prelude                       hiding (div, head, id, span)
import           Text.Blaze.Html               (Html, string, toHtml)
import           Text.Blaze.Html.Renderer.Text (renderHtml)
import           Text.Blaze.Html5              (Html, a, body, button,
                                                dataAttribute, div, docTypeHtml,
                                                form, h1, h2, head, input, li,
                                                link, meta, p, script, span,
                                                style, title, ul, (!))
import           Text.Blaze.Html5.Attributes   (charset, class_, content, href,
                                                httpEquiv, id, media, name,
                                                placeholder, rel, src, type_)
import           Text.Blaze.Internal           (preEscapedText)
import           Types                         (Audiobook (..), MyActionM)
import           Web.Scotty.Trans              (html)

blaze :: Html -> MyActionM ()
blaze = html . renderHtml

layout :: Html -> Html
layout b =
  docTypeHtml $ do
    preEscapedText
      "<!--[if lt IE 7]>      <html class='no-js lt-ie9 lt-ie8 lt-ie7'> <![endif]-->"
    preEscapedText
      "<!--[if IE 7]>         <html class='no-js lt-ie9 lt-ie8'/> <![endif]-->"
    preEscapedText
      "<!--[if IE 8]>         <html class='no-js lt-ie9'> <![endif]-->"
    preEscapedText
      "<!--[if gt IE 8]><!--> <html class='no-js'> <!--<![endif]-->"
    head $ do
      title "Calibre-Audiobook"
      meta ! charset "utf-8"
      meta ! httpEquiv "X-UA-Compatible" ! content "IE=edge,chrome=1"
      meta ! name "description" ! content "Inspire Text"
      meta ! name "viewport" ! content "width=device-width"
      link ! href "/css/bootstrap.min.css" ! rel "stylesheet" ! media "screen"
    body $ do
      b
      script ! src "/js/bootstrap.bundle.min.js" $ mempty

homeView :: [Audiobook] -> MyActionM ()
homeView books =
  blaze $
  layout $ do
    h1 "Audiobooks in Calibre:"
    ul $ forM_ books $ \book -> li $ span $ toHtml $ show book
