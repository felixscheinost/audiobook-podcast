{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Handler.Conversion where

import Import
import qualified Zip
import           Database.Calibre.BookFormat (AudioFormat,
                                              CalibreBookFormat (Audio, ZIP),
                                              filePathAudioFormat)
import qualified Database.Calibre as DB
import Database.Calibre (CalibreBook, CalibreBookData)
import Audiobook (Audiobook)
import qualified Audiobook as AB


getConversionViewR :: Handler Html
getConversionViewR = 
    defaultLayout [whamlet|
        <div .container>       
            <ul .nav.nav-pills.mt-2>
                <li .nav-item>
                    <a .nav-link.active href="#">_{MsgConversionRunning}
                <li .nav-item>
                    <a .nav-link href="#">_{MsgConversionQueued}
                <li .nav-item>
                    <a .nav-link href="#">_{MsgConversionFinished}
            <div .media.conversion-job>
                <img .mr-3 src=@{BookCoverR 128} width="120px">
                <div .media-object>
                    <h5>
                        Konvertiere Schwarz zu MP3
                        <span .fas.fa-ban>
                    <table>
                        <tr>
                            <td>
                                <span .fas.fa-check>
                            <td>
                                Entpacke ZIP an temporären Ort
                            <td>
                        <tr>
                            <td>
                                <span .fas.fa-spinner.fa-spin>
                            <td>
                                Verknüpfe 120 MP3 Dateien zu einer einzelnen Datei
                            <td>
                                <div .progress.float-right.ml-1>
                                    <div .progress-bar role="progressbar" aria-valuenow="25" aria-valuemin="0" aria-valuemax="100" style="width: 25%">
                        <tr .text-muted>
                            <td>
                                <span .fas>
                            <td>
                                Verschiebe Zieldatei in Calibre-Bibliothek
                            <td>
                        <tr .text-muted>
                            <td>
                            <td>
                                Füge Eintrag in Calibre-Bibliothek hinzu
                            <td>
                        <tr .text-muted>
                            <td>
                            <td>
                                <span>Soll die Quell-Datei gelöscht werden?
                                <button .btn.btn-danger.ml-1.btn-sm disabled>Löschen
                            <td>

    |]