-----------------------------------------------------------------------------
-- |
-- Module      :  Handler.Home
-- Copyright   :  (c) Artem Chirkin
-- License     :  BSD3
--
-- Maintainer  :  Artem Chirkin <chirkin@arch.ethz.ch>
-- Stability   :  experimental
--
--
-----------------------------------------------------------------------------
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Handler.Home where


--import Control.Monad.Trans.Resource (runResourceT)
--import Data.Conduit
--import Data.Conduit.Binary
import Data.Default
--import qualified Data.ByteString as S
--import qualified Data.ByteString.Lazy as L
import Yesod
import Yesod.Default.Util
--import Yesod.Auth

import Foundation
import Model

getHomeR :: Handler Html
getHomeR = do
--    maid <- maybeAuthId
--    (formWidget, formEncType) <- generateFormPost uploadForm
    storedFiles <- getImages >>= mapM (\(Entity i story) -> runDB $ do
        place <- get404 $ storyPlace story
        country <- get404 $ placeCountry place
        student <- get404 $ storyAuthor story
        return (i, studentName student
               , storyComment story
               , storyCreationTime story
               , countryName country
               , placeName place)
      )

    defaultLayout $ do
        setTitle "File Processor"
        $(widgetFileNoReload def "home")

getImages :: Handler [Entity Story]
getImages = runDB $ selectList [] []

postHomeR :: Handler Html
postHomeR = do
    ((_result, _), _) <- runFormPost uploadForm
--    case result of
--      FormSuccess fi -> do
--        fileBytes <- runResourceT $ fileSource fi $$ sinkLbs
--        addFile $ StoredFile (fileName fi) (fileContentType fi)
----                              fileBytes
--                             (S.pack . L.unpack $ fileBytes)
--      _ -> return ()
    redirect HomeR

uploadForm :: Html -> MForm Handler (FormResult FileInfo, Widget)
uploadForm = renderDivs $ fileAFormReq "file"


getQuaViewR :: Handler Html
getQuaViewR = sendFile "text/html" "web/qua-view.html"

getQuaViewCSSR :: Handler ()
getQuaViewCSSR = sendFile "text/css" "web/qua-view.css"

getQuaViewJSR :: Handler ()
getQuaViewJSR = sendFile "text/javascript" "web/qua-view.js"

getNumericMinJSR :: Handler ()
getNumericMinJSR = sendFile "text/javascript" "web/numeric.min.js"


getTestR :: Handler ()
getTestR = sendFile "text/html" "static/page.html"
