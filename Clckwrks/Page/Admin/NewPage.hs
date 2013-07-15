{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -F -pgmFhsx2hs #-}
module Clckwrks.Page.Admin.NewPage where

import Clckwrks
import Clckwrks.Page.Acid      as Acid
import Clckwrks.Page.Monad     (PageM)
import Clckwrks.Page.URL       as URL (PageURL(..), PageAdminURL(NewPage, NewPost, EditPage))
import Clckwrks.Admin.Template (template)
import Data.UUID               () -- instance Random UUID
import Data.Time.Clock         (getCurrentTime)
import Data.Text.Lazy          (Text)
import HSP.XML
import HSP.XMLGenerator
import System.Random           (randomIO)

newPage :: PageKind -> PageM Response
newPage pageKind =
    do method GET
       template "Create New Page/Post" () $
         <%>
          <form class="form-inline" action=(PageAdmin URL.NewPage) method="POST" enctype="multipart/form-data">
           <button class="btn" type="submit">Create New Page</button>
          </form>
          <form class="form-inline" action=(PageAdmin URL.NewPost) method="POST" enctype="multipart/form-data">
           <button class="btn" type="submit">Create New Post</button>
          </form>
         </%>

    <|>
    do method POST
       uuid <- liftIO $ randomIO
       now  <- liftIO $ getCurrentTime
       muid <- getUserId
       case muid of
         Nothing -> escape $ internalServerError $ toResponse ("Clcwrks.Admin.NewPage.newPage was unable to obtain the current UserId" :: Text)
         (Just uid) ->
             do page <- update (Acid.NewPage pageKind uid uuid now)
                seeOtherURL (PageAdmin $ EditPage (pageId page))
