{-# OPTIONS_GHC -F -pgmFtrhsx #-}
module Clckwrks.Page.Admin.NewPage where

import Clckwrks
import Clckwrks.Page.Acid      as Acid
import Clckwrks.Page.Monad     (PageM)
import Clckwrks.Page.URL       as URL (PageURL(..), PageAdminURL(NewPage, NewPost, EditPage))
import Clckwrks.Admin.Template (template)
import Data.UUID               () -- instance Random UUID
import Data.Time.Clock         (getCurrentTime)
import System.Random           (randomIO)

newPage :: PageKind -> PageM Response
newPage pageKind =
    do method GET
       template "Create New Page/Post" () $
         <%>
          <form action=(PageAdmin URL.NewPage) method="POST" enctype="multipart/form-data">
           <button type="submit">Create New Page</button>
          </form>
          <form action=(PageAdmin URL.NewPost) method="POST" enctype="multipart/form-data">
           <button type="submit">Create New Post</button>
          </form>
         </%>

    <|>
    do method POST
       uuid <- liftIO $ randomIO
       now  <- liftIO $ getCurrentTime
       muid <- getUserId
       case muid of
         Nothing -> escape $ internalServerError $ toResponse "Clcwrks.Admin.NewPage.newPage was unable to obtain the current UserId"
         (Just uid) ->
             do page <- update (Acid.NewPage pageKind uid uuid now)
                seeOtherURL (PageAdmin $ EditPage (pageId page))
