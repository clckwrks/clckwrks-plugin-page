{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -F -pgmFtrhsx #-}
module Clckwrks.Page.Admin.EditFeedConfig where

import Clckwrks                 (ClckURL(Admin), AdminURL(Console), query, update)
import Clckwrks.Admin.Template  (template)
import Clckwrks.Page.Acid       (GetFeedConfig(..), SetFeedConfig(..))
import Clckwrks.Page.Monad      (PageConfig(pageClckURL), PageM, PageForm, PageFormError)
import Clckwrks.Page.Types      (FeedConfig(..))
import Clckwrks.Page.URL        (PageURL(..))
import Control.Applicative      ((<$>), (<*), (<*>))
import Control.Monad.Reader     (ask)
import Data.Text                (Text, pack)
import Happstack.Server         (Response, seeOther, toResponse)
import HSP
import Text.Reform
import Text.Reform.Happstack
import Text.Reform.HSP.Text
import Web.Routes               (showURL)

editFeedConfig :: PageURL -> PageM Response
editFeedConfig here =
    do feedConfig <- query $ GetFeedConfig
       action <- showURL here
       template "edit feed config" () $
                  <%>
                   <% reform (form action) "ep" updateFeedConfig Nothing (feedConfigForm feedConfig) %>
                  </%>
    where
      updateFeedConfig :: FeedConfig -> PageM Response
      updateFeedConfig fc =
          do update (SetFeedConfig fc)
             showURL <- pageClckURL <$> ask
             seeOther (showURL (Admin Console) []) (toResponse ())

feedConfigForm :: FeedConfig -> PageForm FeedConfig
feedConfigForm fc@FeedConfig{..} =
    fieldset $
     ol $
      ((,) <$> (li $ label "Feed Title:")          ++> (li $ inputText feedTitle)
           <*> (li $ label "Default Author Name:") ++> (li $ inputText feedAuthorName)
           <* inputSubmit (pack "update")
      )
     `transformEither` toFeedConfig
    where
      toFeedConfig :: (Text, Text) -> Either PageFormError FeedConfig
      toFeedConfig (ttl, athr) =
              Right $ fc { feedTitle      = ttl
                         , feedAuthorName = athr
                         }
