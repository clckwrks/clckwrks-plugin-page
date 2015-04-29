{-# LANGUAGE RecordWildCards, OverloadedStrings #-}
{-# OPTIONS_GHC -F -pgmFhsx2hs #-}
module Clckwrks.Page.Admin.EditFeedConfig where

import Clckwrks                 (ClckURL(Admin), AdminURL(Console), query, update)
import Clckwrks.Admin.Template  (template)
import Clckwrks.Page.Acid       (GetFeedConfig(..), SetFeedConfig(..))
import Clckwrks.Page.Monad      (PageConfig(pageClckURL), PageM, PageForm, PageFormError)
import Clckwrks.Page.Types      (FeedConfig(..))
import Clckwrks.Page.URL        (PageURL(..))
import Control.Applicative      ((<$>), (<*), (<*>))
import Control.Monad.Reader     (ask)
import qualified Data.Text      as T
import Data.Text.Lazy           (Text)
import Happstack.Server         (Response, seeOther, toResponse)
import HSP.XML
import HSP.XMLGenerator
import Text.Reform
import Text.Reform.Happstack
import Text.Reform.HSP.Text
import Web.Routes               (showURL)

editFeedConfig :: PageURL -> PageM Response
editFeedConfig here =
    do feedConfig <- query GetFeedConfig
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
    divHorizontal $
      fieldset $
        ((,) <$> (divControlGroup (label' "Feed Title"          ++> (divControls $ inputText feedTitle)))
             <*> (divControlGroup (label' "Default Author Name" ++> (divControls $ inputText feedAuthorName)))
              <* (divControlGroup (divControls $ (inputSubmit (T.pack "Update") `setAttrs`[("class" := "btn") :: Attr Text Text])))
        )
     `transformEither` toFeedConfig
    where
      label' :: Text -> PageForm ()
      label' str      = (labelText str `setAttrs` [("class":="control-label") :: Attr Text Text])
      divHorizontal   = mapView (\xml -> [<div class="form-horizontal"><% xml %></div>])
      divControlGroup = mapView (\xml -> [<div class="control-group"><% xml %></div>])
      divControls     = mapView (\xml -> [<div class="controls"><% xml %></div>])

      toFeedConfig :: (T.Text, T.Text) -> Either PageFormError FeedConfig
      toFeedConfig (ttl, athr) =
              Right $ fc { feedTitle      = ttl
                         , feedAuthorName = athr
                         }

