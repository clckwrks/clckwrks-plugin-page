{-# LANGUAGE OverloadedStrings #-}
module Clckwrks.Page.NavBarCallback where

import Clckwrks            (ClckT, ClckURL, NamedLink(..), query)
import Clckwrks.Page.Acid  (AllPublishedPages(..), PageState)
import Clckwrks.Page.Types (Page(..))
import Clckwrks.Page.URL   (PageURL(..))
import Data.Acid           (AcidState)
import Data.Acid.Advanced  (query')
import Data.Function       (on)
import Data.List           (sortBy)
import Data.Text           (Text)

navBarCallback :: AcidState PageState
             -> (PageURL -> [(Text, Maybe Text)] -> Text)
             -> ClckT ClckURL IO (String, [NamedLink])
navBarCallback acidPageState showPageURL =
    do pages <- query' acidPageState AllPublishedPages
       let blogLink  = NamedLink { namedLinkTitle = "Blog", namedLinkURL = showPageURL Blog [] }
           pageLinks = map (\p -> NamedLink (pageTitle p) (showPageURL (ViewPage (pageId p)) [])) pages
       return ("Page", blogLink : (sortBy (compare `on` namedLinkTitle) pageLinks))
