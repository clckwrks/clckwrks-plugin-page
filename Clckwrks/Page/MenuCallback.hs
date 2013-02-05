{-# LANGUAGE OverloadedStrings #-}
module Clckwrks.Page.MenuCallback where

import Clckwrks  (ClckT, ClckURL, query)
import Clckwrks.Menu.Types (MenuLink(..))
import Clckwrks.Page.Acid (AllPublishedPages(..), PageState)
import Clckwrks.Page.Types (Page(..))
import Clckwrks.Page.URL  (PageURL(..))
import Data.Acid (AcidState)
import Data.Acid.Advanced (query')
import Data.Function (on)
import Data.List (sortBy)
import Data.Text (Text)

menuCallback :: AcidState PageState
             -> (PageURL -> [(Text, Maybe Text)] -> Text)
             -> ClckT ClckURL IO (String, [MenuLink])
menuCallback acidMenuState showPageURL =
    do pages <- query' acidMenuState AllPublishedPages
       let blogLink  = MenuLink { menuItemName = "Blog", menuItemLink = showPageURL Blog [] }
           pageLinks = map (\p -> MenuLink (pageTitle p) (showPageURL (ViewPage (pageId p)) [])) pages
       return ("Page", blogLink : (sortBy (compare `on` menuItemName) pageLinks))
