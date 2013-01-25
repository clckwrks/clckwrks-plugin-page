{-# LANGUAGE DeriveDataTypeable, TemplateHaskell, TypeFamilies #-}
module Clckwrks.Page.URL where

import Data.Data (Data, Typeable)
import Data.SafeCopy               (SafeCopy(..), base, deriveSafeCopy)
import Clckwrks.Page.Acid          (PageId(..))
import Clckwrks.Page.Types         (Slug(..))
import Web.Routes.TH               (derivePathInfo)

data PageAdminURL
    = EditPage PageId
    | PreviewPage PageId
    | Pages
    | NewPage
    | NewPost
    | EditFeedConfig
      deriving (Eq, Ord, Data, Typeable, Read, Show)
$(deriveSafeCopy 0 'base ''PageAdminURL)
$(derivePathInfo ''PageAdminURL)

data PageURL
    = ViewPage PageId
    | ViewPageSlug PageId Slug
    | Blog
    | AtomFeed
    | PageAdmin PageAdminURL
      deriving (Eq, Ord, Data, Typeable, Read, Show)
$(deriveSafeCopy 0 'base ''PageURL)
$(derivePathInfo ''PageURL)
