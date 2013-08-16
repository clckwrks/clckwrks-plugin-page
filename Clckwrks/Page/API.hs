{-# LANGUAGE FlexibleContexts, OverloadedStrings, RecordWildCards #-}
{-# OPTIONS_GHC -F -pgmFhsx2hs #-}
module Clckwrks.Page.API
    ( PageId(..)
--    , getPage
--    , getPageId
--    , getPageTitle
--    , getPageTitleSlug
--    , getPageContent
    , getPagesSummary
    , getPageSummary
    , getPageMenu
    , getPosts
    , extractExcerpt
    , getBlogTitle
    , googleAnalytics
    ) where

import Clckwrks             ( UserId )
import Clckwrks.Acid        (GetUACCT(..))
import Clckwrks.Monad       ( Clck, ClckT(..), ClckState(..), Content(..)
                            , getEnableAnalytics, query, update
                            )
import Clckwrks.Page.Acid   ( PagesSummary(..), Page(..), PageById(..), PageId(..)
                            , Slug(..), AllPosts(..), GetBlogTitle(..))
import Clckwrks.Page.Monad  ( PageM, markupToContent )
import Clckwrks.Page.Types  ( PublishStatus )
import Clckwrks.Page.URL    ( PageURL(ViewPageSlug))
import Clckwrks.URL         (ClckURL(..))
import Control.Applicative  ((<$>))
import Control.Monad.State  (get)
import Control.Monad.Trans  (MonadIO)
import qualified Data.Text  as T
import Data.Text.Lazy       (Text)
import Data.Time            (UTCTime)
import qualified Data.Text  as Text
import Clckwrks.Page.Types  (toSlug)
import Happstack.Server     (Happstack, escape, internalServerError, toResponse)
import HSP.XMLGenerator
import HSP.XML              (XML, cdata, fromStringLit)
import HSP.Google.Analytics (analyticsAsync)
import Text.HTML.TagSoup    ( (~==), isTagCloseName, isTagOpenName, parseTags
                            , renderTags, sections)
{-
getPage :: PageM Page
getPage =
    do ClckState{..} <- get
       mPage <- query (PageById currentPage)
       case mPage of
         Nothing -> escape $ internalServerError $ toResponse ("getPage: invalid PageId " ++ show (unPageId currentPage))
         (Just p) -> return p

getPageId :: PageM PageId
getPageId = currentPage <$> get

getPageTitle :: PageM Text
getPageTitle = pageTitle <$> getPage

getPageTitleSlug :: PageM (Text, Maybe Slug)
getPageTitleSlug =
    do p <- getPage
       return (pageTitle p, pageSlug p)

getPageContent :: PageM Content
getPageContent =
    do mrkup <- pageSrc <$> getPage
       markupToContent mrkup
-}
getPagesSummary :: PageM [(PageId, T.Text, Maybe Slug, UTCTime, UserId, PublishStatus)]
getPagesSummary = query PagesSummary

getPageMenu :: GenXML PageM
getPageMenu =
    do ps <- query PagesSummary
       case ps of
         [] -> <div>No pages found.</div>
         _ -> <ul class="page-menu">
                <% mapM (\(pid, ttl, slug,_,_,_) -> <li><a href=(ViewPageSlug pid (toSlug ttl slug)) title=ttl><% ttl %></a></li>) ps %>
              </ul>

getPageSummary :: PageId -> PageM Content
getPageSummary pid =
    do mPage <- query (PageById pid)
       case mPage of
         Nothing ->
             return $ PlainText $ Text.pack $ "Invalid PageId " ++ (show $ unPageId pid)
         (Just pge) ->
             extractExcerpt pge

getBlogTitle :: PageM T.Text
getBlogTitle = query GetBlogTitle

extractExcerpt :: (MonadIO m, Functor m, Happstack m) =>
                  Page
               -> ClckT url m Content
extractExcerpt Page{..} =
             case pageExcerpt of
               (Just excerpt) ->
                   markupToContent excerpt
               Nothing ->
                   do c <- markupToContent pageSrc
                      case c of
                        (TrustedHtml html) ->
                            let tags = parseTags html
                                paragraphs = sections (~== ("<p>" :: String)) tags
                                paragraph = case paragraphs of
                                              [] -> Text.pack "no summary available."
                                              (p:ps) -> renderTags $ takeThrough (not . isTagCloseName (Text.pack "p")) $ filter (not . isTagOpenName (Text.pack "img")) p
                            in return (TrustedHtml paragraph)
                        (PlainText text) ->
                               return (PlainText text)

takeThrough :: (a -> Bool) -> [a] -> [a]
takeThrough _ [] = []
takeThrough f (p:ps)
    | f p = p : takeThrough f ps
    | otherwise = []

-- | get all posts, sorted reverse cronological
getPosts :: XMLGenT (PageM) [Page]
getPosts = query AllPosts

-- | create a google analytics tracking code block
--
-- This will under two different conditions:
--
--  * the 'enableAnalytics' field in 'ClckState' is 'False'
--
--  * the 'uacct' field in 'PageState' is 'Nothing'
googleAnalytics :: XMLGenT (PageM) XML
googleAnalytics =
    do enabled <- getEnableAnalytics
       case enabled of
         False -> return $ cdata ""
         True ->
             do muacct <- query GetUACCT
                case muacct of
                  Nothing -> return $ cdata ""
                  (Just uacct) ->
                      analyticsAsync uacct
