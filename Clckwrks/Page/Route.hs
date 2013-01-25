{-# LANGUAGE OverloadedStrings #-}
module Clckwrks.Page.Route where

import Clckwrks                     (Role(..), requiresRole_)
import Clckwrks.Monad               ( ClckState(plugins), Theme(themeBlog), query
                                    , update, setUnique, themeTemplate, nestURL
                                    )
import Clckwrks.Page.Types          (Page(..), PageId(..), toSlug)
import Clckwrks.Page.Acid           (GetPageTitle(..), IsPublishedPage(..), PageById(..))
import Clckwrks.Page.Admin.EditFeedConfig (editFeedConfig)
import Clckwrks.Page.Admin.EditPage (editPage)
import Clckwrks.Page.Admin.NewPage  (newPage)
import Clckwrks.Page.Admin.Pages    (pages)
import Clckwrks.Page.Admin.PreviewPage (previewPage)
import Clckwrks.Page.Atom           (handleAtomFeed)
import Clckwrks.Page.Monad          (PageConfig(pageClckURL), PageM, clckT2PageT, markupToContent)
import Clckwrks.Page.Types          (PageKind(PlainPage, Post))
import Clckwrks.Page.URL            (PageURL(..), PageAdminURL(..))
import Control.Applicative          ((<$>))
import Control.Monad.Reader         (ask)
import Control.Monad.State          (get)
import Data.Text                    (Text)
import qualified Data.Set           as Set
import Happstack.Server             ( Response, Happstack, escape, notFound, toResponse
                                    , ok, internalServerError
                                    )
import HSP                          (unXMLGenT)
import Web.Routes.Happstack         (seeOtherURL)
import Web.Plugins.Core             (getTheme)

checkAuth :: PageURL
          -> PageM PageURL
checkAuth url =
    do showFn <- pageClckURL <$> ask
       let requiresRole = requiresRole_ showFn
       case url of
         ViewPage{}     -> return url
         ViewPageSlug{} -> return url
         Blog{}         -> return url
         AtomFeed{}     -> return url
         PageAdmin {}   -> requiresRole (Set.singleton Administrator) url

-- | routes for 'AdminURL'
routePageAdmin :: PageAdminURL -> PageM Response
routePageAdmin url =
    case url of
      (EditPage pid)    -> editPage (PageAdmin url) pid
      NewPage           -> newPage PlainPage
      NewPost           -> newPage Post
      (PreviewPage pid) -> previewPage pid -- FIXME
      EditFeedConfig    -> editFeedConfig (PageAdmin url)
      Pages             -> pages

routePage :: PageURL
          -> PageM Response
routePage url' =
    do url <- checkAuth url'
       setUnique 0
       case url of
         (ViewPage pid) ->
           do r <- query (GetPageTitle pid)
              case r of
                Nothing ->
                    notFound $ toResponse ("Invalid PageId " ++ show (unPageId pid))
                (Just (title, slug)) ->
                    seeOtherURL (ViewPageSlug pid (toSlug title slug))

         (ViewPageSlug pid _slug) ->
           do published <- query (IsPublishedPage pid)
              if published
                 then do cs <- get
                         (Just page) <- query (PageById pid)
                         let ttl = pageTitle page
                         bdy <- markupToContent (pageSrc page)
                         clckT2PageT $ themeTemplate (plugins cs) ttl () bdy
                 else do notFound $ toResponse ("Invalid PageId " ++ show (unPageId pid))

         (Blog) ->
           do p <- plugins <$> get
              mTheme <- getTheme p
              case mTheme of
                Nothing -> escape $ internalServerError $ toResponse $ ("No theme package is loaded." :: Text)
                (Just theme) -> internalServerError $ toResponse ("Theme flattening not supported yet." :: Text)

         AtomFeed ->
             do handleAtomFeed

         (PageAdmin adminURL) -> routePageAdmin adminURL
