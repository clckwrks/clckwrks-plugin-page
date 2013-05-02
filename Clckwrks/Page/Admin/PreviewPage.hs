{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -F -pgmFtrhsx #-}
module Clckwrks.Page.Admin.PreviewPage
    ( previewPage
    ) where

import Clckwrks
import Clckwrks.Admin.Template   (template)
import Clckwrks.ProfileData.Acid (HasRole(..))
import Clckwrks.Page.Acid        (Page(..), PageId(..), PublishStatus(..), PageById(..))
import Clckwrks.Page.Monad       (PageM, clckT2PageT, markupToContent)
import Clckwrks.Unauthorized     ()
import Control.Monad.State       (get)
import qualified Data.Set        as Set
import Web.Plugins.Core          (getTheme)

previewPage :: PageId -> PageM Response
previewPage pid =
    do mPage <- query $ PageById pid
       case mPage of
         Nothing -> do notFound ()
                       template "Page not found" () $ <% "Page not found: " ++ show (unPageId pid) %>
         (Just page) ->
           do muid <- getUserId
              authorized <-
                  case muid of
                    Nothing    -> return False
                    (Just uid) -> query $ HasRole uid (Set.singleton Administrator)
              if authorized
                 then do cs <- get
                         (Just page) <- query (PageById pid)
                         let ttl = pageTitle page
                         bdy <- markupToContent (pageSrc page)
                         addHeaderM "X-XSS-Protection" "0"
                         clckT2PageT $ themeTemplate (plugins cs) ttl () bdy
                 else unauthorized (toResponse $ "Sorry, you need Administrator access to view this page.")
