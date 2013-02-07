{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -F -pgmFtrhsx #-}
module Clckwrks.Page.Admin.EditPage
    ( editPage
    ) where

import Control.Applicative ((<$>), (<*>), (<*))
import Clckwrks
import Clckwrks.Admin.Template (template)
import Clckwrks.Page.Monad          (PageM, PageForm, PageFormError)
import Clckwrks.Page.Acid      (Markup(..), Page(..), PageKind(..), PublishStatus(..), PreProcessor(..), PageById(..), UpdatePage(..))
import Clckwrks.Page.Types     (PageId(..), Slug(..), toSlug, slugify)
import Clckwrks.Page.URL       (PageURL(..), PageAdminURL(..))
import Data.Maybe              (isJust, maybe)
import Data.Text               (Text, pack)
import qualified Data.Text     as Text
import Data.Time.Clock         (getCurrentTime)
import Text.Reform             ((<++), (++>), mapView, transformEitherM)
import Text.Reform.Happstack   (reform)
import Text.Reform.HSP.Text    (form, button, inputCheckbox, inputText, label, inputSubmit, select, textarea, fieldset, ol, li, setAttrs)

data AfterSaveAction
    = EditSomeMore
    | VisitPage
    | ShowPreview

editPage :: PageURL -> PageId -> PageM Response
editPage here pid =
    do mPage <- query $ PageById pid
       case mPage of
         Nothing -> notFound $ toResponse $ "Page not found: " ++ show (unPageId pid)
         (Just page) ->
             do action <- showURL here
                template "edit page" () $
                  <%>
                   <% reform (form action) "ep" updatePage Nothing (pageFormlet page) %>
                  </%>
    where
      updatePage :: (Page, AfterSaveAction) -> PageM Response
      updatePage (page, afterSaveAction) =
          do update (UpdatePage page)
             case afterSaveAction of
               EditSomeMore -> seeOtherURL (PageAdmin $ EditPage    (pageId page))
               VisitPage    -> seeOtherURL (ViewPageSlug (pageId page) (toSlug (pageTitle page) (pageSlug page)))
               ShowPreview  -> seeOtherURL (PageAdmin $ PreviewPage (pageId page))


pageFormlet :: Page -> PageForm (Page, AfterSaveAction)
pageFormlet page =
    divHorizontal $
      (fieldset $
        (,,,,,)
                <$> (divControlGroup (label' "Page Type"            ++> (divControls $ select [(PlainPage, "page"), (Post, "post")] (== (pageKind page)))))
                <*> (divControlGroup (label' "Title"           ++> (divControls $ inputText (pageTitle page) `setAttrs` [("size" := "80"), ("class" := "input-xxlarge")])))
                <*> (divControlGroup (label' "Slug (optional)" ++> (divControls $ inputText (maybe Text.empty unSlug $ pageSlug page) `setAttrs` [("size" := "80"), ("class" := "input-xxlarge")])))
                <*> (divControlGroup (divControls (inputCheckboxLabel "Highlight Haskell code using HsColour" hsColour)))
                <*> (divControlGroup (label' "Body"            ++> (divControls $ textarea 80 25 (markup (pageSrc page)) `setAttrs` [("class" := "input-xxlarge")])))
                <*> (divFormActions
                      ((,,) <$> (inputSubmit' (pack "Save"))
                            <*> (inputSubmit'  (pack "Preview") `setAttrs` ("class" := "btn btn-info"))
                            <*> newPublishStatus (pageStatus page)))
      ) `transformEitherM` toPage

    where
      inputSubmit' str = inputSubmit str `setAttrs` [("class":="btn")]
      inputCheckboxLabel lbl b =
          mapView (\xml -> [<label class="checkbox"><% xml %><% lbl %></label>])
                      (inputCheckbox b)

      label' str       = (label str `setAttrs` [("class":="control-label")])

      labelCB str      = label str `setAttrs` [("class":="checkbox")]
--      divInline        = mapView (\xml -> [<div class="checkbox inline"><% xml %></div>])
      divFormActions   = mapView (\xml -> [<div class="form-actions"><% xml %></div>])
      divHorizontal    = mapView (\xml -> [<div class="form-horizontal"><% xml %></div>])
      divControlGroup  = mapView (\xml -> [<div class="control-group"><% xml %></div>])
      divControls      = mapView (\xml -> [<div class="controls"><% xml %></div>])

      newPublishStatus :: PublishStatus -> PageForm (Maybe PublishStatus)
      newPublishStatus Published = fmap (const Draft)     <$> (inputSubmit' (pack "Unpublish") `setAttrs` [("class" := "btn btn-warning")])
      newPublishStatus _         = fmap (const Published) <$> (inputSubmit' (pack "Publish")   `setAttrs` [("class" := "btn btn-success")])
      hsColour = HsColour `elem` (preProcessors $ pageSrc page)
      toPage :: (MonadIO m) =>
                (PageKind, Text, Text, Bool, Text, (Maybe Text, Maybe Text, Maybe PublishStatus))
             -> m (Either PageFormError (Page, AfterSaveAction))
      toPage (kind, ttl, slug, haskell, bdy, (msave, mpreview, mpagestatus)) =
          do now <- liftIO $ getCurrentTime
             return $ Right $
               ( Page { pageId      = pageId page
                      , pageAuthor  = pageAuthor page
                      , pageTitle   = ttl
                      , pageSlug    = if Text.null slug then Nothing else Just (slugify slug)
                      , pageSrc     = Markup { preProcessors =  (if haskell then ([ HsColour ] ++) else id) [ Markdown ]
                                             , trust = Trusted
                                             , markup = bdy
                                             }
                      , pageExcerpt = Nothing
                      , pageDate    = pageDate page
                      , pageUpdated = now
                      , pageStatus  = case mpagestatus of
                                        (Just newStatus) -> newStatus
                                        Nothing          -> pageStatus page
                      , pageKind    = kind
                      , pageUUID    = pageUUID page
                      }
               , if isJust mpreview
                 then ShowPreview
                 else case mpagestatus of
                       (Just Published) -> VisitPage
                       _                -> EditSomeMore
               )
