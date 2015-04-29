{-# LANGUAGE QuasiQuotes, OverloadedStrings #-}
{-# OPTIONS_GHC -F -pgmFhsx2hs #-}
module Clckwrks.Page.Admin.EditPage
    ( editPage
    ) where

import Control.Applicative ((<$>), (<*>), (<*))
import Clckwrks                hiding (transform)
import Clckwrks.Admin.Template (template)
import Clckwrks.Page.Monad          (PageM, PageForm, PageFormError(..))
import Clckwrks.Page.Acid      (Markup(..), Page(..), PageKind(..), PublishStatus(..), PreProcessor(..), PageById(..), UpdatePage(..))
import Clckwrks.Page.Types     (PageId(..), Slug(..), toSlug, slugify)
import Clckwrks.Page.URL       (PageURL(..), PageAdminURL(..))
import Control.Monad.State     (get)
import Data.Maybe              (isJust, maybe)
import qualified Data.Text     as Text
import Data.Text.Lazy          (Text)
import Data.Time.Clock         (getCurrentTime)
import HSP.XML
import HSP.XMLGenerator
import Text.Reform             ((<++), (++>), mapView, transformEitherM, transform, decimal)
import Text.Reform.Happstack   (reform)
import Text.Reform.HSP.Text    (form, button, inputCheckbox, inputText, labelText, inputSubmit, select, textarea, fieldset, ol, li, setAttrs)

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
                styles <- getThemeStyles =<< plugins <$> get
                template "edit page" () $
                  <%>
                   <% reform (form action) "ep" updatePage Nothing (pageFormlet styles page) %>
                  </%>
    where
      updatePage :: (Page, AfterSaveAction) -> PageM Response
      updatePage (page, afterSaveAction) =
          do update (UpdatePage page)
             case afterSaveAction of
               EditSomeMore -> seeOtherURL (PageAdmin $ EditPage    (pageId page))
               VisitPage    -> seeOtherURL (ViewPageSlug (pageId page) (toSlug (pageTitle page) (pageSlug page)))
               ShowPreview  -> seeOtherURL (PageAdmin $ PreviewPage (pageId page))


pageFormlet :: [(ThemeStyleId, ThemeStyle)] -> Page -> PageForm (Page, AfterSaveAction)
pageFormlet styles' page =
    let styles = map (\(i, ts) -> (i, themeStyleName ts)) styles' in
    divHorizontal $
      (fieldset $
        (,,,,,,)
                <$> (divControlGroup (label' "Page Type"       ++> (divControls $ select [(PlainPage, ("page" :: Text)), (Post, "post")] (== (pageKind page)))))
--                <*> (divControlGroup (label' "Theme Style"     ++> (divControls $ ThemeStyleId <$> (select styles (const True)) `transform` (decimal (const PageErrorInternal)))))
                <*> (divControlGroup (label' "Theme Style"     ++> (divControls $ select styles (== (fst $ head styles)))))
                <*> (divControlGroup (label' "Title"           ++> (divControls $ inputText (pageTitle page) `setAttrs` [("size" := "80"), ("class" := "input-xxlarge") :: Attr Text Text])))
                <*> (divControlGroup (label' "Slug (optional)" ++> (divControls $ inputText (maybe Text.empty unSlug $ pageSlug page) `setAttrs` [("size" := "80"), ("class" := "input-xxlarge")  :: Attr Text Text])))
                <*> (divControlGroup (divControls (inputCheckboxLabel ("Highlight Haskell code using HsColour" :: Text) hsColour)))
                <*> (divControlGroup (label' "Body"            ++> (divControls $ textarea 80 25 (markup (pageSrc page)) `setAttrs` [("class" := "input-xxlarge")  :: Attr Text Text])))
                <*> (divFormActions
                      ((,,) <$> (inputSubmit' (Text.pack "Save"))
                            <*> (inputSubmit'  (Text.pack "Preview") `setAttrs` (("class" := "btn btn-info")  :: Attr Text Text))
                            <*> newPublishStatus (pageStatus page)))
      ) `transformEitherM` toPage

    where
      inputSubmit' :: Text.Text -> PageForm (Maybe Text.Text)
      inputSubmit' str = inputSubmit str `setAttrs` [("class":="btn") :: Attr Text Text]
      inputCheckboxLabel lbl b =
          mapView (\xml -> [<label class="checkbox"><% xml %><% lbl %></label>])
                      (inputCheckbox b)

      label' str       = (labelText str `setAttrs` [("class":="control-label") :: Attr Text Text])

      labelCB str      = labelText str `setAttrs` [("class":="checkbox") :: Attr Text Text]
--      divInline        = mapView (\xml -> [<div class="checkbox inline"><% xml %></div>])
      divFormActions   = mapView (\xml -> [<div class="form-actions"><% xml %></div>])
      divHorizontal    = mapView (\xml -> [<div class="form-horizontal"><% xml %></div>])
      divControlGroup  = mapView (\xml -> [<div class="control-group"><% xml %></div>])
      divControls      = mapView (\xml -> [<div class="controls"><% xml %></div>])

      newPublishStatus :: PublishStatus -> PageForm (Maybe PublishStatus)
      newPublishStatus Published = fmap (const Draft)     <$> (inputSubmit' (Text.pack "Unpublish") `setAttrs` [("class" := "btn btn-warning") :: Attr Text Text])
      newPublishStatus _         = fmap (const Published) <$> (inputSubmit' (Text.pack "Publish")   `setAttrs` [("class" := "btn btn-success") :: Attr Text Text])
      hsColour = HsColour `elem` (preProcessors $ pageSrc page)
      toPage :: (MonadIO m) =>
                (PageKind, ThemeStyleId, Text.Text, Text.Text, Bool, Text.Text, (Maybe Text.Text, Maybe Text.Text, Maybe PublishStatus))
             -> m (Either PageFormError (Page, AfterSaveAction))
      toPage (kind, style, ttl, slug, haskell, bdy, (msave, mpreview, mpagestatus)) =
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
                      , pageThemeStyleId = style
                      }
               , if isJust mpreview
                 then ShowPreview
                 else case mpagestatus of
                       (Just Published) -> VisitPage
                       _                -> EditSomeMore
               )
