{-# LANGUAGE DataKinds, QuasiQuotes, OverloadedStrings #-}
{-# OPTIONS_GHC -F -pgmFhsx2hs #-}
module Clckwrks.Page.Admin.EditPage
    ( editPage
    ) where

import AccessControl.Relation  (Object(..), ObjectType(..), Relation(..), ToObject(toObject), RelationTuple(..), ObjectWildcard(..), WildcardObjectId(Wildcard), toNoWildcard, hasResource, hasRelation, hasSubjectType, ppRelationTuples, unObjectId)
import Control.Applicative ((<$>), (<*>), (<*))
import Clckwrks                hiding (transform)
import Clckwrks.AccessControl  (AccessList(..), emptyAccessList)
import Clckwrks.Admin.Template (template)
import Clckwrks.Page.Monad          (PageM, PageForm, PageFormError(..))
import Clckwrks.Page.Acid      (Markup(..), Page(..), PageKind(..), PublishStatus(..), PreProcessor(..), PageById(..), UpdatePage(..))
import Clckwrks.Page.Types     (PageId(..), Slug(..), toSlug, slugify)
import Clckwrks.Page.URL       (PageURL(..), PageAdminURL(..))
import Clckwrks.Rebac.API      (getRelationTuples)
import Control.Monad.State     (get)
import Data.Maybe              (isJust, maybe, catMaybes)
import Data.List               (sort, find)
import qualified Data.Text     as Text
import Data.Text.Lazy          (Text)
import Data.Time.Clock         (getCurrentTime)
import Debug.Trace             (trace)
import HSP.XML
import HSP.XMLGenerator
import Text.Read               (readMaybe)
import Text.Reform             ((<++), (++>), mapView, transformEitherM, transformEither, transform, decimal)
import Text.Reform.Happstack   (reform)
import Text.Reform.HSP.Text    (errorList, form, button, inputCheckbox, inputText, labelText, inputSubmit, select, textarea, fieldset, ol, li, setAttrs)

data AfterSaveAction
    = EditSomeMore
    | VisitPage
    | ShowPreview

relationTuplesToAccessList :: [ RelationTuple ] -> PageId -> AccessList
relationTuplesToAccessList rts pid =
  let pageTuples      = filter (\rt -> hasResource (toObject pid) rt && hasRelation (Relation "viewer") rt) rts
      userTuples      = filter (hasSubjectType (ObjectType "user")) pageTuples
      usergroupTuples = filter (hasSubjectType (ObjectType "usergroup")) pageTuples

      userIds :: [ UserId ]
      userIds =
        let subjects = catMaybes (map (toNoWildcard . subject) userTuples) :: [ Object NoWildcard ]
        in catMaybes $ map (fmap UserId . readMaybe . Text.unpack . unObjectId . objectId) subjects

      usergroups :: [ Text.Text ]
      usergroups =
        let subjects = catMaybes (map (toNoWildcard . subject) usergroupTuples)
        in map (unObjectId . objectId) subjects

      aa = isJust $ find (\(RelationTuple _ _ (Object _ si) Nothing _) -> si == Wildcard) userTuples

  in trace (show $ (ppRelationTuples userTuples, ppRelationTuples pageTuples, userIds)) $ (emptyAccessList { allowAny = aa
                                                                                                           , allowUserIds = userIds
                                                                                                           , allowUsergroups = usergroups
                                                                                                           })

editPage :: PageURL -> PageId -> PageM Response
editPage here pid =
    do mPage <- query $ PageById pid
       eRelTups <- getRelationTuples
       let relTups = case eRelTups of
               (Left e) -> trace (show e) $ []
               (Right t) -> t
           acl    = relationTuplesToAccessList relTups pid
       case mPage of
         Nothing -> notFound $ toResponse $ "Page not found: " ++ show (unPageId pid)
         (Just page) ->
             do action <- showURL here
                styles <- getThemeStyles =<< plugins <$> get
                template "edit page" () $
                  <%>
                   <% reform (form action) "ep" updatePage Nothing (pageFormlet styles page acl) %>
                  </%>
    where
      updatePage :: (Page, AfterSaveAction) -> PageM Response
      updatePage (page, afterSaveAction) =
          do -- liftIO $ print page
             me <- update (UpdatePage page)
             {-
             case me of
               (Just err) -> liftIO $ putStrLn err
               Nothing    -> pure ()
             -}
             case afterSaveAction of
               EditSomeMore -> seeOtherURL (PageAdmin $ EditPage    (pageId page))
               VisitPage    -> seeOtherURL (ViewPageSlug (pageId page) (toSlug (pageTitle page) (pageSlug page)))
               ShowPreview  -> seeOtherURL (PageAdmin $ PreviewPage (pageId page))


pageFormlet :: [(ThemeStyleId, ThemeStyle)] -> Page -> AccessList -> PageForm (Page, AfterSaveAction)
pageFormlet styles' page acl =
    let styles = map (\(i, ts) -> (i, themeStyleName ts)) styles'
    in errorList ++>
    (divHorizontal $
      (fieldset $
        (,,,,,,,)
                <$> (divControlGroup (label' "Page Type"       ++> (divControls $ select [(PlainPage, ("page" :: Text)), (Post, "post")] (== (pageKind page)))))
--                <*> (divControlGroup (label' "Theme Style"     ++> (divControls $ ThemeStyleId <$> (select styles (const True)) `transform` (decimal (const PageErrorInternal)))))
                <*> (divControlGroup (label' "Theme Style"     ++> (divControls $ select styles (== (pageThemeStyleId page)))))
                <*> (divControlGroup (label' "Title"           ++> (divControls $ inputText (pageTitle page) `setAttrs` [("size" := "80"), ("class" := "input-xxlarge") :: Attr Text Text])))
                <*> (divControlGroup (label' "Slug (optional)" ++> (divControls $ inputText (maybe Text.empty unSlug $ pageSlug page) `setAttrs` [("size" := "80"), ("class" := "input-xxlarge")  :: Attr Text Text])))
                <*> divControlGroup (label' "Markdown processor" ++> (divControls $ select [(Pandoc, "Pandoc"), (Markdown, "markdown perl script (legacy)" :: Text), (HsColour, "markdown perl script + hscolour (legacy)")] (\p -> p `elem` (preProcessors $ pageSrc page))))
                <*> (divControlGroup (label' "Body"            ++> (divControls $ textarea 80 25 (markup (pageSrc page)) `setAttrs` [("class" := "input-xxlarge")  :: Attr Text Text])))
                <*> (accessControlListFormlet acl)
                <*> (divFormActions
                      ((,,) <$> (inputSubmit' (Text.pack "Save"))
                            <*> (inputSubmit'  (Text.pack "Preview") `setAttrs` (("class" := "btn btn-info")  :: Attr Text Text))
                            <*> newPublishStatus (pageStatus page)))
      ) `transformEitherM` toPage)

    where
      inputSubmit' :: Text.Text -> PageForm (Maybe Text.Text)
      inputSubmit' str = inputSubmit str `setAttrs` [("class":="btn") :: Attr Text Text]
      inputCheckboxLabel :: Text -> Bool -> PageForm Bool
      inputCheckboxLabel lbl b =
          mapView (\xml -> [<label class="checkbox"><% xml %><% lbl %></label>])
                      (inputCheckbox b)

      label' :: Text -> PageForm ()
      label' str       = (labelText str `setAttrs` [("class":="control-label") :: Attr Text Text])

      labelCB :: Text -> PageForm ()
      labelCB str      = labelText str `setAttrs` [("class":="checkbox") :: Attr Text Text]
--      divInline        = mapView (\xml -> [<div class="checkbox inline"><% xml %></div>])
      divFormActions   = mapView (\xml -> [<div class="form-actions"><% xml %></div>])
      divHorizontal    = mapView (\xml -> [<div class="form-horizontal"><% xml %></div>])
      divControlGroup  = mapView (\xml -> [<div class="control-group"><% xml %></div>])
      divControls      = mapView (\xml -> [<div class="controls"><% xml %></div>])

      newPublishStatus :: PublishStatus -> PageForm (Maybe PublishStatus)
      newPublishStatus Published = fmap (const Draft)     <$> (inputSubmit' (Text.pack "Unpublish") `setAttrs` [("class" := "btn btn-warning") :: Attr Text Text])
      newPublishStatus _         = fmap (const Published) <$> (inputSubmit' (Text.pack "Publish")   `setAttrs` [("class" := "btn btn-success") :: Attr Text Text])

      inputTextCommaSeparated :: [Text.Text] -> PageForm [Text.Text]
      inputTextCommaSeparated txts =
          let withCommas   = Text.intercalate ", "txts
              removeCommas :: Text.Text -> [Text.Text]
              removeCommas = fmap Text.strip . Text.splitOn ","
          in fmap removeCommas (inputText withCommas)

      accessControlListFormlet :: AccessList -> PageForm AccessList
      accessControlListFormlet (AccessList allowAny allowUserIds allowUsergroups) =
         let showUserId (UserId i) = Text.pack (show i)
             parseUserIds :: [Text.Text] -> Either PageFormError [UserId]
             parseUserIds [] = Right []
             parseUserIds (t:txts) | Text.null t = parseUserIds txts
             parseUserIds (t:txts) =
                case parseUserId t of
                  (Left e) -> error $ (show e)
                  (Right uid) ->
                    case parseUserIds txts of
                      (Left e) -> error $ (show e)
                      (Right uids) -> Right (uid : uids)

             parseUserId :: Text.Text -> Either PageFormError UserId
             parseUserId txt =
                 case readMaybe (Text.unpack txt) of
                  Nothing  -> Left (PageParseError $ "can not parse as userid -> " <> txt)
                  (Just i) -> Right (UserId i)
         in         AccessList <$> (divControlGroup (label' "public" ++> (divControls $ inputCheckbox allowAny)))
                               <*> (divControlGroup (label' "users"     ++> (divControls $ inputTextCommaSeparated (sort $ map showUserId allowUserIds) `transformEither` parseUserIds)))
                               <*> (divControlGroup (label' "groups"    ++> (divControls $ inputTextCommaSeparated (sort $ allowUsergroups))))

      toPage :: (MonadIO m) =>
                (PageKind, ThemeStyleId, Text.Text, Text.Text, PreProcessor, Text.Text, AccessList, (Maybe Text.Text, Maybe Text.Text, Maybe PublishStatus))
             -> m (Either PageFormError (Page, AfterSaveAction))
      toPage (kind, style, ttl, slug, markup, bdy, al, (msave, mpreview, mpagestatus)) =
          do -- liftIO $ putStrLn $ "al = " ++ show al
             now <- liftIO $ getCurrentTime
             return $ Right $
               ( Page { pageId      = pageId page
                      , pageAuthor  = pageAuthor page
                      , pageTitle   = ttl
                      , pageSlug    = if Text.null slug then Nothing else Just (slugify slug)
                      , pageSrc     = Markup { preProcessors =
                                                   case markup of
                                                     Markdown -> [ Markdown ]
                                                     HsColour -> [ Markdown, HsColour ]
                                                     Pandoc   -> [ Pandoc ]
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
