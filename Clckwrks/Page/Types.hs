{-# LANGUAGE DeriveDataTypeable, FlexibleInstances, TemplateHaskell, TypeFamilies, OverloadedStrings #-}
module Clckwrks.Page.Types where

import Clckwrks                 (UserId(..))
import Clckwrks.Markup.HsColour (hscolour)
import Clckwrks.Markup.Markdown (markdown)
import Clckwrks.Markup.Pandoc   (pandoc)
import Clckwrks.Monad           (ThemeStyleId(..))
import Clckwrks.Types           (Trust(..))
import Control.Applicative      ((<$>), optional)
import Control.Monad.Trans      (MonadIO(liftIO))
import Data.Aeson               (ToJSON(..), FromJSON(..))
import Data.Char                (ord, toLower, isAlphaNum)
import Data.Data                (Data, Typeable)
import Data.Maybe               (fromMaybe)
import Data.IxSet               (Indexable(..), IxSet, ixFun, ixSet)
import Data.SafeCopy            (Migrate(..), base, deriveSafeCopy, extension)
import Data.String              (IsString, fromString)
import Data.Text                (Text)
import qualified Data.Text      as Text
import Data.Time                (UTCTime)
import Data.Time.Clock.POSIX    (posixSecondsToUTCTime)
import Data.UUID                (UUID)
import Data.UUID.V5             (generateNamed, namespaceOID)
import Web.Routes               (PathInfo(..), anySegment)
import System.Random            (randomIO)


-- $(deriveSafeCopy 0 'base ''UUID)

instance PathInfo PageId where
    toPathSegments (PageId i) = toPathSegments i
    fromPathSegments = PageId <$> fromPathSegments

newtype PageId = PageId { unPageId :: Integer }
    deriving (Eq, Ord, Show, Read, Data, Typeable)
$(deriveSafeCopy 1 'base ''PageId)

instance ToJSON PageId where
    toJSON (PageId i) = toJSON i
instance FromJSON PageId where
    parseJSON n = PageId <$> parseJSON n

data PreProcessor_1
    = HsColour_1
    | Markdown_1
      deriving (Eq, Ord, Read, Show, Data, Typeable)
$(deriveSafeCopy 1 'base ''PreProcessor_1)

data PreProcessor
    = HsColour
    | Markdown
    | Pandoc
      deriving (Eq, Ord, Read, Show, Data, Typeable)
$(deriveSafeCopy 2 'extension ''PreProcessor)

instance Migrate PreProcessor where
    type MigrateFrom PreProcessor = PreProcessor_1
    migrate HsColour_1 = HsColour
    migrate Markdown_1 = Markdown

runPreProcessors :: (MonadIO m) => [PreProcessor] -> Trust -> Text -> m (Either Text Text)
runPreProcessors [] _ txt = return (Right txt)
runPreProcessors (p:ps) trust txt =
    do e <- runPreProcessor p trust txt
       case e of
         (Left e) -> return (Left e)
         (Right txt') -> runPreProcessors ps trust txt'

runPreProcessor :: (MonadIO m) => PreProcessor -> Trust -> Text -> m (Either Text Text)
runPreProcessor pproc trust txt =
    do let f = case pproc of
                 Markdown -> markdown Nothing trust
                 HsColour -> hscolour Nothing
                 Pandoc   -> pandoc Nothing trust
       f txt

data Markup_001
    = Markup_001 { preProcessors_001 :: [PreProcessor]
                 , markup_001 :: Text
                 }
      deriving (Eq, Ord, Read, Show, Data, Typeable)
$(deriveSafeCopy 1 'base ''Markup_001)

data Markup
    = Markup { preProcessors :: [PreProcessor]
             , markup        :: Text
             , trust         :: Trust
             }
      deriving (Eq, Ord, Read, Show, Data, Typeable)
$(deriveSafeCopy 2 'extension ''Markup)

instance Migrate Markup where
    type MigrateFrom Markup = Markup_001
    migrate (Markup_001 pp mu) = Markup pp mu Trusted

data PublishStatus
    = Draft
    | Revoked
    | Published
    | Scheduled
      deriving (Eq, Ord, Read, Show, Data, Typeable)
$(deriveSafeCopy 1 'base ''PublishStatus)

publishStatusString :: PublishStatus -> String
publishStatusString Draft     = "draft"
publishStatusString Revoked   = "revoked"
publishStatusString Published = "published"
publishStatusString Scheduled = "scheduled"


data PageKind
    = PlainPage
    | Post
      deriving (Eq, Ord, Read, Show, Data, Typeable)
$(deriveSafeCopy 1 'base ''PageKind)

data Page_001
    = Page_001 { pageId_001        :: PageId
               , pageTitle_001     :: Text
               , pageSrc_001       :: Markup
               , pageExcerpt_001   :: Maybe Markup
               , pageDate_001      :: Maybe UTCTime
               , pageStatus_001    :: PublishStatus
               , pageKind_001      :: PageKind
               }
      deriving (Eq, Ord, Read, Show, Data, Typeable)
$(deriveSafeCopy 1 'base ''Page_001)

data Page_002
    = Page_002 { pageId_002        :: PageId
               , pageAuthor_002    :: UserId
               , pageTitle_002     :: Text
               , pageSrc_002       :: Markup
               , pageExcerpt_002   :: Maybe Markup
               , pageDate_002      :: UTCTime
               , pageUpdated_002   :: UTCTime
               , pageStatus_002    :: PublishStatus
               , pageKind_002      :: PageKind
               , pageUUID_002      :: UUID
               }
      deriving (Eq, Ord, Read, Show, Data, Typeable)
$(deriveSafeCopy 2 'extension ''Page_002)

instance Migrate Page_002 where
    type MigrateFrom Page_002 = Page_001
    migrate (Page_001 pi pt ps pe pd pst pk) =
        (Page_002 pi (UserId 1) pt ps pe (fromMaybe epoch pd) (fromMaybe epoch pd) pst pk $ generateNamed namespaceOID (map (fromIntegral . ord) (show pi ++ show ps)))
            where
              epoch = posixSecondsToUTCTime 0

newtype Slug = Slug { unSlug :: Text }
    deriving (Eq, Ord, Data, Typeable, Read, Show)
$(deriveSafeCopy 0 'base ''Slug)

instance PathInfo Slug where
    toPathSegments (Slug txt) = [txt]
    fromPathSegments = Slug <$> anySegment

-- NOTE: this instance will cause faulty behavior if the Maybe Slug is not at the end of the URL
instance PathInfo (Maybe Slug) where
    toPathSegments (Just slug) = toPathSegments slug
    fromPathSegments = optional $ fromPathSegments

slugify :: Text -> Slug
slugify txt = Slug $ Text.dropWhileEnd (=='-') $ Text.map (\c -> if isAlphaNum c then (toLower c) else '-') txt

toSlug :: Text -> Maybe Slug -> Slug
toSlug txt slug = fromMaybe (slugify txt) slug



data Page_3 = Page_3
    { pageId_3        :: PageId
    , pageAuthor_3    :: UserId
    , pageTitle_3     :: Text
    , pageSlug_3      :: Maybe Slug
    , pageSrc_3       :: Markup
    , pageExcerpt_3   :: Maybe Markup
    , pageDate_3      :: UTCTime
    , pageUpdated_3   :: UTCTime
    , pageStatus_3    :: PublishStatus
    , pageKind_3      :: PageKind
    , pageUUID_3      :: UUID
    }
    deriving (Eq, Ord, Read, Show, Data, Typeable)
$(deriveSafeCopy 3 'extension ''Page_3)

instance Migrate Page_3 where
    type MigrateFrom Page_3 = Page_002
    migrate (Page_002 pi pa pt ps pe pd pu pst pk puu) =
        (Page_3 pi pa pt Nothing ps pe pd pu pst pk puu)

data Page
    = Page { pageId           :: PageId
           , pageAuthor       :: UserId
           , pageTitle        :: Text
           , pageSlug         :: Maybe Slug
           , pageSrc          :: Markup
           , pageExcerpt      :: Maybe Markup
           , pageDate         :: UTCTime
           , pageUpdated      :: UTCTime
           , pageStatus       :: PublishStatus
           , pageKind         :: PageKind
           , pageUUID         :: UUID
           , pageThemeStyleId :: ThemeStyleId
           }
      deriving (Eq, Ord, Read, Show, Data, Typeable)
$(deriveSafeCopy 4 'extension ''Page)

-- migration added for clckwrks-plugin-page-0.3.0 on 2013-12-26
instance Migrate Page where
    type MigrateFrom Page = Page_3
    migrate (Page_3 pi pa pt psl ps pe pd pu pst pk puu) =
        (Page pi pa pt psl ps pe pd pu pst pk puu (ThemeStyleId 0))

instance Indexable Page where
    empty = ixSet [ ixFun ((:[]) . pageId)
                  , ixFun ((:[]) . pageDate)
                  , ixFun ((:[]) . pageKind)
                  , ixFun ((:[]) . pageDate)
                  , ixFun ((:[]) . pageStatus)
                  ]

type Pages = IxSet Page

data FeedConfig = FeedConfig
    { feedUUID       :: UUID -- ^ UUID which identifies this feed. Should probably never change
--    , feedCategory :: Set Text
    , feedTitle      :: Text
    , feedLink       :: Text
    , feedAuthorName :: Text
    }
    deriving (Eq, Ord, Read, Show, Data, Typeable)
$(deriveSafeCopy 0 'base ''FeedConfig)

initialFeedConfig :: IO FeedConfig
initialFeedConfig =
    do uuid <- randomIO
       return $ FeedConfig { feedUUID       = uuid
                           , feedTitle      = fromString "Untitled Feed"
                           , feedLink       = fromString ""
                           , feedAuthorName = fromString "Anonymous"
                           }
