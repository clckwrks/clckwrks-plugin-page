{-# LANGUAGE FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, RecordWildCards, TypeFamilies, TypeSynonymInstances #-}
module Clckwrks.Page.Monad where

import Control.Applicative           ((<$>))
import Control.Monad                 (foldM)
import Control.Monad.Reader          (MonadReader(ask,local), ReaderT(runReaderT))
import Control.Monad.State           (StateT, put, get, modify)
import Control.Monad.Trans           (MonadIO(liftIO))
import qualified Data.Text.Lazy      as LT
import Clckwrks.Acid                 (GetAcidState(..))
import Clckwrks.Monad                (Content(..), ClckT(..), ClckFormT, ClckState(..), ClckPluginsSt(..), mapClckT, runClckT, withRouteClckT, getPreProcessors)
import Clckwrks.URL                  (ClckURL)
import Clckwrks.Page.Acid            (PageState(..))
import Clckwrks.Page.Types           (Markup(..), runPreProcessors)
import Clckwrks.Page.URL             (PageURL(..), PageAdminURL(..))
import Clckwrks.Page.Types           (PageId(..))
import Clckwrks.Plugin               (clckPlugin)
import Control.Monad.Trans           (lift)
import Data.Acid                     (AcidState)
import Data.Data                     (Typeable)
import qualified Data.Text           as T
import qualified Data.Text.Lazy      as TL
import Happstack.Server              (Happstack, Input, ServerPartT)
import HSP.XMLGenerator
import HSP.XML
import Text.Reform                   (CommonFormError, FormError(..))
import Web.Plugins.Core              (Plugin(..), getConfig, getPluginsSt, getPluginRouteFn)
import Web.Routes                    (RouteT(..), showURL, withRouteT)

data PageConfig = PageConfig
    { pageState        :: AcidState PageState
    , pageClckURL      :: ClckURL -> [(T.Text, Maybe T.Text)] -> T.Text
    }

type PageT m = ClckT PageURL (ReaderT PageConfig m)
type PageT' url m = ClckT url (ReaderT PageConfig m)
type PageM   = ClckT PageURL (ReaderT PageConfig (ServerPartT IO))
type PageAdminM = ClckT PageAdminURL (ReaderT PageConfig (ServerPartT IO))


runPageT :: PageConfig -> PageT m a -> ClckT PageURL m a
runPageT mc m = mapClckT f m
    where
      f r = runReaderT r mc

runPageT'' :: Monad m =>
               (PageURL -> [(T.Text, Maybe T.Text)] -> T.Text)
            -> PageConfig
            -> PageT m a
            -> ClckT url m a
runPageT'' showPageURL stripeConfig m = ClckT $ withRouteT flattenURL $ unClckT $ runPageT stripeConfig $ m
    where
      flattenURL ::   ((url' -> [(T.Text, Maybe T.Text)] -> T.Text) -> (PageURL -> [(T.Text, Maybe T.Text)] -> T.Text))
      flattenURL _ u p = showPageURL u p


-- withRouteClckT ?
flattenURLClckT :: (url1 -> [(T.Text, Maybe T.Text)] -> T.Text)
                -> ClckT url1 m a
                -> ClckT url2 m a
flattenURLClckT showClckURL m = ClckT $ withRouteT flattenURL $ unClckT m
    where
      flattenURL _ = \u p -> showClckURL u p

clckT2PageT :: (Functor m, MonadIO m, Typeable url1) =>
             ClckT url1 m a
          -> PageT m a
clckT2PageT m =
    do p <- plugins <$> get
       (Just clckShowFn) <- getPluginRouteFn p (pluginName clckPlugin)
       flattenURLClckT clckShowFn $ mapClckT addReaderT m
    where
      addReaderT :: (Monad m) => m (a, ClckState) -> ReaderT PageConfig m (a, ClckState)
      addReaderT m =
          do (a, cs) <- lift m
             return (a, cs)

data PageFormError
    = PageCFE (CommonFormError [Input])
      deriving Show

instance FormError PageFormError where
    type ErrorInputType PageFormError = [Input]
    commonFormError = PageCFE

instance (Functor m, Monad m) => EmbedAsChild (PageT m) PageFormError where
    asChild e = asChild (show e)

type PageForm = ClckFormT PageFormError PageM

instance (Monad m) => MonadReader PageConfig (PageT' url m) where
    ask = ClckT $ ask
    local f (ClckT m) = ClckT $ local f m

instance (Functor m, Monad m) => GetAcidState (PageT' url m) PageState where
    getAcidState =
        pageState <$> ask

instance (IsName n TL.Text) => EmbedAsAttr PageM (Attr n PageURL) where
        asAttr (n := u) =
            do url <- showURL u
               asAttr $ MkAttr (toName n, pAttrVal (TL.fromStrict url))

instance (IsName n TL.Text) => EmbedAsAttr PageM (Attr n ClckURL) where
        asAttr (n := url) =
            do showFn <- pageClckURL <$> ask
               asAttr $ MkAttr (toName n, pAttrVal (TL.fromStrict $ showFn url []))


-- | convert 'Markup' to 'Content' that can be embedded. Generally by running the pre-processors needed.
-- markupToContent :: (Functor m, MonadIO m, Happstack m) => Markup -> ClckT url m Content
markupToContent :: (Functor m, MonadIO m, Happstack m) =>
                   Markup
                -> ClckT url m Content
markupToContent Markup{..} =
    do clckState <- get
       transformers <- getPreProcessors (plugins clckState)
       (Just clckRouteFn) <- getPluginRouteFn (plugins clckState) (pluginName clckPlugin)
       (markup', clckState') <- liftIO $ runClckT clckRouteFn clckState (foldM (\txt pp -> pp txt) (TL.fromStrict markup) transformers)
       put clckState'
       e <- liftIO $ runPreProcessors preProcessors trust (TL.toStrict markup')
       case e of
         (Left err)   -> return (PlainText err)
         (Right html) -> return (TrustedHtml html)

{-
-- | update the 'currentPage' field of 'ClckState'
setCurrentPage :: (MonadIO m) => PageId -> PageT m ()
setCurrentPage pid =
    modify $ \s -> s { pageCurrent = pid }
-}