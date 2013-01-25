{-# LANGUAGE RecordWildCards, FlexibleContexts, OverloadedStrings #-}
module Clckwrks.Page.Plugin where

import Clckwrks                  ( ClckwrksConfig(clckTopDir), ClckState(plugins), ClckT(..), ClckURL, ClckPlugins, Theme
                                 , addAdminMenu, addPreProc                                 )
import Clckwrks.Plugin           (clckPlugin)
import Clckwrks.Page.Acid        (initialPageState)
import Clckwrks.Page.Monad       (PageConfig(..), runPageT)
import Clckwrks.Page.PreProcess  (pageCmd)
import Clckwrks.Page.Route       (routePage)
import Clckwrks.Page.URL         (PageURL(..), PageAdminURL(..))
import Clckwrks.Page.Types       (PageId(..))
import Control.Applicative       ((<$>))
import Control.Monad.State       (get)
import Data.Acid                 as Acid
import Data.Acid.Local           (createCheckpointAndClose, openLocalStateFrom)
import Data.Text                 (Text)
import qualified Data.Text.Lazy  as TL
import Data.Maybe                (fromMaybe)
import Data.Set                  (Set)
import Happstack.Server          (ServerPartT, Response, notFound, toResponse)
import System.Directory          (createDirectoryIfMissing)
import System.FilePath           ((</>))
import Web.Routes                (toPathInfo, parseSegments, withRouteT, fromPathSegments)
import Web.Plugins.Core          (Plugin(..), Plugins(..), When(..), addCleanup, addHandler, initPlugin, getConfig, getPluginRouteFn)

pageHandler :: (PageURL -> [(Text, Maybe Text)] -> Text)
              -> PageConfig
              -> ClckPlugins
              -> [Text]
              -> ClckT ClckURL (ServerPartT IO) Response
pageHandler showPageURL pageConfig plugins paths =
    case parseSegments fromPathSegments paths of
      (Left e)  -> notFound $ toResponse (show e)
      (Right u) ->
          ClckT $ withRouteT flattenURL $ unClckT $ runPageT pageConfig $ routePage u
    where
      flattenURL ::   ((url' -> [(Text, Maybe Text)] -> Text) -> (PageURL -> [(Text, Maybe Text)] -> Text))
      flattenURL _ u p = showPageURL u p

pageInit :: ClckPlugins
           -> IO (Maybe Text)
pageInit plugins =
    do (Just pageShowFn) <- getPluginRouteFn plugins (pluginName pagePlugin)
       (Just clckShowFn) <- getPluginRouteFn plugins (pluginName clckPlugin)
       mTopDir <- clckTopDir <$> getConfig plugins
       let basePath = maybe "_state" (\td -> td </> "_state") mTopDir -- FIXME
           pageDir = maybe "_page" (\td -> td </> "_page") mTopDir
           cacheDir = pageDir </> "_cache"
       createDirectoryIfMissing True cacheDir

       ips  <- initialPageState
       acid <- openLocalStateFrom (basePath </> "page") ips
       addCleanup plugins Always (createCheckpointAndClose acid)

       let pageConfig = PageConfig { pageState     = acid
                                   , pageClckURL   = clckShowFn
                                   }

       addPreProc plugins (pageCmd acid pageShowFn)
       addHandler plugins (pluginName pagePlugin) (pageHandler pageShowFn pageConfig)

       return Nothing

addPageAdminMenu :: ClckT url IO ()
addPageAdminMenu =
    do p <- plugins <$> get
       (Just pageShowURL) <- getPluginRouteFn p (pluginName pagePlugin)
       let newPageURL    = pageShowURL (PageAdmin NewPage) []
           pagesURL      = pageShowURL (PageAdmin Pages) []
           feedConfigURL = pageShowURL (PageAdmin EditFeedConfig) []
       addAdminMenu ("Pages/Posts"
                    , [ ("New Page/Post"        , newPageURL)
                      , ("Edit Page/Post"  , pagesURL)
                      , ("Edit Feed Config", feedConfigURL)
                      ]
                    )

pagePlugin :: Plugin PageURL Theme (ClckT ClckURL (ServerPartT IO) Response) (ClckT ClckURL IO ()) ClckwrksConfig [TL.Text -> ClckT ClckURL IO TL.Text]
pagePlugin = Plugin
    { pluginName       = "page"
    , pluginInit       = pageInit
    , pluginDepends    = []
    , pluginToPathInfo = toPathInfo
    , pluginPostHook   = addPageAdminMenu
    }

plugin :: ClckPlugins -- ^ plugins
       -> Text        -- ^ baseURI
       -> IO (Maybe Text)
plugin plugins baseURI =
    initPlugin plugins baseURI pagePlugin

