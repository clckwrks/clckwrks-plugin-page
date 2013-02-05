{-# LANGUAGE RecordWildCards, FlexibleContexts, OverloadedStrings #-}
module Clckwrks.Page.Plugin where

import Clckwrks                   ( ClckwrksConfig(clckTopDir), ClckState(plugins), ClckT(..), ClckURL, ClckPlugins, Theme
                                  , Role(..), ClckPluginsSt, addAdminMenu, addMenuCallback, addPreProc, query, update
                                  )
import Clckwrks.Acid              (GetUACCT(..), SetUACCT(..))
import Clckwrks.Plugin            (clckPlugin)
import Clckwrks.Page.Acid         (PageState, GetOldUACCT(..), ClearOldUACCT(..), initialPageState)
import Clckwrks.Page.MenuCallback (menuCallback)
import Clckwrks.Page.Monad        (PageConfig(..), runPageT)
import Clckwrks.Page.PreProcess   (pageCmd)
import Clckwrks.Page.Route        (routePage)
import Clckwrks.Page.URL          (PageURL(..), PageAdminURL(..))
import Clckwrks.Page.Types        (PageId(..))
import Control.Applicative        ((<$>))
import Control.Monad.State        (get)
import Data.Acid                  (AcidState)
import Data.Acid.Advanced         (update', query')
import Data.Acid.Local            (createCheckpointAndClose, openLocalStateFrom,)
import Data.Text                  (Text)
import qualified Data.Text.Lazy   as TL
import Data.Maybe                 (fromMaybe)
import Data.Set                   (Set)
import qualified Data.Set         as Set
import Happstack.Server           (ServerPartT, Response, notFound, toResponse)
import System.Directory           (createDirectoryIfMissing)
import System.FilePath            ((</>))
import Web.Routes                 (toPathInfo, parseSegments, withRouteT, fromPathSegments)
import Web.Plugins.Core           (Plugin(..), Plugins(..), When(..), addCleanup, addHandler, initPlugin, getConfig, getPluginRouteFn)

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
           pageDir  = maybe "_page" (\td -> td </> "_page") mTopDir
           cacheDir = pageDir </> "_cache"
       createDirectoryIfMissing True cacheDir

       ips  <- initialPageState
       acid <- openLocalStateFrom (basePath </> "page") ips
       addCleanup plugins Always (createCheckpointAndClose acid)

       let pageConfig = PageConfig { pageState     = acid
                                   , pageClckURL   = clckShowFn
                                   }

       addPreProc plugins (pageCmd acid pageShowFn)
       addMenuCallback plugins (menuCallback acid pageShowFn)
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
                    , [ (Set.fromList [Administrator, Editor], "New Page/Post"   , newPageURL)
                      , (Set.fromList [Administrator, Editor], "Edit Page/Post"  , pagesURL)
                      , (Set.fromList [Administrator, Editor], "Edit Feed Config", feedConfigURL)
                      ]
                    )

migrateUACCT :: AcidState PageState -> ClckT url IO ()
migrateUACCT acidPageState =
    do mOldUACCT <- query' acidPageState GetOldUACCT
       case mOldUACCT of
         Nothing -> return ()
         (Just uacct) ->
             do mNewUACCT <- query GetUACCT
                case mNewUACCT of
                  Nothing -> update (SetUACCT $ Just uacct)
                  (Just _) -> update' acidPageState ClearOldUACCT

pagePlugin :: Plugin PageURL Theme (ClckT ClckURL (ServerPartT IO) Response) (ClckT ClckURL IO ()) ClckwrksConfig ClckPluginsSt
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

