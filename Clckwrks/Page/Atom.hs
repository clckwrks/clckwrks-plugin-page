{-# LANGUAGE RecordWildCards, OverloadedStrings, QuasiQuotes #-}
module Clckwrks.Page.Atom where

import Control.Monad.Trans     (liftIO)
import Clckwrks.Monad          (Clck, Content(..), query, withAbs)
import Clckwrks.Page.Acid
import Clckwrks.Page.Monad     (PageM, markupToContent)
import Clckwrks.Page.Types
import Clckwrks.ProfileData.Acid
import Clckwrks.Page.URL
import Data.Maybe              (fromMaybe)
import Data.Monoid             ((<>))
import Data.String             (fromString)
import qualified Data.Text     as Text
import Data.Text.Lazy          (Text)
import qualified Data.Text.Lazy.Encoding as TL
import Data.Time               (UTCTime)
import Data.Time.Clock.POSIX   (posixSecondsToUTCTime)
import Data.Time.Format        (formatTime)
import Data.UUID               (toString)
import Happstack.Server        (Happstack, Response, ok, toResponseBS)
import HSP.XMLGenerator
import HSP.XML                 (XML, cdata, renderXML, fromStringLit)
import Language.Haskell.HSX.QQ (hsx)
import Data.Time.Locale.Compat (defaultTimeLocale)
import Web.Routes              (showURL)

atom :: FeedConfig  -- ^ feed configuration
     -> [Page]      -- ^ pages to publish in feed
     -> PageM XML
atom FeedConfig{..} pages =
    do blogURL <- withAbs $ showURL Blog
       atomURL <- withAbs $ showURL AtomFeed
       unXMLGenT $ [hsx|
                   <feed xmlns="http://www.w3.org/2005/Atom">
                    <title><% feedTitle %></title>
                    <link type="text/html" href=blogURL />
                    <link rel="self" type="application/atom+xml" href=atomURL />
                    <author>
                     <name><% feedAuthorName %></name>
                    </author>
                    <updated><% atomDate $ mostRecentUpdate pages %></updated>
                    <id><% "urn:uuid:" ++ toString feedUUID %></id>
                    <% mapM entry pages %>
                   </feed> |]

mostRecentUpdate :: [Page]  -- ^ pages to consider
                 -> UTCTime -- ^ most recent updated time
mostRecentUpdate []    = posixSecondsToUTCTime 0
mostRecentUpdate pages =
    maximum $ map pageUpdated pages

entry :: Page
      -> PageM XML
entry Page{..} =
  do viewPageSlug <- withAbs $ showURL (ViewPageSlug pageId (toSlug pageTitle pageSlug))
     unXMLGenT $ [hsx| <entry>
                   <title><% pageTitle %></title>
                   <link href=viewPageSlug />
                   <id><% "urn:uuid:" ++ toString pageUUID %></id>
                   <% author %>
                   <updated><% atomDate pageUpdated %></updated>
                   <% atomContent pageSrc %>
                 </entry> |]
    where
      author :: XMLGenT PageM XML
      author =
          do mu <- query $ UsernameForId pageAuthor
             case mu of
               Nothing -> return $ cdata ""
               (Just n)
                   | Text.null n ->
                       return $ cdata ""
                   | otherwise -> [hsx|
                       <author>
                        <name><% n %></name>
                       </author> |]

atomDate :: UTCTime -> String
atomDate time =
    formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ" time

atomContent :: Markup -> PageM XML
atomContent markup =
    do c <- markupToContent markup
       case c of
         (PlainText txt) ->
              unXMLGenT $ [hsx| <content type="text"><% txt %></content> |]
         (TrustedHtml html) ->
              unXMLGenT $ [hsx| <content type="html"><% html %></content> |]

handleAtomFeed :: PageM Response
handleAtomFeed =
    do ps         <- query AllPosts
       feedConfig <- query GetFeedConfig
       xml <- atom feedConfig ps
       ok $ toResponseBS "application/atom+xml;charset=utf-8" ((TL.encodeUtf8 $ "<?xml version=\"1.0\" encoding=\"utf-8\"?>\n") <> (TL.encodeUtf8 $ renderXML xml))
