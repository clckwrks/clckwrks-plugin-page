{-# LANGUAGE FlexibleContexts, OverloadedStrings #-}
{-# OPTIONS_GHC -F -pgmFtrhsx #-}
module Clckwrks.Page.PreProcess where

import Control.Monad.Trans (MonadIO(..))
import Control.Applicative ((<*>), (*>), (<$>), (<|>), optional, pure)
import Clckwrks.Monad (ClckT, ClckState, transform, query, segments)
import Clckwrks.Page.Acid (GetPageTitle(..), PageState)
import Clckwrks.Page.URL   (PageURL(ViewPageSlug))
import Clckwrks.Page.Types (PageId(..), slugify, toSlug)
import Data.Acid                        (AcidState(..))
import Data.Acid.Advanced               (query')
import Data.Attoparsec.Text.Lazy        (Parser, Result(..), anyChar, char, choice, decimal, parse, skipMany, space, stringCI, skipMany, try)
import Data.Attoparsec.Combinator (many1, manyTill, skipMany)
import Data.String (fromString)
import           Data.Text (Text, pack)
import qualified Data.Text.Lazy         as TL
import           Data.Text.Lazy.Builder (Builder)
import qualified Data.Text.Lazy.Builder as B
import HSP
import HSP.HTML (renderAsHTML)
import Web.Routes (showURL)

-- TODO: move to reusable module
parseAttr :: Text -> Parser ()
parseAttr name =
    do skipMany space
       stringCI name
       skipMany space
       char '='
       skipMany space

qchar :: Parser Char
qchar = (char '\\' *> anyChar) <|> anyChar

text :: Parser Text
text = pack <$> many1 qchar

qtext :: Parser Text
qtext = pack <$> (char '"' *> manyTill qchar (try $ char '"'))

data PageCmd
    = PageLink PageId (Maybe Text) Bool
    | PageTitle PageId
      deriving (Eq, Ord, Show)

parseCmd :: Parser PageCmd
parseCmd =
    do pid      <- parseAttr (fromString "id") *> (PageId <$> decimal)
       linkOnly <- skipMany space >> stringCI "title-only"
       return $ PageTitle pid
    <|>
    do pid      <- parseAttr (fromString "id") *> (PageId <$> decimal)
       mTitle   <- optional $ parseAttr (fromString "title") *> qtext
       linkOnly <- (skipMany space >> (stringCI "link-only")) *> pure True <|> pure False
       return $ PageLink pid mTitle linkOnly

pageCmd :: (Functor m, MonadIO m) =>
           AcidState PageState
        -> (PageURL -> [(Text, Maybe Text)] -> Text)
        -> TL.Text
        -> ClckT url m TL.Text
pageCmd pageAcid clckShowURL txt =
    case parse (segments "page" parseCmd) txt of
      (Fail _ _ e) -> return (TL.pack e)
      (Done _ segments) ->
          do b <- transform (applyCmd pageAcid clckShowURL) segments
             return $ B.toLazyText b

applyCmd pageAcid clckShowURL l@(PageTitle pid) =
    do mttl <- query' pageAcid (GetPageTitle pid)
       case mttl of
         Nothing -> return $ B.fromText "Untitled"
         (Just (ttl,_)) -> return $ B.fromText ttl

applyCmd pageAcid clckShowURL l@(PageLink pid mTitle linkOnly) =
    do (ttl, slug) <-
           case mTitle of
             (Just t) -> return (t, Just $ slugify t)
             Nothing  -> do mttl <- query' pageAcid (GetPageTitle pid)
                            case mttl of
                              Nothing -> return $ (pack "Untitled", Nothing)
                              (Just ttlSlug) -> return ttlSlug
       case linkOnly of
         False ->
             do html <- unXMLGenT $ <a href=(clckShowURL (ViewPageSlug pid (toSlug ttl slug)) [])><% ttl %></a>
                return $ B.fromString $ concat $ lines $ renderAsHTML html
         True ->
             return $ B.fromText $ clckShowURL (ViewPageSlug pid (toSlug ttl slug)) []



