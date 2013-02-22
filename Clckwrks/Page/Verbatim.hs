{-# LANGUAGE TemplateHaskell #-}
module Clckwrks.Page.Verbatim
    ( verbatimText
    ) where

import qualified Data.Text as Text
import Language.Haskell.TH.Quote

verbatimText :: QuasiQuoter
verbatimText = QuasiQuoter
    { quoteExp  = \s -> [| Text.pack s |]
    , quotePat  = error "verbatim-text: quotePat not supported."
    , quoteType = error "verbatim-text: quotePat not supported."
    , quoteDec  = error "verbatim-text: quotePat not supported."
    }

