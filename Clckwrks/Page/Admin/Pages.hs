{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -F -pgmFhsx2hs #-}
module Clckwrks.Page.Admin.Pages where

import Clckwrks                (UserId)
import Clckwrks.Monad          (query)
import Clckwrks.Admin.Template (template)
import Clckwrks.Page.Acid      (PagesSummary(..))
import Clckwrks.Page.Monad     (PageM)
import Clckwrks.Page.URL       (PageAdminURL(..), PageURL(..))
import Clckwrks.Page.Types     (PageId(..), PublishStatus(..), Slug(..), publishStatusString)
import qualified Data.Text     as T
import Data.Text.Lazy          (Text)
import Data.Time               (UTCTime)
import Happstack.Server        (Response)
import HSP.XML
import HSP.XMLGenerator

pages :: PageM Response
pages =
    do pages <- query PagesSummary
       template "Page List" () $ editList pages

editList :: [(PageId, T.Text, Maybe Slug, UTCTime, UserId, PublishStatus)]
         -> GenChildList PageM
editList [] = <%><p>There are currently no pages.</p></%>
editList pgs =
    <%>
     <table class="table table-condensed">
      <thead>
       <tr>
        <th>Page Id</th>
        <th>Title</th>
        <th>Last Updated</th>
        <th>Status</th>
       </tr>
      </thead>
      <tbody>
       <% mapM editPageTR pgs %>
      </tbody>
     </table>
    </%>
    where
      editPageTR :: (PageId, T.Text, Maybe Slug, UTCTime, UserId, PublishStatus) -> GenXML PageM
      editPageTR (pid, ttl, _slug, updated, userId, published) =
          <tr>
           <td><% show $ unPageId pid %></td>
           <td><a href=(PageAdmin $ EditPage pid)><% ttl %></a></td>
           <td><% updated %></td>
           <td><% publishStatusString published %></td>
          </tr>
