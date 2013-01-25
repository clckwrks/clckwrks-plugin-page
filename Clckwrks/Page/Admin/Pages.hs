{-# OPTIONS_GHC -F -pgmFtrhsx #-}
module Clckwrks.Page.Admin.Pages where

import Clckwrks.Monad          (query)
import Clckwrks.Admin.Template (template)
import Clckwrks.Page.Acid      (PagesSummary(..))
import Clckwrks.Page.Monad     (PageM)
import Clckwrks.Page.URL       (PageAdminURL(..), PageURL(..))
import Clckwrks.Page.Types     (PageId, Slug(..))
import Data.Text               (Text)
import Happstack.Server        (Response)
import HSP

pages :: PageM Response
pages =
    do pages <- query PagesSummary
       template "page list" () $ editList pages

editList ::  [(PageId, Text, Maybe Slug)] -> GenChildList PageM
editList [] = <%><p>There are currently no pages.</p></%>
editList pgs =
    <%>
     <p>Edit Page</p>
     <ul class="plain-list">
      <% mapM editPageLI pgs %>
     </ul>
    </%>
    where
      editPageLI :: (PageId, Text, Maybe Slug) -> GenXML PageM
      editPageLI (pid, ttl, _slug) =
          <li><a href=(PageAdmin $ EditPage pid)><% ttl %></a></li>
