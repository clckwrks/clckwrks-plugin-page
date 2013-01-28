{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -F -pgmFtrhsx #-}
module Clckwrks.Page.BlogPage where

import Clckwrks
import Clckwrks.Page.API
import Clckwrks.Page.Monad
import Clckwrks.Page.Types
import Clckwrks.Page.URL
import Control.Monad.State (get)

postsHTML :: XMLGenT PageM XML
postsHTML =
    do posts <- getPosts
       <ol class="blog-posts">
        <% mapM postHTML posts %>
        </ol>

postHTML :: Page -> XMLGenT PageM XML
postHTML Page{..} =
    <li class="blog-post">
     <h2><% pageTitle %></h2>
     <span class="pub-date"><% pageDate %></span>
     <% (markupToContent pageSrc) :: PageM Content %>
     <p><a href=(ViewPage pageId)>permalink</a></p>
    </li>

blog :: PageM Response
blog =
    do ttl <- getBlogTitle
       cs  <- get

       bdy <- unXMLGenT $
               <div id="blog-content">
                 <% postsHTML %>
               </div>

       clckT2PageT $ themeTemplate (plugins cs) ttl () bdy
