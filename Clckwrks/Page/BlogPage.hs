{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
{-# OPTIONS_GHC -F -pgmFhsx2hs #-}
module Clckwrks.Page.BlogPage where

import Clckwrks
import Clckwrks.Page.API
import Clckwrks.Page.Monad
import Clckwrks.Page.Types
import Clckwrks.Page.URL
import Control.Monad.State (get)
import HSP.XML (XML, fromStringLit)
import HSP.XMLGenerator
import Data.Text.Lazy (Text)

postsHTML :: XMLGenT PageM XML
postsHTML =
    do posts <- getPosts
       <ol class="blog-posts">
        <% mapM postHTML posts %>
        </ol>

postHTML :: Page -> XMLGenT PageM XML
postHTML Page{..} =
    <li class="blog-post">
     <h2><a href=(ViewPage pageId)><% pageTitle %></a></h2>
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

       clckT2PageT $ themeTemplate (plugins cs) (ThemeStyleId 0) ttl () bdy
