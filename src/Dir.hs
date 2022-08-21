module Dir (createDirListing) where

import System.Directory
import Response
import Config

-- XXX is_head?  content_type?  last_modified?
-- XXX move elsewhere!
-- perhaps it would be best to return a ResponseBody here and
-- let the upper layers handle the rest?
createDirListing :: String -> String -> IO (ResponseBody,String)
createDirListing urlpath dir = do
     fs <- getDirectoryContents dir
     let l = htmlFiles [f | f <- fs, (head f) /= '.']
         title = "Directory " ++ urlpath -- XXX html quoted!
         parent = "<a href=\"..\">Parent Directory</a>\n"
         s = sec title (parent ++ l)
         p = page title s
     return (HereItIs p, contentTypeHeader "text/html")

-- XXX this should probably be elsewhere
htmlFiles fs = unlines (["<ul>"] ++ hs ++ ["</ul>"]) where
    hs = map htmlFile fs
    htmlFile f = "    <li><a href=\"" ++ f ++ "\">" ++ f' ++ "</a></li>" where
        f' = f -- XXX html quoted

page title body = "<html>\n<head><title>" ++ title ++ "</title></head>\n<body>\n" ++ body ++ "</body>\n</html>\n"

sec title body = "<h1>" ++ title ++ "</h1>\n" ++ body


