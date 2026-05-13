module FileThunder.Content (
    indexHtml,
    ContentInfo (..),
) where

import Data.Text
import Lucid
import Network.Wai (Request (pathInfo))

data Page = Page {title :: Text, content :: Html ()}

baseHtml :: Page -> Html ()
baseHtml page =
    doctypehtml_ $ do
        head_
            ( do
                meta_ [charset_ "utf-8"]
                meta_ [name_ "viewport", content_ "width=device-width, initial-scale=1"]
                title_ $ toHtml $ title page
            )
        body_ $ content page

data ContentInfo = ContentInfo {directory :: Bool, path :: FilePath}

indexHtml :: Request -> [ContentInfo] -> Html ()
indexHtml req files =
    let a = intercalate "/" (pathInfo req)
        b = if isSuffixOf "/" a then a else a <> "/"
        uri = if isPrefixOf "/" b then b else "/" <> b
     in baseHtml
            Page
                { title = uri
                , content = do
                    h1_ $ toHtml $ "Index of " <> uri
                    listHtml "" uri files
                }

listHtml :: Html () -> Text -> [ContentInfo] -> Html ()
listHtml acc context files = case files of
    v : t -> listHtml ((a_ [href_ $ context <> (pack $ path v)] $ toHtml $ path v) <> acc) context t
    [] -> acc
