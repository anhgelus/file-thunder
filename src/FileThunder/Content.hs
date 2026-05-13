module FileThunder.Content (
    indexHtml,
    ContentInfo (..),
) where

import Data.Text
import Lucid

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

indexHtml :: Text -> [ContentInfo] -> Html ()
indexHtml uri files =
    baseHtml
        Page
            { title = uri
            , content = do
                h1_ $ toHtml $ "Index of " <> uri
                listHtml files ""
            }

listHtml :: [ContentInfo] -> Html () -> Html ()
listHtml files acc = case files of
    v : t -> listHtml t ((p_ $ toHtml $ path v) <> acc)
    [] -> acc
