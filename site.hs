--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Data.Monoid            ((<>))
import           Hakyll
import qualified Text.Highlighting.Kate as K

--------------------------------------------------------------------------------
host :: String
host = "https://yoshitsugu.net"


main :: IO ()
main = hakyll $ do
    match "images/*" $ do
        route   idRoute
        compile copyFileCompiler

    match (fromList ["google9af023e4743ca32a.html", "robots.txt"]) $ do
        route   idRoute
        compile copyFileCompiler

    match "css/*.scss" $ do
        route $ setExtension "css"
        compile $ getResourceString
          >>= withItemBody (unixFilter "sass" ["-s", "--scss"])
          >>= return . fmap compressCss

    match "css/*.css" $ do
        route   idRoute
        compile compressCssCompiler

    match "js/*.js" $ do
        route   idRoute
        compile copyFileCompiler

    create ["css/highlight.css"] $ do
      route   idRoute
      compile $ makeItem (compressCss $ K.styleToCss K.haddock)

    match (fromList ["about.md"]) $ do
        route   $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/default.html" defaultContext
            >>= relativizeUrls

    tags <- buildTags "posts/*" (fromCapture "tags/*.html")


    tagsRules tags $ \tag patt -> do
        route   idRoute
        compile $ do
            posts <- recentFirst =<< loadAll patt
            let tagCtx = constField "title" ("Posts tagged " ++ tag)
                      <> listField "posts" (postCtxWithTags tags) (return posts)
                      <> defaultContext

            makeItem ""
                >>= loadAndApplyTemplate "templates/tag.html" tagCtx
                >>= loadAndApplyTemplate "templates/default.html" tagCtx
                >>= relativizeUrls


    match "posts/*" $ do
        route $ setExtension "html"
        compile $ pandocCompiler
            >>= saveSnapshot "content"
            >>= loadAndApplyTemplate "templates/post.html"    (postCtxWithTags tags)
            >>= loadAndApplyTemplate "templates/default.html" (postCtxWithTags tags)
            >>= relativizeUrls

    create ["archive.html"] $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "posts/*"
            let archiveCtx =
                    listField "posts" postCtx (return posts) `mappend`
                    constField "title" "Archives"            `mappend`
                    defaultContext

            makeItem ""
                >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
                >>= loadAndApplyTemplate "templates/default.html" archiveCtx
                >>= relativizeUrls

    match "index.html" $ do
        route   idRoute
        compile $ do
            posts <- recentFirst =<< loadAllSnapshots "posts/*" "content"
            let teaserCtx = teaserField "teaser" "content" `mappend` postCtx
                indexCtx =
                    listField "posts" teaserCtx (return $ take 5 posts) `mappend`
                    defaultContext

            getResourceBody
                >>= applyAsTemplate indexCtx
                >>= loadAndApplyTemplate "templates/default.html" indexCtx
                >>= relativizeUrls

    create ["sitemap.xml"] $ do
         route   idRoute
         compile $ do
           posts <- recentFirst =<< loadAll "posts/*"
           pages <- loadAll $ fromList ["index.html", "about.md"]
           let allPosts = (return (pages ++ posts))
           let sitemapCtx = mconcat
                            [ listField "entries" pageCtx allPosts
                            , constField "host" host
                            , defaultContext
                            ]
           makeItem ""
            >>= loadAndApplyTemplate "templates/sitemap.xml" sitemapCtx
            >>= cleanIndexHtmls

    match "templates/*" $ compile templateCompiler


--------------------------------------------------------------------------------
postCtx :: Context String
postCtx =
    dateField "date" "%B %e, %Y" `mappend`
    defaultContext

postCtxWithTags :: Tags -> Context String
postCtxWithTags tags = tagsField "tags" tags `mappend` postCtx

pageCtx :: Context String
pageCtx = mconcat
    [ modificationTimeField "mtime" "%U"
    , modificationTimeField "lastmod" "%Y-%m-%d"
    , dateField "updated" "%Y-%m-%dT%H:%M:%SZ"
    , constField "host" host
    , dateField "date" "%B %e, %Y"
    , defaultContext
    ]

cleanIndexHtmls :: Item String -> Compiler (Item String)
cleanIndexHtmls = return . fmap (replaceAll pattern replacement)
    where
      pattern = "/index.html"
      replacement = const "/"
