{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Functor
import Hakyll
import qualified Skylighting.Format.HTML as SF
import qualified Skylighting.Styles as SS
import Text.Pandoc.Highlighting
import Text.Pandoc.Options

hostName :: String
hostName = "https://yoshitsugu.net"

main :: IO ()
main = hakyll $ do
  match "images/**/*" $ do
    route idRoute
    compile copyFileCompiler

  match "images/*" $ do
    route idRoute
    compile copyFileCompiler

  match (fromList ["google9af023e4743ca32a.html", "robots.txt"]) $ do
    route idRoute
    compile copyFileCompiler

  match "css/*.scss" $ do
    route $ setExtension "css"
    compile $
      ( getResourceString
          >>= withItemBody (unixFilter "sass" ["--stdin"])
      )
        Data.Functor.<&> fmap compressCss

  create ["css/highlight.css"] $ do
    route idRoute
    compile $ makeItem $ compressCss (SF.styleToCss SS.haddock) ++ "@media (prefers-color-scheme: dark) {" ++ compressCss (SF.styleToCss SS.breezeDark) ++ "}"

  match "css/*.css" $ do
    route idRoute
    compile compressCssCompiler

  match "js/*.js" $ do
    route idRoute
    compile copyFileCompiler

  match (fromList ["about.md"]) $ do
    route $ setExtension "html"
    compile $
      customPandocCompiler
        >>= loadAndApplyTemplate "templates/default.html" defaultCtx
        >>= relativizeUrls

  tags <- buildTags "posts/*" (fromCapture "tags/*.html")

  tagsRules tags $ \tag patt -> do
    route idRoute
    compile $ do
      posts <- recentFirst =<< loadAll patt
      let tagCtx =
            constField "title" ("Posts tagged " ++ tag)
              <> listField "posts" (postCtxWithTags tags) (return posts)
              <> defaultCtx

      makeItem ""
        >>= loadAndApplyTemplate "templates/tag.html" tagCtx
        >>= loadAndApplyTemplate "templates/default.html" tagCtx
        >>= relativizeUrls

  match "posts/*" $ do
    route $ setExtension "html"
    compile $
      customPandocCompiler
        >>= loadAndApplyTemplate "templates/post.html" (postCtxWithTags tags)
        >>= saveSnapshot "content"
        >>= loadAndApplyTemplate "templates/default.html" (postCtxWithTags tags)
        >>= relativizeUrls

  create ["archive.html"] $ do
    route idRoute
    compile $ do
      posts <- recentFirst =<< loadAll "posts/*"
      let archiveCtx =
            listField "posts" postCtx (return posts)
              `mappend` constField "title" "Archives"
              `mappend` defaultCtx

      makeItem ""
        >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
        >>= loadAndApplyTemplate "templates/default.html" archiveCtx
        >>= relativizeUrls

  match "index.html" $ do
    route idRoute
    compile $ do
      posts <- recentFirst =<< loadAllSnapshots "posts/*" "content"
      let teaserCtx = teaserField "teaser" "content" `mappend` postCtx
          indexCtx =
            listField "posts" teaserCtx (return $ take 10 posts)
              `mappend` defaultCtx

      getResourceBody
        >>= applyAsTemplate indexCtx
        >>= loadAndApplyTemplate "templates/default.html" indexCtx
        >>= relativizeUrls

  create ["sitemap.xml"] $ do
    route idRoute
    compile $ do
      posts <- recentFirst =<< loadAll "posts/*"
      pages <- loadAll $ fromList ["index.html", "about.md"]
      let allPosts = return (pages ++ posts)
      let sitemapCtx =
            mconcat
              [ listField "entries" pageCtx allPosts,
                constField "host" hostName,
                defaultContext
              ]
      makeItem ""
        >>= loadAndApplyTemplate "templates/sitemap.xml" sitemapCtx
        >>= cleanIndexHtmls

  match "templates/*" $ compile templateCompiler

  create ["atom.xml"] $ do
    route idRoute
    compile $ do
      let feedCtx = postCtx `mappend` bodyField "description"
      posts <-
        fmap (take 10) . recentFirst
          =<< loadAllSnapshots "posts/*" "content"
      renderRss myFeedConfiguration feedCtx posts

--------------------------------------------------------------------------------
customPandocCompiler :: Compiler (Item String)
customPandocCompiler =
  let writerOptions =
        defaultHakyllWriterOptions
          { writerHighlightStyle = Just zenburn
          }
   in pandocCompilerWith defaultHakyllReaderOptions writerOptions

defaultCtx :: Context String
defaultCtx =
  urlField "ogpurl"
    `mappend` constField "host" hostName
    `mappend` defaultContext

postCtx :: Context String
postCtx =
  dateField "date" "%B %e, %Y"
    `mappend` descriptionField "description" "content"
    `mappend` defaultCtx

postCtxWithTags :: Tags -> Context String
postCtxWithTags tags = tagsField "tags" tags `mappend` postCtx

pageCtx :: Context String
pageCtx =
  mconcat
    [ modificationTimeField "mtime" "%U",
      modificationTimeField "lastmod" "%Y-%m-%d",
      dateField "updated" "%Y-%m-%dT%H:%M:%SZ",
      constField "host" hostName,
      dateField "date" "%B %e, %Y",
      defaultCtx
    ]

cleanIndexHtmls :: Item String -> Compiler (Item String)
cleanIndexHtmls = return . fmap (replaceAll pattern replacement)
  where
    pattern = "/index.html"
    replacement = const "/"

descriptionField :: String -> Snapshot -> Context String
descriptionField = teaserFieldWithSeparatorStripped "<!--more-->"

teaserFieldWithSeparatorStripped ::
  String ->
  String ->
  Snapshot ->
  Context String
teaserFieldWithSeparatorStripped separator key snapshot = field key $ \item -> do
  body <- itemBody <$> loadSnapshot (itemIdentifier item) snapshot
  case needlePrefix separator body of
    Nothing ->
      fail $
        "Hakyll.Web.Template.Context: no teaser defined for "
          ++ show (itemIdentifier item)
    Just t -> return $ stripTags t

myFeedConfiguration :: FeedConfiguration
myFeedConfiguration =
  FeedConfiguration
    { feedTitle = "TSUGULOG",
      feedDescription = "yoshitsugu's blog",
      feedAuthorName = "Kota Yoshitsugu",
      feedAuthorEmail = "kota@yoshitsugu.net",
      feedRoot = "htts://yoshitsugu.net"
    }