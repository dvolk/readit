{-# LANGUAGE PackageImports #-}

import Control.Monad
import "mtl" Control.Monad.Trans (liftIO)
import Text.Printf
import Data.List
import System.FilePath
import System.Process
import System.Directory

import Rascal.Utils
import Rascal.Types
import Rascal.API

import System.Console.Terminfo
import qualified Data.ByteString.Lazy.Char8 as BSL
import Data.Digest.Pure.MD5
import UI.NCurses

import SimpleListBrowser

commentViewer :: String
commentViewer = "less -c" -- edit this

subreddits :: [String]
subreddits = sort -- edit this
  [ "haskell"
  , "idris"
  , "linux"
  , "programming"
  , "science"
  , "emacs"
  , "roguelikes"
  , "rust"
  , "linuxquestions"
  , "debatecommunism"
  , "dwarffortress"
  , "linux4noobs"
  , "eff"
  , "dataisbeautiful"
  , "askscience"
  , "agda"
  , "britishproblems"
  , "commandline"
  , "hackernews"
  , "insightfulquestions"
  , "openbsd"
  , "oxford"
  , "pioneerspacesim"
  , "spiders"
  , "suckless"
  , "askmath"
  , "debatereligion"
  , "kerbalspaceprogram"
  ]

-- we do not use Show because we depend on (an IO generated) width
showLink :: Link -> Int -> String
showLink l width = printf "%4d | %4d | %s" (score l) (numComments l) (take (width - 14) (title l))

showListing :: NamedListing -> Int -> [String]
showListing l width = map (\lnk -> showLink lnk width) (links (listing l))

showComment :: Int -> String -> String -> String -> String -> Comment -> [String]
showComment width prefix addedPrefix futurePrefix op
   (Comment cauthor ups downs _ body children) =
   let prefix' = prefix ++ futurePrefix
       ratio :: Double
       ratio = fromIntegral ups / (fromIntegral ups + fromIntegral downs)
       header = printf "%s─ %.20s (%d; %.1f)" addedPrefix cauthor (ups - downs) ratio
       headerBlock = indentString width prefix header
       commentBlock = indentString width prefix' (unescape body)
   in (prefix ++ "│ "):(headerBlock ++ init commentBlock):
      showCommentListing width prefix' op children

showComment _ _ _ _ _ OriginalArticle =
   []

showCommentListing :: Int -> String -> String -> CommentListing -> [String]
showCommentListing width prefix op (CommentListing cl) =
   case cl of
      [] -> []
      _ -> concatMap (showComment width prefix "├" "│ " op) (init cl) ++
          showComment width prefix "└" "  " op (last cl)

-- |print a listing on screen and ask for a command
displayListing :: NamedListing -> RuntimeConf -> Curses ()
displayListing l conf = do
   let w = textWidth conf
       subreddit = takeWhile (/=' ') (name l)
   simpleListBrowser 
     (showListing l w) 
     (\n -> liftIO $ 
              openComments subreddit 
                           ((links (listing l)) !! (n - 1))
                           conf
     )

-- |display all comments of an article in a subreddit
openComments :: String -> Link -> RuntimeConf -> IO ()
openComments subreddit ln conf = do
  let csort = commentSort conf
  d <- getAppUserDataDirectory "readit"
  (Comments c) <- getComments subreddit (drop 3 (uid ln)) csort
  let commStr = cleanUp pre (textWidth conf)
                ++ unlines (map (unlines . showCommentListing (textWidth conf) "" "") c)
      fileName = d </> "store" </> show (md5 (BSL.pack commStr))
      pre = if isSelf ln
                 then title ln ++ "\n\n" ++ selfText ln
                 else title ln ++ "\n\n" ++ link ln
  writeFile fileName commStr
  _ <- system $ commentViewer ++ " " ++ fileName
  return ()

main ::  IO ()
main = do
  d <- getAppUserDataDirectory "readit"
  createDirectoryIfMissing True (d </> "store")

  term <- setupTerm "vt100"

  let width =
        case getCapability term (tiGetNum "cols") of
          Nothing -> 80
          Just n -> n
      height =
        case getCapability term (tiGetNum "lines") of
          Nothing -> 22
          Just n -> n

  let lSort  = "hot"
      cSort  = "confidence"
      pComments' = True
      conf' = RuntimeConf width height cSort lSort pComments'

  runCurses $ do
    setEcho False
    _ <- setCursorMode CursorInvisible
    forever $ do
      simpleListBrowser subreddits $ \n -> do
        list <- liftIO $ getListing lSort (subreddits !! (n - 1)) 0 Nothing
        displayListing list conf'
