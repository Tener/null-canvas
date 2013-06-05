{-# LANGUAGE OverloadedStrings, TemplateHaskell, GADTs, KindSignatures #-}

module Graphics.Blank
        (
         -- * Starting blank-canvas
          blankCanvas
        , blankCanvasMany
        , blankCanvasParams
        , blankCanvasParamsScotty
        -- * Graphics 'Context'
        , Context       -- abstract
        , send
        , events
         -- * Drawing pictures using the Canvas DSL
        , Canvas        -- abstract
        , module Graphics.Blank.Generated
        , readEvent
        , tryReadEvent
        , size
        -- * Events
        , Event(..)
        , EventName(..)
        , EventQueue
        , readEventQueue
        , tryReadEventQueue
        ) where

import Control.Concurrent
import Control.Monad.IO.Class (liftIO)
import Control.Monad (when)
import qualified Data.String
import Web.Scotty as S
import Network.Wai.Middleware.RequestLogger
import qualified Data.Text.Lazy as T
import System.FilePath

import qualified Data.Map as Map
import Data.List (intercalate)
import Data.List.Split (splitOn)

import Graphics.Blank.Events
import Graphics.Blank.Context
import Graphics.Blank.Canvas
import Graphics.Blank.Generated
import Paths_null_canvas

-- | blankCanvas is the main entry point into blank-canvas.
-- A typical invocation would be
--
-- >module Main where
-- >
-- >import Graphics.Blank
-- >
-- >main = blankCanvas 3000 $ \ context -> do
-- >        send context $ do
-- >                moveTo(50,50)
-- >                lineTo(200,100)
-- >                lineWidth 10
-- >                strokeStyle "red"
-- >                stroke()
-- >

blankCanvas :: Int -> (Context -> IO ()) -> IO ()
blankCanvas port app = blankCanvasMany port [("",app)]

blankCanvasParams :: Int -> (Context -> IO ()) -> FilePath -> Bool -> String -> IO ()
blankCanvasParams port app dataDir performLogging pathPrefix = 
   scotty port =<< blankCanvasParamsScotty app dataDir performLogging pathPrefix

-- | launch multiple canvas apps, each with a prefix, like `/myprefix/foo/bar`
blankCanvasMany :: Int -> [(String, (Context -> IO ()))] -> IO ()
blankCanvasMany port apps = do
   dataDir <- getDataDir
   canvases <- mapM (\ (prefix,app) -> blankCanvasParamsScotty app dataDir False prefix) apps
   scotty port (sequence_ canvases)  

-- | parametrised version of blankCanvas, also returns ScottyM application instead of running a server. use 'scotty' to run it.
blankCanvasParamsScotty :: (Context -> IO ()) -> FilePath -> Bool -> String -> IO (ScottyM ())
blankCanvasParamsScotty actions dataDir performLogging pathPrefix = do
   uVar <- newMVar 0
   let getUniq :: IO Int
       getUniq = do
              u <- takeMVar uVar
              putMVar uVar (u + 1)
              return u

   contextDB <- newMVar $ (Map.empty :: Map.Map Int Context)
   let newContext :: (Float,Float) -> IO Int
       newContext (w,h) = do
            uq <- getUniq
            picture <- newEmptyMVar
            callbacks <- newMVar $ Map.empty
            let cxt = Context (w,h) picture callbacks uq
            db <- takeMVar contextDB
            putMVar contextDB $ Map.insert uq cxt db
            -- Here is where we actually spawn the user code
            _ <- forkIO $ actions cxt
            return uq

   bareIndex <- readFile (dataDir </> "static" </> "index.html")
   let fixedIndex = T.pack $ intercalate pathPrefix $ splitOn "XXX_URL_PREFIX_XXX" bareIndex

   return $ do
        when performLogging (middleware logStdoutDev)
        let addPrefix lit = Data.String.fromString (pathPrefix ++ lit)

        get (addPrefix "/") $ html fixedIndex
        get (addPrefix "/jquery.js") $ file $ dataDir </> "static" </> "jquery.js"
        get (addPrefix "/jquery-json.js") $ file $ dataDir </> "static" </> "jquery-json.js"

        post (addPrefix "/start") $ do
            req <- jsonData
            uq  <- liftIO $ newContext req
            text (T.pack $ "session = " ++ show uq ++ ";redraw();")

        post (addPrefix "/event/:num") $ do
            header "Cache-Control" "max-age=0, no-cache, private, no-store, must-revalidate"
            num <- param "num"
            NamedEvent nm event <- jsonData
            db <- liftIO $ readMVar contextDB
            case Map.lookup num db of
               Nothing -> json ()
               Just (Context _ _ callbacks _) -> do
                   db' <- liftIO $ readMVar callbacks
                   case Map.lookup nm db' of
                       Nothing -> json ()
                       Just var -> do liftIO $ writeEventQueue var event
                                      json ()

        get (addPrefix "/canvas/:num") $ do
            header "Cache-Control" "max-age=0, no-cache, private, no-store, must-revalidate"
            -- do something and return a new list of commands to the client
            num <- param "num"
            let tryPicture picture n = do
                    res <- liftIO $ tryTakeMVar picture
                    case res of
                     Just js -> do
                            text $ T.pack js
                     Nothing | n == 0 ->
                            -- give the browser something to do (approx every second)
                            text (T.pack "")
                     Nothing -> do
                            -- hack, wait a 1/10 of a second
                            liftIO $ threadDelay (100 * 1000)
                            tryPicture picture (n - 1 :: Int)

            db <- liftIO $ readMVar contextDB
            case Map.lookup num db of
               Nothing -> text (T.pack $ "alert('/canvas/, can not find " ++ show num ++ "');")
               Just (Context _ pic _ _) -> tryPicture pic 10

-- | Sends a set of Canvas commands to the canvas. Attempts
-- to common up as many commands as possible.
send :: Context -> Canvas a -> IO a
send cxt@(Context (h,w) _ _ _) commands = send' commands id
  where
      send' :: Canvas a -> (String -> String) -> IO a

      send' (Bind (Return a) k)    cmds = send' (k a) cmds
      send' (Bind (Bind m k1) k2)  cmds = send' (Bind m (\ r -> Bind (k1 r) k2)) cmds
      send' (Bind (Command cmd) k) cmds = send' (k ()) (cmds . shows cmd)
      send' (Bind Size k)          cmds = send' (k (h,w)) cmds
      send' (Bind other k)         cmds = do
              res <- send' other cmds
              send' (k res) id

      send' (Get a op)             cmds = do
              -- clear the commands
              sendToCanvas cxt cmds
              -- get the channel for this event
              chan <- events cxt a
              op chan

      send' (Return a)             cmds = do
              sendToCanvas cxt cmds
              return a
      send' other                  cmds = send' (Bind other Return) cmds

