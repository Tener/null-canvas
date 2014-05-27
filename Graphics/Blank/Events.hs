module Graphics.Blank.Events
        ( -- * Events
          Event(..)
        , NamedEvent(..)
        , EventName(..)
         -- * Event Queue
        , EventQueue            -- not abstract
        , writeEventQueue
        , readEventQueue
        , tryReadEventQueue
        , newEventQueue
        ) where

import Data.Aeson (FromJSON(..))
import qualified Data.Map as Map
import Data.Map (Map)
import Data.Char
import Control.Monad
import Control.Concurrent.STM

-- | Basic Event from Browser, the code is event-type specific.
data Event = Event
        { jsCode  :: Int
        , jsMouse :: Maybe (Int,Int)
        }
        deriving (Show)

-- | When an event is sent to the application, it always has a name.
data NamedEvent = NamedEvent EventName Event
        deriving (Show)

instance FromJSON NamedEvent where
   parseJSON o = do
           (str,code,x,y) <- parseJSON o
           case Map.lookup str namedEventDB of
             Just n -> return $ NamedEvent n (Event code (Just (x,y)))
             Nothing -> do (str',code',(),()) <- parseJSON o
                           case Map.lookup str' namedEventDB of
                             Just n -> return $ NamedEvent n (Event code' Nothing)
                             Nothing -> fail "bad parse"

namedEventDB :: Map String EventName
namedEventDB = Map.fromList
                [ (map toLower (show n),n)
                | n <- [minBound..maxBound]
                ]

-- | 'EventName' mirrors event names from jquery, where 'map toLower (show name)' gives
-- the jquery event name.
data EventName
        -- Keys
        = KeyPress
        | KeyDown
        | KeyUp
        -- Mouse
        | MouseDown
        | MouseEnter
        | MouseMove
        | MouseOut
        | MouseOver
        | MouseUp
        -- Click?
        | Click
        deriving (Eq, Ord, Show, Enum, Bounded)

-- | EventQueue is a STM channel ('TChan') of 'Event's.
-- Intentionally, 'EventQueue' is not abstract.
type EventQueue = TChan Event

writeEventQueue :: EventQueue -> Event -> IO ()
writeEventQueue q e = atomically $ writeTChan q e

readEventQueue :: EventQueue -> IO Event
readEventQueue q = atomically $ readTChan q

tryReadEventQueue :: EventQueue -> IO (Maybe Event)
tryReadEventQueue q = atomically $ do
        b <- isEmptyTChan q
        if b then return Nothing
             else liftM Just (readTChan q)

newEventQueue :: IO EventQueue
newEventQueue = atomically newTChan

