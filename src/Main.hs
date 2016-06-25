module Main where
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Network.HTTP.Conduit
import Control.Applicative
import qualified Data.ByteString.Lazy as B
import Data.Aeson ((.:), (.=), (.:?), decode, encode, ToJSON(..), object, 
                  FromJSON(..), Value(..), eitherDecode)
import Data.List
import Data.Monoid

-- Maximum total char limit allowed in recommended list (minus formatting)
characterLimit :: Int
characterLimit = 50

-- Maximum amount of characters allowed in a board name
boardNameMaxLength :: Int
boardNameMaxLength = 8

-- Where to save the recommended board php file
phpOutLocation :: String
phpOutLocation = "/path/to/public/inc/instance-recommended-boards.php"

-- Where the boards.json is located
boardData :: String
boardData = "http://8ch.net/boards.json"

-- Main function
main :: IO ()
main = do
  bd <- getBoardData
  case bd of
    Left err -> putStrLn err
    Right d  -> do
      let weighted  = map calculateWeight d
          phpOutput = phpify weighted
      print phpOutput -- Prints result to stdout

      -- Uncomment to write your output to a file
      -- T.writeFile phpOutLocation phpOutput 

-- Put the boards into a php wrapper for exporting
wrapPhp :: [T.Text] -> T.Text
wrapPhp bs = "<?php $recommendedBoards=array('" <> bs' <> "');"
  where
    bs' = T.intercalate "','" bs

-- Format boards into a php compatible string
phpify :: [BoardInfo] -> T.Text
phpify boards = wrapPhp . sort $! grabUntil characterLimit sortedBoards []
  where
    
    -- Add boards to list while not exceeding character limit
    grabUntil :: Int -> [T.Text] -> [T.Text] -> [T.Text]
    grabUntil _ [] ts' = ts'
    grabUntil i (t:ts) ts' 
      | i - (T.length t) <= 0 = ts'
      | otherwise = grabUntil (i - T.length t) ts (t:ts')

    -- Filter and sort our boards. This would probably be better split
    -- into two functions, but whatever
    sortedBoards = map _boardURI 
                   -- filter boards that are on the `dontFeature` list
                 . filter (\x -> not $! _boardURI x `elem` dontFeature) 
                   -- filter boards with names longer than boardNameMaxLength
                 . filter (\x -> T.length (_boardURI x) <= boardNameMaxLength)
                   -- filter boards with pph greater than 2
                 . filter (\x -> (_pph x) >= 2)
                 . take 100 . reverse . sort 
                 $ boards

-- Boards not to feature
dontFeature :: [T.Text]
dontFeature = ["sudo","operate","meta"]

-- Get data located at the URI provided
getURI :: String -> IO B.ByteString
getURI uri = simpleHttp uri

-- Grab board data then parse into our JSON type
getBoardData :: IO (Either String [BoardInfo])
getBoardData = do
  d <- (eitherDecode <$> getURI boardData) :: IO (Either String [BoardInfo])
  return d

-- Data type to hold info about our boards
data BoardInfo = BoardInfo {
    _boardURI      :: T.Text
  , _pph           :: Int
  , _ppd           :: Int
  , _activeUsers   :: Int
  , _totalPosts    :: String -- dont know why boards.json has this as string
  , _weightedScore :: Maybe Double
} deriving (Show)

-- Our JSON decoding guide
instance FromJSON BoardInfo where
  parseJSON !(Object v) =
    BoardInfo          <$>
    (v .:  "uri")      <*>
    (v .:  "pph")      <*>
    (v .:  "ppd")      <*>
    (v .:  "active")   <*>
    (v .:  "max")      <*>
    (v .:? "weighted")
  parseJSON _           = empty

-- Equality based on weighted score for easy Ord sorting
instance Eq BoardInfo where
  (BoardInfo _ _ _ _ _ ws1) == (BoardInfo _ _ _ _ _ ws2) 
    = ws1 == ws2

-- Sort by weight, so we can just run a simple sort function on our data
instance Ord BoardInfo where
  (BoardInfo _ _ _ _ _ ws1) `compare` (BoardInfo _ _ _ _ _ ws2) 
    = ws1 `compare` ws2

-- Weight formula:
--     (pph * active users) / (total posts * (log (posts per day)))
weightedScore :: Int -> Int -> Int -> String -> Maybe Double
weightedScore pph ppd activeUsers totalPosts 
    | cTotalPosts == 0 = Nothing
    | otherwise = Just $! 
        (fromIntegral $! pph * activeUsers) 
          / (cTotalPosts * (log $ fromIntegral ppd))  
  where
    
    -- Helper function. Probably not needed
    cTotalPosts :: Double
    cTotalPosts = convertS totalPosts

    -- Convert string to double. 
    -- Warning: no real type checking of input value here, 
    --          will probably crash and burn on malformed input
    convertS :: String -> Double
    convertS !s = read s :: Double

-- Calculate weight and store in BoardInfo
calculateWeight :: BoardInfo -> BoardInfo
calculateWeight b = BoardInfo uri pph ppd au tp score
  where
    uri   = _boardURI b
    pph   = _pph b
    ppd   = _ppd b
    au    = _activeUsers b
    tp    = _totalPosts b
    score = weightedScore pph ppd au tp
