{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
module HW05 where

import Data.ByteString.Lazy (ByteString)
import Data.Map.Strict (Map)
import System.Environment (getArgs)

import qualified Data.ByteString.Lazy as BS
import qualified Data.Map.Strict as Map

import Parser
import Data.Bits
import Data.Char
import Data.Word
import Data.List

-- Exercise 1 -----------------------------------------

mapXOR :: [(Word8, Word8)] -> [Word8]
mapXOR = map (\(x, y) -> xor x y)

getSecret :: FilePath -> FilePath -> IO ByteString
getSecret fileNameX fileNameY = do
    fileX <- BS.readFile fileNameX
    fileY <- BS.readFile fileNameY
    return . BS.pack . filter (/= 0) . mapXOR $ zip (BS.unpack fileX) (BS.unpack fileY)

-- Exercise 2 -----------------------------------------

decryptWithKey :: ByteString -> FilePath -> IO ()
decryptWithKey key fileName = do
    file <- BS.readFile (fileName ++ ".enc")
    BS.writeFile fileName (trim . encode . map w2c . mapXOR $ zip (BS.unpack file) rKey)
    where rKey = (concat . repeat . BS.unpack) key
          trim = sToBS . filter f . tail . init . bsToS
                 where f x = x /= 'n' && x /= '\\'
          bsToS = map w2c . BS.unpack
          sToBS = BS.pack . map c2w
          w2c = chr . fromIntegral
          c2w = fromIntegral . ord

-- Exercise 3 -----------------------------------------

parseFile :: FromJSON a => FilePath -> IO (Maybe a)
parseFile fileName = do
    file <- BS.readFile fileName
    (return . decode) file

-- Exercise 4 -----------------------------------------

getBadTs :: FilePath -> FilePath -> IO (Maybe [Transaction])
getBadTs victimFile transactionFile = do
    (Just victimList) <- parseFile victimFile      :: IO (Maybe [TId])
    (Just transList)  <- parseFile transactionFile :: IO (Maybe [Transaction])
    return $ Just (filterBad victimList transList)
    where filterBad vList = filter f
                            where f x = elem (tid x) vList

-- Exercise 5 -----------------------------------------

getFlow :: [Transaction] -> Map String Integer
getFlow []       = Map.empty
getFlow (x : xs) = Map.union record (getFlow xs)
                   where record = Map.fromList [(from x, - (amount x)), (to x, amount x)]

-- Exercise 6 -----------------------------------------

getCriminal :: Map String Integer -> String
getCriminal tMap = cName
                   where (cName, _) = (maximumBy f . Map.toList) tMap
                         f (_, x) (_, y) = compare y x

-- Exercise 7 -----------------------------------------

undoTs :: Map String Integer -> [TId] -> [Transaction]
undoTs tMap transID = genTrans sortedPas sortedPes transID
    where genTrans [] _  _  = []
          genTrans _  [] _  = []
          genTrans _  _  [] = []
          genTrans (pa : pas) (pe : pes) (tID : ids)
              | payment > payeement = trans : genTrans pas           (newPe : pes) ids
              | payment < payeement = trans : genTrans (newPa : pas) pes           ids
              | otherwise           = trans : genTrans pas           pes           ids
              where payment   = amount pa
                    payeement = - (amount pe)
                    offset = min payment payeement
                    trans  = Transaction (name pa) (name pe) offset tID
                    newPa = (name pa, amount pa - offset)
                    newPe = (name pe, amount pe + offset)
          (rawPas, rawPes)       = partition ((>0) . amount) (Map.toList tMap)
          (sortedPas, sortedPes) = (reverse (sortOn amount rawPas), (sortOn amount rawPes))
          name   (x, _) = x
          amount (_, x) = x

-- Exercise 8 -----------------------------------------

writeJSON :: ToJSON a => FilePath -> a -> IO ()
writeJSON fileName = do
    BS.writeFile fileName . encode

-- Exercise 9 -----------------------------------------

doEverything :: FilePath -> FilePath -> FilePath -> FilePath -> FilePath
             -> FilePath -> IO String
doEverything dog1 dog2 trans vict fids out = do
    key <- getSecret dog1 dog2
    decryptWithKey key vict
    mts <- getBadTs vict trans
    case mts of
        Nothing -> error "No Transactions"
        Just ts -> do
            mids <- parseFile fids
            case mids of
                Nothing  -> error "No ids"
                Just ids -> do
                    let flow = getFlow ts
                    writeJSON out (undoTs flow ids)
                    return (getCriminal flow)

main :: IO ()
main = do
    args <- getArgs
    crim <-
        case args of
            dog1 : dog2 : trans : vict : ids : out : _ ->
                doEverything dog1 dog2 trans vict ids out
            _ -> doEverything "dog-original.jpg"
                              "dog.jpg"
                              "transactions.json"
                              "victims.json"
                              "new-ids.json"
                              "new-transactions.json"
    putStrLn crim