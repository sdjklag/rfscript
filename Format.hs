{-# LANGUAGE OverloadedStrings, FlexibleContexts #-}

module Main where

import Control.Applicative
import Data.Aeson
import qualified Data.ByteString.Lazy as B
import qualified Data.HashMap.Lazy as H
import Data.List
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Encoding as LT
import System.Console.GetOpt
import System.Environment
import System.Process

data Post = Post Int [String] deriving Show

data Occs = Occs {
        occurrences :: Int,
        postNumbers :: [Int],
        regions :: [String]
} deriving Eq

type RegionMap = H.HashMap String Occs

data Option = From Int | MaxOccs Int

main :: IO ()
main = do (args, files, _) <- getOpt Permute options <$> getArgs
          let (n, maxOccs) = case args of
                                  [From n, MaxOccs o] -> (n, o)
                                  [MaxOccs o, From n] -> (n, o)
                                  [From n] -> (n, 10000)
                                  [MaxOccs o] -> (0, o)
                                  [] -> (0, 10000)
          eposts <- eitherDecode . list <$> mapM B.readFile files
          case eposts of
                  Left err -> putStrLn err
                  Right posts ->
                          do let occs = filter (\(Occs o ns _) ->
                                                        last ns >= n &&
                                                        o <= maxOccs)
                                        . H.elems $ regionMap posts
                             tuples <- mapM (\(Occs o n r) ->
                                                country r >>= \cs -> return (
                                                        case cs of
                                                             [c] -> c
                                                             _ -> "Ambiguous"
                                                        , [Occs o n r])) occs
                             let countryMap = H.fromListWith (++) tuples
                             mapM_ (\k -> let regs = countryMap H.! k
                                              sorted = sortOn occurrences regs
                                          in do putStrLn k
                                                mapM (putStrLn . show) sorted)
                                  . sort . H.keys $ countryMap
                          {-
                          let occs = unlines .
                                     map show .
                                     
                                     sortOn occurrences .
                                     H.elems $ regionMap posts
                          in putStrLn occs
                          -}
        where list x = let sql = LT.encodeUtf8 (LT.singleton '[')
                           sqr = LT.encodeUtf8 (LT.singleton ']')
                           comma = LT.encodeUtf8 (LT.singleton ',')
                       in sql `B.append` (B.intercalate comma x) `B.append` sqr

regionMap :: [Post] -> RegionMap
regionMap = flip foldr H.empty $
                \(Post nr regions) map -> H.insertWith combineOccs
                                                       (last regions)
                                                       (Occs 1 [nr] regions)
                                                       map
        where combineOccs (Occs _ [nr] _) (Occs n nrs reg) =
                        Occs (n + 1) (insert nr nrs) reg

options :: [OptDescr Option]
options = [ Option "f" [] (flip OptArg "NR" $ From . readMaybe 0) ""
          , Option "o" [] (flip OptArg "MAX" $ MaxOccs . readMaybe 10000) "" ]
        where readMaybe _ (Just s) = read s
              readMaybe x Nothing = x

splitRegions :: String -> [String]
splitRegions s = let (_, cur, res) = foldr f (False, "", []) s in cur : res
        where f '|' (True, cur, res) = (False, "", cur : res)
              f '|' (False, cur, res) = (True, cur, res)
              f x (_, cur, res) = (False, x : cur, res)

country :: [String] -> IO [String]
country regs = lines <$> readProcess "./country.sh" [intercalate "/" regs] ""

instance Show Occs where
        show (Occs o ns rs) = ">>" ++ show (last ns) ++ " " ++ showRegs rs
                where showRegs [] = ""
                      showRegs (r : []) = r
                      -- showRegs (r : rs) = [head r] ++ "·" ++ showRegs rs
                      showRegs (r : rs) = showRegs rs

instance FromJSON Post where
        parseJSON (Object v) = Post <$> (read <$> v .: "post_nr")
                                    <*> (splitRegions . T.unpack <$> v .: "region")
