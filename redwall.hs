{-# LANGUAGE OverloadedStrings #-}
import Network.Wreq (get, responseBody)
import Control.Lens ((^.), (^?))
import Data.Aeson.Lens (key, _Array)
import Data.Aeson ((.:), Value(String))
import Data.Vector (toList)
import System.Process (callCommand)
import System.Random (randomR, newStdGen)
import System.Environment (getArgs)

myConcat :: [Maybe a] -> [a]
myConcat ((Just x):xs) = x:(myConcat xs)
myConcat (Nothing:xs) = myConcat xs
myConcat [] = []

json2String :: Value -> String
json2String (String x) = show x

unescapeNewlines :: String -> String
unescapeNewlines ('\\':'n':xs) = '\n':xs
unescapeNewlines (x:xs) = x:(unescapeNewlines xs)
unescapeNewlines "" = ""

isDirectImage :: String -> Bool
isDirectImage = (elem '.') . lastSlash
    where lastSlash x
           | '/' `elem` x = lastSlash $ tail $ dropWhile (/= '/') x
           | otherwise = x

main = do
    subreddit <- fmap head getArgs

    posts <- fmap (fmap (^? key "data" . key "url"))
           . fmap (^. responseBody . key "data" . key "children" . _Array)
           $ get $ "https://reddit.com/r/" ++ subreddit ++ "/hot.json"
    let pictures = ((filter isDirectImage) . (map json2String) . myConcat . toList) posts
    whichPicture <- fmap (fst . randomR (0, length pictures - 1)) newStdGen
    let picture = pictures !! whichPicture
    callCommand $ "curl " ++ picture ++ " > pic.jpg"
    callCommand $ "gsettings set org.gnome.desktop.background picture-uri file://$(realpath pic.jpg)"
