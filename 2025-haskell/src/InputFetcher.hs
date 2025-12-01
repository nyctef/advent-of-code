{-# LANGUAGE OverloadedStrings #-}

module InputFetcher (getInput) where

import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as LBS
import Network.HTTP.Simple
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.FilePath ((</>))

-- | Get input for a specific day, downloading if necessary
getInput :: Int -> Int -> IO String
getInput year day = do
  let inputPath = "inputs" </> "day" ++ show day ++ ".txt"

  -- Check if input already exists
  exists <- doesFileExist inputPath
  if exists
    then readFile inputPath
    else do
      putStrLn $ "Downloading input for day " ++ show day ++ "..."
      downloadAndCache year day inputPath

-- | Download input from adventofcode.com and cache it
downloadAndCache :: Int -> Int -> FilePath -> IO String
downloadAndCache year day outputPath = do
  -- Read session cookie
  sessionExists <- doesFileExist ".session"
  if not sessionExists
    then
      error $
        unlines
          [ "Error: .session file not found!",
            "",
            "To download inputs, create a .session file with your AoC session cookie:",
            "  1. Log in to https://adventofcode.com",
            "  2. Open browser dev tools (F12)",
            "  3. Go to Application/Storage -> Cookies",
            "  4. Copy the value of the 'session' cookie",
            "  5. Save it to .session file in this directory"
          ]
    else do
      session <- BS.strip <$> BS.readFile ".session"

      -- Build request
      let url = "https://adventofcode.com/" ++ show year ++ "/day/" ++ show day ++ "/input"
      request <- parseRequest url
      let requestWithCookie = setRequestHeader "Cookie" ["session=" <> session] request

      -- Download
      response <- httpLBS requestWithCookie
      let body = LBS.unpack (getResponseBody response)

      -- Cache
      createDirectoryIfMissing True "inputs"
      writeFile outputPath body

      putStrLn $ "Input cached to " ++ outputPath
      return body
