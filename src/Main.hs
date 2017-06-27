----------------------------------------------------------------------------
-- |
-- Module      :  Main
-- License     :  MIT (see the LICENSE file)
-- Maintainer  :  Felix Klein (klein@react.uni-saarland.de)
--
-- The main module.
--
-----------------------------------------------------------------------------

{-# LANGUAGE

    MultiWayIf

  #-}

-----------------------------------------------------------------------------

module Main
  ( main
  ) where

-----------------------------------------------------------------------------

import Syfco

import Info
  ( prTitle
  , prDescription
  , prSemantics
  , prTarget
  , prTags
  , prParameters
  , prInputs
  , prOutputs
  , prInfo
  , prVersion
  , prHelp
  , prReadme
  , prReadmeMd
  , prError
  )

import Arguments
  ( parseArguments
  )

import Data.Maybe
  ( isJust
  , fromJust
  )

import System.Environment
  ( getArgs
  )

import System.Directory
  ( doesFileExist
  )

import Control.Monad
  ( when
  )

import Control.Exception
  ( assert
  )

import GHC.IO.Encoding
  ( setLocaleEncoding
  , setFileSystemEncoding
  , setForeignEncoding
  , utf8
  )

-----------------------------------------------------------------------------

-- | The main function executed at the program invocation.

main
  :: IO ()

main = do
  setLocaleEncoding utf8
  setFileSystemEncoding utf8
  setForeignEncoding utf8
  args <- getArgs
  c <- parseArguments args
  if | pHelp c     -> prHelp
     | pVersion c  -> prVersion
     | pReadme c   -> prReadme
     | pReadmeMd c -> prReadmeMd
     | otherwise   -> readContents c

-----------------------------------------------------------------------------

readContents
  :: Configuration -> IO ()

readContents c = do
  mapM_ (writeConfiguration c) $ saveConfig c
  when (not (null (inputFiles c)) || fromStdin c) $ do
    contents <- readInput c
    mapM_ (readContent c) contents

-----------------------------------------------------------------------------

readContent
  :: Configuration -> (String,Maybe String) -> IO ()

readContent c (content,file) = case readSpecification content of
  Left err -> prError $ show err
  Right s
    | check c       -> do
        case writeSpecification c s of
          Left err -> prError $ show err
          Right _  ->
            case file of
              Nothing -> putStrLn "valid"
              Just f  -> putStrLn $ "valid: " ++ f
    | cGR c       ->
        case checkGR s of
          Left err   -> prError $ show err
          Right (-1) -> case file of
            Nothing ->
              putStrLn "NOT in the Generalized Reacitvity fragment"
            Just f  ->
              putStrLn $ "NOT in the Generalized Reactivity fragment: " ++ f
          Right x    -> case file of
            Nothing ->
              putStrLn $ "IN GR(" ++ show x ++ ")"
            Just f  -> do
              putStrLn $ "IN GR(" ++ show x ++ "): " ++ f
    | pTitle c      -> prTitle s
    | pDesc c       -> prDescription s
    | pSemantics c  -> prSemantics s
    | pTarget c     -> prTarget s
    | pTags c       -> prTags s
    | pParameters c -> prParameters s
    | pInputs c     -> prInputs c s
    | pOutputs c    -> prOutputs c s
    | pInfo c       -> prInfo s
    | otherwise     -> writeOutput c s

-----------------------------------------------------------------------------

readInput
  :: Configuration -> IO [(String,Maybe String)]

readInput c = case inputFiles c of
  [] -> do
    x <- getContents
    return [(x,Nothing)]
  xs -> mapM (\f -> do
    exists <- doesFileExist f
    if exists then do
      r <- readFile f
      return (r,Just f)
    else
      prError $ "File does not exist: " ++ f) xs

-----------------------------------------------------------------------------

writeOutput
  :: Configuration -> Specification -> IO ()

writeOutput c s = case writeSpecification c s of
  Left err -> prError $ show err
  Right wc -> do
    case outputFile c of
      Nothing -> putStrLn wc
      Just f  -> case outputFormat c of
        BASIC   -> writeFile (rmSuffix f ++ ".tlsf") wc
        FULL    -> writeFile (rmSuffix f ++ ".tlsf") wc
        UNBEAST -> writeFile (rmSuffix f ++ ".xml") wc
        _       -> writeFile f wc

    when (isJust $ partFile c) $ case writePartition c s of
      Left err -> prError $ show err
      Right f  -> writeFile (fromJust $ partFile c) f

  where
    rmSuffix path = case reverse path of
      'f':'s':'l':'t':'.':xr -> reverse xr
      'l':'m':'x':'.':xr     -> reverse xr
      'l':'t':'l':'.':xr     -> reverse xr
      _                      -> path

-----------------------------------------------------------------------------

writeConfiguration
  :: Configuration -> FilePath -> IO ()

writeConfiguration c file =
  writeFile file $ writeCfg c

-----------------------------------------------------------------------------
