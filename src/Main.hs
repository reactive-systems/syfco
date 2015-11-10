-----------------------------------------------------------------------------
-- |
-- Module      :  Main
-- License     :  MIT (see the LICENSE file)
-- Maintainer  :  Felix Klein (klein@react.uni-saarland.de)
-- 
-- The main module.
-- 
-----------------------------------------------------------------------------

module Main
    ( main
    ) where

-----------------------------------------------------------------------------

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
    )
    
import Config
    ( Configuration(..)
    , parseArguments
    )
    
import Reader
    ( readSpecification
    )
    
import Writer
    ( WriteFormat(..)
    , WriteContents(..)  
    , writeSpecification
    )  

import Data.Error
    ( prError
    , argsError 
    )
    
import Data.Specification
    ( Specification
    )  

import System.Environment
    ( getArgs
    )
      
import System.Directory
    ( doesFileExist
    )
    
import Control.Monad
    ( unless
    )  

-----------------------------------------------------------------------------

-- | The main function executed at the program invocation.

main
  :: IO ()

main = do
  args <- getArgs
  case parseArguments args of
    Left err -> prError err
    Right c
      | pHelp c    -> prHelp
      | pVersion c -> prVersion
      | otherwise  -> readContents c

-----------------------------------------------------------------------------

readContents
  :: Configuration -> IO ()

readContents c = do
  contents <- readInput c
  mapM_ (readContent c) contents

-----------------------------------------------------------------------------

readContent
  :: Configuration -> (String,Maybe String) -> IO ()

readContent c (content,file) = 
  case readSpecification content (owSemantics c) (owTarget c) (owParameter c) of
    Left err -> prError err
    Right s  
      | check c       -> case file of
        Nothing -> putStrLn "valid"
        Just f  -> putStrLn $ "valid " ++ f
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

readInput c = case inputFile c of
  [] -> do
    x <- getContents
    return [(x,Nothing)]
  xs -> mapM (\f -> do
    b <- doesFileExist f
    if b then do
      r <- readFile f
      return (r,Just f)
    else case argsError $ "File does not exist: " ++ f of
      Left err -> prError err
      _        -> error "Internal Error") xs

-----------------------------------------------------------------------------

writeOutput
  :: Configuration -> Specification -> IO ()

writeOutput c s = case writeSpecification c s of
  Left err -> prError err
  Right wc -> case outputFile c of
    Nothing -> putStrLn $ mainFile wc
    Just f  -> do
      let ending = case outputFormat c of
            SHORT   -> ".tlsf"
            UNBEAST -> ".xml"
            _       -> ".ltl"

      unless (onlyPartition c) $ 
        writeFile (rmSuffix f ++ ending) $ mainFile wc

      case partitionFile wc of
        Nothing   -> return ()
        Just part -> unless (noPartition c) $
                    writeFile (rmSuffix f ++ ".part") part

  where
    rmSuffix path = case reverse path of
      'f':'s':'l':'t':'.':xr -> reverse xr
      'l':'m':'x':'.':xr     -> reverse xr
      'l':'t':'l':'.':xr     -> reverse xr
      _                      -> path

-----------------------------------------------------------------------------
