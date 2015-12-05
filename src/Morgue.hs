module Morgue where

import Data.List.Split (splitOn)

import qualified Data.Morgue.Agenda as A
import qualified Data.Morgue.Outline as O
import Data.Morgue.Options

import Files
import Types
import Util

-- | get an Agenda
getAgenda :: ProcessingRequest -> IO (ApiResponse String)
getAgenda (ProcessingRequest user opts files) =
    success <$> (concat <$> mapM (readFile . file') files >>=
        processor opts opts)
        where processor AgendaOptions{} = A.getAgenda
              processor OutlineOptions{} = O.getOutline
              file' = ("data/"++) . concat . splitOn "../"
