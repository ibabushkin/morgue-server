module Morgue where

import qualified Data.Morgue.Agenda as A
import qualified Data.Morgue.Outline as O
import Data.Morgue.Options

import Files
import Types
import Util

-- | get an Agenda
getAgenda :: ProcessingRequest -> IO (ApiResponse String)
getAgenda (ProcessingRequest _ opts files) =
    success <$> (concat <$> mapM readFile files >>= processor opts opts)
        where processor AgendaOptions{} = A.getAgenda
              processor OutlineOptions{} = O.getOutline
