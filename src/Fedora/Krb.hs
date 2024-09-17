module Fedora.Krb (
  fasIdFromKrb,
  maybeFasIdFromKrb,
  krbTicket
  )
where

import Control.Monad
import qualified Data.List as L
import Data.Maybe
import SimpleCmd

krbTicket :: IO ()
krbTicket = do
  krb <- klistEntryFedora
  if null krb
    then error' "No krb5 ticket found for FEDORAPROJECT.ORG"
    else
    when (last krb == "(Expired)") $ do
      putStrLn $ unwords krb
      fkinit
      putStrLn ""
  where
    fkinit = do
      -- FIXME test for fkinit
      ok <- cmdBool "fkinit" []
      unless ok fkinit

maybeFasIdFromKrb :: IO (Maybe String)
maybeFasIdFromKrb =
  fmap (removeSuffix "@FEDORAPROJECT.ORG") . L.find ("@FEDORAPROJECT.ORG" `L.isSuffixOf`) <$> klistEntryFedora

fasIdFromKrb :: IO String
fasIdFromKrb = do
  mfasid <- maybeFasIdFromKrb
  case mfasid of
    Nothing -> error' "Could not determine fasid from klist"
    Just fasid -> return fasid

klistEntryFedora :: IO [String]
klistEntryFedora = do
  -- FIXME test for klist
  mres <- cmdMaybe "klist" ["-l"]
  return $
    maybe []
    (words . fromMaybe "" . L.find ("@FEDORAPROJECT.ORG" `L.isInfixOf`) . lines)
    mres
