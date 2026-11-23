module Fedora.Krb (
  krbTicket,
  maybeFasIdFromKrb,
  fasIdFromKrb
  )
where

import Control.Monad (unless, when)
import Data.List.Extra (dropSuffix, find, isInfixOf, isSuffixOf)
import Data.Maybe (fromMaybe)
import SimpleCmd (cmdBool, cmdMaybe, error', (+-+))
import SimplePrompt (prompt)
import System.Posix.User (getLoginName)

krbTicket :: IO ()
krbTicket = do
  entry <- klistEntryFedora
  if null entry
    then do
      putStrLn "No krb5 ticket found for FEDORAPROJECT.ORG"
      fkinit Nothing
    else
    when (last entry == "(Expired)") $ do
    putStrLn $ unwords entry
    fkinit $ extractFasId entry
    putStrLn ""
  where
    fkinit muser = do
      user <-
        case muser of
          Just u -> return u
          Nothing -> do
            username <- getLoginName
            inp <- prompt $ "Enter FAS username [default" +-+ username ++ "]"
            return $ if null inp then username else inp
      -- FIXME test for fkinit
      ok <- cmdBool "fkinit" ["-u", user]
      unless ok $ fkinit muser

extractFasId :: [String] -> Maybe String
extractFasId =
  fmap (dropSuffix "@FEDORAPROJECT.ORG") . find ("@FEDORAPROJECT.ORG" `isSuffixOf`)

maybeFasIdFromKrb :: IO (Maybe String)
maybeFasIdFromKrb = extractFasId <$> klistEntryFedora

fasIdFromKrb :: IO String
fasIdFromKrb = do
  mfasid <- maybeFasIdFromKrb
  case mfasid of
    Nothing -> error' "Could not determine FAS id from klist"
    Just fasid -> return fasid

-- gets first FEDORAPROJECT.ORG entry
klistEntryFedora :: IO [String]
klistEntryFedora = do
  -- FIXME test for klist
  mres <- cmdMaybe "klist" ["-l"]
  return $
    maybe []
    (words . fromMaybe "" . find ("@FEDORAPROJECT.ORG" `isInfixOf`) . lines)
    mres
