module User where

import Crypto.Scrypt

import qualified Data.ByteString.Lazy as B
import Data.Digest.Pure.SHA
import Data.Maybe (fromMaybe)

import System.Entropy

import Types
import Util

-- | generate a new user
mkUser :: Credentials -> IO InternalUser
mkUser (Credentials uName uPass) =
    (InternalUser uName) <$>
        ((B.fromStrict . getEncryptedPass) <$>
            (encryptPassIO' (Pass $ B.toStrict uPass))) <*> genApiKey

-- | generate a user's API key
genApiKey :: IO B.ByteString
genApiKey = bytestringDigest <$> (hmacSha256 <$>
    (B.fromStrict <$> getEntropy 256) <*>
    (B.fromStrict <$> getEntropy 256))

-- | convert an internal user to a user
toUser :: InternalUser -> User
toUser = User <$> iUserName <*> iApiKey

-- | get a user from the data store
loadUser :: UserName -> IO (Maybe InternalUser)
loadUser (UserName uName) = load ["data", "u", uName ++ ".json"]

-- | verify a User
verifyUser :: User -> IO Bool
verifyUser user@(User uName _) =
    (fromMaybe False . fmap ((==user) . toUser)) <$> loadUser uName

-- | authenticate a User
authUser :: Credentials -> IO Bool
authUser (Credentials uName pass) = do
    user <- loadUser uName
    case user of
      Just (InternalUser _ _ pw) -> return $
          verifyPass' (Pass $ B.toStrict pass)
             (EncryptedPass $ B.toStrict pw)
      Nothing -> return False
