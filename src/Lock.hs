-- | A mutually exclusive lock
module Lock (acquire, release) where

import           System.Posix.Files (ownerReadMode, ownerWriteMode, unionFileModes)
import           System.Posix.Semaphore (semOpen, OpenSemFlags (..), 
                                         semPost, semTryWait)

-- | Try to acquire the lock, return `True` on success
acquire :: IO Bool
acquire = do
   s <- semOpen "/pwsafe" (OpenSemFlags True False) readWriteMode 1
   semTryWait s
  where
    readWriteMode = ownerReadMode `unionFileModes` ownerWriteMode

-- | Release the lock, return `True` on success
release :: IO Bool
release = do
   s <- semOpen "/pwsafe" (OpenSemFlags True False) readWriteMode 1
   free <- semTryWait s
   semPost s
   return $ not free
  where
    readWriteMode = ownerReadMode `unionFileModes` ownerWriteMode

