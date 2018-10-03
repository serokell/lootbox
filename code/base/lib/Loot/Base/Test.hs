-- | Test utils.
module Loot.Base.Test
       ( thisLine
       ) where

import Control.Exception.Safe (StringException (StringException))
import GHC.Stack (HasCallStack, SrcLoc (srcLocStartLine), getCallStack)


-- | Get the number of the line with the call to this function.
thisLine :: HasCallStack => Int
thisLine = case getCallStack callStack of
    (_, srcLoc) : _ -> srcLocStartLine srcLoc
    _               -> bug (StringException "Broken call stack" callStack)
