module Handler (handler) where

import Prelude

import Control.Monad.Eff.Exception (Error, error)
import Control.Monad.Except (runExcept)
import Data.Either (Either(..))
import Data.Foreign (Foreign, F, toForeign)
import Data.Foreign.NullOrUndefined (undefined)
import Data.Function.Uncurried (Fn2, Fn3, mkFn3, runFn2)
import Simple.JSON (read, write)

--------------------------------------------------------------------------------
-- | Type alias for an uncurried callback function from Lambda.
type LambdaCallbackFn = Fn2 Foreign Foreign Unit

-- | Type alias for a curried callback function from Lambda.
type LambdaCallback = Foreign -> Foreign -> Unit

-- | Type alias for a Lambda handler result.
type Result a = Either Error a

handleResult :: ∀ a. LambdaCallback -> Result a -> Unit
handleResult cb (Left  err) = cb (toForeign err) (write undefined)
handleResult cb (Right msg) = cb (write undefined) (toForeign msg)

--------------------------------------------------------------------------------
-- | Expected record fields for the event passed to the Lambda function.
type Event =
  { payload ::
       { message :: String }
  }

-- | PureScript implementation of the Lambda handler logic.
handlerImpl :: Foreign -> Foreign -> LambdaCallbackFn -> Unit
handlerImpl event _ callback =
  let cb = runFn2 callback
      cbHandler = handleResult cb
      decoded = map _.payload.message $ runExcept (read event :: F Event)

  -- v-- A one-liner for the case statement
  -- cbHandler $ lmap (error <<< show) decoded

  in case decoded of
    Left  err -> cbHandler $ Left (error $ show err)
    Right msg -> cbHandler $ Right msg

-- | Expose the PureScript handler in a way that Lambda can consume.
handler :: Fn3 Foreign Foreign LambdaCallbackFn Unit
handler = mkFn3 handlerImpl
