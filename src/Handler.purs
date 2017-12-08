module Handler (handler) where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Exception (Error, error)
import Control.Monad.Eff.Uncurried (EffFn3, mkEffFn3)
import Control.Monad.Except (runExcept)
import Data.Either (Either(..))
import Data.Foreign (Foreign, MultipleErrors, toForeign)
import Data.Foreign.NullOrUndefined (undefined)
import Data.Function.Uncurried (Fn2, runFn2)
import Simple.JSON (read, write)

--------------------------------------------------------------------------------
-- | The effects our Handler is able to perform.
type HandlerEffs e =
  ( console :: CONSOLE
  | e
  )

-- | Type alias for an uncurried AWS Lambda Handler.
type LambdaHandlerFn e =
  EffFn3
    (HandlerEffs e)
    Foreign
    Foreign
    LambdaCallbackFn
    Unit

-- | Type alias for a curried AWS Lambda Handler.
type LambdaHandler e
  =  Foreign
  -> Foreign
  -> LambdaCallbackFn
  -> Eff (HandlerEffs e) Unit

type LambdaError = Foreign
type LambdaSuccess = Foreign

-- | Type alias for an uncurried callback function from Lambda.
type LambdaCallbackFn = Fn2 LambdaError LambdaSuccess Unit

-- | Type alias for a curried callback function from Lambda.
type LambdaCallback = LambdaError -> LambdaSuccess -> Unit

-- | Finalize the Lambda function by invoking the success state in its callback.
succeed :: ∀ a. LambdaCallback -> a -> Unit
succeed cb message = cb (write undefined) (toForeign message)

-- | Finalize the Lambda function by invoking the failure state in its callback.
fail :: LambdaCallback -> Error -> Unit
fail cb err = cb (toForeign $ show err) (write undefined)

--------------------------------------------------------------------------------
-- | Expected record fields for the event passed to the Lambda function.
type Event =
  { payload ::
       { message :: String }
  }

-- | Attempt to decode a `Foreign` input into an `Event`.
decodeEvent :: Foreign -> Either MultipleErrors Event
decodeEvent = runExcept <<< read

-- | PureScript implementation of the Lambda handler logic.
handlerImpl :: ∀ e. LambdaHandler e
handlerImpl event _ callback =
  let cb = runFn2 callback
      res = map _.payload.message $ decodeEvent event
  in case res of
    Left  err -> pure $ fail cb (error $ show err)
    Right msg -> pure $ succeed cb msg

-- | Expose the PureScript handler in a way that Lambda can consume.
handler :: ∀ e. LambdaHandlerFn e
handler = mkEffFn3 handlerImpl
