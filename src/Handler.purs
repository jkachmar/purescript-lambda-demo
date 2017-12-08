module Handler (handler) where

import Prelude

import Control.Monad.Aff (Aff, runAff_)
import Control.Monad.Aff.Class (liftAff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Exception (Error, error)
import Control.Monad.Eff.Uncurried (EffFn3, mkEffFn3)
import Control.Monad.Except (except, runExceptT)
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.Foreign (Foreign, MultipleErrors, toForeign)
import Data.Foreign.NullOrUndefined (undefined)
import Data.Function.Uncurried (Fn2, runFn2)
import Milkis (defaultFetchOptions, fetch, text)
import Node.HTTP (HTTP)
import Simple.JSON (readJSON, write)

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
type HttpBinRes = { origin :: String }

getIpAddr :: ∀ e. Aff (http :: HTTP | e) (Either MultipleErrors HttpBinRes)
getIpAddr = runExceptT $ do
  res <- liftAff $ text =<< fetch "https://httpbin.org/ip" defaultFetchOptions
  except $ readJSON res

--------------------------------------------------------------------------------
-- | PureScript implementation of the Lambda handler logic.
handlerImpl
  :: ∀ e
   . Foreign
  -> Foreign
  -> LambdaCallbackFn
  -> Eff (http :: HTTP | e) Unit
handlerImpl _event _ callback =
  let cb = runFn2 callback
      cbHandler = pure <<< handleResult cb

  in flip runAff_ getIpAddr $ \res ->

    -- v-- A one-liner for the case statement
    -- cbHandler $ lmap (error <<< show) decoded

    case res of
      Left  err -> cbHandler $ Left (error $ show err)
      Right msg -> cbHandler $ Right msg

-- | Expose the PureScript handler in a way that Lambda can consume.
handler
  :: ∀ e.
    EffFn3
      (http :: HTTP | e)
      Foreign
      Foreign
      LambdaCallbackFn Unit
handler = mkEffFn3 handlerImpl
