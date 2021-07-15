module Frontend.MathJax
  ( typesetMathJax
  ) where

import           Control.Monad
import qualified Data.Text as T

import           Language.Javascript.JSaddle (MonadJSM, eval, liftJSM)

typesetMathJax :: MonadJSM m => m ()
typesetMathJax =
  void . liftJSM $
    eval ("window.MathJax !== undefined && MathJax.typeset();" :: T.Text)
