{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
module Reflex.Dom.Widget.SVG.Types.CornerRadius
  ( CornerRadius
  , _CornerRadiusX
  , _CornerRadiusY
  ) where

import           Control.Lens                    (Iso', Rewrapped, Wrapped (..),
                                                  iso, _Wrapped)

import           Reflex.Dom.Widget.SVG.Types.Pos (X, Y)

newtype CornerRadius p =
  CornerRadius Float

instance (CornerRadius p) ~ t => Rewrapped (CornerRadius p) t

instance Wrapped (CornerRadius p) where
  type Unwrapped (CornerRadius p) = Float
  _Wrapped' = iso (\(CornerRadius x) -> x) CornerRadius

_CornerRadiusX :: Iso' (CornerRadius X) Float
_CornerRadiusX = _Wrapped

_CornerRadiusY :: Iso' (CornerRadius Y) Float
_CornerRadiusY = _Wrapped