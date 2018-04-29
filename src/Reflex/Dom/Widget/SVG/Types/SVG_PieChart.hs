{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

-- | Types and functions for the \<circle\> SVG element.
module Reflex.Dom.Widget.SVG.Types.SVG_PieChart
  ( SVG_PieChart (..)
  , svg_piechart_pos_centerX
  , svg_piechart_pos_centerY
  , svg_piechart_radius
  , svg_piechart_stroke
  , svg_piechart_stroke_width
  , svg_piechart_stroke_dashArray
  , svg_piechart_stroke_dashOffset
  , makePieChartProps
  , _Stroke
  , _StrokeWidth
  , _Stroke_DashArray
  , _Stroke_DashOffset
  ) where

import           Control.Lens
import           Data.Function                        ((&))
import           Data.Map                             (Map)
import           Data.Text                            (Text)
import           Reflex.Dom.Widget.SVG.Types.Internal (wrappedToText)
import           Reflex.Dom.Widget.SVG.Types.Pos      (CenterX, CenterY, Pos)
import           Reflex.Dom.Widget.SVG.Types.Radius   (Radius)

newtype Stroke            p = Stroke Text
newtype StrokeWidth       p = StrokeWidth Int
newtype Stroke_DashArray  p = Stroke_DashArray [Int]
newtype Stroke_DashOffset p = Stroke_DashOffset Int

instance (Stroke            p) ~ t => Rewrapped (Stroke            p) t where
instance (StrokeWidth       p) ~ t => Rewrapped (StrokeWidth       p) t where
instance (Stroke_DashArray  p) ~ t => Rewrapped (Stroke_DashArray  p) t where
instance (Stroke_DashOffset p) ~ t => Rewrapped (Stroke_DashOffset p) t where

instance Wrapped (Stroke p) where
  type Unwrapped (Stroke p) = Text
  _Wrapped' = iso (\(Stroke x) -> x) Stroke

instance Wrapped (StrokeWidth p) where
  type Unwrapped (StrokeWidth p) = Int
  _Wrapped' = iso (\(StrokeWidth x) -> x) StrokeWidth

instance Wrapped (Stroke_DashArray p) where
  type Unwrapped (Stroke_DashArray p) = [Int]
  _Wrapped' = iso (\(Stroke_DashArray x) -> x) Stroke_DashArray

instance Wrapped (Stroke_DashOffset p) where
  type Unwrapped (Stroke_DashOffset p) = Int
  _Wrapped' = iso (\(Stroke_DashOffset x) -> x) Stroke_DashOffset

data SVG_PieChart = SVG_PieChart
  { _svg_piechart_pos_centerX       :: Pos CenterX
  , _svg_piechart_pos_centerY       :: Pos CenterY
  , _svg_piechart_radius            :: Radius ()
  , _svg_piechart_stroke            :: Stroke ()
  , _svg_piechart_stroke_width      :: StrokeWidth ()
  , _svg_piechart_stroke_dashArray  :: Stroke_DashArray ()
  , _svg_piechart_stroke_dashOffset :: Stroke_DashOffset ()
  }

svg_piechart_pos_centerX :: Lens' SVG_PieChart (Pos CenterX)
svg_piechart_pos_centerX f (SVG_PieChart x1 x2 x3 x4 x5 x6 x7)
  = fmap (\y1 -> SVG_PieChart y1 x2 x3 x4 x5 x6 x7) (f x1)
{-# INLINE svg_piechart_pos_centerX #-}

svg_piechart_pos_centerY :: Lens' SVG_PieChart (Pos CenterY)
svg_piechart_pos_centerY f (SVG_PieChart x1 x2 x3 x4 x5 x6 x7)
  = fmap (\y1 -> SVG_PieChart x1 y1 x3 x4 x5 x6 x7) (f x2)
{-# INLINE svg_piechart_pos_centerY #-}

svg_piechart_radius :: Lens' SVG_PieChart (Radius ())
svg_piechart_radius f (SVG_PieChart x1 x2 x3 x4 x5 x6 x7)
  = fmap (\y1 -> SVG_PieChart x1 x2 y1 x4 x5 x6 x7) (f x3)
{-# INLINE svg_piechart_radius #-}

svg_piechart_stroke :: Lens' SVG_PieChart (Stroke ())
svg_piechart_stroke f (SVG_PieChart x1 x2 x3 x4 x5 x6 x7)
  = fmap (\y1 -> SVG_PieChart x1 x2 x3 y1 x5 x6 x7) (f x4)
{-# INLINE svg_piechart_stroke #-}

svg_piechart_stroke_width :: Lens' SVG_PieChart (StrokeWidth ())
svg_piechart_stroke_width f (SVG_PieChart x1 x2 x3 x4 x5 x6 x7)
  = fmap (\y1 -> SVG_PieChart x1 x2 x3 x4 y1 x6 x7) (f x5)
{-# INLINE svg_piechart_stroke_width #-}

svg_piechart_stroke_dashArray :: Lens' SVG_PieChart (Stroke_DashArray ())
svg_piechart_stroke_dashArray f (SVG_PieChart x1 x2 x3 x4 x5 x6 x7)
  = fmap (\y1 -> SVG_PieChart x1 x2 x3 x4 x5 y1 x7) (f x6)
{-# INLINE svg_piechart_stroke_dashArray #-}

svg_piechart_stroke_dashOffset :: Lens' SVG_PieChart (Stroke_DashOffset ())
svg_piechart_stroke_dashOffset f (SVG_PieChart x1 x2 x3 x4 x5 x6 x7)
  = fmap (\y1 -> SVG_PieChart x1 x2 x3 x4 x5 x6 y1) (f x7)
{-# INLINE svg_piechart_stroke_dashOffset #-}

makePieChartProps
  :: SVG_PieChart
  -> Map Text Text
makePieChartProps c = mempty
  & at "cx"                ?~ c ^. svg_piechart_pos_centerX      . wrappedToText
  & at "cy"                ?~ c ^. svg_piechart_pos_centerY      . wrappedToText
  & at "r"                 ?~ c ^. svg_piechart_radius           . wrappedToText
  & at "stroke"            ?~ c ^. svg_piechart_stroke           . wrappedToText
  & at "stroke-width"      ?~ c ^. svg_piechart_stroke_width     . wrappedToText
  & at "stroke-dasharray"  ?~ c ^. svg_piechart_stroke_dashArray . wrappedToText
  & at "stroke-dashoffset" ?~ c ^. svg_piechart_stroke_dashArray . wrappedToText

_Stroke :: Iso' (Stroke ()) Text
_Stroke = _Wrapped

_StrokeWidth :: Iso' (StrokeWidth ()) Int
_StrokeWidth = _Wrapped

_Stroke_DashArray :: Iso' (Stroke_DashArray ()) [Int]
_Stroke_DashArray = _Wrapped

_Stroke_DashOffset :: Iso' (Stroke_DashOffset ()) Int
_Stroke_DashOffset = _Wrapped
