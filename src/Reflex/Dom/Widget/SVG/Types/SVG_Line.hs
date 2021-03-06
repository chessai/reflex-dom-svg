{-# LANGUAGE OverloadedStrings #-}
-- | Types and functions for \<line\> SVG element.
module Reflex.Dom.Widget.SVG.Types.SVG_Line
  ( SVG_Line (..)
  , svg_line_pos_end
  , svg_line_pos_start
  , makeSVGLineProps
  ) where

import           Control.Lens                         (Lens', at, (?~), (^.),
                                                       _1, _2)

import           Data.Function                        ((&))

import           Data.Map                             (Map)

import           Data.Text                            (Text)

import           Reflex.Dom.Widget.SVG.Types.Internal (wrappedToText)
import           Reflex.Dom.Widget.SVG.Types.Pos      (Pos, X, Y)

-- | Properties for the <https://developer.mozilla.org/en-US/docs/Web/SVG/Element/line \<line\>> element.
data SVG_Line = SVG_Line
  { _svg_line_pos_start :: ( Pos X, Pos Y )
  , _svg_line_pos_end   :: ( Pos X, Pos Y )
  }

-- | Lens for the end of the @SVG_Line@
svg_line_pos_end :: Lens' SVG_Line (Pos X, Pos Y)
svg_line_pos_end f (SVG_Line x1 x2)
  = fmap (SVG_Line x1) (f x2)
{-# INLINE svg_line_pos_end #-}

-- | Lens for the start of the @SVG_Line@
svg_line_pos_start :: Lens' SVG_Line (Pos X, Pos Y)
svg_line_pos_start f (SVG_Line x1 x2)
  = fmap (`SVG_Line` x2) (f x1)
{-# INLINE svg_line_pos_start #-}

-- | Convert our @SVG_Line@ to the correct attribute map for Reflex.
makeSVGLineProps
  :: SVG_Line
  -> Map Text Text
makeSVGLineProps l = mempty
  & at "x1" ?~ l ^. svg_line_pos_start . _1 . wrappedToText
  & at "y1" ?~ l ^. svg_line_pos_start . _2 . wrappedToText
  & at "x2" ?~ l ^. svg_line_pos_end . _1 . wrappedToText
  & at "y2" ?~ l ^. svg_line_pos_end . _2 . wrappedToText
