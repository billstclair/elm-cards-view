----------------------------------------------------------------------
--
-- CardsView.elm
-- SVG for playing cards.
-- Copyright (c) 2023 Bill St. Clair <billstclair@gmail.com>
-- Some rights reserved.
-- Distributed under the MIT License
-- See LICENSE
--
----------------------------------------------------------------------


module CardsView exposing (cardToSvg)

{-| Turn a playing card into an `Svg` instance.

@docs cardToSvg

-}

import Cards exposing (Card(..))
import Svg exposing (Svg)


{-| Convert a `Card` and a height into `Svg`.
-}
cardToSvg : Card -> Int -> Svg msg
cardToSvg card height =
    Svg.text "TODO"
