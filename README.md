[billstclair/elm-cards-view](https://package.elm-lang.org/packages/billstclair/elm-cards-view/latest) adds SVG card images to [sudo-rushil/elm-cards](https://package.elm-lang.org/packages/sudo-rushil/elm-cards/latest)

The SVG for the cards came from
[here](https://commons.wikimedia.org/wiki/Category:SVG_English_pattern_playing_cards).

## Required libraries

```
elm install sudo-rushil/elm-cards
elm install billstclair/elm-cards-view
```

## Elm code

```
import Cards
import CardsView

kingOfSpades = Cards.new "spades" 13
CardsView.cardToSvg kingOfSpades 200
```

The example is live at https://say-uncle.ninja/cards.html
