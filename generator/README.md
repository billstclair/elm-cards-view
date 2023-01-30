This directory contains an Elm webapp for generating the file `.../elm-cards-view/src/CardsView/cards.elm` from the SVG files in `.../elm-cards-view/generator/src/svg/`, indexed by `.../elm-cards-view/generator/srcsvg/index.json`.

`index.json` is of the form:

    {"<number>+<suit>": "<SVG file name>.svg",
     ...
    }
    
Where `<number>` is a number from 1 to 13, and `<suit>` is one of `clubs`, `diamonds`, `hearts`, or `spades`.

`<SVG file name>.svg` is the name of one of the `SVG` files in the same directory as `index.json`, containing the SVG to draw the card: `Card.new "<suit>" <number>`.

If <number> is not a number from 1 to 13, or <suit> is not one of the four choices, then `<SVG file name>.svg` contains SVG for the card back.

I created `index.json` in [Emacs](https://www.gnu.org/software/emacs/), from a directory listing and a bunch of keyboard macros.

To run the generator:

```
$ cd .../elm-cards-view/generator
$ elm reactor
```

Then aim your browser at http://localhost/src/Main.elm.

If all goes well, the code for `Cards.elm` will be generated. That's how I created `.../elm-cards-view/src/CardsView/Cards.elm`.
