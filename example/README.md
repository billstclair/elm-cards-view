This directory contains an example of using `CardsView.cardToSvg`.

To run it:

```
$ cd .../CardsView/example
$ elm reactor
```

Then aim your browser at `http://localhost/src/Main.elm`.

To compile the code to `site/card.html`:

```
$ cd .../CardsView/example
$ bin/build
```

To upload `cards.html` to https://say-uncle.ninja/cards.html (which you can only do if you're me):

```
$ cd .../CardsView/example/site
$ [rsyncit](https://github.com/billstclair/wws-scripts/blob/master/bin/rsyncit)
```
