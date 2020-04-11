# Nature of code: Elm edition

[Nature of code](https://natureofcode.com/) exercises and examples implemented
in Elm. Let's see how far we can get in a functional language :).

See the sketches live [here](https://mark-gerarts.github.io/nature-of-code-elm).
Note that 3D sketches are omitted since these are not supported by elm-canvas.

## Running the examples locally

Run a single sketch (requires [elm-live](https://github.com/wking-io/elm-live#installation)):

```
$ elm-live src/Introduction/TraditionalRandomWalk.elm --open --start-page=sketch.html -- --output=sketch.js
```

## Building the website

To build the site you require:

- [Elm](https://guide.elm-lang.org/install/elm.html)
- [UglifyJS](https://github.com/elm/compiler/blob/9d97114702bf6846cab622a2203f60c2d4ebedf2/hints/optimize.md)
- PHP (I wanted it quick and dirty, okay!)

Make sure `site-structure.json` is up-to-date and then run:

```
$ ./generate-site
```

## Related

[Nature of code in Common Lisp](https://github.com/mark-gerarts/nature-of-code).
