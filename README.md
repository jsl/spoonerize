# Spoonerize

A simple Haskell script to create random 'spoonerisms' from an input
sentence. A spoonerism is a sentence where parts of words are
reversed, often in a funny way.

I decided to write this after reading the book "Runny Babbit" by Shel
Silverstein to my kids about 30 times.

This is a work in progress as I experiment with Haskell.

## Usage

Install the library using cabal:

    runhaskell Setup.hs configure --user
    runhaskell Setup.hs build
    runhaskell install

 Import the library in your program or ghci:

    import Text.Spoonerize (spoonerize)

Call the spoonerize function with a String:

    λ: spoonerize "One time I had a bunny rabbit."
    "One bime I had a tunny rabbit."

## TODO

* Write tests
* Extract several functions, general cleanup

## Future considerations

Other approaches to this problem could be either by part-of-speech
tagging or statistical / machine learning done on a corpora of
human-made spoonerisms. These features are not planned at this time.

## Author

Justin Leitgeb

## License

MIT

## References

* http://en.wikipedia.org/wiki/Spoonerism
* http://en.wikipedia.org/wiki/Runny_Babbit
