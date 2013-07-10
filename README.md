# Spoonerize

A simple Haskell script to create random 'spoonerisms' from an input
sentence. A spoonerism is a sentence where parts of words are
reversed, often in a funny way.

I decided to write this after reading the book "Runny Babbit" by Shel
Silverstein to my kids about 30 times.

This is a work in progress as I experiment with Haskell.

## Usage

Call runghc on Main.hs and enter a sentence:

    $ runghc Main.hs
    Enter a sentence to spoonerize:
    One time I had a bunny rabbit.
    Your spoonerized sentence:
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
