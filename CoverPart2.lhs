    [BLOpts]
    profile    = nanonaren
    title      = Haskell Abstractions At Work (Part II - An Interlude)
    tags       = pattern, functor, applicative
    categories = Haskell

[View literate file on Github](https://github.com/nanonaren/blog/blob/master/CoverPart2.lhs)

The boss wants more
-------------------

So the boss comes back and says, "Naren, this is great but I don't
want to write code to specify my schedule". He scribbles some notes on
the whiteboard to illustrate that he'd rather specify his schedules in
plain text.

```
Meeting with CTO
Mo, We,Fr 10-12, 3-5

Lunch
Mo,Tu,We,Th,Fr 12-1
```

<h3>Amazingly, your colleague has a parser</h3>

You don't scramble into a panic because your best pal already has a
parser created and it reads as follows. Some imports;

> {-# LANGUAGE OverloadedStrings #-}
> import CoverPart1
> import SchedulePrimitives
> import Prelude hiding (takeWhile)
> import Control.Applicative
> import Control.Monad
> import Data.Attoparsec.Text
> import Data.Attoparsec.Combinator
> import Data.Char
> import Data.Functor.Compose
> import Data.Text (Text,unpack,pack)

a few individual parsers for the label, the day of week and time range components;

> label :: Parser Text
> label = skipSpace *> takeWhile1 (\c -> not (isEndOfLine c) && (isAlphaNum c || isSpace c)) <* endOfLine
>
> dayOfWeekP :: Parser Text
> dayOfWeekP = skipSpace *> choice ["Mo","Tu","We","Th","Fr","Sa","Su"]
> 
> hourRangeP :: Parser (Int,Int)
> hourRangeP = do
>   skipSpace
>   x <- decimal
>   guard (x >= 0 && x <= 23)
>   char '-'
>   y <- decimal
>   guard (y >= 0 && y <= 23 && x < y)
>   return (x,y)

and, in keeping with good coding, a full parser is created using combinations of the above.

> schedule :: Parser [(Text,[Text],[(Int,Int)])]
> schedule = many $
>   (,,)
>   <$> label
>   <*> (dayOfWeekP `sepBy1` (skipSpace *> char ','))
>   <*> (hourRangeP `sepBy1` (skipSpace *> char ','))

You try it out.

> sched1 :: Text
> sched1 = "Meet with CTO\n\
>          \Mo,We,Fr 10-12, 15-16\n\
>          \\n\
>          \Lunch\n\
>          \Mo,Tu,We,Tu,Fr 12-13"

    [ghci]
    let Right s = parseOnly schedule sched1
    mapM_ print s

I don't want to write another interpreter!
-----------------------------------------

Nice. Technically, you could write a converter to take the output of
this parser and convert it to a schedule. That's just a whole lot of
double work. Because, you essentially end up writing another parser --
only this time, it parses a data structure. So, let's have a look at our
humble friend from the last post that allowed us to hang `IO`
actions within a `Schedule r`. Surely, what we want is to hang
`Schedule r` within a `Parser`!

> type ParserS = Compose Parser (Schedule DT)
> 
> (<$$>) :: (b -> Schedule DT a) -> Parser b -> ParserS a
> f <$$> p = Compose $ f <$> p
> 
> liftP :: Parser a -> ParserS a
> liftP = (<$$>) pure

Here's the new schedule parser,

> scheduleS :: ParserS [Text]
> scheduleS = fmap (foldr (++) []) . many $
>   (\l a b -> if a && b then [l] else [])
>   <$> liftP label
>   <*> fmap or (dayOfWeekS `sepBy1` (liftP $ skipSpace *> char ','))
>   <*> fmap or (hourRangeS `sepBy1` (liftP $ skipSpace *> char ','))
>   where dayOfWeekS = (fmap (snd.fst) . dayOfWeek . unpack) <$$> dayOfWeekP
>         hourRangeS = (uncurry hourRange) <$$> hourRangeP
>
> hourRange :: Int -> Int -> Schedule DT Bool
> hourRange i j = (\(_,(a,_)) (_,(b,_)) -> a > b)
>   <$> arbitraryRange (fromIntegral i*3600) (24*3600)
>   <*> arbitraryRange (fromIntegral j*3600) (24*3600)

et voila!

    [ghci]
    let Right s = parseOnly (getCompose scheduleS) sched1
    :t s
    pretty . filter (not . null . fst) $ runSchedule s (0,7*24*3600)

There are no problems with overlapping events either.

> sched2 :: Text
> sched2 = "Meet with CTO\n\
>          \Mo,We,Fr 11-13, 15-16\n\
>          \\n\
>          \Lunch\n\
>          \Mo,Tu,We,Tu,Fr 12-13"

    [ghci]
    let Right s = parseOnly (getCompose scheduleS) sched2
    pretty . filter (not . null . fst) $ runSchedule s (0,7*24*3600)

Next time
---------

I've often been under the impression that `Applicative`'s are pretty
boring; I tend to spend no time with them as I tuck into `Monad`s
straight way; so, I'll leave myself and you, the reader, with a
collection of great blog-posts and Haskell libraries on this topic
that are well worth the read. Next time, I'll go back to the problem of
leap years and general constraint specification.

* Gabriel Gonzalez: [Using Applicative and Alterative to model database table joins](http://www.haskellforall.com/2014/12/a-very-general-api-for-relational-joins.html)
* A masterclass (using folds) not only in Applicative but also in treating computations as primitives:
    - Conal Elliott: [Another lovely example of type class morphisms](http://conal.net/blog/posts/another-lovely-example-of-type-class-morphisms)
    - Conal Elliott: [More beautiful fold zipping](http://conal.net/blog/posts/more-beautiful-fold-zipping)
    - Gabriel's post: [Composable streaming folds](http://www.haskellforall.com/2013/08/composable-streaming-folds.html)
* Paolo Capriotti: [Applicative is thoroughly embraced in this package](http://hackage.haskell.org/package/optparse-applicative)
* Of course, I can't list posts on abstractions without Mr. Edward Kmett's input on this matter: [Abstracting With
Applicatives](https://web.archive.org/web/20140630054823/http://comonad.com/reader/2012/abstracting-with-applicatives/)

There are many more in the back of my head; I'll add them here as I
recall them. Meanwhile, please leave your links in the comments below!
