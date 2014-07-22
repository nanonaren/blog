    [BLOpts]
    profile    = nanonaren
    title      = "Example: Parsing Hackage with Tagsoup"
    tags       = data, concurrency
    categories = haskell

[View literate file on Github](https://github.com/nanonaren/blog/blob/master/tagsoup.lhs)

This post should provide a good example for anyone wondering about
parsing with the [tagsoup](http://hackage.haskell.org/package/tagsoup)
library and why one would choose it over a more traditional approach
to parsing like parsec or attoparsec. Tagsoup is well-documented by
its author [Neil Mitchell](http://community.haskell.org/~ndm/tagsoup/)
along with his other great libraries like *shake*.

What I thought I'd do here is provide a more substantial example and
explanation. This post outputs the list of packages on Hackage along
with its dependencies as a simple tab-separated file. The tasks
include

* scouring Hackage in parallel using simple *http-client* primitives and *monad-par*
* parsing each package using tagsoup
* writing the output file

The output will look like

~~~~
transformers-compose    base    transformers
omaketex                base    optparse-applicative    shakespeare-text        shelly  text
singletons              base    containers      mtl     template-haskell        th-desugar
bindings-levmar         base    bindings-DSL
~~~~

**Note:** This example started as a way for me to gather
document-like data to explore using topic models. It's just fun to
play with.

Getting imports imports out of the way first.

> {-# LANGUAGE OverloadedStrings #-}
>
> module Main
>     (
>       main
>     ) where
>
> import Network.HTTP.Client
> import Text.HTML.TagSoup
> import Data.ByteString.Lazy (ByteString)
> import qualified Data.ByteString.Lazy.Char8 as CB
> import Data.Text.Encoding (decodeUtf8)
> import qualified Data.Text.Lazy as T
> import Control.Monad.Par.Class
> import Control.Monad.Par.IO
> import Control.Monad.IO.Class (liftIO)
> import System.IO (stdout,hSetBuffering,BufferMode(..))
> import Control.Monad ((>=>),forever)
> import Control.Concurrent.MVar
> import Control.DeepSeq (($!!))

Parsing
-------

The first task is to get a list of packages in hackage from
[http://hackage.haskell.org/packages/](http://hackage.haskell.org/packages/). We wish to extract entries that look like

`
<a href="/package/conduit">conduit</a>
`

This is where tagsoup shines because we can completely ignore the rest
of the document structure and simply focus on finding entires such as
this. Invoking `parseTags` parses the document and simply produces a *soup* of
tags decribed by nicely named data constructors -- we just pattern
match on them!

> packages :: ByteString -> [ByteString]
> packages = map (fromAttrib "href") . filter check . parseTags

If we hit an open tag check that it

* is the **a** tag
* has **href** as an attribute
* the href value has prefix **/package/**

>     where check t@(TagOpen _ xs) = isTagOpenName "a" t &&
>                                    not (null xs) &&
>                                    ((=="href") . fst $ head xs) &&
>                                    ("/package/" `CB.isPrefixOf` snd (head xs))
>           check _ = False

Next, we parse the dependencies of an individual package without the
version numbers.

> dependencies :: ByteString -> [ByteString]
> dependencies = parseTags

First, we skip everything in the page for an individual package until
we hit the **Dependencies** section of the file, which is the table
row

`
<tr><th>Dependencies</th><td>...</td>
`

>   & dropWhile (\x -> not $ isTagText x && fromTagText x == "Dependencies")
>   & dropWhile (not . isTagOpenName "td")
>   & takeWhile (not . isTagCloseName "td")

The dependencies are listed as links within the **href** attribute of
an **a** tag as in `<a href="/package/base">base</a>` and we just grab
the name from the link.

>   & filter (isTagOpenName "a")
>   & map (CB.drop 9 . fromAttrib "href")
>     where (&) = flip (.)

Requesting the content
----------------------

We move on to fetching from the pages from the URLs and extracting the
packages using the functions we just defined.

> fetchPackageList :: Manager -> IO [ByteString]
> fetchPackageList manager = do
>   req <- parseUrl "http://hackage.haskell.org/packages/"
>   (packages . responseBody) `fmap` httpLbs req manager

For each package url

* download page
* parse dependencies
* create string `<pkg_name> TAB <dep1> TAB <dep2> ...`
* write to MVar

We perform this action in ParIO, which is from the package
*monad-par*. I'll write about parallelism another time but anyone
wanting to write parallel/concurrent code in Haskell cannot overlook
Simon Marlow's fantastically thorough introduction to [Parallel and
Concurrent Programming in
Haskell](http://chimera.labs.oreilly.com/books/1230000000929).

> fetchPackage :: MVar ByteString -> Manager -> ByteString -> ParIO ()
> fetchPackage mvar manager str = liftIO $ do
>   req <- parseUrl ("http://hackage.haskell.org" ++ (init . tail . show $ str))
>   src <- responseBody `fmap` httpLbs req manager
>   let pkgName = CB.drop 9 str
>   putMVar mvar $!! CB.intercalate "\t" . (pkgName:) . dependencies $ src

Finally, a loop that reads from MVar and writes to `stdout`. We don't
bother killing this thread for this example and just let it die
(rather inelegantly) following the main thread.

> writeIt :: MVar ByteString -> IO ()
> writeIt mvar = forever $ do
>   x <- takeMVar mvar
>   CB.putStrLn x

And, the main function.

> main = do
>   hSetBuffering stdout LineBuffering
>   manager <- newManager defaultManagerSettings
>   ps <- fetchPackageList manager
>   mvar <- newEmptyMVar
>   runParIO $ fork (liftIO $ writeIt mvar) >>
>              mapM_ (fork . fetchPackage mvar manager) ps
>   closeManager manager

Conclusion
----------

I hope this example has made the case for `tagsoup`'s simplicity that
encourages a highly functional coding style resiliant to annoying
little changes that often occur in webpages.

**WARNING**: Output contains repeated packages; to clean duplicates do

`ghc --make -O -o test -threaded tagsoup.lhs` \
`./test +RTS -N4 | sort | uniq > output.txt`
