    [BLOpts]
    profile    = nanonaren
    title      = "Choices: Command Line Parsers"
    tags       = choices, parsers
    categories = Haskell, Choices

[View literate file on Github](https://github.com/nanonaren/blog/blob/master/cmd.lhs)

Like everything in Haskell, parsing command line arguments comes with its own marketplace.

> {-# LANGUAGE DeriveDataTypeable #-}
>
> module Main where
> import Prelude hiding (lines)
> import Options.Applicative
> import System.Environment (getArgs)
> import qualified System.Console.CmdArgs as C
>
> main = error "choose a main<name> to test with"

getArgs
-------

There is no need to pull in extra packages if all you want is to parse
is one or two options (and you're pretty sure it won't grow over
time). The function `getArgs :: IO [String]` parses a list of strings
from the command line (not including the program name).

> mainGetArgs = getArgs >>= mapM_ putStrLn

~~~~{ .bash }
$ ghc --make -o test cmd.lhs
$ ./test hello there "one two" 12 3
hello
there
one two
12
3
~~~~

With `getArgs`, not only do you have to design the structure of
arguments but you also parse them manually into appropriate types.

cmdargs
-------

You will quickly run out of patience with `getArgs` when you need any
amount of flexibility. The package I've most used so far for parsing
needs is `cmdargs`. It makes it quite easy to construct complex interfaces requiring

1. parsing multiple types
2. optional and required flags
3. free arguments
4. multiple modes like git clone, git commit, git ...

The standard procedure with `cmdargs` is to start with algebraic data
types, derive `Data` and `Typeable` instances, and begin annotating
the fields. I mainly want to explore the next package so I'll pull the
example for `cmdargs` from hackage:

> data Sample = Sample {hello :: String} deriving (Show, C.Data, C.Typeable)
> sample = Sample {hello = C.def C.&= C.help "World argument" C.&= C.opt "world"}
>          C.&= C.summary "Sample v1"
> mainCmdArgs = print =<< C.cmdArgs sample

Running `--help` gives

```
$ ./cmd --help
Sample v1

sample [OPTIONS]

Common flags:
  -h --hello[=ITEM]  World argument
  -? --help          Display help message
  -V --version       Print version information
```

optparse-applicative
--------------------

Recently, I discovered yet another package. Unlike `cmdargs`,
`optparse-applicative` provides an applicative interface to
constructing a command-line interface. I have to say that constructing
complex parsers with multiple modes, flags, and other settings is
simpler in this package. These can also be achieved by using the
`System.Console.CmdArgs.Explicit` in `cmdargs` but at the cost of
sacrificing readability and composability.

Let's consider a tiny version of modal git commands *clone* and
*commit*. Here are the data types you might setup for these.

> data Clone = Clone
>     {
>       bare :: Bool
>     , depth :: Int
>     } deriving (Show)
>
> data Commit = Commit
>     {
>       dry_run :: Bool
>     , author :: String
>     } deriving (Show)
>
> data Commands = Commands
>     {
>       clone  :: Clone
>     , commit :: Commit
>     } deriving (Show)

The corresponding parsers are constructed as follows.

> mainOptParse = execParser p_all >>= print
>
> p_all :: ParserInfo Commands
> p_all = info (helper <*> prsr) mod
>     where prsr = Commands
>              -- create clone and commit as subparsers
>              <$> subparser (command "clone" p_clone)
>              <*> subparser (command "commit" p_commit)
>           mod = fullDesc <> footer "footer"
>
> p_commit :: ParserInfo Commit
> p_commit = flip info mod . (helper <*>) $ Commit
>     <$> flag False True
>         (  short 'd'
>         <> long "dry-run"
>         <> help "dry run"
>         )
>     <*> strOption
>         (  short 'a'
>         <> long "author"
>         <> help "override author for commit"
>         <> metavar "<author>"
>         )
>     where mod = fullDesc
>              <> footer "commit footer"
>
> p_clone :: ParserInfo Clone
> p_clone = flip info mod . (helper <*>) $ Clone
>     <$> flag False True
>         (  short 'b'
>         <> long "bare"
>         <> help "create a bare repository"
>         )
>     <*> option
>         (  short 'd'
>         <> long "depth"
>         <> help "create a shallow clone at that depth"
>         <> metavar "<depth>"
>         )
>     where mod = fullDesc
>              <> footer "clone footer"

How does the output look like?

```
$ ./cmd -h
Usage: cmd COMMAND COMMAND

Available options:
  -h,--help                Show this help text

Available commands:
  clone                      commit

footer

$ ./cmd clone
Usage: cmd clone [-b|--bare] (-d|--depth <depth>)

$ ./cmd clone --help
Usage: cmd clone [-b|--bare] (-d|--depth <depth>)

Available options:
  -h,--help                Show this help text
  -b,--bare                create a bare repository
  -d,--depth <depth>       create a shallow clone at that depth

clone footer
```

I think I may end up switching to `optparse-applicative` from now
on. It's true to Haskell and I also like the fact that you do not need
to have `Data` and `Typeable` instances for the data types holding the
arguments.
