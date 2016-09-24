    [BLOpts]
    profile    = nanonaren
    postid     = 809
    title      = "More Time Utilities (13/365)"
    tags       = daily, haskell
    categories =

Suppose you want to add one second to the current date. One approach
is to convert it to unix-time (seconds from epoch), add $1$, and then
convert it back. The cost of converting is a little too high if we
frequently perfom this wrapping and unwrapping (technically we could
catch `unwrap . wrap` using rules and fuse it to `id`). Furthermore,
this method doesn't necessarily help if we want to perform more
complex tasks like the following: given a date range pick out the
sub-ranges that match mondays. For this purpose, the original
representation of year, month, and day is more helpful.

I have written some utilities to make these tasks simpler. Today, I
will introduce the date-time representation and then provide a way to
tick the date-time forward by one second or to tick it back by one
second and then in the next post present a matching DSL.

> import Text.Printf
> import Data.Time.Calendar          (fromGregorianValid)
> import Data.Time.Calendar.WeekDate (toWeekDate)
> import Control.Monad               (guard, when)
>
> data DT = DT
>   {
>     year  :: {-# UNPACK #-} !Int
>   , month :: {-# UNPACK #-} !Int
>   , day   :: {-# UNPACK #-} !Int
>   , dow   :: {-# UNPACK #-} !Int
>   , tod   :: {-# UNPACK #-} !Int
>   }

Some instances

> instance Eq DT where
>   dt == dt' = tod dt == tod dt' &&
>               day dt == day dt' &&
>               month dt == month dt' &&
>               year dt == year dt'
>
> instance Ord DT where
>   compare dt dt' =
>     compare (year dt,month dt,day dt,tod dt)
>             (year dt',month dt',day dt',tod dt')
>
> instance Show DT where
>   show dt = printf "%d-%02d-%02d %02d:%02d:%02d"
>                    (year dt) (month dt) (day dt)
>                    (tod dt `div` 3600) (((tod dt-s) `div` 60) `mod` 60) s
>     where s = tod dt `mod` 60

For constructing it

> toDT :: Int -> Int -> Int -> Int -> Int -> Int -> Either String DT
> toDT year month day hour min sec = do
>   dayObj <- maybe (Left "Invalid Year/Month/Day") return $ fromGregorianValid (fromIntegral year) month day
>   when (0 > hour || hour > 23) $ Left "Invalid Hour"
>   when (0 > min  || min  > 59) $ Left "Invalid Minute"
>   when (0 > sec  || sec  > 59) $ Left "Invalid Second"
>   let (_,_,dow) = toWeekDate dayObj
>   return $ DT year month day (dow-1) (hour*3600 + min*60 + sec)

So far we have

    [ghci]
    toDT 2016 08 02 11 32 21
    toDT 2016 06 31 11 32 21
    toDT 2016 08 02 11 32 21 > toDT 2016 08 02 11 32 19

Finally, the ability to `tick` and `untick` the date-time.

> tick :: DT -> DT
> tick = tickTOD
>   where tickTOD (dt@DT{tod=s}) =
>           let dt' = if s < secsInDay-1
>                     then dt{ tod = s+1 }
>                     else dt{ tod = 0 }
>           in if s==(secsInDay-1) then tickDay dt' else dt'
>
>         tickDay (dt@DT{day=d,dow=dayOfWeek,month=m,year=y}) =
>           let dt' = dt{ dow = if dayOfWeek < 6 then dayOfWeek+1 else 0
>                       , day = day'}
>               day' = if d < 27
>                      then d+1
>                      else if d == numDays y m
>                           then 1
>                           else d+1
>           in if day'==1 then tickMonth dt' else dt'
>
>         tickMonth (dt@DT{month=m}) =
>           let dt' = dt{ month = if m < 12 then m+1 else 1}
>           in if m==12 then tickYear dt' else dt'
>
>         tickYear (dt@DT{year=y}) = dt{year = y+1}
>
>
> untick :: DT -> DT
> untick = untickTOD
>   where untickTOD (dt@DT{tod=s}) =
>           let dt' = if s == 0
>                     then dt{ tod = secsInDay-1 }
>                     else dt{ tod = s-1 }
>           in if s==0 then untickDay dt' else dt'
>
>         untickDay (dt@DT{day=d,dow=dayOfWeek,month=m,year=y}) =
>           let dt' = dt{ dow = if dayOfWeek == 0 then 6 else dayOfWeek-1
>                       , day = day'}
>               day' = if d == 1
>                      then if m==1 then numDays (y-1) 12 else numDays y (m-1)
>                      else d - 1
>           in if d==1 then untickMonth dt' else dt'
>
>         untickMonth (dt@DT{month=m}) =
>           let dt' = dt{ month = if m == 1 then 12 else m-1}
>           in if m==1 then untickYear dt' else dt'
>         untickYear (dt@DT{year=y}) = dt{year = y-1}
>
> numDays :: Int -> Int -> Int
> numDays y m
>   | m == 2 = if (mod y 4 == 0) && ((mod y 400 == 0) || not (mod y 100 == 0))
>              then 29
>              else 28
>   | m == 1 || m == 3 || m == 5 || m == 7 || m == 8 || m == 10 || m == 12 = 31
>   | otherwise = 30
>
> secsInDay :: Int
> secsInDay = 86400

Examples

    [ghci]
    let Right d = toDT 2016 08 02 11 32 21
    mapM_ (print . head) $ take 10 $ iterate (drop 10000) $ iterate tick d

    let Right d = toDT 2016 08 03 12 32 21
    mapM_ (print . head) $ take 10 $ iterate (drop 10000) $ iterate untick d
