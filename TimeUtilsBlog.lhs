    [BLOpts]
    profile    = nanonaren
    postid     = 769
    title      = Dealing With Dates and Times
    tags       = daily, probability
    categories = Haskell

In the past, I've had to deal with reading, writing, and performing
arithmetic on dates and times in Haskell and the experience is not a
pleasant one. Actually, I've had to do this in PHP and Erlang and the
experience is no better.

So, along the way I created some utilities to make life a little
easier. I'll take a few posts to cover them but let me start with the
most common annoyances I've had. They are as follows

1. There is no type to represent seconds since Unix epoch (I'll call this Unix time).
2. There is no simple way to read and write Day, UTC datetime, Local time in the ISO8601 format.
3. Most of all, there is no easy way to convert between UTC time and Local time or Unix time.

Now, there are a few libraries to help you. Most notably, `datetime`
provides many of these utilities but the types are not very
helpful. Believe me when I say a huge chunk of your datetime problems
are solved if only you could do those three things easily and in an
error-free way.

I have a single file
[here](https://github.com/nanonaren/timeutils/blob/master/TimeUtils.hs),
that allows you to do the following. Reading

    [ghci]
    :l TimeUtils.hs
    readISO8601 "2016-05-03" :: Maybe Day
    readISO8601 "2016-05-03" :: Maybe UTCTime
    readISO8601 "2016-05-03T12:05:23" :: Maybe UTCTime
    readISO8601 "2016-05-03T12:05:23" :: Maybe LocalTime
    readISO8601 "2016-05-03T12:05:23Z" :: Maybe UTCTime
    readISO8601 "2016-05-03T12:05:23Z" :: Maybe UnixTime

Writing

    [ghci]
    showISO8601 <$> (readISO8601 "2016-05-03" :: Maybe Day)
    showISO8601 <$> (readISO8601 "2016-05-03T12:05:23Z" :: Maybe UnixTime)

And converting

    [ghci]
    utcToUnixTime <$> readISO8601 "2016-05-03T12:05:23Z"
    localTimeToUnixTime <$> readISO8601 "2016-05-03T12:05:23"
    (unixTimeToUTC . succ) <$> readISO8601 "2016-05-03T12:05:23Z"

For fun

    [ghci]
    let Just t = readISO8601 "2016-05-03T12:05:23Z" :: Maybe UnixTime
    mapM_ (putStrLn . showISO8601) . take 10 . enumFrom $ t
