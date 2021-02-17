# fruitbar
API to manage consumption data for delicious fruit bars

## Getting Started
```stack run```

## API
`/people` (GET) - List all fruit bar consumers

`/consumptions` (GET) - List all fruit bar consumptions

`/consumptions` (POST) - Add a consumption

`/consumptions/streaks` (GET) - List lists of days of increasing consumption

## Tech
Haskell service utilizing [Scotty](https://hackage.haskell.org/package/scotty), [Persistent](https://hackage.haskell.org/package/persistent), and [Aeson](https://hackage.haskell.org/package/aeson)
