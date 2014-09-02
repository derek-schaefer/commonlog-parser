# CommonLog Parser

A CommonLog parser in Haskell.

## Examples

```haskell
parseEvent "127.0.0.1 - frank [10/Oct/2000:13:55:36 -0700] \"GET /apache_pb.gif HTTP/1.0\" 200 2326
```
```haskell
Event {
  remote = "127.0.0.1",
  logName = Nothing,
  authUser = Just "frank",
  timestamp = 2000-10-10 13:55:36 -0700,
  request = "GET /apache_pb.gif HTTP/1.0",
  status = 200,
  bytes = 2326
}
```

## License

Copyright &copy; 2014 Derek Schaefer (<derek.schaefer@gmail.com>)

Licensed under the [MIT License](http://opensource.org/licenses/MIT).
