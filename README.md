# effectful-mcp

A server of the [Model Context Protocol](https://modelcontextprotocol.io)
(MCP), revision 2026-07-28, for
[Effectful](https://hackage.haskell.org/package/effectful): declare tools as
`Eff` actions, and serve them over stdio.

```haskell
import Data.Aeson
import Data.Text (Text)
import Effectful
import Effectful.Concurrent (runConcurrent)
import Effectful.MCP.Server
import Effectful.MCP.Tool

newtype Greeting = Greeting Text

instance FromJSON Greeting where
  parseJSON = withObject "Greeting" $ \fields -> Greeting <$> fields .: "name"

main :: IO ()
main =
  runEff . runConcurrent . serveStdio $
    serverConfig
      (implementation "greeter" "0.1.0")
      [ tool "greet" "Greet someone" schema $
          withArguments (\(Greeting name) -> pure (textResult ("Hello, " <> name)))
      ]
  where
    schema =
      object
        [ "type" .= ("object" :: Text)
        , "properties" .= object ["name" .= object ["type" .= ("string" :: Text)]]
        , "required" .= ["name" :: Text]
        ]
```

## What it does

- Revision 2026-07-28 only: every request carries its protocol version and
  the capabilities of the client in `_meta`, and is served on its own.
  `initialize`, the handshake of earlier revisions, gets an
  `UnsupportedProtocolVersionError` naming the version served. Clients that
  speak both eras, such as current versions of Claude Code, probe with
  `server/discover` first and use the modern revision.
- `server/discover`, `tools/list` (in a fixed order, with cache hints) and
  `tools/call`.
- Tool failures, including arguments that do not decode, come back as
  results with `isError`, which the model can act on; protocol errors use
  the JSON-RPC codes of the specification.
- Requests are served concurrently; `notifications/cancelled` stops one.
  When the input ends, running requests get a grace period to finish.
- Not yet: resources, prompts, subscriptions, multi round-trip requests,
  progress, the Streamable HTTP transport.

Requests run in threads of their own, forked as Effectful forks them: a tool
that changes a local `State` changes its own copy. Use shared effects for
state that must outlive a call.

Tested with GHC 9.12.4, effectful 2.7.1, streamly-core 0.3.3.

## License

BSD-3-Clause. See [LICENSE](LICENSE).
