# Revision history for effectful-mcp

## 0.1.1.1 -- 2026-11-27

* Builds with GHC 9.8 and 9.10 as well as 9.12: the language is GHC2021
  with the extensions GHC2024 adds, and the bounds on `base` and on the
  libraries that come with the compiler are wider.
* Every exported name is documented.
* The links of the documentation say which name they mean, where a type
  and its constructor, or two modules, share one.

## 0.1.1.0 -- 2026-11-21

* The server also speaks the earlier revisions of the protocol, to the
  clients that start with `initialize`: `handleLegacyRequest`,
  `legacyVersions`. Both eras are served on one connection.

## 0.1.0.0 -- 2026-11-21

* MCP server, revision 2026-07-28, over stdio: `server/discover`,
  `tools/list` and `tools/call`, concurrent requests and cancellation.
* Tools as `Eff` actions, with text, image or structured results.
