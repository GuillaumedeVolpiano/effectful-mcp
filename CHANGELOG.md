# Revision history for effectful-mcp

## 0.1.1.0 -- unreleased

* The server also speaks the earlier revisions of the protocol, to the
  clients that start with `initialize`: `handleLegacyRequest`,
  `legacyVersions`. Both eras are served on one connection.

## 0.1.0.0 -- 2026-11-21

* MCP server, revision 2026-07-28, over stdio: `server/discover`,
  `tools/list` and `tools/call`, concurrent requests and cancellation.
* Tools as `Eff` actions, with text, image or structured results.
