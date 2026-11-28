# Revision history for effectful-mcp

## 0.1.0.0 -- 2026-11-28

* First release on Hackage.
* An MCP server over stdio: `server/discover`, `tools/list` and
  `tools/call`, concurrent requests and cancellation.
* Revision 2026-07-28 of the protocol, and the earlier revisions for the
  clients that start with `initialize`; both on one connection.
* Tools as `Eff` actions, with text, image or structured results.
