// (c) Meta Platforms, Inc. and affiliates. Confidential and proprietary.

// OSS stub for glean/if/facebook/auth.thrift. The real internal version lives
// at fbcode/glean/if/facebook/auth.thrift and carries the CryptoAuth Token
// (CAT) authentication surface used by Meta's internal Glean deployment.
//
// The OSS build resolves `include "glean/if/facebook/auth.thrift"` from
// glean/if/glean.thrift and glean/glass/if/glass.thrift via the OSS include
// path to this stub, which mirrors the same enum + constant so that
// downstream OSS consumers can build and decode the new optional response
// fields without pulling in any internal CAT machinery. The fields are
// always UNSET in the OSS build because no CAT verification pipeline runs.
//
// Mirrors the internal definition (same enum values, same constant string)
// so old/new clients interop on the wire.
//
// NOTE: Unlike the internal source, this OSS stub uses an explicit empty
// `package;` instead of declaring `package "meta.com/glean/auth"`. The
// implicit Thrift URI must be owned by exactly one non-synced file — the
// internal fbcode/glean/if/facebook/auth.thrift owns it. Declaring the same
// URI here would trip the duplicate Thrift URI linter (this stub is a manual
// copy, not a synced copy). The empty `package;` matches the existing
// glean/github/if/fb303_core.thrift stub convention: it satisfies the
// "package required" lint without declaring a URI, and does not affect wire
// format or codegen symbols.
package;

namespace cpp2 facebook.glean.auth
namespace java.swift com.facebook.glean.auth
namespace hs Glean.Auth
namespace php glean_auth
namespace py glean.auth
namespace py3 glean.auth

const string kCryptoAuthTokensHeader = "crypto_auth_tokens";

enum AuthStatus {
  UNSET = 0,
  FULL = 1,
  FILTERED = 2,
  NONE = 3,
  FAILED = 4,
} (hs.nounknown)
