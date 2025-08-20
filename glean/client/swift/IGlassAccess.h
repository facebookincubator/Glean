/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

#pragma once

#include <optional>
#include <string>
#include <vector>

namespace protocol {

struct Position {
  int line;
  int character;

  Position() : line(0), character(0) {}
  Position(int l, int c) : line(l), character(c) {}
};

struct Range {
  Position start;
  Position end;

  Range() = default;
  Range(const Position& s, const Position& e) : start(s), end(e) {}
};

struct Location {
  std::string uri;
  Range range;

  Location() = default;
  Location(const std::string& u, const Range& r) : uri(u), range(r) {}
};

using LocationList = std::vector<Location>;

} // namespace protocol

class IGlassAccess {
 public:
  virtual ~IGlassAccess() = default;

  virtual std::optional<protocol::LocationList> usrToDefinition(
      const std::string& usr,
      const std::optional<std::string>& revision = std::nullopt) = 0;
};
