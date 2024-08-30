/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

union ResponseCard {
  1: string static_card;
}

struct GetNavigationRequest {
  1: string identifier;
}

struct GetNavigationResponse {
  1: string tab_group;
}

safe permanent exception GetNavigationException {
  1: string message_detail;
}

struct GetCardRequest {
  1: string identifier;
  2: string card_name;
}

struct GetCardResponse {
  1: ResponseCard card;
}

safe permanent exception GetCardException {
  1: string message_detail;
}

service TestService {
  GetNavigationResponse thrift1(1: GetNavigationRequest request) throws (
    1: GetNavigationException get_navigation_exception,
  );

  GetCardResponse thrift2(1: GetCardRequest request) throws (
    1: GetCardException get_card_exception,
  );
}

struct ReservationHotfixConfig {
  // @lint-ignore thrift-compiler-warning Negative field id is deprecated, don't add new ones.
  -1: map<string, set<string>> capabilities_to_remove = {};
  // @lint-ignore thrift-compiler-warning Negative field id is deprecated, don't add new ones.
  -2: map<string, map<string, i64>> preferences_to_add = {};
}

enum CountOnlyMode {
  Disabled = 0,
  HitCount = 1,
  FileCount = 2,
}

const string ENGINE_STRMATCH = "apr_strmatch";
const string ENGINE_RE2 = "re2";
const string ENGINE_FILENAME = "filename";
const string ENGINE_FUZZY = "fuzzy";
