<?hh
/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

class SourceA {
  /**
   * A function
   */
  public async function f(): Awaitable<void> {
    $client = new TestServiceAsyncClient();
    $request = new GetNavigationRequest(); // thrift struct
    $request->identifier = "foo"; // thrift field
    $exception = new GetNavigationException(); // thrift exception
    $exception->message_detail = "bar"; // thrift exception field
    $response = await $client->thrift1($request); // thrift function (thrift1)
    $tab_group = $response->tab_group; // thrift field
  }

  public async function g(): Awaitable<void> {
    $client = new TestServiceAsyncClient();
    $request = new GetCardRequest(); // thrift struct
    $response = await $client->thrift2($request); // thrift function (thrift2)
    $card = $response->card; // thrift struct field
    $_ = $card?->static_card; // thrift union field
    $_ = CountOnlyMode::Disabled; // thrift enum and enum value
  }
}
