/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

fn main() {
    #[cfg(not(feature = "facebook"))]
    {
        protobuf_codegen::Codegen::new()
            .pure()
            .include("proto")
            .input("proto/scip.proto")
            .cargo_out_dir("proto_gen")
            .run_from_script();
    }
}
