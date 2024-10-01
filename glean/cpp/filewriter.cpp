/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include "glean/cpp/filewriter.h"
#include "glean/cpp/glean.h"
#include "glean/cpp/sender.h"
#include "glean/rts/serialize.h"

#include <folly/FileUtil.h>

namespace facebook {
namespace glean {

namespace {

using namespace facebook::glean::cpp;
using namespace facebook::glean::rts;

class FileWriter : public Sender {
 public:
  explicit FileWriter(std::string p) : path(std::move(p)) {}

  void rebaseAndSend(BatchBase&, bool) override {
    // don't do anything
    // NOTE: we ignore the 'wait' flag for now
  }

  void flush(BatchBase& batch) override {
    auto r = batch.serialize();
    binary::Output out;
    // Serialize as facebook::glean::thrift::Batch without using fbthrift. These
    // field numbers and types must match those in glean.thrift.
    serialize::thriftcompact::put(
        out,
        {
            {1, r.first.toThrift()},
            {2, static_cast<int64_t>(r.count)},
            {3, folly::ByteRange(r.facts.data(), r.facts.size())},
            {5, batch.serializeOwnership()},
        });
    folly::writeFile(folly::ByteRange(out.data(), out.size()), path.c_str());
  }

 private:
  std::string path;
};

} // namespace

std::unique_ptr<Sender> fileWriter(std::string path) {
  return std::make_unique<FileWriter>(std::move(path));
}

} // namespace glean
} // namespace facebook
