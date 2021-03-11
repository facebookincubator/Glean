#include "glean/rts/binary.h"

#include <glog/logging.h>
#include <gtest/gtest.h>
#include <string>

namespace facebook {
namespace glean {
namespace rts{

namespace {

void roundtripString(const std::string& s) {
  binary::Output original;
  original.mangleString(binary::byteRange(s));
  binary::Input inp(original.bytes());
  binary::Output demangled;
  inp.demangleUntrustedString(demangled);
  CHECK_EQ(s, binary::mkString(demangled.bytes()));
}

}

using namespace std::string_literals;


TEST(BinaryTest, utf8String) {
  roundtripString(""s);
  roundtripString("abcd"s);
  roundtripString("\0"s);
  roundtripString("abc\0def"s);
  roundtripString("abc\0\0");
}


}
}
}
