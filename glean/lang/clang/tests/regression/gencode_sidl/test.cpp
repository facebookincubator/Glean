/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

/** Glean {"idl":"sidl","file":"xplat/arfx/osf/core/sidl/BindParams.sidl","name":"BindParams","kind":"struct","line":9,"column":0,"endLine":16,"endColumn":1} */
struct BindParams {
  /** Glean {"idl":"sidl","file":"xplat/arfx/osf/core/sidl/BindParams.sidl","name":"targetVersion","kind":"field","line":11,"column":2,"endLine":11,"endColumn":38} */
  int targetVersion;
};

/** Glean {"idl":"sidl","file":"xplat/arfx/osf/examples/echo/EchoService.sidl","name":"EchoStringStruct","kind":"struct","line":3,"column":0,"endLine":5,"endColumn":1} */
struct EchoStringStruct {};

// Clang hands Baz the comment above Bar, which is skipped because it names another enumerator.
/** Glean {"idl":"sidl","file":"xplat/arfx/osf/examples/echo/EchoService.sidl","name":"EchoEnum","kind":"enum","line":13,"column":0,"endLine":17,"endColumn":1} */
enum class EchoEnum {
  /** Glean {"idl":"sidl","file":"xplat/arfx/osf/examples/echo/EchoService.sidl","name":"Foo","kind":"enumerator","line":14,"column":2,"endLine":14,"endColumn":5} */
  Foo = 0,
  /** Glean {"idl":"sidl","file":"xplat/arfx/osf/examples/echo/EchoService.sidl","name":"Bar","kind":"enumerator","line":15,"column":2,"endLine":15,"endColumn":9} */
  Bar = 1,
  Baz = 10,
};

/** Glean {"idl":"sidl","file":"xplat/arfx/osf/core/sidl/NodeType.sidl","name":"NodeType","kind":"enum","line":5,"column":0,"endLine":12,"endColumn":1} */
enum class NodeType {
  Phone = 0,
  Glasses = 1,
};

/** Glean {"idl":"sidl","file":"xplat/arfx/osf/examples/echo/EchoService.sidl","name":"Millis","kind":"alias","line":20,"column":0,"endLine":20,"endColumn":21} */
using Millis = long;

/** Glean {"idl":"sidl","file":"xplat/arfx/osf/sidl/src/__tests__/snapshots/alias_primitive/alias_primitive.sidl","name":"Timestamp","kind":"alias","line":4,"column":0,"endLine":4,"endColumn":25} */
using Timestamp = unsigned long long;

/** Glean {"idl":"sidl","file":"xplat/arfx/osf/examples/echo/EchoService.sidl","name":"kLimit","kind":"const","line":22,"column":0,"endLine":22,"endColumn":23} */
inline constexpr int kLimit = 7;

/** Glean {"idl":"sidl","file":"xplat/arfx/osf/examples/const_cross_package/dep_consts.sidl","name":"kServiceId","kind":"const","line":5,"column":0,"endLine":5,"endColumn":29} */
inline constexpr unsigned short kServiceId = 42;

/** Glean {"idl":"sidl","file":"xplat/arfx/osf/examples/echo/EchoService.sidl","name":"IEchoService","kind":"interface","line":24,"column":0,"endLine":27,"endColumn":1} */
struct IEchoService {
  /** Glean {"idl":"sidl","file":"xplat/arfx/osf/examples/echo/EchoService.sidl","name":"echo","kind":"method","line":25,"column":2,"endLine":25,"endColumn":30} */
  virtual int echo(int x);
  /** Glean {"idl":"sidl","file":"xplat/arfx/osf/examples/echo/EchoService.sidl","name":"echoString","kind":"method","line":23,"column":2,"endLine":23,"endColumn":47} */
  virtual int echoString(int x);
  virtual int unannotated(int x);
};

/** Glean {"idl":"sidl","file":"xplat/arfx/osf/examples/echo/EchoService.sidl","name":"IEchoCallback","kind":"interface","line":7,"column":0,"endLine":11,"endColumn":1} */
struct IEchoCallback {};

/** Glean {"idl":"sidl","file":"xplat/arfx/osf/core/sidl/BindParams.sidl","name":"HasFields","kind":"struct","line":40,"column":0,"endLine":46,"endColumn":1} */
struct HasFields {
  /** Glean {"idl":"sidl","file":"xplat/arfx/osf/core/sidl/BindParams.sidl","name":"version","kind":"field","line":41,"column":2,"endLine":41,"endColumn":14} */
  int version;
  /** Glean {"idl":"sidl","file":"xplat/arfx/osf/core/sidl/BindParams.sidl","name":"flags","kind":"field","line":42,"column":2,"endLine":42,"endColumn":12} */
  int flags;
  int plain;
};

/** Glean {"idl":"sidl","file":"x.sidl","name":"Broken","kind":"struct"} */
struct Broken {};

/** Glean {"idl":"sidl","file":"x.sidl","name":"Mystery","kind":"spaceship","line":1,"column":0,"endLine":2,"endColumn":1} */
struct Mystery {};

/** Glean {"idl":"sidl", nope} */
struct NotJson {};

/** Glean {"file": "some/module.thrift", "service": "MyService", "function": "do_it"} */
void generated_from_thrift();

enum class EnumeratorNegatives {
  /** Glean {"idl":"sidl", nope} */
  NotJsonVal = 0,
  /** Glean {"idl":"sidl","file":"x.sidl","name":"MysteryVal","kind":"spaceship","line":15,"column":2,"endLine":15,"endColumn":16} */
  MysteryVal = 1,
  /** Glean {"idl":"sidl","file":"x.sidl","name":"NoRangeVal","kind":"enumerator"} */
  NoRangeVal = 2,
  /** Glean {"idl":"sidl","file":"x.sidl","name":"KeptVal","kind":"enumerator","line":17,"column":2,"endLine":17,"endColumn":13} */
  KeptVal = 3,
};

enum class MisnamedEnum {
  /** Glean {"idl":"sidl","file":"x.sidl","name":"UnrelatedVal","kind":"enumerator","line":12,"column":2,"endLine":12,"endColumn":14} */
  MisnamedVal = 0,
};

/** Glean {"idl":"sidl","file":"x.sidl","name":"Multi","kind":"struct","line":3,"column":0,"endLine":4,"endColumn":1} Glean {"idl":"other","file":"y"} */
struct Multi {};

enum class MultiEnum {
  /** Glean {"idl":"sidl","file":"x.sidl","name":"MultiVal","kind":"enumerator","line":5,"column":2,"endLine":5,"endColumn":10} Glean {"idl":"other","file":"y"} */
  MultiVal = 0,
};

/** Glean {"idl":"other","file":"y"} Glean {"idl":"sidl","file":"x.sidl","name":"MultiSecond","kind":"struct","line":6,"column":0,"endLine":7,"endColumn":1} */
struct MultiSecond {};

enum class MultiSecondEnum {
  /** Glean {"idl":"other","file":"y"} Glean {"idl":"sidl","file":"x.sidl","name":"MultiSecondVal","kind":"enumerator","line":8,"column":2,"endLine":8,"endColumn":16} */
  MultiSecondVal = 0,
};

/** Glean {"idl":"sidl","file":"x.sidl","name":"Nested","kind":"struct","extra":{"opts":[{"k":1,"v":"}","p":"a\\"}],"tags":["{","Glean"]},"note":"}{\"}","line":10,"column":0,"endLine":11,"endColumn":1} */
struct Nested {};

enum class NestedEnum {
  /** Glean {"idl":"sidl","file":"x.sidl","name":"NestedVal","kind":"enumerator","extra":{"opts":[{"k":1,"v":"}","p":"a\\"}],"tags":["{","Glean"]},"note":"}{\"}","line":13,"column":2,"endLine":13,"endColumn":11} */
  NestedVal = 0,
};

// Stand-ins for the OSF runtime types the generator output below names.
using int16_t = short;
using int32_t = int;
using uint32_t = unsigned int;
#define ARFX_FORCE_CONSTEXPR_CTTID(type) 0

namespace arfx {
using cttid_t = unsigned long long;
}

namespace arfx::osf {
struct InterfaceId {
  unsigned long long value;
};
using ServiceId = InterfaceId;
constexpr InterfaceId createInterfaceId(const char*) {
  return InterfaceId{0};
}
template <typename T>
class FutureStatusOr {};
struct ObjectId {};
struct ConnectionContext {};
class IStructCodec;
class EndpointConnection;
class IService {
 public:
  virtual ~IService() = default;
  virtual ::arfx::cttid_t typeId() const noexcept = 0;
};
class RemoteClientBase {
 public:
  virtual ~RemoteClientBase() = default;
  virtual EndpointConnection* hostingEndpointConnection() noexcept;
};
}

namespace arfx::osf::examples::default_values {
class DefaultValuesHandlerClientMethodsHandler {};
}

// Copied from xplat/arfx/osf/sidl/src/__tests__/snapshots/docComments/cpp-out, section cpp-out/types/Types.hpp.
namespace arfx::osf::examples::doccomments {

struct Sample;
class ISampleListener;
class ISampleService;

/** Glean {"idl":"sidl","file":"xplat/arfx/osf/sidl/src/__tests__/snapshots/docComments/docComments.sidl","name":"ReadError","kind":"enum","line":28,"column":0,"endLine":35,"endColumn":1} */
/// Why a read did not return a sample.
///
/// A second paragraph, so the goldens show that blank-line paragraph breaks survive the trip
/// through the IR and into every generated comment style.
enum class ReadError : int16_t {
  /** Glean {"idl":"sidl","file":"xplat/arfx/osf/sidl/src/__tests__/snapshots/docComments/docComments.sidl","name":"NotConnected","kind":"enumerator","line":31,"column":2,"endLine":31,"endColumn":18} */
  /// No sensor is attached to the channel.
  NotConnected = 0,

  /** Glean {"idl":"sidl","file":"xplat/arfx/osf/sidl/src/__tests__/snapshots/docComments/docComments.sidl","name":"NotReady","kind":"enumerator","line":33,"column":2,"endLine":33,"endColumn":31} */
  /// The sensor is attached but still warming up.
  NotReady [[deprecated("since 1.1")]] = 1,
  /** Glean {"idl":"sidl","file":"xplat/arfx/osf/sidl/src/__tests__/snapshots/docComments/docComments.sidl","name":"Unknown","kind":"enumerator","line":34,"column":2,"endLine":34,"endColumn":13} */
  Unknown = 2
};

/** Glean {"idl":"sidl","file":"xplat/arfx/osf/sidl/src/__tests__/snapshots/docComments/docComments.sidl","name":"kMaxSamples","kind":"const","line":15,"column":0,"endLine":15,"endColumn":29} */
/// Largest number of samples a caller may ask for at once.
inline constexpr int32_t kMaxSamples = 16;

/** Glean {"idl":"sidl","file":"xplat/arfx/osf/sidl/src/__tests__/snapshots/docComments/docComments.sidl","name":"kUndocumentedLimit","kind":"const","line":17,"column":0,"endLine":17,"endColumn":35} */
inline constexpr int32_t kUndocumentedLimit = 4;

}

// Copied from xplat/arfx/osf/sidl/src/__tests__/snapshots/flagsToolCalls/cpp-out, section cpp-out/types/Types.hpp.
namespace arfx::osf::examples::flags_tools {

struct GrantResult;
class IPermissionService;

/** Glean {"idl":"sidl","file":"xplat/arfx/osf/sidl/src/__tests__/snapshots/flagsToolCalls/PermissionService.sidl","name":"Permission","kind":"enum","line":5,"column":0,"endLine":9,"endColumn":1} */
enum class Permission : uint32_t {
  None = 0x0,
  /** Glean {"idl":"sidl","file":"xplat/arfx/osf/sidl/src/__tests__/snapshots/flagsToolCalls/PermissionService.sidl","name":"Read","kind":"enumerator","line":6,"column":2,"endLine":6,"endColumn":15} */
  Read = 0x01,
  /** Glean {"idl":"sidl","file":"xplat/arfx/osf/sidl/src/__tests__/snapshots/flagsToolCalls/PermissionService.sidl","name":"Write","kind":"enumerator","line":7,"column":2,"endLine":7,"endColumn":15} */
  Write = 0x02,
  /** Glean {"idl":"sidl","file":"xplat/arfx/osf/sidl/src/__tests__/snapshots/flagsToolCalls/PermissionService.sidl","name":"Admin","kind":"enumerator","line":8,"column":2,"endLine":8,"endColumn":15} */
  Admin = 0x80,
};

/** Glean {"idl":"sidl","file":"xplat/arfx/osf/sidl/src/__tests__/snapshots/flagsToolCalls/PermissionService.sidl","name":"GrantResult","kind":"struct","line":14,"column":0,"endLine":18,"endColumn":1} */
/// What the grant produced.
struct GrantResult {
  /// The set now held
  /** Glean {"idl":"sidl","file":"xplat/arfx/osf/sidl/src/__tests__/snapshots/flagsToolCalls/PermissionService.sidl","name":"granted","kind":"field","line":17,"column":2,"endLine":17,"endColumn":24} */
  ::arfx::osf::examples::flags_tools::Permission granted = {};
};

}

// Copied from xplat/arfx/osf/sidl/src/__tests__/snapshots/defaultValues/cpp-out, section cpp-out/types/Types.hpp, structs left out.
namespace arfx::osf::examples::default_values {

struct DefaultValuesStruct;
struct LargeDefaultsStruct;
struct LiteralSpellingDefaultsStruct;
struct MixedDefaultsStruct;
struct InnerDefaults;
struct OuterWithNested;
struct EnumDefaultsStruct;
struct DefaultInitStruct;
struct LiteralDefaultsStruct;
class IDefaultValuesHandler;

/** Glean {"idl":"sidl","file":"xplat/arfx/osf/sidl/src/__tests__/snapshots/defaultValues/defaultValues.sidl","name":"IDefaultValuesHandler","kind":"interface","line":82,"column":0,"endLine":85,"endColumn":1} */
class IDefaultValuesHandler : public ::arfx::osf::IService {
 public:
  constexpr static ::arfx::osf::InterfaceId kInterfaceId = createInterfaceId("arfx.osf.examples.default_values.IDefaultValuesHandler");
  /// Compat alias for existing callers.
  constexpr static ::arfx::osf::ServiceId kServiceId = kInterfaceId;

  [[nodiscard]] ::arfx::cttid_t typeId() const noexcept final;

  /** Glean {"idl":"sidl","file":"xplat/arfx/osf/sidl/src/__tests__/snapshots/defaultValues/defaultValues.sidl","name":"getDefaults","kind":"method","line":83,"column":2,"endLine":83,"endColumn":39} */
  virtual ::arfx::osf::FutureStatusOr<::arfx::osf::examples::default_values::DefaultValuesStruct> getDefaultsAsync() noexcept = 0;
  /** Glean {"idl":"sidl","file":"xplat/arfx/osf/sidl/src/__tests__/snapshots/defaultValues/defaultValues.sidl","name":"echoDefaults","kind":"method","line":84,"column":2,"endLine":84,"endColumn":65} */
  virtual ::arfx::osf::FutureStatusOr<::arfx::osf::examples::default_values::DefaultValuesStruct> echoDefaultsAsync(const ::arfx::osf::examples::default_values::DefaultValuesStruct& input) noexcept = 0;
};

[[nodiscard]] inline ::arfx::cttid_t IDefaultValuesHandler::typeId() const noexcept {
  return ARFX_FORCE_CONSTEXPR_CTTID(IDefaultValuesHandler);
}

}

// Copied from xplat/arfx/osf/sidl/src/__tests__/snapshots/defaultValues/cpp-out, section cpp-out/protocol/common/RemoteInterfaces.hpp.
namespace arfx::osf::examples::default_values {

class RemoteDefaultValuesHandler : public ::arfx::osf::examples::default_values::IDefaultValuesHandler, public ::arfx::osf::RemoteClientBase {
 public:
  RemoteDefaultValuesHandler(::arfx::osf::ObjectId objectId,
    ::arfx::osf::ConnectionContext connCtx,
    const ::arfx::osf::IStructCodec& codec) noexcept;
  ~RemoteDefaultValuesHandler() override;

  RemoteDefaultValuesHandler(RemoteDefaultValuesHandler&& other) noexcept;
  RemoteDefaultValuesHandler& operator=(RemoteDefaultValuesHandler&&) = delete;

  ::arfx::osf::FutureStatusOr<::arfx::osf::examples::default_values::DefaultValuesStruct> getDefaultsAsync() noexcept override;
  ::arfx::osf::FutureStatusOr<::arfx::osf::examples::default_values::DefaultValuesStruct> echoDefaultsAsync(const ::arfx::osf::examples::default_values::DefaultValuesStruct& input) noexcept override;

  [[nodiscard]] ::arfx::osf::EndpointConnection* hostingEndpointConnection() noexcept override {
    return ::arfx::osf::RemoteClientBase::hostingEndpointConnection();
  }

 private:
  const ::arfx::osf::IStructCodec& codec_;
  ::arfx::osf::examples::default_values::DefaultValuesHandlerClientMethodsHandler methodsHandler_;
};

}
