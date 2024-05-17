/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

#pragma once

#include "glean/rts/binary.h"
#include "glean/rts/id.h"

namespace facebook {
namespace glean {
namespace rts {

/// Type of functions which can be called from bytecode
using SysFun =
  void(*)(void *context, uint64_t *frame, const uint64_t *regs, uint64_t nregs);

/// A typed bytecode register which can be read from and written to
template<typename T>
struct Reg {
  uint64_t *ptr;

  explicit Reg(uint64_t *ptr = nullptr) : ptr(ptr) {}

  T get() const {
    T x;
    fromWord(x, *ptr);
    return x;
  }

  T operator*() const {
    return get();
  }

  void set(T x) {
    *ptr = toWord(x);
  }

  // This is ugly but not having an operator for this is uglier.
  void operator<<(T x) {
    set(x);
  }

private:
  // Marshalling values from bytecode registers to C++ values and back

  // Everything that is implicitly convertible to and from uint64_t
  template<typename U>
  static std::enable_if_t<std::is_convertible_v<uint64_t, U>, void>
  fromWord(U& x, uint64_t w) {
    x = w;
  }

  template<typename U>
  static std::enable_if_t<std::is_convertible_v<U, uint64_t>, uint64_t>
  toWord(U x) {
    return x;
  }

  // Id and Pid
  template<typename Tag>
  static void fromWord(WordId<Tag>& x, uint64_t w) {
    x = WordId<Tag>::fromWord(w);
  }

  template<typename Tag>
  static uint64_t toWord(WordId<Tag> x) {
    return x.toWord();
  }

  // data pointers
  static void fromWord(const unsigned char *&x, uint64_t w) {
    x = reinterpret_cast<const unsigned char *>(w);
  }

  static uint64_t toWord(const unsigned char *x) {
    return reinterpret_cast<uint64_t>(x);
  }

  // Outputs
  static void fromWord(binary::Output *&x, uint64_t w) {
    x = reinterpret_cast<binary::Output *>(w);
  }

  static void fromWord(const binary::Output *&x, uint64_t w) {
    x = reinterpret_cast<binary::Output *>(w);
  }

  static uint64_t toWord(binary::Output *x) {
    return reinterpret_cast<uint64_t>(x);
  }

  static uint64_t toWord(const binary::Output *x) {
    return reinterpret_cast<uint64_t>(x);
  }

  // SysFun
  static void fromWord(SysFun& x, uint64_t w) {
    x = reinterpret_cast<SysFun>(w);
  }

  static uint64_t toWord(SysFun x) {
    return reinterpret_cast<uint64_t>(x);
  }
};

template<typename T>
inline std::ostream& operator<<(std::ostream& os, const Reg<T>& reg)
{
    return (os << reg.get());
}

/// Arguments to syscalls.
///
/// The primary template handles inputs which are passed by value
template<typename T>
struct SysArg {
  using type = T;
  static T arg(uint64_t* ptr) {
    return Reg<T>(ptr).get();
  }
};

// Outputs, i.e., registers which can be written to
template<typename T>
struct SysArg<Reg<T>> {
  using type = Reg<T>;
  static Reg<T> arg(uint64_t *ptr) {
    return Reg<T>(ptr);
  }
};

/// Typed wrapper over a function which can be invoked from bytecode
template<typename... Args>
struct SysCall {
  static constexpr size_t arity = sizeof...(Args);
};

/// Utility class for constructing 'SysCall's from an object and a method.
template<typename C, auto F, typename R, typename... Args>
struct SysHandlerMethod {
  using syscall_t = std::conditional_t<
    std::is_void_v<R>,
    SysCall<Args...>,
    SysCall<Args..., Reg<R>>>;

  static void call(
      void *context,
      uint64_t *frame,
      const uint64_t *regs,
      uint64_t nregs) {
    assert(nregs == syscall_t::arity);

    // Here, we invoke `f` with the marshalled arguments and if it returns a
    // result, marshal it back. These are the steps.
    //
    //   * Build an `std::tuple` of all arguments to `F`, i.e., a pointer to the
    //     context and all the marshalled args. We do via a tuple mostly because
    //     we need to know the index for each arg and this seems easiest.
    //
    //   * Use 'std::apply' to apply `F` to the arg tuple.
    //
    //   * If `F` isn't void, stored its result in the result register.

    // Brace initialisation guarantees that individual components are
    // evaluated from left to right (as opposed to function arguments)
    // - meaning that the multiple increments of `regs` are well defined.
    std::tuple<C&, Args...> args{
      *static_cast<std::remove_reference_t<C>*>(context),
      SysArg<Args>::arg(&frame[*regs++])...
    };

    if constexpr (std::is_void_v<R>) {
      std::apply(F, args);
    } else {
      Reg<R>(&frame[*regs]).set(std::apply(F, args));
    }
  }
};

template<typename C, auto F> struct SysHandler;

template<typename C, typename D, typename R, typename... Args, R (D::*F)(Args...)>
struct SysHandler<C,F> : SysHandlerMethod<C, F, R, Args...> {};

template<typename C, typename D, typename R, typename... Args, R (D::*F)(Args...) const>
struct SysHandler<C,F> : SysHandlerMethod<C, F, R, Args...> {};

/// A collection of system call handlers
template<typename Context, typename... Handlers>
class SysCalls {
public:
  SysCalls(Context&& context, const uint64_t *handlers)
    : context(std::forward<Context>(context)),
      handlers(handlers)
    {}

  void *contextptr() const {
    return const_cast<void *>(static_cast<const void *>(&context));
  }

  const uint64_t *handlers_begin() const {
    return handlers;
  }

  const uint64_t *handlers_end() const {
    return handlers + sizeof...(Handlers);
  }

private:
  Context context;
  const uint64_t *handlers;
};

/// Construct a 'SysCall' from an object and a method. The object can be passed
/// either by value/move reference (in which case it is captured in the
/// 'SysCall' object) or by reference (in which case it must be alive when the
/// 'SysCall' is invoked).
template<auto... Calls, typename Context>
SysCalls<Context, typename SysHandler<Context,Calls>::syscall_t...>
syscalls(Context&& context) {
  // Note that this is static so we don't reconstruct it on each call.
  static const uint64_t handlers[sizeof...(Calls)] = {
    reinterpret_cast<uint64_t>(&SysHandler<Context,Calls>::call)...
  };
  return SysCalls<Context, typename SysHandler<Context,Calls>::syscall_t...>(
    std::forward<Context>(context),
    handlers);
}

/// Construct a 'SysCall' from a functional object. The object can be passed
/// either by value/move reference (in which case it is captured in the
/// 'SysCall' object) or by reference (in which case it must be alive when the
/// 'SysCall' is invoked).
template<typename Context>
auto syscall(Context&& context) {
  return syscalls<&std::remove_reference_t<Context>::operator()>(
    std::forward<Context>(context));
}

}
}
}
