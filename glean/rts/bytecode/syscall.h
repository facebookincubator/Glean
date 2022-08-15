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

  void set(T x) {
    *ptr = toWord(x);
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
  static void fromWord(unsigned char *&x, uint64_t w) {
    x = reinterpret_cast<unsigned char *>(w);
  }

  static uint64_t toWord(unsigned char *x) {
    return reinterpret_cast<uint64_t>(x);
  }

  // Outputs
  static void fromWord(binary::Output *&x, uint64_t w) {
    x = reinterpret_cast<binary::Output *>(w);
  }

  static uint64_t toWord(binary::Output *x) {
    return reinterpret_cast<uint64_t>(x);
  }

  // Also support const data and output pointers
  template<typename U>
  static void fromWord(const U *&x, uint64_t w) {
    U *y;
    fromWord(y,w);
    x = y;
  }

  template<typename U>
  static uint64_t toWord(const U *x) {
    return toWord(const_cast<U *>(x));
  }
};

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

/// Type of functions which can be called from bytecode
using SysFun =
  std::function<void(uint64_t *frame, const uint64_t *regs, uint64_t nregs)>;

/// Typed wrapper over a function which can be invoked from bytecode
template<typename... Args>
class SysCall {
public:
  static constexpr size_t arity = sizeof...(Args);

  explicit SysCall(SysFun fun) : fun(std::move(fun)) {}

  /// Produce a word which can be passed to Call_*_* insns as the function
  /// parameter. The SysCall object must be alive throughout the execution
  /// of the bytecode that uses the function.
  uint64_t toWord() const& {
    return reinterpret_cast<uint64_t>(&fun);
  }

private:
  SysFun fun;
};

template<typename R, typename... Args>
struct SysHandler {
  /// When constructing 'SysCall's from functions which return values, we write
  /// the return value into the last register passed to the call. This is purely
  /// a convenience for our C++ code. This alias produces the correct 'SysCall'
  /// type from the function's return type (which can be `void`) and its
  /// argument types.
  using syscall_t = std::conditional_t<
    std::is_void_v<R>,
    SysCall<Args...>,
    SysCall<Args..., Reg<R>>>;

  template<typename C, typename F>
  static syscall_t syscall(C&& c, F f) {
    // Here, we invoke `f` with the marshalled arguments and if it returns a
    // result, marshal it back. These are the steps.
    //
    //   * Build an `std::tuple` of all arguments to `f`, i.e., a pointer to the
    //     context and all the marshalled args. We do via a tuple mostly because
    //     we need to know the index for each arg and this seems easiest.
    //
    //   * Use 'std::apply' to apply `f` to the arg tuple.
    //
    //   * If `f` isn't void, stored its result in the result register.

    return syscall_t(
      // use tuple for perfect capture (either by value or by reference)
      [ctx = std::tuple<C>(std::forward<C>(c)), f](
          uint64_t *frame,
          const uint64_t *regs,
          uint64_t nregs) mutable {
        assert(nregs == syscall_t::arity);

        // Brace initialisation guarantees that individual components are
        // evaluated from left to right (as opposed to function arguments)
        // - meaning that the multiple increments of `regs` are well defined.
        const std::tuple<C&, Args...> args{
          std::get<0>(ctx),
          SysArg<Args>::arg(&frame[*regs++])...
        };

        if constexpr (std::is_void_v<R>) {
          std::apply(f, args);
        } else {
          Reg<R>(&frame[*regs]).set(std::apply(f, args));
        }
      }
    );
  }
};

/// Construct a 'SysCall' from an object and a method. The object can be passed
/// either by value/move reference (in which case it is captured in the
/// 'SysCall' object) or by reference (in which case it must be alive when the
/// 'SysCall' is invoked).
template<typename C, typename R, typename... Args>
typename SysHandler<R, Args...>::syscall_t
syscall(C&& c, R (std::remove_reference_t<C>::*f)(Args...)) {
  return SysHandler<R, Args...>::syscall(std::forward<C>(c), f);
}

/// Construct a 'SysCall' from an object and a const method. The object can be
/// passed either by value/move reference (in which case it is captured in the
/// 'SysCall' object) or by reference (in which case it must be alive when the
/// 'SysCall' is invoked).
template<typename C, typename R, typename... Args>
typename SysHandler<R, Args...>::syscall_t
syscall(C&& c, R (std::remove_reference_t<C>::*f)(Args...) const) {
  return SysHandler<R, Args...>::syscall(std::forward<C>(c), f);
}

/// Construct a 'SysCall' from a functional object. The object can be passed
/// either by value/move reference (in which case it is captured in the
/// 'SysCall' object) or by reference (in which case it must be alive when the
/// 'SysCall' is invoked).
template<typename C>
auto syscall(C&& c) {
  return syscall(std::forward<C>(c), &std::remove_reference_t<C>::operator());
}

}
}
}
