// Copyright (c) Facebook, Inc. and its affiliates.

#include <coroutine>

struct Task;

struct Promise {
  bool await_ready() noexcept;
  template<typename T> void await_suspend(T) noexcept {}
  int await_resume() noexcept;

  Promise initial_suspend() noexcept;

  Promise final_suspend() noexcept;

  Task get_return_object() noexcept;
  void unhandled_exception() noexcept;
  void return_void() noexcept;
};

struct Task {
  using promise_type = Promise;
};

namespace N0 {

Promise foo();

}

namespace N1 {

Task bar() noexcept {
  using namespace N0;
  co_await foo();
}

}
