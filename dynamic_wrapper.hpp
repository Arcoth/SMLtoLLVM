#pragma once

#include <memory>
#include <utility>

namespace SMLNJAbsynInterface {

template <typename T>
class dynamic_wrapper {
  std::unique_ptr<T> _ptr;

public:

  using value_type = T;

  template <typename... Ts>
  dynamic_wrapper(Ts&&... ts)
    : _ptr(std::make_unique<T>(std::forward<Ts>(ts)...)){}

  dynamic_wrapper(dynamic_wrapper&&) = default;
  dynamic_wrapper(dynamic_wrapper const& other)
    : _ptr(std::make_unique<T>((T const&)other)) {}

  dynamic_wrapper& operator=(dynamic_wrapper&&) = default;
  dynamic_wrapper& operator=(dynamic_wrapper const& other) {
    get() = other.get();
    return *this;
  }
  dynamic_wrapper& operator=(T const& other) {
    get() = other;
    return *this;
  }

        T& get()      & noexcept {assert(_ptr); return *_ptr;}
       T&& get()     && noexcept {assert(_ptr); return std::move(*_ptr);}
  T const& get() const& noexcept {assert(_ptr); return *_ptr;}
        operator T&()      & noexcept {return get();}
       operator T&&()     && noexcept {return get();}
  operator T const&() const& noexcept {return get();}
};

}

