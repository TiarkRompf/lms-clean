#ifndef IMMER_CONTRIB_H
#define IMMER_CONTRIB_H

#include <immer/flex_vector.hpp>
#include <immer/map.hpp>
#include <immer/set.hpp>
#include <immer/algorithm.hpp>

namespace ImmerContrib {
  // Iterative implementation of map
  template<typename U, typename T, typename Fn>
  inline immer::flex_vector<U> vmap(immer::flex_vector<T> vec, Fn f) {
    static_assert(std::is_convertible<Fn, std::function<U(T)>>::value,
      "Vec::map requires a function of type U(T)");
    auto res = immer::flex_vector<U>();
    for (int i = 0; i < vec.size(); i++) {
      res = res.push_back(f(vec.at(i)));
    }
    return res;
  }

  // Recursive implementation of map
  template<typename U, typename T, typename Fn>
  immer::flex_vector<U> map_rec(immer::flex_vector<T> vec, Fn f) {
    static_assert(std::is_convertible<Fn, std::function<U(T)>>::value,
      "Vec::map_rec requires a function of type U(T)");
    if (vec.size() == 0) {
      return immer::flex_vector<U>();
    } else {
      U head = f(vec.front());
      immer::flex_vector<U> tail = map_rec<U>(vec.drop(1), f);
      return tail.push_front(head);
    }
  }

  // Iterative implementation of filter
  template<typename T, typename P>
  inline auto filter(immer::flex_vector<T> vec, P p) {
    static_assert(std::is_convertible<P, std::function<bool(T)>>::value,
      "Vec::filter requires a function of type bool(T)");
    auto res = immer::flex_vector<T>();
    for (int i = 0; i < vec.size(); i++) {
      auto e = vec.at(i);
      if (p(e)) res = res.push_back(e);
    }
    return res;
  }

  // Recursive implementation of filter
  template<typename T, typename P>
  auto filter_rec(immer::flex_vector<T> vec, P p) {
    static_assert(std::is_convertible<P, std::function<bool(T)>>::value,
      "Vec::filter_rec requires a function of type bool(T)");
    if (vec.size == 0) return immer::flex_vector<T>();
    if (p(vec.front())) {
      auto head = vec.front();
      auto tail = filter_rec(vec.drop(1), p);
      return tail.push_front(head);
    }
    else {
      return filter_rec(vec.drop(1), p);
    }
  }

  // Iterative implementation of foldLeft
  template<typename T, typename U, typename Fn>
  inline U foldLeft(immer::flex_vector<T> vec, U acc, Fn f) {
    static_assert(std::is_convertible<Fn, std::function<U(U, T)>>::value,
      "Vec::foldLeft requires a function of type U(U, T)");
    for (int i = 0; i < vec.size(); i++) {
      acc = f(acc, vec.at(i));
    }
    return acc;
  }

  // Recursive implementation of foldLeft
  template<typename T, typename U, typename Fn>
  U foldLeft_rec(immer::flex_vector<T> vec, U acc, Fn f) {
    static_assert(std::is_convertible<Fn, std::function<U(U, T)>>::value,
      "Vec::foldLeft_rec requires a function of type U(U, T)");
    if (vec.size() == 0)
      return acc;
    else
      return foldLeft_rec(vec.drop(1), f(acc, vec.front()), f);
  }

  // Iterative implementation of foldRight
  template<typename T, typename U, typename Fn>
  inline U foldRight(immer::flex_vector<T> vec, U acc, Fn f) {
    static_assert(std::is_convertible<Fn, std::function<U(T, U)>>::value,
      "Vec::foldLeft requires a function of type U(T, U)");
    for (int i = vec.size()-1; i >= 0; i--) {
      acc = f(vec.at(i), acc);
    }
    return acc;
  }

  // Recursive implementation of foldRight
  template<typename T, typename U, typename Fn>
  U foldRight_rec(immer::flex_vector<T> vec, U acc, Fn f) {
    static_assert(std::is_convertible<Fn, std::function<U(T, U)>>::value,
      "Vec::foldRight_rec requires a function of type U(T, U)");
    if (vec.size() == 0) return acc;
    else return f(vec.front(), foldRight_rec(vec.drop(1), acc, f));
  }

  template<typename U, typename T, typename Fn>
  inline auto flatMap(immer::flex_vector<T> vec, Fn f) {
    static_assert(std::is_convertible<Fn, std::function<immer::flex_vector<U>(T)>>::value,
      "Vec::flatMap requires a function of type flex_vector<U>(T)");
    auto v1 = vmap<immer::flex_vector<U>>(vec, f);
    auto res = immer::flex_vector<U>();
    for (int i = 0; i < v1.size(); i++) res = res + v1.at(i);
    return res;
  }

  template<typename T>
  inline auto reverse(immer::flex_vector<T> vec) {
    return foldLeft(vec, immer::flex_vector<T>(), [](auto acc, auto x) { return acc.push_front(x); });
  }

  template<typename T, typename U>
  inline immer::flex_vector<std::tuple<T, U>> zip(immer::flex_vector<T> v1, immer::flex_vector<U> v2) {
    ASSERT(v1.size() == v2.size(), "Vectors must have same size");
    auto res = immer::flex_vector<std::tuple<T, U>>();
    for (int i = 0; i < v1.size(); i++) {
      res = res.push_back(std::make_tuple(v1.at(i), v2.at(i)));
    }
    return res;
  }

}

#endif
