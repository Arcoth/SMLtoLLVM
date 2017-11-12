#pragma once

#include <variant>

#include <boost/preprocessor.hpp>
#include <boost/preprocessor/facilities/identity.hpp>

template <typename T>
struct identity {
  using type = T;
};

template <auto f>
using extract_type = typename decltype(f())::type;

#define GET_TYPE_(...) BOOST_PP_IF(BOOST_PP_DEC(BOOST_PP_VARIADIC_SIZE(__VA_ARGS__)), BOOST_PP_TUPLE_POP_FRONT((__VA_ARGS__)), (std::monostate))
// Need operator * applied to the lambda to decay it into a function pointer
#define GET_TYPE(...) extract_type<*[] {[[maybe_unused]] typedef BOOST_PP_REMOVE_PARENS(GET_TYPE_(__VA_ARGS__)) _3495; return identity<_3495>{};}>
#define PROJ_1_(a, ...) a
#define PROJ_1(a,b, pair) PROJ_1_ pair

#define LABELLED_VARIANT_TYPE_EXPANDER_(...) __VA_ARGS__
#define LABELLED_VARIANT_TYPE_EXPANDER(z, n, seq) LABELLED_VARIANT_TYPE_EXPANDER_(GET_TYPE BOOST_PP_SEQ_ELEM(n, seq))
#define DEF_LABELLED_VARIANT_(seq) \
 std::variant<BOOST_PP_ENUM(BOOST_PP_SEQ_SIZE(seq), LABELLED_VARIANT_TYPE_EXPANDER, seq)> {using variant::variant;}; \
 enum {BOOST_PP_SEQ_ENUM(BOOST_PP_SEQ_TRANSFORM(PROJ_1, , seq))}
#define LABELLED_VARIANT(seq) DEF_LABELLED_VARIANT_(BOOST_PP_VARIADIC_SEQ_TO_SEQ(seq))

