// Stitch runtime types represented in the template language

#ifndef TYPING_INCLUDED_H
#define TYPING_INCLUDED_H

#include <cstdint>

namespace ty {

// type ty
//   = 0 | 1
//   | I int | U int
//   | Ptr ty
//   | Struct ty ty
//   | Union ty ty
struct Void;
struct Unit;
template<int width> struct I;
template<int width> struct U;
template<typename ty> struct Ptr;
template<typename hd, typename tl> struct Struct;
template<typename hd, typename tl> struct Union;

// (::T) : ty -> C++ type
// (::T) = function
//   | 0 -> union {} | 1 -> struct {}
//   | I 1 -> bool | I 8 -> int8_t | ..
//   | U 1 -> bool | U 8 -> int8_t | ..
//   | Ptr ty -> ty::T*
//   | Struct h t -> struct {h::T a; t::T b}
//   | Union h t -> union {h::T a; t::T b}
struct Void { struct T {}; };
struct Unit { union T {}; };
template<> struct I<1> { using T = bool; };
template<> struct I<8> { using T = int8_t; };
template<> struct I<16> { using T = int16_t; };
template<> struct I<32> { using T = int32_t; };
template<> struct I<64> { using T = int64_t; };
template<> struct U<1> { using T = bool; };
template<> struct U<8> { using T = uint8_t; };
template<> struct U<16> { using T = uint16_t; };
template<> struct U<32> { using T = uint32_t; };
template<> struct U<64> { using T = uint64_t; };
template<typename ty> struct Ptr { using T = typename ty::T*; };
template<typename h, typename t> struct Struct
{ struct T { typename h::T a; typename t::T b; }; };
template<typename h, typename t> struct Union
{ union T { typename h::T a; typename t::T b; }; };

} // ty

#endif // TYPING_INCLUDED_H
