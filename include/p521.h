/* src/Crypto/EllipticCurve/OpenSSL/p521.h
 *
 * An implementation of the prime field modulo 2^521 - 1. This file was
 * initially part of OpenSSL, and has been modified for EC. The original
 * copyright notice is reproduced below.
 */
/* crypto/ec/ecp_nistp521.c */
/*
 * Written by Adam Langley (Google) for the OpenSSL project
 */
/* Copyright 2011 Google Inc.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 *
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 */

/*
 * A 64-bit implementation of the NIST P-521 elliptic curve point multiplication
 *
 * OpenSSL integration was taken from Emilia Kasper's work in ecp_nistp224.c.
 * Otherwise based on Emilia's P224 work, which was inspired by my curve25519
 * work which got its smarts from Daniel J. Bernstein's work on the same.
 */

#include <stdint.h>

#include <string.h>

#if defined(__GNUC__) && (__GNUC__ > 3 || (__GNUC__ == 3 && __GNUC_MINOR__ >= 1))
  /* even with gcc, the typedef won't work for 32-bit platforms */
  typedef __uint128_t uint128_t; /* nonstandard; implemented by gcc on 64-bit platforms */
#else
  #error "Need GCC 3.1 or later to define type uint128_t"
#endif

typedef uint8_t u8;
typedef uint64_t u64;
typedef int64_t s64;

/* The underlying field.
 *
 * P521 operates over GF(2^521-1). We can serialise an element of this field
 * into 66 bytes where the most significant byte contains only a single bit. We
 * call this an felem_bytearray. */

typedef u8 felem_bytearray[66];


/* The representation of field elements.
 * ------------------------------------
 *
 * We represent field elements with nine values. These values are either 64 or
 * 128 bits and the field element represented is:
 *   v[0]*2^0 + v[1]*2^58 + v[2]*2^116 + ... + v[8]*2^464  (mod p)
 * Each of the nine values is called a 'limb'. Since the limbs are spaced only
 * 58 bits apart, but are greater than 58 bits in length, the most significant
 * bits of each limb overlap with the least significant bits of the next.
 *
 * A field element with 64-bit limbs is an 'felem'. One with 128-bit limbs is a
 * 'largefelem' */

#define NLIMBS 9

typedef uint64_t limb;
typedef limb felem[NLIMBS];
typedef uint128_t largefelem[NLIMBS];


/* bin66_to_felem takes a little-endian byte array and converts it into felem
 * form. This assumes that the CPU is little-endian. */
void bin66_to_felem(felem out, const u8 in[66]);


/* felem_to_bin66 takes an felem and serialises into a little endian, 66 byte
 * array. This assumes that the CPU is little-endian. */
void felem_to_bin66(u8 out[66], const felem in);

///* To preserve endianness when using BN_bn2bin and BN_bin2bn */
//void flip_endian(u8 *out, const u8 *in, unsigned len);


/* Field operations
 * ---------------- */

//void felem_one(felem out);

//void felem_assign(felem out, const felem in);

///* felem_sum64 sets out = out + in. */
//void felem_sum64(felem out, const felem in);

/* felem_sum sets out = in1 + in2.
 * This operation cannot be repeated indefinitely without overflowing the
 * felem.
 */
void felem_sum(felem out, const felem in1, const felem in2);

///* felem_scalar sets out = in * scalar */
//void felem_scalar(felem out, const felem in, limb scalar);

/* felem_neg sets |out| to |-in|
 * On entry:
 *   in[i] < 2^59 + 2^14
 * On exit:
 *   out[i] < 2^62
 */
void felem_neg(felem out, const felem in);

///* felem_diff64 subtracts |in| from |out|
// * On entry:
// *   in[i] < 2^59 + 2^14
// * On exit:
// *   out[i] < 2^62
// */
//void felem_diff64(felem out, const felem in);

/* felem_diff subtracts |in2| from |in1| and stores the result in |out|.
 */
void felem_diff(felem out, const felem in1, const felem in2);

///* felem_diff_128_64 subtracts |in| from |out|
// * On entry:
// *   in[i] < 2^62 + 2^17
// * On exit:
// *   out[i] < out[i] + 2^63
// */
//void felem_diff_128_64(largefelem out, const felem in);
//
///* felem_diff_128_64 subtracts |in| from |out|
// * On entry:
// *   in[i] < 2^126
// * On exit:
// *   out[i] < out[i] + 2^127 - 2^69
// */
//void felem_diff128(largefelem out, const largefelem in);

///* felem_square sets |out| = |in|^2
// * On entry:
// *   in[i] < 2^62
// * On exit:
// *   out[i] < 17 * max(in[i]) * max(in[i])
// */
//void felem_square(largefelem out, const felem in);
//
///* felem_mul sets |out| = |in1| * |in2|
// * On entry:
// *   in1[i] < 2^64
// *   in2[i] < 2^63
// * On exit:
// *   out[i] < 17 * max(in1[i]) * max(in2[i])
// */
//void felem_mul(largefelem out, const felem in1, const felem in2);
//
//
///* felem_reduce converts a largefelem to an felem.
// * On entry:
// *   in[i] < 2^128
// * On exit:
// *   out[i] < 2^59 + 2^14
// */
//void felem_reduce(felem out, const largefelem in);

void felem_square_reduce(felem out, const felem in);

void felem_mul_reduce(felem out, const felem in1, const felem in2);

/* felem_inv calculates |out| = |in|^{-1}
 *
 * Based on Fermat's Little Theorem:
 *   a^p = a (mod p)
 *   a^{p-1} = 1 (mod p)
 *   a^{p-2} = a^{-1} (mod p)
 */
void felem_inv(felem out, const felem in);


///* felem_is_zero returns a limb with all bits set if |in| == 0 (mod p) and 0
// * otherwise.
// * On entry:
// *   in[i] < 2^59 + 2^14
// */
//limb felem_is_zero(const felem in);

int felem_is_zero_int(const felem in);

///* felem_contract converts |in| to its unique, minimal representation.
// * On entry:
// *   in[i] < 2^59 + 2^14
// */
//void felem_contract(felem out, const felem in);

