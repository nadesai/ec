/* cbits/p521.c
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


#include "p521.h"


static void flip_endian(u8 *out, const u8 *in, unsigned len);
static void felem_one(felem out);
static void felem_assign(felem out, const felem in);
static void felem_sum64(felem out, const felem in);
static void felem_scalar(felem out, const felem in, limb scalar);
static void felem_diff64(felem out, const felem in);
static void felem_diff_128_64(largefelem out, const felem in);
static void felem_diff128(largefelem out, const largefelem in);
static void felem_square(largefelem out, const felem in);
static void felem_mul(largefelem out, const felem in1, const felem in2);
static void felem_reduce(felem out, const largefelem in);
static limb felem_is_zero(const felem in);
static void felem_contract(felem out, const felem in);


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


static const limb bottom57bits = 0x1ffffffffffffff;
static const limb bottom58bits = 0x3ffffffffffffff;

/* bin66_to_felem takes a little-endian byte array and converts it into felem
 * form. This assumes that the CPU is little-endian. */
void bin66_to_felem(felem out, const u8 in[66])
{
    out[0] = (*((limb*) &in[0])) & bottom58bits;
    out[1] = (*((limb*) &in[7]) >> 2) & bottom58bits;
    out[2] = (*((limb*) &in[14]) >> 4) & bottom58bits;
    out[3] = (*((limb*) &in[21]) >> 6) & bottom58bits;
    out[4] = (*((limb*) &in[29])) & bottom58bits;
    out[5] = (*((limb*) &in[36]) >> 2) & bottom58bits;
    out[6] = (*((limb*) &in[43]) >> 4) & bottom58bits;
    out[7] = (*((limb*) &in[50]) >> 6) & bottom58bits;
    out[8] = (*((limb*) &in[58])) & bottom57bits;
}

/* felem_to_bin66 takes an felem and serialises into a little endian, 66 byte
 * array. This assumes that the CPU is little-endian. */
void felem_to_bin66(u8 out[66], const felem in)
{
    felem tmp;
    felem_contract(tmp, in);
    memset(out, 0, 66);
    (*((limb*) &out[0])) = tmp[0];
    (*((limb*) &out[7])) |= tmp[1] << 2;
    (*((limb*) &out[14])) |= tmp[2] << 4;
    (*((limb*) &out[21])) |= tmp[3] << 6;
    (*((limb*) &out[29])) = tmp[4];
    (*((limb*) &out[36])) |= tmp[5] << 2;
    (*((limb*) &out[43])) |= tmp[6] << 4;
    (*((limb*) &out[50])) |= tmp[7] << 6;
    (*((limb*) &out[58])) = tmp[8];
}

/* To preserve endianness when using BN_bn2bin and BN_bin2bn */
static void flip_endian(u8 *out, const u8 *in, unsigned len)
{
    unsigned i;
    for (i = 0; i < len; ++i)
        out[i] = in[len-1-i];
}


/* Field operations
 * ---------------- */

static void felem_one(felem out)
{
    out[0] = 1;
    out[1] = 0;
    out[2] = 0;
    out[3] = 0;
    out[4] = 0;
    out[5] = 0;
    out[6] = 0;
    out[7] = 0;
    out[8] = 0;
}

static void felem_assign(felem out, const felem in)
{
    out[0] = in[0];
    out[1] = in[1];
    out[2] = in[2];
    out[3] = in[3];
    out[4] = in[4];
    out[5] = in[5];
    out[6] = in[6];
    out[7] = in[7];
    out[8] = in[8];
}

/* felem_sum64 sets out = out + in. */
static void felem_sum64(felem out, const felem in)
{
    out[0] += in[0];
    out[1] += in[1];
    out[2] += in[2];
    out[3] += in[3];
    out[4] += in[4];
    out[5] += in[5];
    out[6] += in[6];
    out[7] += in[7];
    out[8] += in[8];
}

/* felem_sum sets out = in1 + in2.
 * This operation cannot be repeated indefinitely without overflowing the felem.
 */
void felem_sum(felem out, const felem in1, const felem in2)
{
    felem_assign(out, in1);
    felem_sum64(out, in2);
}

/* felem_scalar sets out = in * scalar */
static void felem_scalar(felem out, const felem in, limb scalar)
{
    out[0] = in[0] * scalar;
    out[1] = in[1] * scalar;
    out[2] = in[2] * scalar;
    out[3] = in[3] * scalar;
    out[4] = in[4] * scalar;
    out[5] = in[5] * scalar;
    out[6] = in[6] * scalar;
    out[7] = in[7] * scalar;
    out[8] = in[8] * scalar;
}

/* felem_neg sets |out| to |-in|
 * On entry:
 *   in[i] < 2^59 + 2^14
 * On exit:
 *   out[i] < 2^62
 */
void felem_neg(felem out, const felem in)
{
    /* In order to prevent underflow, we subtract from 0 mod p. */
    static const limb two62m3 = (((limb)1) << 62) - (((limb)1) << 5);
    static const limb two62m2 = (((limb)1) << 62) - (((limb)1) << 4);

    out[0] = two62m3 - in[0];
    out[1] = two62m2 - in[1];
    out[2] = two62m2 - in[2];
    out[3] = two62m2 - in[3];
    out[4] = two62m2 - in[4];
    out[5] = two62m2 - in[5];
    out[6] = two62m2 - in[6];
    out[7] = two62m2 - in[7];
    out[8] = two62m2 - in[8];
}

/* felem_diff64 subtracts |in| from |out|
 * On entry:
 *   in[i] < 2^59 + 2^14
 * On exit:
 *   out[i] < out[i] + 2^62
 */
static void felem_diff64(felem out, const felem in)
{
    /* In order to prevent underflow, we add 0 mod p before subtracting. */
    static const limb two62m3 = (((limb)1) << 62) - (((limb)1) << 5);
    static const limb two62m2 = (((limb)1) << 62) - (((limb)1) << 4);

    out[0] += two62m3 - in[0];
    out[1] += two62m2 - in[1];
    out[2] += two62m2 - in[2];
    out[3] += two62m2 - in[3];
    out[4] += two62m2 - in[4];
    out[5] += two62m2 - in[5];
    out[6] += two62m2 - in[6];
    out[7] += two62m2 - in[7];
    out[8] += two62m2 - in[8];
}

/* felem_diff subtracts |in2| from |in1| and stores the result in |out|.
 */
void felem_diff(felem out, const felem in1, const felem in2)
{
    felem_assign(out, in1);
    felem_diff64(out, in2);
}

/* felem_diff_128_64 subtracts |in| from |out|
 * On entry:
 *   in[i] < 2^62 + 2^17
 * On exit:
 *   out[i] < out[i] + 2^63
 */
static void felem_diff_128_64(largefelem out, const felem in)
{
    /* In order to prevent underflow, we add 0 mod p before subtracting. */
    static const limb two63m6 = (((limb)1) << 62) - (((limb)1) << 5);
    static const limb two63m5 = (((limb)1) << 62) - (((limb)1) << 4);

    out[0] += two63m6 - in[0];
    out[1] += two63m5 - in[1];
    out[2] += two63m5 - in[2];
    out[3] += two63m5 - in[3];
    out[4] += two63m5 - in[4];
    out[5] += two63m5 - in[5];
    out[6] += two63m5 - in[6];
    out[7] += two63m5 - in[7];
    out[8] += two63m5 - in[8];
}

/* felem_diff_128_64 subtracts |in| from |out|
 * On entry:
 *   in[i] < 2^126
 * On exit:
 *   out[i] < out[i] + 2^127 - 2^69
 */
static void felem_diff128(largefelem out, const largefelem in)
{
    /* In order to prevent underflow, we add 0 mod p before subtracting. */
    static const uint128_t two127m70 = (((uint128_t)1) << 127) - (((uint128_t)1) << 70);
    static const uint128_t two127m69 = (((uint128_t)1) << 127) - (((uint128_t)1) << 69);

    out[0] += (two127m70 - in[0]);
    out[1] += (two127m69 - in[1]);
    out[2] += (two127m69 - in[2]);
    out[3] += (two127m69 - in[3]);
    out[4] += (two127m69 - in[4]);
    out[5] += (two127m69 - in[5]);
    out[6] += (two127m69 - in[6]);
    out[7] += (two127m69 - in[7]);
    out[8] += (two127m69 - in[8]);
}

/* felem_square sets |out| = |in|^2
 * On entry:
 *   in[i] < 2^62
 * On exit:
 *   out[i] < 17 * max(in[i]) * max(in[i])
 */
static void felem_square(largefelem out, const felem in)
{
    felem inx2, inx4;
    felem_scalar(inx2, in, 2);
    felem_scalar(inx4, in, 4);

    /* We have many cases were we want to do
     *   in[x] * in[y] +
     *   in[y] * in[x]
     * This is obviously just
     *   2 * in[x] * in[y]
     * However, rather than do the doubling on the 128 bit result, we
     * double one of the inputs to the multiplication by reading from
     * |inx2| */

    out[0] = ((uint128_t) in[0]) * in[0];
    out[1] = ((uint128_t) in[0]) * inx2[1];
    out[2] = ((uint128_t) in[0]) * inx2[2] +
     ((uint128_t) in[1]) * in[1];
    out[3] = ((uint128_t) in[0]) * inx2[3] +
     ((uint128_t) in[1]) * inx2[2];
    out[4] = ((uint128_t) in[0]) * inx2[4] +
     ((uint128_t) in[1]) * inx2[3] +
     ((uint128_t) in[2]) * in[2];
    out[5] = ((uint128_t) in[0]) * inx2[5] +
     ((uint128_t) in[1]) * inx2[4] +
     ((uint128_t) in[2]) * inx2[3];
    out[6] = ((uint128_t) in[0]) * inx2[6] +
     ((uint128_t) in[1]) * inx2[5] +
     ((uint128_t) in[2]) * inx2[4] +
     ((uint128_t) in[3]) * in[3];
    out[7] = ((uint128_t) in[0]) * inx2[7] +
     ((uint128_t) in[1]) * inx2[6] +
     ((uint128_t) in[2]) * inx2[5] +
     ((uint128_t) in[3]) * inx2[4];
    out[8] = ((uint128_t) in[0]) * inx2[8] +
     ((uint128_t) in[1]) * inx2[7] +
     ((uint128_t) in[2]) * inx2[6] +
     ((uint128_t) in[3]) * inx2[5] +
     ((uint128_t) in[4]) * in[4];

    /* The remaining limbs fall above 2^521, with the first falling at
     * 2^522. They correspond to locations one bit up from the limbs
     * produced above so we would have to multiply by two to align them.
     * Again, rather than operate on the 128-bit result, we double one of
     * the inputs to the multiplication. If we want to double for both this
     * reason, and the reason above, then we end up multiplying by four. */

    /* 9 */
    out[0] += ((uint128_t) in[1]) * inx4[8] +
      ((uint128_t) in[2]) * inx4[7] +
      ((uint128_t) in[3]) * inx4[6] +
      ((uint128_t) in[4]) * inx4[5];

    /* 10 */
    out[1] += ((uint128_t) in[2]) * inx4[8] +
      ((uint128_t) in[3]) * inx4[7] +
      ((uint128_t) in[4]) * inx4[6] +
      ((uint128_t) in[5]) * inx2[5];

    /* 11 */
    out[2] += ((uint128_t) in[3]) * inx4[8] +
      ((uint128_t) in[4]) * inx4[7] +
      ((uint128_t) in[5]) * inx4[6];

    /* 12 */
    out[3] += ((uint128_t) in[4]) * inx4[8] +
      ((uint128_t) in[5]) * inx4[7] +
      ((uint128_t) in[6]) * inx2[6];

    /* 13 */
    out[4] += ((uint128_t) in[5]) * inx4[8] +
      ((uint128_t) in[6]) * inx4[7];

    /* 14 */
    out[5] += ((uint128_t) in[6]) * inx4[8] +
      ((uint128_t) in[7]) * inx2[7];

    /* 15 */
    out[6] += ((uint128_t) in[7]) * inx4[8];

    /* 16 */
    out[7] += ((uint128_t) in[8]) * inx2[8];
}

/* felem_mul sets |out| = |in1| * |in2|
 * On entry:
 *   in1[i] < 2^64
 *   in2[i] < 2^63
 * On exit:
 *   out[i] < 17 * max(in1[i]) * max(in2[i])
 */
static void felem_mul(largefelem out, const felem in1, const felem in2)
{
    felem in2x2;
    felem_scalar(in2x2, in2, 2);

    out[0] = ((uint128_t) in1[0]) * in2[0];

    out[1] = ((uint128_t) in1[0]) * in2[1] +
             ((uint128_t) in1[1]) * in2[0];

    out[2] = ((uint128_t) in1[0]) * in2[2] +
     ((uint128_t) in1[1]) * in2[1] +
             ((uint128_t) in1[2]) * in2[0];

    out[3] = ((uint128_t) in1[0]) * in2[3] +
     ((uint128_t) in1[1]) * in2[2] +
     ((uint128_t) in1[2]) * in2[1] +
     ((uint128_t) in1[3]) * in2[0];

    out[4] = ((uint128_t) in1[0]) * in2[4] +
     ((uint128_t) in1[1]) * in2[3] +
     ((uint128_t) in1[2]) * in2[2] +
     ((uint128_t) in1[3]) * in2[1] +
     ((uint128_t) in1[4]) * in2[0];

    out[5] = ((uint128_t) in1[0]) * in2[5] +
     ((uint128_t) in1[1]) * in2[4] +
     ((uint128_t) in1[2]) * in2[3] +
     ((uint128_t) in1[3]) * in2[2] +
     ((uint128_t) in1[4]) * in2[1] +
     ((uint128_t) in1[5]) * in2[0];

    out[6] = ((uint128_t) in1[0]) * in2[6] +
     ((uint128_t) in1[1]) * in2[5] +
     ((uint128_t) in1[2]) * in2[4] +
     ((uint128_t) in1[3]) * in2[3] +
     ((uint128_t) in1[4]) * in2[2] +
     ((uint128_t) in1[5]) * in2[1] +
     ((uint128_t) in1[6]) * in2[0];

    out[7] = ((uint128_t) in1[0]) * in2[7] +
     ((uint128_t) in1[1]) * in2[6] +
     ((uint128_t) in1[2]) * in2[5] +
     ((uint128_t) in1[3]) * in2[4] +
     ((uint128_t) in1[4]) * in2[3] +
     ((uint128_t) in1[5]) * in2[2] +
     ((uint128_t) in1[6]) * in2[1] +
     ((uint128_t) in1[7]) * in2[0];

    out[8] = ((uint128_t) in1[0]) * in2[8] +
     ((uint128_t) in1[1]) * in2[7] +
     ((uint128_t) in1[2]) * in2[6] +
     ((uint128_t) in1[3]) * in2[5] +
     ((uint128_t) in1[4]) * in2[4] +
     ((uint128_t) in1[5]) * in2[3] +
     ((uint128_t) in1[6]) * in2[2] +
     ((uint128_t) in1[7]) * in2[1] +
     ((uint128_t) in1[8]) * in2[0];

    /* See comment in felem_square about the use of in2x2 here */

    out[0] += ((uint128_t) in1[1]) * in2x2[8] +
      ((uint128_t) in1[2]) * in2x2[7] +
      ((uint128_t) in1[3]) * in2x2[6] +
      ((uint128_t) in1[4]) * in2x2[5] +
      ((uint128_t) in1[5]) * in2x2[4] +
      ((uint128_t) in1[6]) * in2x2[3] +
      ((uint128_t) in1[7]) * in2x2[2] +
      ((uint128_t) in1[8]) * in2x2[1];

    out[1] += ((uint128_t) in1[2]) * in2x2[8] +
      ((uint128_t) in1[3]) * in2x2[7] +
      ((uint128_t) in1[4]) * in2x2[6] +
      ((uint128_t) in1[5]) * in2x2[5] +
      ((uint128_t) in1[6]) * in2x2[4] +
      ((uint128_t) in1[7]) * in2x2[3] +
      ((uint128_t) in1[8]) * in2x2[2];

    out[2] += ((uint128_t) in1[3]) * in2x2[8] +
      ((uint128_t) in1[4]) * in2x2[7] +
      ((uint128_t) in1[5]) * in2x2[6] +
      ((uint128_t) in1[6]) * in2x2[5] +
      ((uint128_t) in1[7]) * in2x2[4] +
      ((uint128_t) in1[8]) * in2x2[3];

    out[3] += ((uint128_t) in1[4]) * in2x2[8] +
      ((uint128_t) in1[5]) * in2x2[7] +
      ((uint128_t) in1[6]) * in2x2[6] +
      ((uint128_t) in1[7]) * in2x2[5] +
      ((uint128_t) in1[8]) * in2x2[4];

    out[4] += ((uint128_t) in1[5]) * in2x2[8] +
      ((uint128_t) in1[6]) * in2x2[7] +
      ((uint128_t) in1[7]) * in2x2[6] +
      ((uint128_t) in1[8]) * in2x2[5];

    out[5] += ((uint128_t) in1[6]) * in2x2[8] +
      ((uint128_t) in1[7]) * in2x2[7] +
      ((uint128_t) in1[8]) * in2x2[6];

    out[6] += ((uint128_t) in1[7]) * in2x2[8] +
      ((uint128_t) in1[8]) * in2x2[7];

    out[7] += ((uint128_t) in1[8]) * in2x2[8];
}

static const limb bottom52bits = 0xfffffffffffff;

/* felem_reduce converts a largefelem to an felem.
 * On entry:
 *   in[i] < 2^128
 * On exit:
 *   out[i] < 2^59 + 2^14
 */
static void felem_reduce(felem out, const largefelem in)
{
    u64 overflow1, overflow2;

    out[0] = ((limb) in[0]) & bottom58bits;
    out[1] = ((limb) in[1]) & bottom58bits;
    out[2] = ((limb) in[2]) & bottom58bits;
    out[3] = ((limb) in[3]) & bottom58bits;
    out[4] = ((limb) in[4]) & bottom58bits;
    out[5] = ((limb) in[5]) & bottom58bits;
    out[6] = ((limb) in[6]) & bottom58bits;
    out[7] = ((limb) in[7]) & bottom58bits;
    out[8] = ((limb) in[8]) & bottom58bits;

    /* out[i] < 2^58 */

    out[1] += ((limb) in[0]) >> 58;
    out[1] += (((limb) (in[0] >> 64)) & bottom52bits) << 6;
    /* out[1] < 2^58 + 2^6 + 2^58
     *        = 2^59 + 2^6 */
    out[2] += ((limb) (in[0] >> 64)) >> 52;

    out[2] += ((limb) in[1]) >> 58;
    out[2] += (((limb) (in[1] >> 64)) & bottom52bits) << 6;
    out[3] += ((limb) (in[1] >> 64)) >> 52;

    out[3] += ((limb) in[2]) >> 58;
    out[3] += (((limb) (in[2] >> 64)) & bottom52bits) << 6;
    out[4] += ((limb) (in[2] >> 64)) >> 52;

    out[4] += ((limb) in[3]) >> 58;
    out[4] += (((limb) (in[3] >> 64)) & bottom52bits) << 6;
    out[5] += ((limb) (in[3] >> 64)) >> 52;

    out[5] += ((limb) in[4]) >> 58;
    out[5] += (((limb) (in[4] >> 64)) & bottom52bits) << 6;
    out[6] += ((limb) (in[4] >> 64)) >> 52;

    out[6] += ((limb) in[5]) >> 58;
    out[6] += (((limb) (in[5] >> 64)) & bottom52bits) << 6;
    out[7] += ((limb) (in[5] >> 64)) >> 52;

    out[7] += ((limb) in[6]) >> 58;
    out[7] += (((limb) (in[6] >> 64)) & bottom52bits) << 6;
    out[8] += ((limb) (in[6] >> 64)) >> 52;

    out[8] += ((limb) in[7]) >> 58;
    out[8] += (((limb) (in[7] >> 64)) & bottom52bits) << 6;
    /* out[x > 1] < 2^58 + 2^6 + 2^58 + 2^12
     *            < 2^59 + 2^13 */
    overflow1 = ((limb) (in[7] >> 64)) >> 52;

    overflow1 += ((limb) in[8]) >> 58;
    overflow1 += (((limb) (in[8] >> 64)) & bottom52bits) << 6;
    overflow2 = ((limb) (in[8] >> 64)) >> 52;

    overflow1 <<= 1;  /* overflow1 < 2^13 + 2^7 + 2^59 */
    overflow2 <<= 1;  /* overflow2 < 2^13 */

    out[0] += overflow1;  /* out[0] < 2^60 */
    out[1] += overflow2;  /* out[1] < 2^59 + 2^6 + 2^13 */

    out[1] += out[0] >> 58; out[0] &= bottom58bits;
    /* out[0] < 2^58
     * out[1] < 2^59 + 2^6 + 2^13 + 2^2
     *        < 2^59 + 2^14 */
}

void felem_square_reduce(felem out, const felem in)
{
    largefelem tmp;
    felem_square(tmp, in);
    felem_reduce(out, tmp);
}

void felem_mul_reduce(felem out, const felem in1, const felem in2)
{
    largefelem tmp;
    felem_mul(tmp, in1, in2);
    felem_reduce(out, tmp);
}

/* felem_inv calculates |out| = |in|^{-1}
 *
 * Based on Fermat's Little Theorem:
 *   a^p = a (mod p)
 *   a^{p-1} = 1 (mod p)
 *   a^{p-2} = a^{-1} (mod p)
 */
void felem_inv(felem out, const felem in)
{
    felem ftmp, ftmp2, ftmp3, ftmp4;
    largefelem tmp;
    unsigned i;

    felem_square(tmp, in); felem_reduce(ftmp, tmp);    /* 2^1 */
    felem_mul(tmp, in, ftmp); felem_reduce(ftmp, tmp);    /* 2^2 - 2^0 */
    felem_assign(ftmp2, ftmp);
    felem_square(tmp, ftmp); felem_reduce(ftmp, tmp);    /* 2^3 - 2^1 */
    felem_mul(tmp, in, ftmp); felem_reduce(ftmp, tmp);    /* 2^3 - 2^0 */
    felem_square(tmp, ftmp); felem_reduce(ftmp, tmp);    /* 2^4 - 2^1 */

    felem_square(tmp, ftmp2); felem_reduce(ftmp3, tmp);    /* 2^3 - 2^1 */
    felem_square(tmp, ftmp3); felem_reduce(ftmp3, tmp);    /* 2^4 - 2^2 */
    felem_mul(tmp, ftmp3, ftmp2); felem_reduce(ftmp3, tmp);    /* 2^4 - 2^0 */

    felem_assign(ftmp2, ftmp3);
    felem_square(tmp, ftmp3); felem_reduce(ftmp3, tmp);    /* 2^5 - 2^1 */
    felem_square(tmp, ftmp3); felem_reduce(ftmp3, tmp);    /* 2^6 - 2^2 */
    felem_square(tmp, ftmp3); felem_reduce(ftmp3, tmp);    /* 2^7 - 2^3 */
    felem_square(tmp, ftmp3); felem_reduce(ftmp3, tmp);    /* 2^8 - 2^4 */
    felem_assign(ftmp4, ftmp3);
    felem_mul(tmp, ftmp3, ftmp); felem_reduce(ftmp4, tmp);    /* 2^8 - 2^1 */
    felem_square(tmp, ftmp4); felem_reduce(ftmp4, tmp);    /* 2^9 - 2^2 */
    felem_mul(tmp, ftmp3, ftmp2); felem_reduce(ftmp3, tmp);    /* 2^8 - 2^0 */
    felem_assign(ftmp2, ftmp3);

    for (i = 0; i < 8; i++)
    {
        felem_square(tmp, ftmp3); felem_reduce(ftmp3, tmp);    /* 2^16 - 2^8 */
    }
    felem_mul(tmp, ftmp3, ftmp2); felem_reduce(ftmp3, tmp);    /* 2^16 - 2^0 */
    felem_assign(ftmp2, ftmp3);

    for (i = 0; i < 16; i++)
    {
        felem_square(tmp, ftmp3); felem_reduce(ftmp3, tmp);    /* 2^32 - 2^16 */
    }
    felem_mul(tmp, ftmp3, ftmp2); felem_reduce(ftmp3, tmp);    /* 2^32 - 2^0 */
    felem_assign(ftmp2, ftmp3);

    for (i = 0; i < 32; i++)
    {
        felem_square(tmp, ftmp3); felem_reduce(ftmp3, tmp);    /* 2^64 - 2^32 */
    }
    felem_mul(tmp, ftmp3, ftmp2); felem_reduce(ftmp3, tmp);    /* 2^64 - 2^0 */
    felem_assign(ftmp2, ftmp3);

    for (i = 0; i < 64; i++)
    {
        felem_square(tmp, ftmp3); felem_reduce(ftmp3, tmp);    /* 2^128 - 2^64 */
    }
    felem_mul(tmp, ftmp3, ftmp2); felem_reduce(ftmp3, tmp);    /* 2^128 - 2^0 */
    felem_assign(ftmp2, ftmp3);

    for (i = 0; i < 128; i++)
    {
        felem_square(tmp, ftmp3); felem_reduce(ftmp3, tmp);    /* 2^256 - 2^128 */
    }
    felem_mul(tmp, ftmp3, ftmp2); felem_reduce(ftmp3, tmp);    /* 2^256 - 2^0 */
    felem_assign(ftmp2, ftmp3);

    for (i = 0; i < 256; i++)
    {
        felem_square(tmp, ftmp3); felem_reduce(ftmp3, tmp);    /* 2^512 - 2^256 */
    }
    felem_mul(tmp, ftmp3, ftmp2); felem_reduce(ftmp3, tmp);    /* 2^512 - 2^0 */

    for (i = 0; i < 9; i++)
    {
        felem_square(tmp, ftmp3); felem_reduce(ftmp3, tmp);    /* 2^521 - 2^9 */
    }
    felem_mul(tmp, ftmp3, ftmp4); felem_reduce(ftmp3, tmp);    /* 2^512 - 2^2 */
    felem_mul(tmp, ftmp3, in); felem_reduce(out, tmp);    /* 2^512 - 3 */
}

/* This is 2^521-1, expressed as an felem */
static const felem kPrime =
{
    0x03ffffffffffffff, 0x03ffffffffffffff, 0x03ffffffffffffff,
    0x03ffffffffffffff, 0x03ffffffffffffff, 0x03ffffffffffffff,
    0x03ffffffffffffff, 0x03ffffffffffffff, 0x01ffffffffffffff
};

/* felem_is_zero returns a limb with all bits set if |in| == 0 (mod p) and 0
 * otherwise.
 * On entry:
 *   in[i] < 2^59 + 2^14
 */
static limb felem_is_zero(const felem in)
{
    felem ftmp;
    limb is_zero, is_p;
    felem_assign(ftmp, in);

    ftmp[0] += ftmp[8] >> 57; ftmp[8] &= bottom57bits;
    /* ftmp[8] < 2^57 */
    ftmp[1] += ftmp[0] >> 58; ftmp[0] &= bottom58bits;
    ftmp[2] += ftmp[1] >> 58; ftmp[1] &= bottom58bits;
    ftmp[3] += ftmp[2] >> 58; ftmp[2] &= bottom58bits;
    ftmp[4] += ftmp[3] >> 58; ftmp[3] &= bottom58bits;
    ftmp[5] += ftmp[4] >> 58; ftmp[4] &= bottom58bits;
    ftmp[6] += ftmp[5] >> 58; ftmp[5] &= bottom58bits;
    ftmp[7] += ftmp[6] >> 58; ftmp[6] &= bottom58bits;
    ftmp[8] += ftmp[7] >> 58; ftmp[7] &= bottom58bits;
    /* ftmp[8] < 2^57 + 4 */

    /* The ninth limb of 2*(2^521-1) is 0x03ffffffffffffff, which is
     * greater than our bound for ftmp[8]. Therefore we only have to check
     * if the zero is zero or 2^521-1. */

    is_zero = 0;
    is_zero |= ftmp[0];
    is_zero |= ftmp[1];
    is_zero |= ftmp[2];
    is_zero |= ftmp[3];
    is_zero |= ftmp[4];
    is_zero |= ftmp[5];
    is_zero |= ftmp[6];
    is_zero |= ftmp[7];
    is_zero |= ftmp[8];

    is_zero--;
    /* We know that ftmp[i] < 2^63, therefore the only way that the top bit
     * can be set is if is_zero was 0 before the decrement. */
    is_zero = ((s64) is_zero) >> 63;

    is_p = ftmp[0] ^ kPrime[0];
    is_p |= ftmp[1] ^ kPrime[1];
    is_p |= ftmp[2] ^ kPrime[2];
    is_p |= ftmp[3] ^ kPrime[3];
    is_p |= ftmp[4] ^ kPrime[4];
    is_p |= ftmp[5] ^ kPrime[5];
    is_p |= ftmp[6] ^ kPrime[6];
    is_p |= ftmp[7] ^ kPrime[7];
    is_p |= ftmp[8] ^ kPrime[8];

    is_p--;
    is_p = ((s64) is_p) >> 63;

    is_zero |= is_p;
    return is_zero;
}

int felem_is_zero_int(const felem in)
{
    return (int) (felem_is_zero(in) & ((limb)1));
}

/* felem_contract converts |in| to its unique, minimal representation.
 * On entry:
 *   in[i] < 2^59 + 2^14
 */
static void felem_contract(felem out, const felem in)
{
    limb is_p, is_greater, sign;
    static const limb two58 = ((limb)1) << 58;

    felem_assign(out, in);

    out[0] += out[8] >> 57; out[8] &= bottom57bits;
    /* out[8] < 2^57 */
    out[1] += out[0] >> 58; out[0] &= bottom58bits;
    out[2] += out[1] >> 58; out[1] &= bottom58bits;
    out[3] += out[2] >> 58; out[2] &= bottom58bits;
    out[4] += out[3] >> 58; out[3] &= bottom58bits;
    out[5] += out[4] >> 58; out[4] &= bottom58bits;
    out[6] += out[5] >> 58; out[5] &= bottom58bits;
    out[7] += out[6] >> 58; out[6] &= bottom58bits;
    out[8] += out[7] >> 58; out[7] &= bottom58bits;
    /* out[8] < 2^57 + 4 */

    /* If the value is greater than 2^521-1 then we have to subtract
     * 2^521-1 out. See the comments in felem_is_zero regarding why we
     * don't test for other multiples of the prime. */

    /* First, if |out| is equal to 2^521-1, we subtract it out to get zero. */

    is_p = out[0] ^ kPrime[0];
    is_p |= out[1] ^ kPrime[1];
    is_p |= out[2] ^ kPrime[2];
    is_p |= out[3] ^ kPrime[3];
    is_p |= out[4] ^ kPrime[4];
    is_p |= out[5] ^ kPrime[5];
    is_p |= out[6] ^ kPrime[6];
    is_p |= out[7] ^ kPrime[7];
    is_p |= out[8] ^ kPrime[8];

    is_p--;
    is_p &= is_p << 32;
    is_p &= is_p << 16;
    is_p &= is_p << 8;
    is_p &= is_p << 4;
    is_p &= is_p << 2;
    is_p &= is_p << 1;
    is_p = ((s64) is_p) >> 63;
    is_p = ~is_p;

    /* is_p is 0 iff |out| == 2^521-1 and all ones otherwise */

    out[0] &= is_p;
    out[1] &= is_p;
    out[2] &= is_p;
    out[3] &= is_p;
    out[4] &= is_p;
    out[5] &= is_p;
    out[6] &= is_p;
    out[7] &= is_p;
    out[8] &= is_p;

    /* In order to test that |out| >= 2^521-1 we need only test if out[8]
     * >> 57 is greater than zero as (2^521-1) + x >= 2^522 */
    is_greater = out[8] >> 57;
    is_greater |= is_greater << 32;
    is_greater |= is_greater << 16;
    is_greater |= is_greater << 8;
    is_greater |= is_greater << 4;
    is_greater |= is_greater << 2;
    is_greater |= is_greater << 1;
    is_greater = ((s64) is_greater) >> 63;

    out[0] -= kPrime[0] & is_greater;
    out[1] -= kPrime[1] & is_greater;
    out[2] -= kPrime[2] & is_greater;
    out[3] -= kPrime[3] & is_greater;
    out[4] -= kPrime[4] & is_greater;
    out[5] -= kPrime[5] & is_greater;
    out[6] -= kPrime[6] & is_greater;
    out[7] -= kPrime[7] & is_greater;
    out[8] -= kPrime[8] & is_greater;

    /* Eliminate negative coefficients */
    sign = -(out[0] >> 63); out[0] += (two58 & sign); out[1] -= (1 & sign);
    sign = -(out[1] >> 63); out[1] += (two58 & sign); out[2] -= (1 & sign);
    sign = -(out[2] >> 63); out[2] += (two58 & sign); out[3] -= (1 & sign);
    sign = -(out[3] >> 63); out[3] += (two58 & sign); out[4] -= (1 & sign);
    sign = -(out[4] >> 63); out[4] += (two58 & sign); out[5] -= (1 & sign);
    sign = -(out[0] >> 63); out[5] += (two58 & sign); out[6] -= (1 & sign);
    sign = -(out[6] >> 63); out[6] += (two58 & sign); out[7] -= (1 & sign);
    sign = -(out[7] >> 63); out[7] += (two58 & sign); out[8] -= (1 & sign);
    sign = -(out[5] >> 63); out[5] += (two58 & sign); out[6] -= (1 & sign);
    sign = -(out[6] >> 63); out[6] += (two58 & sign); out[7] -= (1 & sign);
    sign = -(out[7] >> 63); out[7] += (two58 & sign); out[8] -= (1 & sign);
}

