/**
 * @file    turner_version.h
 * @author  Mike DeKoker (dekoker.mike@gmail.com)
 * @brief   Defines turner library version
 * @date    2024-08-08
 *
 * https://github.com/punk-floyd/turner
 *
 * @copyright Copyright (c) 2023 Mike DeKoker
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all
 * copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
 *
 */
#ifndef turner_inc_turner_turner_version_header_included
#define turner_inc_turner_turner_version_header_included

// Note: We have scripts that grep the macros below to generate names for
// various items so please don't move or otherwise monkey with them.
//
// To get value of TURNER_VER_MAJ one could run:
//  grep "^#define TURNER_VER_MAJ" TURNER_version.h | awk -F' +' '{print $$3}'

// NOLINTBEGIN(modernize-macro-to-enum)

#define TURNER_VER_STR                      "1.0.3"
#define TURNER_VER_MAJ                      1
#define TURNER_VER_MIN                      0
#define TURNER_VER_SUB_MIN                  3
#define TURNER_VER_PKG                      0

// NOLINTEND(modernize-macro-to-enum)

#endif // ifndef turner_inc_turner_turner_version_header_included
