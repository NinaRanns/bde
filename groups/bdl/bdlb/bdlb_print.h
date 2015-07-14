// bdlb_print.h                                                       -*-C++-*-
#ifndef INCLUDED_BDLB_PRINT
#define INCLUDED_BDLB_PRINT

#ifndef INCLUDED_BSLS_IDENT
#include <bsls_ident.h>
#endif
BSLS_IDENT("$Id: $")

//@PURPOSE: Provide platform-independent stream utilities.
//
//@CLASSES:
//                          bdlb::Print: namespace for procedures on streams
//           bdlb::PrintStringHexDumper: construct/print hex buffers - multiline
// bdlb::PrintStringSingleLineHexDumper: construct/print hex buffers on one line
//
//@AUTHOR: Paul Staniforth (pstaniforth), John Lakos (jlakos)
//
//@DESCRIPTION: This component provides a namespace for a collection of
// pure procedures that augment the standard 'iostream' objects derived from
// interfaces defined in 'iostream.h'.
//
///Usage Example
///-------------
// For example, the default output produced from pointer values is
// non-standard across vendor platforms.  The 'printPtr' method addresses this
// inconsistency by always producing a consistent format for a given pointer
// size:
//..
//  const void *a = (void *)0x0;
//  const void *b = (void *)0xf2ff;
//  const void *c = (void *)0x0123;
//  const void *d = (void *)0xf1f2abc9;
//
//  bdlb::Print::printPtr(bsl::cout, a);
//  bsl::cout << bsl::endl;
//  bdlb::Print::printPtr(bsl::cout, b);
//  bsl::cout << bsl::endl;
//  bdlb::Print::printPtr(bsl::cout, c);
//  bsl::cout << bsl::endl;
//  bdlb::Print::printPtr(bsl::cout, d);
//  bsl::cout << bsl::endl;
//..
// will produce the following output on 'stdout':
//..
//  0
//  f2ff
//  123
//  f1f2abc9
//..
///'xxd'-Compatible 'hexDump'
/// - - - - - - - - - - - - -
// The output generated by the 'hexDump' functions is not 'xxd'-compatible
// (see 'http://gd.tuwien.ac.at/linuxcommand.org/man_pages/xxd1.html').
// The following perl script is provided that will convert 'hexDump' output
// into 'xxd'-compatible form.  Run the script with a file containing the
// 'hexDump' output as the first argument.
//..
//  #!/usr/bin/perl -w
//
//  use strict;
//
//  my $num = 0;
//  while (<>) {
//      next if (!$_);
//      my $str = $_;
//      next if !($str =~ s/^[^:]*?:\s*//);
//      my $h = sprintf("%08X",$num);
//      $str =~ s/(\S{4})([\S\W]{4})\s?([\S\W]{4})([\S\W]{4})\s?([\S\W]{4})?
//            ([\S\W]{4})?\s?([\S\W]{4})?([\S\W]{4})?/$1 $2 $3 $4 $5 $6 $7 $8/;
//      $str =~ s/\s \|([^|]+)\|.*$/ $1/;
//      print "$h: ";
//      print $str;
//      $num = $num + 16;
//  }
//..

#ifndef INCLUDED_BDLSCM_VERSION
#include <bdlscm_version.h>
#endif

#ifndef INCLUDED_BSLS_ASSERT
#include <bsls_assert.h>
#endif

#ifndef INCLUDED_BSL_OSTREAM
#include <bsl_ostream.h>
#endif

#ifndef INCLUDED_BSL_UTILITY
#include <bsl_utility.h>
#endif

namespace BloombergLP {

namespace bdlb {
                             // =================
                             // struct Print
                             // =================

struct Print {
    // Provide a namespace for the interface to a suite of procedural stream
    // operations.

    // CLASS METHODS
    static bsl::ostream& indent(bsl::ostream& stream,
                                int           level,
                                int           spacesPerLevel = 4);
        // Emit the number of spaces specified by absolute value of the
        // product of 'level' and 'spacesPerLevel' to the specified output
        // 'stream' and return a reference to the modifiable 'stream'.  If the
        // 'level' is negative, this function has no effect.

    static bsl::ostream& newlineAndIndent(bsl::ostream& stream,
                                          int           level,
                                          int           spacesPerLevel = 4);
        // If 'spacesPerLevel' is non-negative, emit a newline and the number
        // of spaces specified by the absolute value of the product of 'level'
        // and 'spacesPerLevel' to the specified output 'stream'.  If
        // 'spacesPerLevel' is negative, emit a single space (no newline).
        // Return a reference to the modifiable 'stream'.

    static void printPtr(bsl::ostream& stream, const void *value);
        // Print the specified pointer 'value' to the specified 'stream' in a
        // standard format.  The output is a hexadecimal string with a maximum
        // length of '2 * sizeof(void *)' and does not have leading zeros.  The
        // hexadecimal digits a-f are expressed in lower case.  The output is
        // *not* preceded by 0x.

    static bsl::ostream& printString(bsl::ostream&  stream,
                                     const char    *string,
                                     int            length,
                                     bool           escapeBackSlash = false);
        // Print the specified 'string' of the specified 'length' to the
        // specified output 'stream' and return a reference to 'stream'.  If
        // the optionally-specified 'escapeBackSlash' flag is 'true', then all
        // occurrences of the back-slash character ('\') in the 'string' are
        // escaped (i.e., expanded to "\\") when written to the 'stream'.
        // Note that non-printable characters in 'string' will be printed in
        // their hexadecimal representation ('\xHH').  If 'stream' is not
        // valid on entry, this operation has no effect.  The behavior is
        // undefined unless '0 <= length'.

    static bsl::ostream& hexDump(bsl::ostream&  stream,
                                 const char    *buffer,
                                 int            length);
        // Print the contents of the specified 'buffer' having the specified
        // 'length' in hexadecimal to the specified 'stream', and return a
        // reference to the modifiable 'stream'.  The behavior is undefined
        // unless '0 <= length'.

    static bsl::ostream& hexDump(bsl::ostream&                  stream,
                                 bsl::pair<const char *, int > *buffers,
                                 int                            numBuffers);
        // Print to the specified 'stream' the specified 'numBuffers' buffers
        // supplied by specified 'buffers' in a hexadecimal representation (16
        // chars per line) followed by the ASCII representation.  Return a
        // reference to the modifiable 'stream'.  The array of buffers are
        // supplied as a 'bsl::pair<const char*, int>*' where the first element
        // is a pointer to the data, and the second element is the length of
        // the buffer.  The behavior is undefined unless '0 <= numBuffers'.
        // Note that the contents of the buffers are concatenated and
        // boundaries between buffers are not demarcated.

    template <typename INPUT_ITERATOR>
    static bsl::ostream& singleLineHexDump(bsl::ostream&  stream,
                                           INPUT_ITERATOR begin,
                                           INPUT_ITERATOR end);
        // Write the uppercase hex encoding of the byte sequence defined
        // by the specified 'begin' and 'end' iterators of the parameterized
        // 'INPUT_ITERATOR' type into the specified 'stream', and return a
        // reference to the modifiable 'stream'.  Note that 'INPUT_ITERATOR'
        // need not be random-access, i.e., it need support only increment
        // ('++') and equality comparison ('==').  See the non-template
        // version of this function if insulation and/or code bloat are a
        // concern.

    static bsl::ostream& singleLineHexDump(bsl::ostream&  stream,
                                           const char    *begin,
                                           const char    *end);
        // Write the uppercase hex encoding of the byte sequence defined
        // by the specified 'begin' and 'end' iterators into the specified
        // 'stream', and return a reference to the modifiable 'stream'.   Note
        // that this function insulates clients from its implementation, but
        // unlike the member template version (above), requires random access
        // iterators of type 'const char *'.  The behavior is undefined unless
        // both 'begin' and 'end' refer to the same block of contiguous memory,
        // and 'begin <= end'.

    static bsl::ostream& singleLineHexDump(bsl::ostream&  stream,
                                           const char    *buffer,
                                           int            length);
        // Print the contents of the specified 'buffer' having the specified
        // 'length' on a single line in hexadecimal to the specified 'stream',
        // and return a reference to the modifiable 'stream'.  The behavior is
        // undefined unless '0 <= length'.
};

                      // ================================
                      // struct PrintStringHexDumper
                      // ================================

struct PrintStringHexDumper {
    // Utility for hex dumping a blob to standard output streams.  This class
    // has 'operator<<' defined for it, so it can be used as follows:
    //..
    //  bsl::vector<char> blob;
    //  blob.resize(1024);
    //
    //  // ... fill up the blob with some data ...
    //
    //  bsl::cout << PrintStringHexDumper(blob.data(), blob.size())
    //            << bsl::endl;
    //..

    // DATA
    const char *d_data_p;
    int         d_length;

    // CREATORS
    PrintStringHexDumper(const char *data, int length);
};

// FREE OPERATORS
inline
bsl::ostream& operator<<(bsl::ostream&                    stream,
                         const PrintStringHexDumper& rhs);
    // Hex dump the data referenced by the specified 'rhs' to the specified
    // 'stream'.

                 // ==========================================
                 // struct PrintStringSingleLineHexDumper
                 // ==========================================

struct PrintStringSingleLineHexDumper {
    // Utility for hex dumping a string with no extra formatting to standard
    // output streams.  This class has 'operator<<' defined for it, so it can
    // be used as follows:
    //..
    //  bsl::string str;
    //
    //  // ... fill up the str with some data ...
    //
    //  bsl::cout
    //         << PrintStringSingleLineHexDumper(str.c_str(), str.size())
    //         << bsl::endl;
    //..

    // DATA
    const char *d_data_p;
    int         d_length;

    // CREATORS
    PrintStringSingleLineHexDumper(const char *data, int length);
};

// FREE OPERATORS
inline
bsl::ostream& operator<<(bsl::ostream&                              stream,
                         const PrintStringSingleLineHexDumper& rhs);
    // Hex dump the data referenced by the specified 'rhs' to the specified
    // 'stream'.

// ============================================================================
//                          INLINE FUNCTION DEFINITIONS
// ============================================================================

                      // -----------------
                      // struct Print
                      // -----------------

// CLASS METHODS
template <typename INPUT_ITERATOR>
bsl::ostream& Print::singleLineHexDump(bsl::ostream&  stream,
                                            INPUT_ITERATOR begin,
                                            INPUT_ITERATOR end)
{
    enum { BDEU_LOCAL_BUF_SIZE = 512 };
    static const char HEX[] = "0123456789ABCDEF";

    char buf[BDEU_LOCAL_BUF_SIZE];

    unsigned int offset = 0;

    for (; begin != end; ++begin) {

        if (offset >= (BDEU_LOCAL_BUF_SIZE - 1)) {
             stream.write(buf, offset);
             offset = 0;
        }

        const unsigned char c = *begin;

        buf[offset++] = HEX[(c >> 4) & 0xF];
        buf[offset++] = HEX[c        & 0xF];
    }

    if (offset != 0) {
        stream.write(buf, offset);
    }

    return stream;
}

inline
bsl::ostream& Print::singleLineHexDump(bsl::ostream&  stream,
                                            const char    *buffer,
                                            int            length)
{
    BSLS_ASSERT_SAFE(buffer);
    BSLS_ASSERT_SAFE(0 <= length);

    return singleLineHexDump(stream, buffer, buffer + length);
}

                      // --------------------------------
                      // struct PrintStringHexDumper
                      // --------------------------------

// CREATORS
inline
PrintStringHexDumper::PrintStringHexDumper(const char *data,
                                                     int         length)
: d_data_p(data)
, d_length(length)
{
    BSLS_ASSERT_SAFE(data);
    BSLS_ASSERT_SAFE(0 <= length);

}
}  // close package namespace

// FREE OPERATORS
inline
bsl::ostream& bdlb::operator<<(bsl::ostream&                    stream,
                         const PrintStringHexDumper& rhs)
{
    return Print::hexDump(stream, rhs.d_data_p, rhs.d_length);
}

namespace bdlb {
                 // ------------------------------------------
                 // struct PrintStringSingleLineHexDumper
                 // ------------------------------------------

// CREATORS
inline
PrintStringSingleLineHexDumper::PrintStringSingleLineHexDumper(
                                                            const char *data,
                                                            int         length)
: d_data_p(data)
, d_length(length)
{
    BSLS_ASSERT_SAFE(data);
    BSLS_ASSERT_SAFE(0 <= length);
}
}  // close package namespace

// FREE OPERATORS
inline
bsl::ostream& bdlb::operator<<(bsl::ostream&                              stream,
                         const PrintStringSingleLineHexDumper& rhs)
{
    return Print::singleLineHexDump(stream, rhs.d_data_p, rhs.d_length);
}

}  // close namespace BloombergLP

#endif

// ---------------------------------------------------------------------------
// NOTICE:
//      Copyright (C) Bloomberg L.P., 2009
//      All Rights Reserved.
//      Property of Bloomberg L.P. (BLP)
//      This software is made available solely pursuant to the
//      terms of a BLP license agreement which governs its use.
// ----------------------------- END-OF-FILE ---------------------------------
