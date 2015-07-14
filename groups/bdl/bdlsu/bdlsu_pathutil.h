// bdlsu_pathutil.h                                                   -*-C++-*-
#ifndef INCLUDED_BDLSU_PATHUTIL
#define INCLUDED_BDLSU_PATHUTIL

#ifndef INCLUDED_BSLS_IDENT
#include <bsls_ident.h>
#endif
BSLS_IDENT("$Id: $")

//@PURPOSE: Provide portable file path manipulation.
//
//@CLASSES:
//  bdlsu::PathUtil: Portable utility methods for manipulating paths
//
//@AUTHOR: David Schumann (dschumann1)
//
//@SEE_ALSO: bdlsu_xxxfileutil
//
//@DESCRIPTION: This component provides utility methods for manipulating
// strings that represent paths in the filesystem.  Class methods of
// 'bdlsu::PathUtil' include platform-independent operations to add or remove
// filenames or relative paths at the end of a path string (by "filenames" we
// are referring to the names of any filesystem item, including regular files
// and directories).  There are also methods to parse the path to delimit the
// "root" as defined for the current platform; see "Parsing and Performance"
// below.
//
// Paths that have a root are called *absolute* paths, whereas paths that do
// not have a root are *relative* paths.
//
// Note that this component does not perform filesystem operations.  In
// particular, no effort is made to verify the existence or accessibility of
// any segment of any path.
//
///Parsing and Performance
///-----------------------
// Most methods of this component will perform basic parsing of the beginning
// part of the path to determine what part of it is the "root" as defined for
// the current platform.  This parsing is trivial on Unix platforms but is
// slightly more involved for the Windows operating system.  To accommodate
// client code which is willing to store parsing results in order to maximize
// performance, all methods which parse the "root" of the path accept an
// optional argument delimiting the "root"; if this argument is specified,
// parsing is skipped.
//
///Usage
///-----
// We start with strings representing an absolute native path and a relative
// native path, respectively:
//..
//  #ifdef BSLS_PLATFORM_OS_WINDOWS
//      bsl::string tempPath  = "c:\\windows\\temp";
//      bsl::string otherPath = "22jan08\\log.txt";
//  #else
//      bsl::string tempPath  = "/var/tmp";
//      bsl::string otherPath = "22jan08/log.txt";
//  #endif
//..
// 'tempPath' is an absolute path, since it has a root.  It also has a leaf
// element ("temp"):
//..
//  assert(false == bdlsu::PathUtil::isRelative(tempPath));
//  assert(true  == bdlsu::PathUtil::isAbsolute(tempPath));
//  assert(true  == bdlsu::PathUtil::hasLeaf(tempPath));
//..
// We can add filenames to the path one at a time, or we can add another path
// if is relative.  We can also remove filenames from the end of the path
// one at a time:
//..
//  bdlsu::PathUtil::appendRaw(&tempPath, "myApp");
//  bdlsu::PathUtil::appendRaw(&tempPath, "logs");
//  assert(true == bdlsu::PathUtil::isRelative(otherPath));
//  assert(0 == bdlsu::PathUtil::appendIfValid(&tempPath, otherPath));
//  assert(true == bdlsu::PathUtil::hasLeaf(tempPath));
//  bdlsu::PathUtil::popLeaf(tempPath);
//  bdlsu::PathUtil::appendRaw(&tempPath, "log2.txt");
//
//  #ifdef BSLS_PLATFORM_OS_WINDOWS
//      assert("c:\\windows\\temp\\myApp\\logs\\22jan08\\log2.txt" ==
//                                                                   tempPath);
//  #else
//      assert("/var/tmp/myApp/logs/22jan08/log2.txt" == tempPath);
//  #endif
//..
// A relative path may be appended to any other path, even itself.  An absolute
// path may not be appended to any path, or undefined behavior will result:
//..
//   assert(0 == bdlsu::PathUtil::appendIfValid(&otherPath, otherPath));  // OK
//   /* bdlsu::PathUtil::append(&otherPath, tempPath); */ // UNDEFINED BEHAVIOR!
//..
// Note that there is no attempt to distinguish filenames that are regular
// files from filenames that are directories, or to verify the existence of
// paths in the filesystem.  On POSIX:
//..
//   assert("22jan08/log.txt/22jan08/log.txt" == otherPath);
//..

#ifndef INCLUDED_BDLSCM_VERSION
#include <bdlscm_version.h>
#endif

#ifndef INCLUDED_BDLB_XXXSTRINGREF
#include <bdlb_xxxstringref.h>
#endif

#ifndef INCLUDED_BSLS_ASSERT
#include <bsls_assert.h>
#endif

#ifndef INCLUDED_BSL_STRING
#include <bsl_string.h>
#endif

namespace BloombergLP {

namespace bdlsu {
                              // =====================
                              // struct PathUtil
                              // =====================

struct PathUtil {
    // This struct contains utility methods for platform-independent
    // manipulation of filesystem paths.  No method of this struct provides
    // any filesystem operations.

    // CLASS METHODS
    static int appendIfValid(bsl::string            *path,
                             const bdlb::StringRef&  filename);
        // Append the specified 'filename' to the end of the specified 'path'
        // if 'filename' represents a relative path.  Return 0 on success, and
        // a non-zero value otherwise.  Note that any filesystem separator
        // characters at the end of 'filename' or 'path' will be discarded.

    static void appendRaw(bsl::string *path,
                            const char  *filename,
                            int          length  = -1,
                            int          rootEnd = -1);
        // Append the specified 'filename' up to the optionally specified
        // 'length' to the end of the specified 'path'.  If 'length' is
        // negative, append the entire string.  If the optionally specified
        // 'rootEnd' offset is non-negative, it is taken as the position in
        // 'path' of the character following the root.  The behavior is
        // undefined if 'filename' represents an absolute path or if either
        // 'filename' or 'path' ends with the filesystem separator character.
        // The behavior is also undefined if 'filename' points to any part of
        // 'path' (i.e., 'filename' may not be an alias for 'path').

    static int popLeaf(bsl::string *path, int rootEnd = -1);
        // Remove from 'path' the rightmost filename following the root; that
        // is, remove the leaf element.  If the optionally specified 'rootEnd'
        // offset is non-negative, it is taken as the position in 'path' of the
        // character following the root.  Return 0 on success, and a nonzero
        // value otherwise; in particular, return a nonzero value if 'path'
        // does not have a leaf.

    static int getBasename(bsl::string            *leaf,
                           const bdlb::StringRef&  path,
                           int                     rootEnd = -1);
        // Load into the specified 'leaf' the value of the rightmost
        // filename in 'path' that follows the root; that is, the leaf element.
        // If the optionally specified 'rootEnd' offset is non-negative, it is
        // taken as the position in 'path' of the character following the root.
        // Return 0 on success, and a non-zero value otherwise; in particular,
        // return nonzero if 'path' does not have a leaf.  Note that
        // 'getBasename' is a synonym for 'getLeaf'.

    static int getDirname(bsl::string            *dirname,
                          const bdlb::StringRef&  path,
                          int                     rootEnd = -1);
        // Load into the specified 'dirname' the value of the directory part
        // of the specified 'path', that is, the root if it exists and all the
        // filenames except the last one (the leaf).  If the optionally
        // specified 'rootEnd' offset is non-negative, it is taken as the
        // position in 'path' of the character following the root.  Return 0 on
        // success, and a non-zero value otherwise; in particular, return a
        // nonzero value if 'path' does not have a leaf.  Note that in the
        // case of a relative path with a single filename, the function will
        // succeed and 'dirname' will be the empty string.

    static int getLeaf(bsl::string            *leaf,
                       const bdlb::StringRef&  path,
                       int                     rootEnd = -1);
        // Load into the specified 'leaf' the value of the rightmost
        // filename in 'path' that follows the root; that is, the leaf element.
        // If the optionally specified 'rootEnd' offset is non-negative, it is
        // taken as the position in 'path' of the character following the root.
        // Return 0 on success, and a non-zero value otherwise; in particular,
        // return nonzero if 'path' does not have a leaf.  Note that
        // 'getBasename' is a synonym for 'getLeaf'.

    static int getRoot(bsl::string            *root,
                       const bdlb::StringRef&  path,
                       int                     rootEnd = -1);
        // Load into the specified 'root' the value of the root part of the
        // specified 'path'.  If the optionally specified 'rootEnd' offset is
        // non-negative, it is taken as the position in 'path' of the character
        // following the root.  Return 0 on success, and a non-zero value
        // otherwise; in particular, return a nonzero value if 'path' is
        // relative.  Note that the meaning of the root part is
        // platform-dependent.

    static bool isAbsolute(const bdlb::StringRef& path, int rootEnd = -1);
        // Return 'true' if the specified 'path' is absolute (has a root),
        // and 'false' otherwise.  If the optionally specified 'rootEnd' offset
        // is non-negative, it is taken as the position in 'path' of the
        // character following the root.

    static bool isRelative(const bdlb::StringRef& path, int rootEnd = -1);
        // Return 'true' if the specified 'path' is relative (lacks a root),
        // and 'false' otherwise.  If the optionally specified 'rootEnd' offset
        // is non-negative, it is taken as the position in 'path' of the
        // character following the root.

    static bool hasLeaf(const bdlb::StringRef& path, int rootEnd = -1);
        // Return 'true' if the specified path has a filename following the
        // root, and 'false' otherwise.  If the optionally specified 'rootEnd'
        // offset is non-negative, it is taken as the position in 'path' of
        // the character following the root.

    static int getRootEnd(const bdlb::StringRef& path);
        // Return the 0-based position in 'path' of the character following
        // the root.  Note that a return value of 0 indicates a relative path.
};

// ============================================================================
//                      INLINE FUNCTION DEFINITIONS
// ============================================================================

                           // --------------------
                           // class PathUtil
                           // --------------------

// CLASS METHODS
inline
int PathUtil::getBasename(bsl::string            *leaf,
                                const bdlb::StringRef&  path,
                                int                     rootEnd)
{
    BSLS_ASSERT_SAFE(leaf);

    return getLeaf(leaf, path, rootEnd);
}
}  // close package namespace

}  // close namespace BloombergLP

#endif

// ---------------------------------------------------------------------------
// NOTICE:
//      Copyright (C) Bloomberg L.P., 2008
//      All Rights Reserved.
//      Property of Bloomberg L.P. (BLP)
//      This software is made available solely pursuant to the
//      terms of a BLP license agreement which governs its use.
// ----------------------------- END-OF-FILE ---------------------------------
