// btlscm_versiontag.h -*-C++-*-
#ifndef INCLUDED_BTLSCM_VERSIONTAG
#define INCLUDED_BTLSCM_VERSIONTAG

#ifndef INCLUDED_BSLS_IDENT
#include <bsls_ident.h>
#endif
BSLS_IDENT("$Id: $")



//@PURPOSE: Provide versioning information for the 'bte' package group.
//
//@SEE_ALSO: btlscm_version
//
//@AUTHOR: Pablo Halpern (phalpern)
//
//@DESCRIPTION: This component provides versioning information for the 'bte'
// package group.  The 'BTE_VERSION' macro that is supplied can be used for
// conditional-compilation based on 'bte' version information.  The following
// usage example illustrates this basic capability.
//
// Note that since 'bte' is always released in lock-step with 'bde', they
// share common versioning, and the 'BTE_VERSION' will always equal the
// 'BSL_VERSION'.
//
///Usage
///-----
// At compile time, the version of BTE can be used to select an older or newer
// way to accomplish a task, to enable new functionality, or to accommodate an
// interface change.  For example, if the name of a function changes (a rare
// occurrence, but potentially disruptive when it does happen), the impact on
// affected code can be minimized by conditionally calling the function by its
// old or new name using conditional compilation.  In the following, the '#if'
// preprocessor directive compares 'BTE_VERSION' (i.e., the latest BTE version,
// excluding the patch version) to a specified major and minor version composed
// using the 'BSL_MAKE_VERSION' macro:
//..
//  #if BTE_VERSION > BSL_MAKE_VERSION(1, 3)
//      // Call 'newFunction' for BTE versions later than 1.3.
//      int result = newFunction();
//  #else
//      // Call 'oldFunction' for BTE version 1.3 or earlier.
//      int result = oldFunction();
//  #endif
//..

#ifndef INCLUDED_BSLSCM_VERSIONTAG
#include <bdlscm_versiontag.h>
#endif

#define BTE_VERSION_MAJOR BSL_VERSION_MAJOR
    // BTE release major version

#define BTE_VERSION_MINOR BSL_VERSION_MINOR
    // BTE release major version

#define BTE_VERSION BSL_MAKE_VERSION(BTE_VERSION_MAJOR, \
                                     BTE_VERSION_MINOR)
    // Construct a composite version number in the range [ 0 .. 999900 ] from
    // the specified 'BTE_VERSION_MAJOR' and 'BTE_VERSION_MINOR' numbers
    // corresponding to the major and minor version numbers, respectively, of
    // the current (latest) BTE release.  Note that the patch version number is
    // intentionally not included.  For example, 'BTE_VERSION' produces 10300
    // (decimal) for BTE version 1.3.1.

#endif

// ---------------------------------------------------------------------------
// NOTICE:
//      Copyright (C) Bloomberg L.P., 2006
//      All Rights Reserved.
//      Property of Bloomberg L.P. (BLP)
//      This software is made available solely pursuant to the
//      terms of a BLP license agreement which governs its use.
// ----------------------------- END-OF-FILE ---------------------------------
