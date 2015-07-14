// ball_severityutil.h              -*-C++-*-
#ifndef INCLUDED_BALL_SEVERITYUTIL
#define INCLUDED_BALL_SEVERITYUTIL

#ifndef INCLUDED_BSLS_IDENT
#include <bsls_ident.h>
#endif
BSLS_IDENT("$Id: $")

//@PURPOSE: Provide a suite of utility functions on 'ball::Severity' levels.
//
//@CLASSES:
//   ball::SeverityUtil: namespace for pure procedures on 'ball::Severity::Level'
//
//@AUTHOR: Steven Breitstein (sbreitst), Henry Verschell (hversche)
//
//@SEE_ALSO: ball_severity
//
//@DESCRIPTION: This component provides a suite of pure procedures that apply
// to the 'ball::Severity::Level' enumeration.  In particular, the
// 'ball::SeverityUtil' 'struct' provides a 'fromAsciiCaseless' function that
// returns the 'ball::Severity::Level' enumerator value corresponding to a
// given ASCII string (without regard to the case of the characters in the
// string) and an 'isValidNameCaseless' function that confirms that a given
// string corresponds to one of the enumerators in the 'ball::Severity::Level'
// enumeration (similarly, without regard to the case of the characters in the
// string).
//
///Synopsis
///--------
// The following is a list of functions available in this component:
//..
//    static int  ball::SeverityUtil::fromAsciiCaseless(
//                                              ball::Severity::Level *level,
//                                              const char           *name);
//
//    static bool ball::SeverityUtil::isValidNameCaseless(const char *name);
//..
///Usage
///-----
// In this example, we show how to validate that a set of C-style strings
// correspond to 'ball::Severity::Level' enumerators, and then use those strings
// to generate enumerator values that, in turn, may be used to administer a
// logger manager.  Here, for convenience, we define our strings in an array,
// much as how we might receive them from a command line:
//..
//    const char *argv[] = {
//        "INFO",   // record
//        "WARN",   // pass
//        "ERROR",  // trigger
//        "FATAL"   // trigger-all
//    };
//
//    assert(ball::SeverityUtil::isValidNameCaseless(argv[0]));
//    assert(ball::SeverityUtil::isValidNameCaseless(argv[1]));
//    assert(ball::SeverityUtil::isValidNameCaseless(argv[2]));
//    assert(ball::SeverityUtil::isValidNameCaseless(argv[3]));
//
//    ball::Severity::Level record;
//    ball::Severity::Level pass;
//    ball::Severity::Level trigger;
//    ball::Severity::Level triggerAll;
//
//    assert(0 == ball::SeverityUtil::fromAsciiCaseless(&record,     argv[0]));
//    assert(0 == ball::SeverityUtil::fromAsciiCaseless(&pass,       argv[1]));
//    assert(0 == ball::SeverityUtil::fromAsciiCaseless(&trigger,    argv[2]));
//    assert(0 == ball::SeverityUtil::fromAsciiCaseless(&triggerAll, argv[3]));
//
//    assert(ball::Severity::BAEL_INFO  == record);
//    assert(ball::Severity::BAEL_WARN  == pass);
//    assert(ball::Severity::BAEL_ERROR == trigger);
//    assert(ball::Severity::BAEL_FATAL == triggerAll);
//..

#ifndef INCLUDED_BALSCM_VERSION
#include <balscm_version.h>
#endif

#ifndef INCLUDED_BALL_SEVERITY
#include <ball_severity.h>
#endif

namespace BloombergLP {

namespace ball {
                        // ========================
                        // struct SeverityUtil
                        // ========================

struct SeverityUtil {
    // This 'struct' provides a namespace for non-primitive procedures on the
    // 'Severity::Level' enumeration.

    // CLASS METHODS
    static int fromAsciiCaseless(Severity::Level *level,
                                 const char           *name);
        // Load into the specified 'level' the value of the
        // 'Severity::Level' enumerator corresponding to the specified
        // 'name' (without regard to the case of the characters in 'name').
        // Return 0 on success, and a non-zero value with no effect on
        // 'level' otherwise.  The behavior is undefined unless 'level' is
        // non-null and 'name' is a null-terminated (C-style) string.  Note
        // that this procedure will fail unless true == isValidName(name).

    static bool isValidNameCaseless(const char *name);
        // Return 'true' if the specified 'name' corresponds to an enumerator
        // in the 'Severity::Level' enumeration (without regard to the
        // characters in 'name'), and 'false' otherwise.  The behavior is
        // undefined unless 'name' is a null-terminated (C-style) string.
        // Note that the names corresponding to 'Severity::Level'
        // enumerators are case *insensitive*.

    static int fromAscii(Severity::Level *level, const char *name);
        // Load into the specified 'level' the value of the
        // 'Severity::Level' enumerator corresponding to the specified
        // 'name' (without regard to the case of the characters in 'name').
        // Return 0 on success, and a non-zero value with no effect on
        // 'level' otherwise.  The behavior is undefined unless 'level' is
        // non-null and 'name' is a null-terminated (C-style) string.  Note
        // that this procedure will fail unless true == isValidName(name).
        //
        // DEPRECATED: Use 'fromAsciiCaseless'.  Note that this method
        // provides backwards compatibility for clients of the deprecated
        // 'baelu::Severity'.

    static bool isValidName(const char *name);
        // Return 'true' if the specified 'name' corresponds to an enumerator
        // in the 'Severity::Level' enumeration, and 'false' otherwise.
        // The behavior is undefined unless 'name' is a null-terminated
        // (C-style) string.  Note that the names corresponding to
        // 'Severity::Level' enumerators are case *insensitive*.
        //
        // DEPRECATED: Use 'isValidNameCaseless'.  Note that this method
        // provides backwards compatibility for clients of the deprecated
        // 'baelu::Severity'.
};

// ===========================================================================
//                        INLINE FUNCTION DEFINITIONS
// ===========================================================================

                        // ------------------------
                        // struct SeverityUtil
                        // ------------------------

// CLASS METHODS
inline
int SeverityUtil::fromAscii(Severity::Level *level,
                                 const char           *name)
{
    return fromAsciiCaseless(level, name);
}

inline
bool SeverityUtil::isValidName(const char *name)
{
    return isValidNameCaseless(name);
}
}  // close package namespace

}  // close namespace BloombergLP

#endif

// ---------------------------------------------------------------------------
// NOTICE:
//      Copyright (C) Bloomberg L.P., 2005
//      All Rights Reserved.
//      Property of Bloomberg L.P. (BLP)
//      This software is made available solely pursuant to the
//      terms of a BLP license agreement which governs its use.
// ----------------------------- END-OF-FILE ---------------------------------
