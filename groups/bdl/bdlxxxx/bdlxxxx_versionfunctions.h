// bdlxxxx_versionfunctions.h                                            -*-C++-*-
#ifndef INCLUDED_BDLXXXX_VERSIONFUNCTIONS
#define INCLUDED_BDLXXXX_VERSIONFUNCTIONS

#ifndef INCLUDED_BSLS_IDENT
#include <bsls_ident.h>
#endif
BSLS_IDENT("$Id: $")

//@PURPOSE: Provide functions to return version information for various types.
//
//@CLASSES:
// bdlxxxx_versionfunctions: namespace for functions returning version numbers
//
//@AUTHOR: Rohan Bhindwale (rbhindwa)
//
//@DESCRIPTION: This component provides a namespace that enables finding the
// 'bdex' version number for various types.
//
// This namespace defines the 'maxSupportedVersion' function which is
// specialized to return a predecided value 'BDEX_NO_VERSION_NUMBER', also
// defined in this namespace, for each of the fundamental types, 'enum' types,
// and 'bsl::string'.  For 'bsl::vector' the 'maxSupportedVersion' returns 1 if
// either the vector is empty, or is parameterized on one of the three types
// mentioned above.  Otherwise the version number returned is the same as that
// returned by its parameterized type.  For value-semantic types it returns the
// 'bdex' version number returned by the 'maxSupportedBdexVersion' function
// defined by that type.
//
// In general this component is used by higher level 'bdex' components wanting
// to find out the version number for various types.
//
// Users can return a different 'bdex' version number for enum types than that
// returned by this component.  That can be done by defining the following
// function, where 'my_EnumType' is the 'enum' type for which the version
// information is required:
//..
// namespace bdex_VersionFunctions {
//
//     int maxSupportedVersion(my_EnumType value) {
//         const int versionNumber = 5;
//         return versionNumber;
//     }
// }
//..
///Usage
///-----
// This component will be used by clients to find out the version number for
// various types in a convenient manner.  Consider the list of fundamental
// types below:
//..
// {
//     char               c;
//     int                i;
//     double             d;
//     bsl::string        s;
//..
// Calling the 'maxSupportedVersion' function on these types will return the
// value 'BDEX_NO_VERSION_NUMBER' declared in this namespace.
//..
//    using namespace bdex_VersionFunctions;
//
//    ASSERT(BDEX_NO_VERSION_NUMBER == maxSupportedVersion(c));
//    ASSERT(BDEX_NO_VERSION_NUMBER == maxSupportedVersion(i));
//    ASSERT(BDEX_NO_VERSION_NUMBER == maxSupportedVersion(d));
//    ASSERT(BDEX_NO_VERSION_NUMBER == maxSupportedVersion(s));
//    ASSERT(BDEX_NO_VERSION_NUMBER == maxSupportedVersion(v));
// }
//..
// Now consider that the user has declared an 'enum' type and wants to find out
// the 'bdex' version number for it.  The 'maxSupportedVersion' function is
// specialized to return 'BDEX_NO_VERSION_NUMBER' for 'enum' types.
//..
//  enum my_Enum {
//      ENUM_VALUE1,
//      ENUM_VALUE2,
//      ENUM_VALUE3,
//      ENUM_VALUE4
//  };
//
//  {
//      using namespace bdex_VersionFunctions;
//
//      my_Enum e = my_Enum::ENUM_VALUE1;
//      ASSERT(BDEX_NO_VERSION_NUMBER == maxSupportedVersion(e));
//  }
//..
// If a class defines the 'maxSupportedBdexVersion' function then the
// 'maxSupportedVersion' function in this component calls that function to find
// out the version number.  Consider the 'my_Class' value-semantic type below:
//..
//  class my_Class {
//    public:
//      enum {
//        VERSION = 999
//      };
//
//      // ACCESSORS
//      int maxSupportedBdexVersion() const { return VERSION; }
//  };
//
//  {
//      using namespace bdex_VersionFunctions;
//
//      my_Class c;
//      ASSERT(my_Class::VERSION == maxSupportedVersion(c));
//  }
//..

#ifndef INCLUDED_BDLSCM_VERSION
#include <bdlscm_version.h>
#endif

#ifndef INCLUDED_BSLMF_ISENUM
#include <bslmf_isenum.h>
#endif

#ifndef INCLUDED_BSLMF_ISFUNDAMENTAL
#include <bslmf_isfundamental.h>
#endif

#ifndef INCLUDED_BSLMF_IF
#include <bslmf_if.h>
#endif

#ifndef INCLUDED_BSLS_PLATFORM
#include <bsls_platform.h>
#endif

#ifndef INCLUDED_BSL_STRING
#include <bsl_string.h>
#endif

#ifndef INCLUDED_BSL_VECTOR
#include <bsl_vector.h>
#endif

namespace BloombergLP {

namespace bdlxxxx {

struct VersionFunctions_HasVersion {
    // This 'struct' is a dummy struct used to perform function overload
    // resolution for types that have versions.

    // This struct contains no interface or implementation by design.
};

struct VersionFunctions_DoesNotHaveVersion {
    // This 'struct' is a dummy struct used to perform function overload
    // resolution for types that do *not* have versions.

    // This struct contains no interface or implementation by design.
};
}  // close package namespace

                        // ===========================
                        // class bdex_VersionFunctions
                        // ===========================

namespace bdex_VersionFunctions {
    // This 'namespace' provides functions for returning the version number for
    // various types.

    bool versionFunctionsDeprecatedFunctionIsInUse();
        // !DEPRECATED!: Temporary facility for debugging only.
        //
        // Return true.  This method exists only to provide a non-inline symbol
        // that can be found in object files, allowing us to detect if they
        // call the deprecated 'bdex_VersionFunctions::maxSupportedVersion'
        // function.  The deprecated function is inline and cannot always be
        // found in object files.

    enum {
        BDEX_NO_VERSION_NUMBER = -1           // Version number to use for
                                              // fundamental types, enums and
                                              // other types that do not have
                                              // versions.

#ifndef BDE_OMIT_INTERNAL_DEPRECATED
      , NO_VERSION_NUMBER = BDEX_NO_VERSION_NUMBER
#endif // BDE_OMIT_INTERNAL_DEPRECATED
    };

#ifdef BSLS_PLATFORM_CMP_MSVC
#pragma warning(push)
#pragma warning(disable : 4100)  // VC2008 fails to detect use of 'object'.
#endif
    template <typename TYPE>
    inline
    int maxSupportedVersion_Imp(
                                const TYPE&                             object,
                                const bdlxxxx::VersionFunctions_HasVersion&)
        // Return the maximum 'bdex' version supported by 'TYPE'.  The behavior
        // is undefined unless 'TYPE' supports a 'bdex'-compliant method
        // 'maxSupportedBdexVersion'.
    {
        // A compilation error indicating the next line of code implies the
        // class of 'TYPE' does not support the 'maxSupportedBdexVersion'
        // method.

        // Refer to non-inline symbol to help detect uses of this function.
        versionFunctionsDeprecatedFunctionIsInUse();

        return object.maxSupportedBdexVersion();
    }
#ifdef BSLS_PLATFORM_CMP_MSVC
#pragma warning(pop)
#endif

    template <typename TYPE>
    inline
    int maxSupportedVersion_Imp(
                        const TYPE&,
                        const bdlxxxx::VersionFunctions_DoesNotHaveVersion&)
        // Return 'BDEX_NO_VERSION_NUMBER'.  This function is called if the
        // specified 'object' of parameterized 'TYPE' is a fundamental or enum
        // type.  Objects of these two types do not have a version number.
    {
        // Refer to non-inline symbol to help detect uses of this function.
        versionFunctionsDeprecatedFunctionIsInUse();

        return BDEX_NO_VERSION_NUMBER;
    }

    template <typename TYPE>
    inline
    int maxSupportedVersion(const TYPE& object)
        // !DEPRECATED!: only detection of specific versions is supported.
        //
        // Return the maximum 'bdex' version supported by 'TYPE'.  If 'TYPE' is
        // a fundamental or enum type return 'BDEX_NO_VERSION_NUMBER'.  For all
        // other types the behavior is undefined unless 'TYPE' supports a
        // 'bdex'-compliant method having the same name.
    {
        // Select the function to call depending on whether the 'TYPE' supports
        // version numbers or not.  Note the use of the dummy structs
        // 'bdlxxxx::VersionFunctions_HasVersion' and
        // 'bdlxxxx::VersionFunctions_DoesNotHaveVersion' to choose the correct
        // function.

        typedef typename bslmf::If<bslmf::IsFundamental<TYPE>::VALUE
                               || bslmf::IsEnum<TYPE>::VALUE,
                                  bdlxxxx::VersionFunctions_DoesNotHaveVersion,
                                  bdlxxxx::VersionFunctions_HasVersion>::Type
                                                                     dummyType;

        // Refer to non-inline symbol to help detect uses of this function.
        versionFunctionsDeprecatedFunctionIsInUse();

        return maxSupportedVersion_Imp(object, dummyType());
    }

    // This specialization implements 'maxSupportedVersion' for 'bsl::string'.

    inline
    int maxSupportedVersion(const bsl::string&)
        // !DEPRECATED!: only detection of specific versions is supported.
        //
        // Return the max supported version of the contained string.
    {
        return BDEX_NO_VERSION_NUMBER;
    }

    // This specialization implements 'maxSupportedVersion' for 'bsl::vector'.

    template <typename TYPE, typename ALLOC>
    inline
    int maxSupportedVersion(const bsl::vector<TYPE, ALLOC>& object)
        // !DEPRECATED!: only detection of specific versions is supported.
        //
        // Return the max supported version of the contained element.
    {
        // Refer to non-inline symbol to help detect uses of this function.
        versionFunctionsDeprecatedFunctionIsInUse();

        // Vectors always put out a version number.  If 'object' is empty or
        // type does not have a version number, then put out a dummy value (1),
        // else put out the version number specified by 'object'.

        if (0 == object.size()) {
            return 1;
        }
        else {
            int version = maxSupportedVersion(object[0]);
            return version <= 0 ? 1 : version;
        }
    }

}  // close namespace bdex_VersionFunctions

}  // close namespace BloombergLP



#endif // ! defined(INCLUDED_BDEX_VERSIONFUNCTIONS)

// ---------------------------------------------------------------------------
// NOTICE:
//      Copyright (C) Bloomberg L.P., 2005
//      All Rights Reserved.
//      Property of Bloomberg L.P. (BLP)
//      This software is made available solely pursuant to the
//      terms of a BLP license agreement which governs its use.
// ----------------------------- END-OF-FILE ---------------------------------
