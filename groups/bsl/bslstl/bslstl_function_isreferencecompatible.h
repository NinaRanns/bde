// bslstl_function_isreferencecompatible.h                            -*-C++-*-

#ifndef INCLUDED_BSLSTL_FUNCTION_ISREFERENCECOMPATIBLE
#define INCLUDED_BSLSTL_FUNCTION_ISREFERENCECOMPATIBLE

#include <bsls_ident.h>
BSLS_IDENT("$Id: $")

//@PURPOSE:
//
//@CLASSES:
//
//@SEE_ALSO:
//
//@DESCRIPTION:
//
///Usage
///-----

#include <bslscm_version.h>

#include <bslmf_assert.h>
#include <bslmf_conditional.h>
#include <bslmf_issame.h>
#include <bslmf_movableref.h>

#include <bsls_compilerfeatures.h>

namespace BloombergLP {

namespace bslstl {

                // =============================================
                // class template Function_IsReferenceCompatible
                // =============================================

template <class FROM_TYPE, class TO_TYPE>
struct Function_IsReferenceCompatible : bsl::conditional<
        bslmf::MovableRefUtil::IsReference<FROM_TYPE>::value,
        Function_IsReferenceCompatible<
            typename bslmf::MovableRefUtil::RemoveReference<FROM_TYPE>::type,
            TO_TYPE
        >,
        typename bsl::is_same<FROM_TYPE, TO_TYPE>::type
    >::type
{
    // This metafunction is derived from 'true_type' if a reference to the
    // specified 'FROM_TYPE' parameter type can be substituted for a reference
    // to the specified 'TO_TYPE' parameter type with no loss of information;
    // otherwise, it is derived from 'false_type'.  By default, this
    // metafunction yields 'true_type' if, after stripping off any reference
    // and/or 'const' qualifier from 'FROM_TYPE', it is the same as 'TO_TYPE';
    // else it yields 'false_type'.  However, this template can be specialized
    // to yield 'true_type for other parameters that have compatible
    // references.  This metafunction is used within an 'enable_if' to prevent
    // types that are reference compatible with 'bsl::function' from matching
    // template parameters in 'function' constructors and assignment operators,
    // preferring, instead, the non-template overloads for copy and move
    // construction and assignment.  In practice, this metafunction is used to
    // detect types supplied to generic functions that are identical to
    // 'bsl::function' or a type wrapping 'bsl::function' with no additional
    // data members (i.e., interface wrappers around 'bsl::function' would
    // specialize this trait to derive from 'true_type'). Note that reference
    // qualifiers on 'TO_TYPE' will cause instantiation to fail.
#ifndef BDE_OMIT_INTERNAL_DEPRECATED
    // 'bdef_Function' should specialize this template to yield 'true_type'
    // when 'FROM_TYPE' is an instantiation of 'bdef_Function' and 'TO_TYPE' is
    // the corresponding instantiation of 'bsl::function' with the same
    // function prototype.
#endif

    // Force compilation failure if 'TO_TYPE' is a reference type.
    BSLMF_ASSERT(! bslmf::MovableRefUtil::IsReference<TO_TYPE>::value);
};

template <class FROM_TYPE, class TO_TYPE>
struct Function_IsReferenceCompatible<FROM_TYPE, const TO_TYPE>
    : Function_IsReferenceCompatible<
          typename bsl::remove_const<
              typename bslmf::MovableRefUtil::RemoveReference<FROM_TYPE>::type
          >::type, TO_TYPE>
{
    // Partial specialization of 'Function_IsReferenceCompatible' for 'TO_TYPE'
    // being const.  The evaluation is forwarded to other specializations after
    // stripping the const qualifiers.  Note that if 'FROM_TYPE' is const and
    // 'TO_TYPE' is mutable, this partial specialization will not be selected
    // and the resulting evaluation will yield 'false_type', reflecting the
    // fact that a reference to const type cannot be bound to reference to
    // mutable type.
};

}  // close package namespace

}  // close enterprise namespace

#endif // ! defined(INCLUDED_BSLSTL_FUNCTION_ISREFERENCECOMPATIBLE)

// ----------------------------------------------------------------------------
// Copyright 2020 Bloomberg Finance L.P.
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
//     http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.
// ----------------------------- END-OF-FILE ----------------------------------
