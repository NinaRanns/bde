// bslstl_function_isreferencecompatible.t.cpp                        -*-C++-*-

#include "bslstl_function_isreferencecompatible.h"

#include <bsls_asserttest.h>
#include <bsls_bsltestutil.h>

#include <stdio.h>   // 'printf'
#include <stdlib.h>  // 'atoi'

using namespace BloombergLP;

//=============================================================================
//                             TEST PLAN
//-----------------------------------------------------------------------------
//
//

//-----------------------------------------------------------------------------

//=============================================================================
//                       STANDARD BDE ASSERT TEST MACRO
//-----------------------------------------------------------------------------
// NOTE: THIS IS A LOW-LEVEL COMPONENT AND MAY NOT USE ANY C++ LIBRARY
// FUNCTIONS, INCLUDING IOSTREAMS.
static int testStatus = 0;

void aSsErT(bool b, const char *s, int i)
{
    if (b) {
        printf("Error " __FILE__ "(%d): %s    (failed)\n", i, s);
        if (testStatus >= 0 && testStatus <= 100) ++testStatus;
    }
}

# define ASSERT(X) { aSsErT(!(X), #X, __LINE__); }

//=============================================================================
//                       STANDARD BDE TEST DRIVER MACROS
//-----------------------------------------------------------------------------
#define LOOP_ASSERT  BSLS_BSLTESTUTIL_LOOP_ASSERT
#define LOOP2_ASSERT BSLS_BSLTESTUTIL_LOOP2_ASSERT
#define LOOP3_ASSERT BSLS_BSLTESTUTIL_LOOP3_ASSERT
#define LOOP4_ASSERT BSLS_BSLTESTUTIL_LOOP4_ASSERT
#define LOOP5_ASSERT BSLS_BSLTESTUTIL_LOOP5_ASSERT
#define LOOP6_ASSERT BSLS_BSLTESTUTIL_LOOP6_ASSERT

#define Q   BSLS_BSLTESTUTIL_Q   // Quote identifier literally.
#define P   BSLS_BSLTESTUTIL_P   // Print identifier and value.
#define P_  BSLS_BSLTESTUTIL_P_  // P(X) without '\n'.
#define T_  BSLS_BSLTESTUTIL_T_  // Print a tab (w/o newline).
#define L_  BSLS_BSLTESTUTIL_L_  // current Line number

//=============================================================================
//                  SEMI-STANDARD NEGATIVE-TESTING MACROS
//-----------------------------------------------------------------------------
#define ASSERT_SAFE_PASS(EXPR) BSLS_ASSERTTEST_ASSERT_SAFE_PASS(EXPR)
#define ASSERT_SAFE_FAIL(EXPR) BSLS_ASSERTTEST_ASSERT_SAFE_FAIL(EXPR)
#define ASSERT_PASS(EXPR)      BSLS_ASSERTTEST_ASSERT_PASS(EXPR)
#define ASSERT_FAIL(EXPR)      BSLS_ASSERTTEST_ASSERT_FAIL(EXPR)
#define ASSERT_OPT_PASS(EXPR)  BSLS_ASSERTTEST_ASSERT_OPT_PASS(EXPR)
#define ASSERT_OPT_FAIL(EXPR)  BSLS_ASSERTTEST_ASSERT_OPT_FAIL(EXPR)

//=============================================================================
//                                VERBOSITY
//-----------------------------------------------------------------------------

static int verbose = 0;
static int veryVerbose = 0;
static int veryVeryVerbose = 0;
static int veryVeryVeryVerbose = 0; // For test allocators

//=============================================================================
//                  GLOBAL TYPEDEFS/CONSTANTS FOR TESTING
//-----------------------------------------------------------------------------


//=============================================================================
//                             USAGE EXAMPLES
//-----------------------------------------------------------------------------


//=============================================================================
//                              MAIN PROGRAM
//-----------------------------------------------------------------------------

namespace bsl {

template <class PROTOTYPE>
class function {
    // Stub implementation of 'bsl::function' for testing.
    // An object of this type is never created in this test driver, but
    // conversion to and from this type is tested.
};

} // Close namespace bsl

namespace BloombergLP {

template <class PROTOTYPE> class bdef_Function;

template <class PROTOTYPE>
class bdef_Function<PROTOTYPE*> : public bsl::function<PROTOTYPE> {
    // Stub implementation of 'BloombergLP::bdef_Function' for testing.
    // An object of this type is never created in this test driver, but
    // conversion to and from this type is tested.
};

namespace bslstl {

// Specialize 'bsl::Function_IsReferenceCompatible' metafunction to indicate
// that references to 'bdef_Function' can be used as references to
// 'bsl::function'.
template <class PROTOTYPE>
struct Function_IsReferenceCompatible<BloombergLP::bdef_Function<PROTOTYPE*>,
                                      bsl::function<PROTOTYPE> >
    : bsl::true_type
{
};

}  // close namespace bslstl

}  // close enterprise namespace

typedef int PROTO(const char *, int);  // Function prototype

// Return 1 if called with a non-const 'bsl::function<PROTO>'.
int bdefOverload(BloombergLP::bdef_Function<PROTO*>&)
{
    return 1;
}

// Return 2 if called with a const 'bsl::function<PROTO>'.
int bdefOverload(const BloombergLP::bdef_Function<PROTO*>&)
{
    return 2;
}

struct BaseClass { };
struct DerivedClass : BaseClass { };

template <class T, class U, bool EXP, int LINE>
void testIsReferenceCompatible()
    // Instantiates 'Function_IsReferenceCompatible<T, U>' and verifies that
    // the result type has a const member 'value' such that 'value ==
    // EXP'. Also instantiate 'Function_IsReferenceCompatible<T, U>' with
    // every combination of const and reference qualifiers on 'T' and const
    // quailifier on 'U', verifying the expected result 'value' (which is
    // sometimes 'false' even if 'EXP' is true).
{
    using namespace BloombergLP;

    const bool res = bslstl::Function_IsReferenceCompatible<T, U>::value;
    LOOP_ASSERT(LINE, EXP == res);

#define TEST(TT, UU, EE)                                                     \
    LOOP_ASSERT(LINE,                                                        \
                (EE == bslstl::Function_IsReferenceCompatible<TT, UU>::value))

    // Test reference qualifiers
    TEST(T&                         , U       , res  );
    TEST(bslmf::MovableRef<T>       , U       , res  );

    // Test const qualifiers
    TEST(T                          , U const , res  );
    TEST(T&                         , U const , res  );
    TEST(bslmf::MovableRef<T>       , U const , res  );
    TEST(T const                    , U const , res  );
    TEST(T const&                   , U const , res  );
    TEST(bslmf::MovableRef<T const> , U const , res  );
    TEST(T const                    , U       , false);
    TEST(T const&                   , U       , false);
    TEST(bslmf::MovableRef<T const> , U       , false);

#undef TEST
}

int main(int argc, char *argv[])
{
    int test = argc > 1 ? atoi(argv[1]) : 0;
    verbose = argc > 2;
    veryVerbose = argc > 3;
    veryVeryVerbose = argc > 4;
    veryVeryVeryVerbose = argc > 5;

    printf("TEST " __FILE__ " CASE %d\n", test);

    switch (test) { case 0:  // Zero is always the leading case.
      case 2: {
        // --------------------------------------------------------------------
        // TESTING 'Function_IsReferenceCompatible'
        //
        // Concerns:
        //: 1 An instantiation of 'Function_IsReferenceCompatible<T,U>' where
        //:   'T' and 'U' are the same type yields (class derived from)
        //:   'true_type'.
        //: 2 An *unspecialized* instantiation of
        //:   'Function_IsReferenceCompatible<T,U>' where 'T' and 'U' are
        //:   different types yields 'false_type', even if 'T' is derived from
        //:   'U'.
        //: 3 An instantiation of 'Function_IsReferenceCompatible<T,U>' that
        //:   matches an explicit specialization or partial specialization
        //:   yields the result of that specialization or partial
        //:   specialization.
        //: 4 Lvalue and rvalue reference qualifiers on 'T' are ignored when
        //:   evaluating 'Function_IsReferenceCompatible<T,U>'.
        //: 5 If 'T' is const qualified and 'U' is not const qualified,
        //:   'Function_IsReferenceCompatible<T,U>' yields 'false_type' even
        //:   if 'T' and 'U' are reference compatible. If 'U' is const
        //:   qualified, then the const qualifiers on both 'T' and 'U' are
        //:   ignored.  (This semantic adheres to the language rule that const
        //:   references do not bind to modifiable references).
        //
        // Plan:
        //: 1 For concern 1, instantiate
        //:   'Function_IsReferenceCompatible<T,T>', where 'T' is 'int', a
        //:   'bsl::function' specialization and a 'bdef_Function'
        //:   specialization. Verify that the contained 'value' constant is
        //:   'true'.
        //: 2 For concern 2, instantiate 'Function_IsReferenceCompatible<T,U>'
        //:   where 'T' and 'U' are different, incompatible types, including
        //:   the case where 'T' is derived from 'U'. Verify that the
        //:   contained 'value' constant is 'false'.
        //: 3 For concern 3, instantiate 'Function_IsReferenceCompatible<T,U>'
        //:   where 'T' is a specialization of 'bdef_Function' and 'U' is a
        //:   specializaton of 'bsl::function' based on the same function
        //:   prototype. Verify that it yields 'true_type' (because there is
        //:   an explicit specialization). Repeat this step using
        //:   'bdef_Function' and 'bsl::function' based on different function
        //:   prototypes and verify that it yields 'false_type'.
        //: 4 For concern 4 repeat the above steps, adding lvalue and rvalue
        //:   qualifiers to the first parameter of
        //:   'Function_IsReferenceCompatible'. Verify that the result does
        //:   not change.
        //: 5 For concern 5, repeat the above steps with const qualifiers on
        //:   the first parameter, the second parameter, or both. Verify that
        //:   if the first paramter is const and the second is not const, then
        //:   the result is 'false_type'; otherwise the result is the same as
        //:   without the const qualifiers.
        //: 6 To simplify testing the cross-product of types,
        //:   reference qualifiers, and const-qualifiers, encapsulate steps 4
        //:   and 5 into a function, 'testIsReferenceCompatible', which is
        //:   instantiated with a 'T', a 'U', and an expected result and which
        //:   tests each combination of reference and const qualifiers.
        //
        // Testing:
        //      Function_IsReferenceCompatible
        // --------------------------------------------------------------------

        if (verbose) printf("\nTESTING 'Function_IsReferenceCompatible'"
                            "\n=======================================\n");

        typedef bsl::function<int(char*)>    bslFunc1;
        typedef bdef_Function<int(*)(char*)> bdefFunc1;
        typedef bdef_Function<int(*)(void*)> bdefFunc2;

#define TEST(T, U, EXP) testIsReferenceCompatible<T, U, EXP, __LINE__>()

        // All tests perform steps 4, 5, and 6

        //   T             U              EXP
        //   ============  =============  =====
        // Step 1
        TEST(int          , int          , true );
        TEST(bslFunc1     , bslFunc1     , true );
        TEST(bdefFunc1    , bdefFunc1    , true );
        TEST(BaseClass    , BaseClass    , true );
        TEST(DerivedClass , DerivedClass , true );

        // Step 2
        TEST(int          , short        , false);
        TEST(DerivedClass , BaseClass    , false);
        TEST(BaseClass    , DerivedClass , false);
        TEST(bdefFunc1    , bdefFunc2    , false);

        // Step 3
        TEST(bdefFunc1    , bslFunc1     , true );
        TEST(bdefFunc2    , bslFunc1     , false);
        TEST(bslFunc1     , bdefFunc1    , false);

#undef TEST

      } break;
      case 1: {
        // --------------------------------------------------------------------
        // BREATHING/USAGE TEST
        //
        // Concerns:
        //
        // Plan:
        //
        // Testing:
        //
        // --------------------------------------------------------------------

        if (verbose) printf("\nBREATHING TEST"
                            "\n==============\n");

      } break;

      default: {
        fprintf(stderr, "WARNING: CASE `%d' NOT FOUND.\n", test);
        testStatus = -1;
      }
    }

    if (testStatus > 0) {
        fprintf(stderr, "Error, non-zero test status = %d.\n", testStatus);
    }

    return testStatus;
}

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
