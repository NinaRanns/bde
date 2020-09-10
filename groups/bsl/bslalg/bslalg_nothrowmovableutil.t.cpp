// bslalg_nothrowmovableutil.t.cpp                                 -*-C++-*-

#include <bslalg_nothrowmovableutil.h>
#include <bslalg_constructorproxy.h>

#include <bslma_defaultallocatorguard.h>
#include <bslma_usesbslmaallocator.h>
#include <bslma_testallocator.h>

#include <bsls_asserttest.h>
#include <bsls_bsltestutil.h>

#include <bsltf_testvaluesarray.h>
#include <bsltf_templatetestfacility.h>

#include <stdio.h>   // 'printf'
#include <stdlib.h>  // 'atoi'

using namespace BloombergLP;

// ============================================================================
//                                  TEST PLAN
// ----------------------------------------------------------------------------
//                                  Overview
//                                  --------
//
//  Internal implementation types:
// [ 3] 'NothrowMovableUtil_Traits' class
//
// NothrowMovableWrapper class
// ---------------------------
//
// TRAITS
// [ 4] bsl::is_nothrow_move_constructible
// [ 4] bslma::UsesBslmaAllocator
// [ 4] bslma::UsesAllocatorArgT
// [ 4] bslma::IsBitwiseMoveable
//
// TYPEDEFS
// [ 4] allocator_type
// [ 4] ValueType
//
// CREATORS
// [ 5] NothrowMovableWrapper();
// [ 5] NothrowMovableWrapper(bsl::allocator_arg_t, const allocator_type& allocator);
// [ 6] NothrowMovableWrapper(const TYPE& val);  
// [ 6] NothrowMovableWrapper(bsl::allocator_arg_t,
//                            const allocator_type& allocator,
//                            const TYPE&           val);
// [ 6] NothrowMovableWrapper(bslmf::MovableRef<TYPE> val);
// [ 6] NothrowMovableWrapper(bsl::allocator_arg_t    ,
//                            const allocator_type&   allocator,
//                            bslmf::MovableRef<TYPE> val);
// [ 7] NothrowMovableWrapper(const NothrowMovableWrapper& original);
// [ 7] NothrowMovableWrapper(bsl::allocator_arg_t      ,
//                            const allocator_type&     alloc,
//                            const NothrowMovableWrapper& original);
// [ 7] NothrowMovableWrapper(bslmf::MovableRef<NothrowMovableWrapper> original)
//                            BSLS_KEYWORD_NOEXCEPT;
// [ 7] NothrowMovableWrapper(bsl::allocator_arg_t                  ,
//                            const allocator_type&                 alloc,
//                            bslmf::MovableRef<NothrowMovableWrapper> original);
// [  ] ~NothrowMovableWrapper();
//
// MANIPULATORS
// [ 8] ValueType& unwrap();
// [ 8] operator ValueType&();
//
// ACCESSORS
// [ 8] ValueType const& unwrap() const;
// [ 8] operator const ValueType&() const;
// [ 8] allocator_type get_allocator() const;
//
//
//  'NothrowMovableUtil' class
//  ---------------------------------
//
// TRAITS
// [  ] IsWrapped
// [  ] WrappedType
// [  ] UnwrappedType
//
// STATIC METHODS
// [  ] WrappedType<TYPE>::type wrap(TYPE&);
// [  ] WrappedType<TYPE>::type wrap(TYPE const&);
// [  ] WrappedType<:RemoveReference<TYPE>::type>::type
//        wrap(BSLMF_NOTHROWMOVABLEWRAPPER_DEDUCE_RVREF(TYPE);
// [  ] UnwrappedType<TYPE>::type unwrap(TYPE&);
// [  ] UnwrappedType<TYPE>::type unwrap(TYPE const&);
// [  ] WrappedType<RemoveReference<TYPE>::type>::type
//          unwrap(BSLMF_NOTHROWMOVABLEWRAPPER_DEDUCE_RVREF(TYPE);
//
// ----------------------------------------------------------------------------
// [ 3] BREATHING TEST

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

#define RUN_EACH_TYPE BSLTF_TEMPLATETESTFACILITY_RUN_EACH_TYPE

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

#if __cplusplus >= 201703L
#  define MAYBE_UNUSED [[maybe_unused]]
#elif defined(BSLS_PLATFORM_CMP_GNU) || defined(BSLS_PLATFORM_CMP_CLANG)
#  define MAYBE_UNUSED __attribute__((unused))
#else
#  define MAYBE_UNUSED
#endif

typedef bslmf::MovableRefUtil MoveUtil;

int simpleFunction(int i)
    // A simple function that can be stored in nothrow wrapper.
{
    return i;
}

typedef void (*BSLALG_NOTHROWMOVABLEUTIL_FUNCTION_PTR_TYPE)();
typedef void (BSLALG_NOTHROWMOVABLEUTIL_FUNCTION_TYPE)();


//=============================================================================
//                             USAGE EXAMPLES
//-----------------------------------------------------------------------------

namespace {

///Example 1
///- - - - -
// In this example, we define a class template, 'CountedType<TYPE>',
// that is little more than just a a wrapper around 'TYPE' that counts the
// count member along with the single value member:
//..
    template <class TYPE>
    class CountedType {
        // CLASS DATA
        static int s_count;

        // DATA
        TYPE       d_value;
//..
// Because of externally-imposed requirements, the move constructor for
// 'CountedType' must provide the strong guarantee; i.e., if the move
// constructor of 'TYPE' throws an exception, then the moved-from
// 'CountedType' object must be left unchanged.  To support this requirement,
// we next define a private static function, 'MoveIfNoexcept', similar to the
// standard 'std::move_if_noexcept', that returns a movable reference if its
// argument is nothrow move constructible and a const lvalue reference
// otherwise:
//..
        // PRIVATE CLASS FUNCTIONS
        template <class TP>
        static typename
        bsl::conditional<bsl::is_nothrow_move_constructible<TP>::value,
                         bslmf::MovableRef<TP>, const TP&>::type
        MoveIfNoexcept(TP& x);
//..
// We next finish out the class definition with a constructor, copy
// constructor, move constructor, destructor, and member functions to retrieve
// the count and value:
//..
    public:
        // CLASS FUNCTIONS
        static int count() { return s_count; }

        // CREATORS
        CountedType(const TYPE& val);
            // Construct 'CountedType' from the specified 'val'.

        CountedType(const CountedType& original);
            // Copy construct '*this' from the specified 'original' object.

        CountedType(bslmf::MovableRef<CountedType> original);
            // Move construct '*this' from 'original'.  If an exception is
            // thrown, by the constructor for 'TYPE' 'original' is unchanged.

        ~CountedType() { --s_count; }
            // Destroy this object.

        // MANIPULATORS
        TYPE& value() { return d_value; }

        // ACCESSORS
        const TYPE& value() const { return d_value; }
    };
//..
// Next, we implement 'MoveIfNoexcept', which calls 'move' on its argument,
// allowing it to convert back to an lvalue if the return type is an lvalue
// reference:
//..
    template <class TYPE>
    template <class TP>
    inline typename
    bsl::conditional<bsl::is_nothrow_move_constructible<TP>::value,
                     bslmf::MovableRef<TP>, const TP&>::type
    CountedType<TYPE>::MoveIfNoexcept(TP& x)
    {
        return bslmf::MovableRefUtil::move(x);
    }
//..
// Next, we implement the value constructor and move constructor, which simply
// copy their argument into the 'd_value' data members and increment the
// count:
//..
    template <class TYPE>
    CountedType<TYPE>::CountedType(const TYPE& val) : d_value(val)
    {
        ++s_count;
    }

    template <class TYPE>
    CountedType<TYPE>::CountedType(const CountedType& original)
        : d_value(original.d_value)
    {
        ++s_count;
    }
//..
// We're now ready implement the move constructor.  Logically, we would simply
// move the value from 'original' into the 'd_value' member of '*this', but an
// exception thrown 'TYPE''s move constructor would leave 'original' in a
// (valid but) unspecified state, violating the strong guarantee.  Instead, we
// move the value only if we know that the move will succeed; otherwise, we
// copy it.  This behavior is facilitated by the 'MoveIfNoexcept' function
// defined above:
//..
    template <class TYPE>
    CountedType<TYPE>::CountedType(bslmf::MovableRef<CountedType> original)
        : d_value(
            MoveIfNoexcept(bslmf::MovableRefUtil::access(original).d_value))
    {
        ++s_count;
    }
//..
// Finally, we define the 's_count' member to complete the class
// implementation:
//..
    template <class TYPE>
    int CountedType<TYPE>::s_count = 0;
//..
// To test the 'CountedType' class template, assume a simple client type,
// 'SomeType' that makes it easy to detect if it was move constructed.
// 'SomeType' holds an 'int' value which is set to -1 when it is moved from,
// as shown here:
//..
    class SomeType {
        int d_value;
    public:
        SomeType(int v = 0) : d_value(v) { }                        // IMPLICIT
        SomeType(const SomeType& original) : d_value(original.d_value) { }
        SomeType(bslmf::MovableRef<SomeType> original)
            : d_value(bslmf::MovableRefUtil::access(original).d_value)
            { bslmf::MovableRefUtil::access(original).d_value = -1; }

        int value() const { return d_value; }
    };
//..
// Notice that 'SomeType' neglected to declare its move constructor as
// 'noexcept'.  This might be an oversight or it could be an old class that
// predates both 'noexcept' and the 'bsl::is_nothrow_move_constructible'
// trait.  It is even be possible that the move constructor might throw
// (though, of course, it doesn't in this simplified example).  Regardless,
// the effect is that move-constructing a 'CountedType<SomeType>' will result
// in the move constructor actually performing a copy:
//..
    void usageExample1()
    {
        CountedType<SomeType> obj1(1);
        CountedType<SomeType> obj2(bslmf::MovableRefUtil::move(obj1));
        ASSERT(1 == obj1.value().value());  // Copied, not moved from
        ASSERT(1 == obj2.value().value());
//..
// For the purpose of this example, we can be sure that 'SomeThing' will not
// throw on move, at least not in our applcation.  In order to obtain the
// expected move optimiztion, we next wrap our 'SomeType in a
// 'bslalg::NothrowMovableWrapper':
//..
        CountedType<bslalg::NothrowMovableWrapper<SomeType> >
            obj3(SomeType(3));
        CountedType<bslalg::NothrowMovableWrapper<SomeType> >
            obj4(bslmf::MovableRefUtil::move(obj3));
        ASSERT(-1 == obj3.value().unwrap().value());  // moved from
        ASSERT(3 == obj4.value().unwrap().value());
    }
//..
//
///Example 2
///- - - - -
// Note that, in the last two lines of Example 1, we must call 'unwrap' in
// order to access the 'SomeType' object inside of the
// 'NothrowMovableWrapper'.  This is one situation where it would be
// attractive to have an overloadable "operator dot" so that both
// 'CoundedThing' and 'NothrowMovableWrapper' could be transparent proxy
// types.  C++ does not have overloadable operator dot, but we can create a
// 'CountedType' that is more intelligent about the existance of
// 'NothrowMovableWrapper' and automatically unwraps values for the user's
// convenience.
//
// Rather than starting from scratch, we'll build our new counted type,
// 'CountedType2' on 'CountedType'.  We start be defining a single data
// member of type 'CountedType':
//..
    template <class TYPE>
    class CountedType2 {
        CountedType<TYPE> d_data;
//..
// Next, for convenience, we add a public data type, 'ValueType' for the value
// stored within 'CountedType2'.  However, rather than defining 'ValueType'
// as simply 'TYPE', we unwrap it, in case 'TYPE' is an instantiation of
// 'NothrowMovableWrapper':
//..
    public:
        // TYPES
        typedef typename
        bslalg::NothrowMovableUtil::UnwrappedType<TYPE>::type ValueType;
//..
// Note that the 'UnwrappedType' metafunction has no affect of 'TYPE' is not
// wrapped.
//
// Next, we declare (and define) the class functions, constructors, and
// destructor, simply forwarding to the corresponding 'CountedType' function,
// constructor, or destructor:
//..
        // CLASS FUNCTIONS
        static int count() { return CountedType<TYPE>::count(); }

        // CREATORS
        CountedType2(const TYPE& val) : d_data(val) { }
        CountedType2(const CountedType2& original)
            : d_data(original.d_data) { }
        CountedType2(bslmf::MovableRef<CountedType2> original)
            : d_data(bslmf::MovableRefUtil::move(
                         bslmf::MovableRefUtil::access(original).d_data)) { }
//..
// Finally, we implement the 'value()' members such that the returned values
// do not need to be unwrapped.  As in the case of the 'UnwrappedType'
// metafunction, the 'unwrap()' function in 'NothrowMovableUtil'
// handles both wrapped and unwrapped arguments, unwrapping the latter and
// returning an unmodified reference to the former:
//..
        // MANIPULATORS
        ValueType& value()
        {
            return bslalg::NothrowMovableUtil::unwrap(d_data.value());
            // Alternatively: 'return d_data.value();'
        }

        // ACCESSORS
        const ValueType& value() const
        {
            return bslalg::NothrowMovableUtil::unwrap(d_data.value());
            // Alternatively: 'return d_data.value();'
        }
    };
//..
// Note the alternative code for these members: A 'NothrowMovableWrapper<TP>'
// object is implicitly convertible to 'TP&', so if 'TYPE' is a
// 'NothrowMovableWrapper', the simple return statement will implicitly unwrap
// it.
//
// Using a similar example for 'CountedType2' as we used for 'CountedType', we
// see that the usage of 'CountedType2' with and without
// 'NothrowMovableWrapper' is the same:
//..
    void usageExample2()
    {
        CountedType2<SomeType> obj1(1);
        CountedType2<SomeType> obj2(bslmf::MovableRefUtil::move(obj1));
        ASSERT(1 == obj1.value().value());  // Copied, not moved from
        ASSERT(1 == obj2.value().value());

        CountedType2<bslalg::NothrowMovableWrapper<SomeType> >
            obj3(SomeType(3));
        CountedType2<bslalg::NothrowMovableWrapper<SomeType> >
            obj4(bslmf::MovableRefUtil::move(obj3));
        ASSERT(-1 == obj3.value().value());  // moved from
        ASSERT(3 == obj4.value().value());   // No need to call 'unwrap'
    }

} // end unnamed namespace

// ============================================================================
//                          TEST DRIVER TEMPLATE
// ----------------------------------------------------------------------------

template <class TEST_TYPE>
class TestDriver {
    // This class template provides a namespace for testing the 'bslalg::NothrowMovableWrapper'
    // type.

  private:
    // PRIVATE TYPES
    typedef TEST_TYPE ValueType;
        // ValueType under test.

    typedef bslalg::NothrowMovableUtil Obj;
        // Type under test.

    typedef bslalg::NothrowMovableWrapper<ValueType> Wrapper;
        // Type under test.    

  public:

    static void testCase3();
};

template <class TYPE>
void TestDriver<TYPE>::testCase3()
{
    // --------------------------------------------------------------------
    // TESTING 'NothrowMovableUtil_Traits' CLASS
    //
    // Concerns:
    //
    // Plan:
    //
    // Testing:
    //  NothrowMovableUtil_Traits::IsWrapped
    //  NothrowMovableUtil_Traits::UnwrappedType
    //  NothrowMovableUtil_Traits::WrappedType
    // --------------------------------------------------------------------
    if (verbose)
        printf("\nTESTING 'NothrowMovableUtil_Traits' CLASS"
                "\n===========================================\n");

    {
        ASSERT(!(bslalg::NothrowMovableUtil_Traits<TYPE>::IsWrapped::value));
        ASSERT(!(bsl::is_function<TYPE>::value) &&
            (bsl::is_same<
            typename bslalg::NothrowMovableUtil_Traits<Wrapper>::UnwrappedType,
            TYPE>::value) || 
            (bsl::is_function<TYPE>::value) &&
            (bsl::is_same<
            typename bslalg::NothrowMovableUtil_Traits<Wrapper>::UnwrappedType,
            TYPE *>::value));
        ASSERT(!(bsl::is_function<TYPE>::value) &&
            (bsl::is_same<
            typename bslalg::NothrowMovableUtil_Traits<Wrapper>::WrappedType,
            typename bslalg::NothrowMovableWrapper<typename bsl::remove_cv<TYPE>::type> >::value)
            ||
            (bsl::is_function<TYPE>::value) &&
            (bsl::is_same<
            typename bslalg::NothrowMovableUtil_Traits<Wrapper>::WrappedType,
            typename bslalg::NothrowMovableWrapper<TYPE*> >::value));

        ASSERT((bslalg::NothrowMovableUtil_Traits<Wrapper>::IsWrapped::value));
        ASSERT((bsl::is_same<
            typename bslalg::NothrowMovableUtil_Traits<Wrapper>::UnwrappedType,
            ValueType>::value));
        ASSERT((bsl::is_same<
            typename bslalg::NothrowMovableUtil_Traits<Wrapper>::WrappedType,
            Wrapper>::value));   
    }

}

//=============================================================================
//                              MAIN PROGRAM
//-----------------------------------------------------------------------------

int main(int argc, char *argv[])
{
    int test = argc > 1 ? atoi(argv[1]) : 0;
    verbose = argc > 2;
    veryVerbose = argc > 3;
    veryVeryVerbose = argc > 4;
    veryVeryVeryVerbose = argc > 5;

    printf("TEST " __FILE__ " CASE %d\n", test);

    switch (test) { case 0:  // Zero is always the leading case.
        case 4: {
            // --------------------------------------------------------------------
            // TESTING 'NothrowMovableUtil' TRAITS
            //
            //  This test checks the correct behaviour of 'NothrowMovableUtil' 
            //  traits when `TYPE` is not a specialization of NothrowMovableWrapper`
            //
            // Concerns:
            //: 1 `IsWrapped` trait is satisfied if and only if 'TYPE' is a
            //:  specialization of 'NothrowMovableWrapper`.
            //: 2 `WrappedType::type` is `NothrowMovableWrapper<TYPE>' if `TYPE` is 
            //:   not a specialization of `NothrowMovableWrapper`, and `TYPE' 
            //:   otherwise.
            //: 2 `WrappedType::type` is `TYPE` if `TYPE` is not a specialization of
            //:   `NothrowMovableWrapper`, and `TYPE::ValueType` otherwise.
            //
            // Plan:
            //: 1 Using a `TYPE` which is not a specialization of 
            //:   'NothrowMovableWrapper` check that `IsWrapped` trait is not 
            //:   satisfied.  [C-1]
            //: 2 In step 1, check that `WrappedType::type` is 
            //:   `NothrowMovableWrapper<TYPE>'.  [C-2] 
            //: 3 In step 1, check that `UnwrappedType::type` is 
            //:   `TYPE`.  [C-2] 
            //: 4 Using a `NothrowMovableWrapper<TYPE> as test type, check that 
            //:   `IsWrapped` trait is satisfied.  [C-1]
            //: 5 In step 5, check that `WrappedType::type` is `TYPE'.  [C-2] 
            //: 6 In step 6, check that `UnwrappedType::type` is 
            //:   `TYPE::ValueType`.  [C-2] 
            //  
            // Testing:
            //  NothrowMovableUtil::IsWrapped
            //  NothrowMovableUtil::UnwrappedType
            //  NothrowMovableUtil::WrappedType
            // --------------------------------------------------------------------
            if (verbose)
                printf("\nTESTING 'NothrowMovableUtil' TRAITS"
                        "\n=========================================\n");

            {
                ASSERT(!(bslalg::NothrowMovableUtil::IsWrapped<int>::value));
                ASSERT((bsl::is_same<
                    bslalg::NothrowMovableUtil::UnwrappedType<int>::type,
                    int>::value));
                ASSERT((bsl::is_same<
                    bslalg::NothrowMovableUtil::WrappedType<int>::type,
                    bslalg::NothrowMovableWrapper<int> >::value));

                ASSERT(!(bslalg::NothrowMovableUtil::IsWrapped<const int>::value));
                ASSERT((bsl::is_same<
                    bslalg::NothrowMovableUtil::UnwrappedType<const int>::type,
                    const int>::value));
                ASSERT((bsl::is_same<
                    bslalg::NothrowMovableUtil::WrappedType<const int>::type,
                    bslalg::NothrowMovableWrapper<int> >::value));
                    
                ASSERT(!(bslalg::NothrowMovableUtil::IsWrapped<BSLALG_NOTHROWMOVABLEUTIL_FUNCTION_PTR_TYPE>::value));
                ASSERT((bsl::is_same<
                    bslalg::NothrowMovableUtil::UnwrappedType<BSLALG_NOTHROWMOVABLEUTIL_FUNCTION_PTR_TYPE>::type,
                    BSLALG_NOTHROWMOVABLEUTIL_FUNCTION_PTR_TYPE>::value));
                ASSERT((bsl::is_same<
                    bslalg::NothrowMovableUtil::WrappedType<BSLALG_NOTHROWMOVABLEUTIL_FUNCTION_PTR_TYPE>::type,
                    bslalg::NothrowMovableWrapper<BSLALG_NOTHROWMOVABLEUTIL_FUNCTION_PTR_TYPE> >::value));

                ASSERT(!(bslalg::NothrowMovableUtil::IsWrapped<BSLALG_NOTHROWMOVABLEUTIL_FUNCTION_TYPE>::value));
                ASSERT((bsl::is_same<
                    bslalg::NothrowMovableUtil::UnwrappedType<BSLALG_NOTHROWMOVABLEUTIL_FUNCTION_TYPE>::type,
                    BSLALG_NOTHROWMOVABLEUTIL_FUNCTION_TYPE *>::value));
                ASSERT((bsl::is_same<
                    bslalg::NothrowMovableUtil::WrappedType<BSLALG_NOTHROWMOVABLEUTIL_FUNCTION_TYPE>::type,
                    bslalg::NothrowMovableWrapper<BSLALG_NOTHROWMOVABLEUTIL_FUNCTION_TYPE *> >::value));
            }
            {

                typedef bslalg::NothrowMovableWrapper<int> NTMB_INT;
                typedef bslalg::NothrowMovableWrapper<const int> NTMB_CONST_INT;
                typedef bslalg::NothrowMovableWrapper<BSLALG_NOTHROWMOVABLEUTIL_FUNCTION_PTR_TYPE>
                             NTMB_FUNCTION_PTR_TYPE;
                

                
                ASSERT((bslalg::NothrowMovableUtil::IsWrapped<NTMB_INT>::value));
                ASSERT((bsl::is_same<
                    bslalg::NothrowMovableUtil::UnwrappedType<NTMB_INT>::type,
                    int>::value));
                ASSERT((bsl::is_same<
                    bslalg::NothrowMovableUtil::WrappedType<NTMB_INT>::type,
                    NTMB_INT>::value));

                ASSERT((bslalg::NothrowMovableUtil::IsWrapped<NTMB_CONST_INT>::value));
                ASSERT((bsl::is_same<
                    bslalg::NothrowMovableUtil::UnwrappedType<NTMB_CONST_INT>::type,
                    const int>::value));
                ASSERT((bsl::is_same<
                    bslalg::NothrowMovableUtil::WrappedType<NTMB_CONST_INT>::type,
                    NTMB_CONST_INT >::value));
                    
                ASSERT((bslalg::NothrowMovableUtil::IsWrapped<NTMB_FUNCTION_PTR_TYPE>::value));
                ASSERT((bsl::is_same<
                    bslalg::NothrowMovableUtil::UnwrappedType<NTMB_FUNCTION_PTR_TYPE>::type,
                    BSLALG_NOTHROWMOVABLEUTIL_FUNCTION_TYPE *>::value));
                ASSERT((bsl::is_same<
                    bslalg::NothrowMovableUtil::WrappedType<NTMB_FUNCTION_PTR_TYPE>::type,
                    NTMB_FUNCTION_PTR_TYPE >::value));
            }
        } break;    
      case 3: {
        RUN_EACH_TYPE(TestDriver,
                      testCase3,
                      BSLTF_TEMPLATETESTFACILITY_TEST_TYPES_REGULAR);
      } break;
      case 2: {
        // --------------------------------------------------------------------
        // USAGE EXAMPLE
        // --------------------------------------------------------------------

        if (verbose) printf("\nBREATHING TEST"
                            "\n==============\n");

        usageExample2();
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

        bslalg::NothrowMovableUtil::WrappedType<int(int)>::type
            funcWrap2 = bslalg::NothrowMovableUtil::wrap(
                simpleFunction);
        int (*&pfunc2)(int) = funcWrap2.unwrap();
        ASSERT(pfunc2 == &simpleFunction);

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
