// bslalg_nothrowmovableutil.t.cpp                                    -*-C++-*-
#include <bslalg_nothrowmovableutil.h>

#include <bslalg_constructorproxy.h>

#include <bslma_defaultallocatorguard.h>
#include <bslma_testallocator.h>
#include <bslma_usesbslmaallocator.h>

#include <bslmf_isconst.h>
#include <bsls_asserttest.h>
#include <bsls_bsltestutil.h>

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
// TRAITS
// [ 4] IsWrapped
// [ 4] WrappedType
// [ 4] UnwrappedType
//
// STATIC METHODS
// [ 5] WrappedType<TYPE>::type wrap(TYPE&);
// [ 5] WrappedType<TYPE>::type wrap(TYPE const&);
// [ 5] WrappedType<:RemoveReference<TYPE>::type>::type
//        wrap(BSLMF_NOTHROWMOVABLEWRAPPER_DEDUCE_RVREF(TYPE);
// [ 6] UnwrappedType<TYPE>::type unwrap(TYPE&);
// [ 6] UnwrappedType<TYPE>::type unwrap(TYPE const&);
// [ 6] WrappedType<RemoveReference<TYPE>::type>::type
//          unwrap(BSLMF_NOTHROWMOVABLEWRAPPER_DEDUCE_RVREF(TYPE);
//
// ----------------------------------------------------------------------------
// [ 2] USAGE EXAMPLE
// [ 1] BREATHING TEST
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
        if (testStatus >= 0 && testStatus <= 100)
            ++testStatus;
    }
}

#define ASSERT(X)                                                             \
    {                                                                         \
        aSsErT(!(X), #X, __LINE__);                                           \
    }

//=============================================================================
//                       STANDARD BDE TEST DRIVER MACROS
//-----------------------------------------------------------------------------
#define LOOP_ASSERT BSLS_BSLTESTUTIL_LOOP_ASSERT
#define LOOP2_ASSERT BSLS_BSLTESTUTIL_LOOP2_ASSERT
#define LOOP3_ASSERT BSLS_BSLTESTUTIL_LOOP3_ASSERT
#define LOOP4_ASSERT BSLS_BSLTESTUTIL_LOOP4_ASSERT
#define LOOP5_ASSERT BSLS_BSLTESTUTIL_LOOP5_ASSERT
#define LOOP6_ASSERT BSLS_BSLTESTUTIL_LOOP6_ASSERT

#define Q BSLS_BSLTESTUTIL_Q    // Quote identifier literally.
#define P BSLS_BSLTESTUTIL_P    // Print identifier and value.
#define P_ BSLS_BSLTESTUTIL_P_  // P(X) without '\n'.
#define T_ BSLS_BSLTESTUTIL_T_  // Print a tab (w/o newline).
#define L_ BSLS_BSLTESTUTIL_L_  // current Line number

#define RUN_EACH_TYPE BSLTF_TEMPLATETESTFACILITY_RUN_EACH_TYPE

//=============================================================================
//                  SEMI-STANDARD NEGATIVE-TESTING MACROS
//-----------------------------------------------------------------------------
#define ASSERT_SAFE_PASS(EXPR) BSLS_ASSERTTEST_ASSERT_SAFE_PASS(EXPR)
#define ASSERT_SAFE_FAIL(EXPR) BSLS_ASSERTTEST_ASSERT_SAFE_FAIL(EXPR)
#define ASSERT_PASS(EXPR) BSLS_ASSERTTEST_ASSERT_PASS(EXPR)
#define ASSERT_FAIL(EXPR) BSLS_ASSERTTEST_ASSERT_FAIL(EXPR)
#define ASSERT_OPT_PASS(EXPR) BSLS_ASSERTTEST_ASSERT_OPT_PASS(EXPR)
#define ASSERT_OPT_FAIL(EXPR) BSLS_ASSERTTEST_ASSERT_OPT_FAIL(EXPR)

//=============================================================================
//                                VERBOSITY
//-----------------------------------------------------------------------------

static int verbose             = 0;
static int veryVerbose         = 0;
static int veryVeryVerbose     = 0;
static int veryVeryVeryVerbose = 0;  // For test allocators

//=============================================================================
//                  GLOBAL TYPEDEFS/CONSTANTS FOR TESTING
//-----------------------------------------------------------------------------

#if __cplusplus >= 201703L
#define MAYBE_UNUSED [[maybe_unused]]
#elif defined(BSLS_PLATFORM_CMP_GNU) || defined(BSLS_PLATFORM_CMP_CLANG)
#define MAYBE_UNUSED __attribute__((unused))
#else
#define MAYBE_UNUSED
#endif

typedef bslmf::MovableRefUtil MoveUtil;

int simpleFunction(int i)
    // A simple function that can be stored in nothrow wrapper.
{
    return i;
}

typedef void (*BSLALG_NOTHROWMOVABLEUTIL_FUNCTION_PTR_TYPE)();
typedef void(BSLALG_NOTHROWMOVABLEUTIL_FUNCTION_TYPE)();

typedef bslalg::NothrowMovableWrapper<int>       NTMB_INT;
typedef bslalg::NothrowMovableWrapper<const int> NTMB_CONST_INT;
typedef bslalg::NothrowMovableWrapper<
    BSLALG_NOTHROWMOVABLEUTIL_FUNCTION_PTR_TYPE>
    NTMB_FUNCTION_PTR_TYPE;

#define BSLALG_NOTHROWMOVABLEUTIL_TEST_TYPES                                  \
    int, const int, BSLALG_NOTHROWMOVABLEUTIL_FUNCTION_PTR_TYPE,              \
        BSLALG_NOTHROWMOVABLEUTIL_FUNCTION_TYPE, NTMB_INT, NTMB_CONST_INT,    \
        NTMB_FUNCTION_PTR_TYPE

class TrackableValue {
    // This class tracks a value through a series of move and copy operations
    // and has an easily-testable moved-from state.  An instance stores an
    // unsigned integer value and a pair of bits, one indicating if the value
    // was copied, another if it was moved (or neither or both).  When assigned
    // a value at construction, via assignment, or via a mutating operation
    // (such as +=), the move and copy bits are cleared, indicating that the
    // new value has been neither moved nor copied.  When assigned a new value
    // via copy construction or copy assignment, the copy bit is set and the
    // move bit is cleared. When assigned a new value via move construction,
    // move assignment, or swap, the copy bit is transferred from the original
    // value and the move bit is set. Thus a value that is copied then moved
    // will have both bits set but a value that is moved then copy has only the
    // copy bit set.  The copy and move bits are not salient attributes of the
    // value and are thus not used for testing equality.

    // PRIVATE CONSTANTS
    enum {
        e_NUM_FLAGS = 2,
        // Value is multiplied by the following value to make room for the
        // flag bits.  Multiply and divide are used to scale the value
        // because shift operations are not guaranteed to preserve sign.  The
        // compiler will almost certainly replace these multiplication and
        // divisions with more efficient arithmetic shift operations.
        e_VALUE_MULTIPLIER = 1 << e_NUM_FLAGS,
        e_FLAGS_MASK       = e_VALUE_MULTIPLIER - 1,
        e_VALUE_MASK       = ~e_FLAGS_MASK,
        e_COPIED_FLAG      = 0x01,
        e_MOVED_FLAG       = 0x02,
        e_MOVED_FROM_VAL = 0x11111111,  // Value assigned to moved-from object

        // The moved from state has a value of e_MOVED_FROM_VAL, with neither
        // flag set.
        e_MOVED_FROM_STATE = e_MOVED_FROM_VAL * e_VALUE_MULTIPLIER
    };

    // PRIVATE DATA
    mutable int d_valueAndFlags;

    TrackableValue& setValue(int  v,
                             bool movedFlag  = false,
                             bool copiedFlag = false);
        // Set the value to the specified 'v'. Set the 'isMoved()' flag to the
        // optionally-specified 'movedFlag' bit and the 'isCopied()' flag to
        // the optionally-specified 'copiedFlag' bit.

  public:
    // TRAITS
    BSLMF_NESTED_TRAIT_DECLARATION(TrackableValue, bslmf::IsBitwiseMoveable);

    // Constants
    enum {
        e_MIN = INT_MIN / e_VALUE_MULTIPLIER,
        e_MAX = INT_MAX / e_VALUE_MULTIPLIER
    };

    // CREATORS
    TrackableValue(int v = 0)
        // Set value to the specified 'v' and set 'isMoved()' and 'isCopied()'
        // to false.
    {
        setValue(v);
    }

    TrackableValue(const TrackableValue& other)
        // Copy value from 'other', set 'isMoved()' to false and 'isCopied()'
        // to true.
    {
        setValue(other.value(), false, true);
    }

    TrackableValue(bslmf::MovableRef<TrackableValue> other)
        // Move value from 'other', set 'isMoved()' to true, and set
        // 'isCopied()' to 'other.isCopied()', then set 'other' to the
        // moved-from state.
    {
        *this = bslmf::MovableRefUtil::move(other);
    }

    //! ~TrackableValue() = default;

    // MANIPULATORS
    TrackableValue& operator=(int v)
        // Set value to the specified 'v' and set 'isMoved()' and 'isCopied()'
        // to false; then return '*this'.
    {
        return setValue(v);
    }

    TrackableValue& operator=(const TrackableValue& rhs)
        // Set value to the specified 'rhs.value()' and set 'isMoved()' to
        // false and 'isCopied()' to true; then return '*this'.
    {
        return setValue(rhs.value(), false, true);
    }

    TrackableValue& operator=(bslmf::MovableRef<TrackableValue> rhs);
        // Move value from 'rhs', set 'isMoved()' to true, and set 'isCopied()'
        // to 'other.isCopied()', then assign 'rhs' the value
        // 'e_MOVED_FROM_VAL' and return '*this'.

    void swap(TrackableValue& other);
        // Exchange the values AND 'isCopied()' flags of '*this' and 'other',
        // then set the 'isMoved()' flag of both to true.

    void resetMoveCopiedFlags() const
        // Set 'isMoved()' and 'isCopied() to false.
    {
        d_valueAndFlags &= e_VALUE_MASK;
    }

    void setValueRaw(int v);
    void setIsMovedRaw(bool movedFlag);
    void setIsCopiedRaw(bool copiedFlag);
        // Set the constituent parts of this object without modifying the other
        // parts. It is up to the caller to ensure that the flags are set
        // consistently.

    // ACCESSORS
    int  value() const { return d_valueAndFlags / e_VALUE_MULTIPLIER; }
    bool isMoved() const { return d_valueAndFlags & e_MOVED_FLAG; }
    bool isCopied() const { return d_valueAndFlags & e_COPIED_FLAG; }
    bool isMovedFrom() const { return d_valueAndFlags == e_MOVED_FROM_STATE; }
};

// TrackableValue FREE FUNCTIONS
MAYBE_UNUSED
inline
bool operator==(const TrackableValue& a, const TrackableValue& b)
{
    return a.value() == b.value();
}

MAYBE_UNUSED
inline
bool operator!=(const TrackableValue& a, const TrackableValue& b)
{
    return a.value() != b.value();
}

MAYBE_UNUSED
inline
void swap(TrackableValue& a, TrackableValue& b)
{
    a.swap(b);
}

// IMPLEMENTATION OF TrackableValue
inline
TrackableValue& TrackableValue::setValue(int  v,
                                         bool movedFlag,
                                         bool copiedFlag)
{
    ASSERT(e_MIN <= v && v <= e_MAX);
    d_valueAndFlags = v * e_VALUE_MULTIPLIER;
    if (movedFlag)
        d_valueAndFlags |= e_MOVED_FLAG;
    if (copiedFlag)
        d_valueAndFlags |= e_COPIED_FLAG;
    return *this;
}

inline
TrackableValue& TrackableValue::operator=(
                                       bslmf::MovableRef<TrackableValue> other)
{
    TrackableValue& otherRef = bslmf::MovableRefUtil::access(other);
    d_valueAndFlags          = otherRef.d_valueAndFlags | e_MOVED_FLAG;
    otherRef.d_valueAndFlags = e_MOVED_FROM_STATE;
    return *this;
}

void TrackableValue::swap(TrackableValue& other)
{
    // Don't use std::swap<int> because don't want to #include <algorithm>
    int tmp               = d_valueAndFlags;
    d_valueAndFlags       = other.d_valueAndFlags;
    other.d_valueAndFlags = tmp;
    d_valueAndFlags |= e_MOVED_FLAG;
    other.d_valueAndFlags |= e_MOVED_FLAG;
}

void TrackableValue::setValueRaw(int v)
{
    d_valueAndFlags &= e_FLAGS_MASK;
    d_valueAndFlags |= v * e_VALUE_MULTIPLIER;
}

void TrackableValue::setIsMovedRaw(bool movedFlag)
{
    d_valueAndFlags &= (e_VALUE_MASK | e_COPIED_FLAG);
    if (movedFlag)
        d_valueAndFlags |= e_MOVED_FLAG;
}

void TrackableValue::setIsCopiedRaw(bool copiedFlag)
{
    d_valueAndFlags &= (e_VALUE_MASK | e_MOVED_FLAG);
    if (copiedFlag)
        d_valueAndFlags |= e_COPIED_FLAG;
}

// End implementation of TrackableValue

class TrackableValueWithAlloc {
    // Trackable value with allocator.  - Uses allocator - Not bitwise moveable
    // - potentially-throwing move constructor

    TrackableValue       d_trackable;
    bsl::allocator<char> d_alloc;

  public:
    BSLMF_NESTED_TRAIT_DECLARATION(TrackableValueWithAlloc,
                                   bslma::UsesBslmaAllocator);

    typedef bsl::allocator<char> allocator_type;

    explicit TrackableValueWithAlloc(int v = 0)
    : d_trackable(v)
    , d_alloc()
    {
    }

    explicit TrackableValueWithAlloc(const allocator_type& alloc)
    : d_trackable(0)
    , d_alloc(alloc)
    {
    }

    TrackableValueWithAlloc(int v, const allocator_type& alloc)
    : d_trackable(v)
    , d_alloc(alloc)
    {
    }

    TrackableValueWithAlloc(
                       const TrackableValueWithAlloc& original,
                       const allocator_type&          alloc = allocator_type())
    : d_trackable(original.d_trackable)
    , d_alloc(alloc)
    {
    }

    // Move constructor propagates allocator
    TrackableValueWithAlloc(
        bslmf::MovableRef<TrackableValueWithAlloc> original)
    : d_trackable(bslmf::MovableRefUtil::move(
          bslmf::MovableRefUtil::access(original).d_trackable))
    , d_alloc(bslmf::MovableRefUtil::access(original).d_alloc)
    {
    }

    TrackableValueWithAlloc(
        bslmf::MovableRef<TrackableValueWithAlloc> original,
        const allocator_type&                      alloc)
    : d_trackable(bslmf::MovableRefUtil::move(
          bslmf::MovableRefUtil::access(original).d_trackable))
    , d_alloc(alloc)
    {
    }

    ~TrackableValueWithAlloc() { memset((void *)this, 0xbb, sizeof(*this)); }

    TrackableValueWithAlloc& operator=(const TrackableValueWithAlloc& rhs)
    {
        d_trackable = rhs.d_trackable;
        return *this;
    }

    TrackableValueWithAlloc& operator=(
                                bslmf::MovableRef<TrackableValueWithAlloc> rhs)
    {
        TrackableValueWithAlloc& rhsRef = rhs;
        d_trackable = bslmf::MovableRefUtil::move(rhsRef.d_trackable);

        return *this;
    }

    void setValue(int v) { d_trackable = v; }
    void resetMoveCopiedFlags() const
        // Set 'isMoved()' and 'isCopied() to false.
    {
        d_trackable.resetMoveCopiedFlags();
    }

    // ACCESSORS
    int  value() const { return d_trackable.value(); }
    bool isMoved() const { return d_trackable.isMoved(); }
    bool isCopied() const { return d_trackable.isCopied(); }
    bool isMovedFrom() const { return d_trackable.isMovedFrom(); }

    allocator_type    get_allocator() const { return d_alloc; }
    bslma::Allocator *allocator() const { return d_alloc.mechanism(); }
};

MAYBE_UNUSED inline bool operator==(const TrackableValueWithAlloc& a,
                                    const TrackableValueWithAlloc& b)
{
    return a.value() == b.value();
}

MAYBE_UNUSED inline bool operator!=(const TrackableValueWithAlloc& a,
                                    const TrackableValueWithAlloc& b)
{
    return a.value() != b.value();
}

//=============================================================================
//                             USAGE EXAMPLES
//-----------------------------------------------------------------------------

namespace {

///Example 1
///- - - - -
// In this example, we define a class template, 'CountedType<TYPE>', that is
// little more than just a a wrapper around 'TYPE' that counts the count member
// along with the single value member:
//..
template <class TYPE>
class CountedType {
    // CLASS DATA
    static int s_count;

    // DATA
    TYPE d_value;
        //..
        // Because of externally-imposed requirements, the move constructor for
        // 'CountedType' must provide the strong guarantee; i.e., if the move
        // constructor of 'TYPE' throws an exception, then the moved-from
        // 'CountedType' object must be left unchanged.  To support this
        // requirement, we next define a private static function,
        // 'MoveIfNoexcept', similar to the standard 'std::move_if_noexcept',
        // that returns a movable reference if its argument is nothrow move
        // constructible and a const lvalue reference otherwise:
        //..
        // PRIVATE CLASS FUNCTIONS
    template <class TP>
    static typename bsl::conditional<
        bsl::is_nothrow_move_constructible<TP>::value,
        bslmf::MovableRef<TP>,
        const TP&>::type
    MoveIfNoexcept(TP& x);
        //..
        // We next finish out the class definition with a constructor, copy
        // constructor, move constructor, destructor, and member functions to
        // retrieve the count and value:
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
        // Move construct '*this' from 'original'.  If an exception is thrown,
        // by the constructor for 'TYPE' 'original' is unchanged.

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
inline
typename bsl::conditional<bsl::is_nothrow_move_constructible<TP>::value,
                          bslmf::MovableRef<TP>,
                          const TP&>::type
CountedType<TYPE>::MoveIfNoexcept(TP& x)
{
    return bslmf::MovableRefUtil::move(x);
}
//..
// Next, we implement the value constructor and move constructor, which simply
// copy their argument into the 'd_value' data members and increment the count:
//..
template <class TYPE>
CountedType<TYPE>::CountedType(const TYPE& val)
: d_value(val)
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
: d_value(MoveIfNoexcept(bslmf::MovableRefUtil::access(original).d_value))
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
// 'SomeType' holds an 'int' value which is set to -1 when it is moved from, as
// shown here:
//..
class SomeType {
    int d_value;

  public:
    SomeType(int v = 0)
    : d_value(v)
    {
    }  // IMPLICIT
    SomeType(const SomeType& original)
    : d_value(original.d_value)
    {
    }
    SomeType(bslmf::MovableRef<SomeType> original)
    : d_value(bslmf::MovableRefUtil::access(original).d_value)
    {
        bslmf::MovableRefUtil::access(original).d_value = -1;
    }

    int value() const { return d_value; }
};
//..
// Notice that 'SomeType' neglected to declare its move constructor as
// 'noexcept'.  This might be an oversight or it could be an old class that
// predates both 'noexcept' and the 'bsl::is_nothrow_move_constructible' trait.
// It is even be possible that the move constructor might throw (though, of
// course, it doesn't in this simplified example).  Regardless, the effect is
// that move-constructing a 'CountedType<SomeType>' will result in the move
// constructor actually performing a copy:
//..
void usageExample1()
{
    CountedType<SomeType> obj1(1);
    CountedType<SomeType> obj2(bslmf::MovableRefUtil::move(obj1));
    ASSERT(1 == obj1.value().value());  // Copied, not moved from
    ASSERT(1 == obj2.value().value());
    //..
    // For the purpose of this example, we can be sure that 'SomeThing' will
    // not throw on move, at least not in our applcation.  In order to obtain
    // the expected move optimiztion, we next wrap our 'SomeType in a
    // 'bslalg::NothrowMovableWrapper':
    //..
    CountedType<bslalg::NothrowMovableWrapper<SomeType> > obj3(SomeType(3));
    CountedType<bslalg::NothrowMovableWrapper<SomeType> > obj4(
                                            bslmf::MovableRefUtil::move(obj3));
    ASSERT(-1 == obj3.value().unwrap().value());  // moved from
    ASSERT(3 == obj4.value().unwrap().value());
}
//..
//
///Example 2
///- - - - -
// Note that, in the last two lines of Example 1, we must call 'unwrap' in
// order to access the 'SomeType' object inside of the 'NothrowMovableWrapper'.
// This is one situation where it would be attractive to have an overloadable
// "operator dot" so that both 'CoundedThing' and 'NothrowMovableWrapper' could
// be transparent proxy types.  C++ does not have overloadable operator dot,
// but we can create a 'CountedType' that is more intelligent about the
// existance of 'NothrowMovableWrapper' and automatically unwraps values for
// the user's convenience.
//
// Rather than starting from scratch, we'll build our new counted type,
// 'CountedType2' on 'CountedType'.  We start be defining a single data member
// of type 'CountedType':
//..
template <class TYPE>
class CountedType2 {
    CountedType<TYPE> d_data;
        //..
        // Next, for convenience, we add a public data type, 'ValueType' for
        // the value stored within 'CountedType2'.  However, rather than
        // defining 'ValueType' as simply 'TYPE', we unwrap it, in case 'TYPE'
        // is an instantiation of 'NothrowMovableWrapper':
        //..
  public:
    // TYPES
    typedef typename bslalg::NothrowMovableUtil::UnwrappedType<TYPE>::type
        ValueType;
        //..
        // Note that the 'UnwrappedType' metafunction has no affect of 'TYPE'
        // is not wrapped.
        //
        // Next, we declare (and define) the class functions, constructors, and
        // destructor, simply forwarding to the corresponding 'CountedType'
        // function, constructor, or destructor:
        //..
        // CLASS FUNCTIONS
    static int count() { return CountedType<TYPE>::count(); }

    // CREATORS
    CountedType2(const TYPE& val)
    : d_data(val)
    {
    }
    CountedType2(const CountedType2& original)
    : d_data(original.d_data)
    {
    }
    CountedType2(bslmf::MovableRef<CountedType2> original)
    : d_data(bslmf::MovableRefUtil::move(
          bslmf::MovableRefUtil::access(original).d_data))
    {
    }
    //..
    // Finally, we implement the 'value()' members such that the returned
    // values do not need to be unwrapped.  As in the case of the
    // 'UnwrappedType' metafunction, the 'unwrap()' function in
    // 'NothrowMovableUtil' handles both wrapped and unwrapped arguments,
    // unwrapping the latter and returning an unmodified reference to the
    // former:
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

    CountedType2<bslalg::NothrowMovableWrapper<SomeType> > obj3(SomeType(3));
    CountedType2<bslalg::NothrowMovableWrapper<SomeType> > obj4(
                                            bslmf::MovableRefUtil::move(obj3));
    ASSERT(-1 == obj3.value().value());  // moved from
    ASSERT(3 == obj4.value().value());   // No need to call 'unwrap'
}

}  // end unnamed namespace

template <class TYPE,
          bool USES_BSLMA_ALLOC =
              BloombergLP::bslma::UsesBslmaAllocator<TYPE>::value>
class Test_Util {
    // This class provided test utilities which have different behaviour
    // depending on whether 'TYPE is allocator-aware or not.  The main template
    // is for allocator-aware types.

  public:
    static bool checkAllocator(const TYPE&                 obj,
                               const bsl::allocator<char>& expected);
        // Check if, for the specified 'obj', 'obj.get_allocator()' returns the
        // specified 'expected' allocator.

    static bool hasSameAllocator(const TYPE& obj, const TYPE& other);
        // Check if, for the specified 'obj' and specified 'other',
        // 'obj.get_allocator() == other.get_allocator()';
};

template <class TYPE>
class Test_Util<TYPE, false> {
    // This class provided test utilities which have different behaviour
    // depending on whether 'TYPE is allocator-aware or not.  This
    // specialization is for non allocator-aware types.

  public:
    static bool checkAllocator(const TYPE&, const bsl::allocator<char>&);
        // return 'true'.

    static bool hasSameAllocator(const TYPE&, const TYPE&);
        // return 'true'.
};

template <class TYPE, bool USES_BSLMA_ALLOC>
inline
bool Test_Util<TYPE, USES_BSLMA_ALLOC>::checkAllocator(
                                          const TYPE&                 obj,
                                          const bsl::allocator<char>& expected)
{
    return (expected == obj.get_allocator());
}

template <class TYPE, bool USES_BSLMA_ALLOC>
inline
bool Test_Util<TYPE, USES_BSLMA_ALLOC>::hasSameAllocator(const TYPE& obj,
                                                         const TYPE& other)
{
    return (obj.allocator() == other.allocator());
}

template <class TYPE>
inline bool
Test_Util<TYPE, false>::checkAllocator(const TYPE&,
                                       const bsl::allocator<char>&)
{
    return true;
}

template <class TYPE>
inline bool
Test_Util<TYPE, false>::hasSameAllocator(const TYPE&, const TYPE&)
{
    return true;
}

template <class TYPE>
bool checkAllocator(const TYPE& obj, const bsl::allocator<char>& allocator)
{
    return Test_Util<TYPE>::checkAllocator(obj, allocator);
}

template <class TYPE>
bool hasSameAllocator(const TYPE& obj1, const TYPE& obj2)
{
    return Test_Util<TYPE>::hasSameAllocator(obj1, obj2);
}

// ============================================================================
//                          TEST DRIVER TEMPLATE
// ----------------------------------------------------------------------------

template <class TEST_TYPE>
class TestDriver {
    // This class template provides a namespace for testing the
    // 'bslalg::NothrowMovableWrapper' type.

  public:
    //PUBLIC TYPES
    enum { E_LVALUE_REF, E_CONST_LVALUE_REF, E_MOVABLE_REF };

  private:
    // PRIVATE TYPES
    typedef BloombergLP::bslalg::NothrowMovableUtil Obj;
        // Type under test.

    typedef typename Obj::UnwrappedType<TEST_TYPE>::type ValueType;
        // Unwrapped type

    typedef typename bsl::remove_cv<ValueType>::type NonConstValueType;
        // non const version of ValueType.  This is needed for
        // checkValueCategory overloads.

    typedef typename Obj::WrappedType<TEST_TYPE>::type WrapperType;
        // Wrapped type

  public:
    static int checkValueCategory(NonConstValueType&);
    static int checkValueCategory(const NonConstValueType&);
    static int checkValueCategory(
                            BloombergLP::bslmf::MovableRef<NonConstValueType>);
        // Helper functions to determine the value category of an expression

    static void testCase6();
        // TESTING 'unwrap' METHOD

    static void testCase5();
        // TESTING 'wrap' METHOD

    static void testCase4();
        // TESTING 'NothrowMovableUtil' TRAITS
};

template <class TYPE>
int TestDriver<TYPE>::checkValueCategory(NonConstValueType&)
{
    return E_LVALUE_REF;
}
template <class TYPE>
int TestDriver<TYPE>::checkValueCategory(const NonConstValueType&)
{
    return E_CONST_LVALUE_REF;
}
template <class TYPE>
int TestDriver<TYPE>::checkValueCategory(
                             BloombergLP::bslmf::MovableRef<NonConstValueType>)
{
    return E_MOVABLE_REF;
}

template <class TYPE>
void TestDriver<TYPE>::testCase6()
{
    // --------------------------------------------------------------------
    // TESTING 'unwrap' METHOD
    //
    // Concerns:
    //: 1 Invoking `unwrap` method with an argument of an unwrapped type
    //:   returns a reference to the argument.
    //: 2 Invoking `unwrap` method with an argument of a wrapped type returns
    //:   a reference to the wrapped object.
    //: 3 The value category of the returned reference matches the value
    //:   category of the argument.
    //
    // Plan:
    //: 1 Using a matrix of all value categories, invoke unwrap and
    //:   verify the returned reference has the correct value category using
    //:   `checkValueCategory` helper function.  [C-3]
    //: 2 in step 1, check the returned reference object matches the value
    //:   of the `unwrap` argument.  [C-1], [C-2]
    //
    // Testing:
    //   UnwrappedType<TYPE>::type unwrap(TYPE&);
    //   UnwrappedType<TYPE>::type unwrap(TYPE const&);
    //   WrappedType<RemoveReference<TYPE>::type>::type
    //          unwrap(BSLMF_NOTHROWMOVABLEWRAPPER_DEDUCE_RVREF(TYPE);
    // --------------------------------------------------------------------
    if (verbose)
        printf("\nTESTING `unwrap' METHOD"
               "\n=======================\n");
    typedef bslalg::ConstructorProxy<TYPE> TypeWithAllocator;
    bslma::TestAllocator                   da("default", veryVeryVeryVerbose);
    bslma::TestAllocator                   oa("other", veryVeryVeryVerbose);
    bslma::DefaultAllocatorGuard           dag(&da);
    if (veryVerbose)
        printf("\tCalling unwrape with a non const lvalue ref.\n");
    {
        TypeWithAllocator xBuffer(ValueType(3), &oa);
        TYPE&             x = xBuffer.object();
        ValueType         exp(3);
        ValueType&        unwrappedX = Obj::unwrap(x);

        ASSERT(checkValueCategory(Obj::unwrap(x)) ==
               (bsl::is_const<ValueType>::value ? E_CONST_LVALUE_REF
                                                : E_LVALUE_REF));
        ASSERT(unwrappedX == exp);
        ASSERT(checkAllocator(unwrappedX, &oa));
    }
    if (veryVerbose)
        printf("\tCalling unwrape with a const lvalue ref.\n");
    {
        TypeWithAllocator xBuffer(ValueType(3), &oa);
        const TYPE&       x = xBuffer.object();
        ValueType         exp(3);

        const ValueType& unwrappedX = Obj::unwrap(x);
        ASSERT(checkValueCategory(Obj::unwrap(x)) == E_CONST_LVALUE_REF);
        ASSERT(unwrappedX == exp);
        ASSERT(checkAllocator(unwrappedX, &oa));
    }
    if (veryVerbose)
        printf("\tCalling unwrape with a non const movable ref.\n");
    {
        TypeWithAllocator                         xBuffer(ValueType(3), &oa);
        TYPE&                                     x = xBuffer.object();
        ValueType                                 exp(3);
        BloombergLP::bslmf::MovableRef<ValueType> unwrappedX =
            Obj::unwrap(MoveUtil::move(x));

        ASSERT(checkValueCategory(Obj::unwrap(MoveUtil::move(x))) ==
               (bsl::is_const<ValueType>::value ? E_CONST_LVALUE_REF
                                                : E_MOVABLE_REF));
        ASSERT(unwrappedX == exp);
        ASSERT(checkAllocator(unwrappedX, &oa));
    }
}
template <class TYPE>
void TestDriver<TYPE>::testCase5()
{
    // --------------------------------------------------------------------
    // TESTING 'wrap' METHOD
    //
    // Concerns:
    //: 1 Invoking `wrap` method with an argument of an unwrapped type returns
    //:   a wrapper object containing a copy of the argument.
    //: 2 Invoking `wrap` method with an argument of a wrapped type returns a
    //:   copy of the same object.
    //: 3 It is possible to wrap a const object.
    //: 3 The argument is moved from whenever possible.
    //
    // Plan:
    //: 1 Create an object of `TYPE` from a `ValueType` object.  Using the
    //:   `wrap`  method, created a Wrapper object. Verify that the Wrapper
    //:   object contains a copy of the original ValueType object.  [C-1][C-2]
    //: 2 In step 1, use a const object of `TYPE`.  [C-3]
    //: 3 In step 1, invoke wrap by moving from the original object.  Verify
    //:   the object inside the returned wrapper has been created by move
    //:   construction.
    //
    // Testing:
    //   static typename WrappedType<TYPE>::type wrap(TYPE&       f);
    //   static typename WrappedType<TYPE>::type wrap(TYPE const& f);
    //   static typename WrappedType<
    //      typename bslmf::MovableRefUtil::RemoveReference<TYPE>::type>::type
    //      wrap(BSLMF_NOTHROWMOVABLEWRAPPER_DEDUCE_RVREF(TYPE)         f);
    // --------------------------------------------------------------------
    if (verbose)
        printf("\nTESTING `wrap' METHOD"
               "\n=====================\n");

    typedef bslalg::ConstructorProxy<TYPE> TypeWithAllocator;
    bslma::TestAllocator                   da("default", veryVeryVeryVerbose);
    bslma::TestAllocator                   oa("other", veryVeryVeryVerbose);
    bslma::DefaultAllocatorGuard           dag(&da);
    if (veryVerbose)
        printf("\tWrapping a non const object.\n");
    {
        TypeWithAllocator xBuffer(ValueType(3), &oa);
        TYPE&             x = xBuffer.object();
        ValueType         exp(3);

        const WrapperType& wrappedX = Obj::wrap(x);

        ASSERT(wrappedX.unwrap() == exp);
        ASSERT(wrappedX.unwrap().isCopied());
        ASSERT(!wrappedX.unwrap().isMoved());
        ASSERT(checkAllocator(wrappedX.unwrap(), &da));
    }
    if (veryVerbose)
        printf("\tWrapping a const object.\n");
    {
        TypeWithAllocator xBuffer(ValueType(3), &oa);
        const TYPE&       x = xBuffer.object();
        ValueType         exp(3);

        const WrapperType& wrappedX = Obj::wrap(x);

        ASSERT(wrappedX.unwrap() == exp);
        ASSERT(wrappedX.unwrap().isCopied());
        ASSERT(!wrappedX.unwrap().isMoved());
        ASSERT(checkAllocator(wrappedX.unwrap(), &da));
    }
    if (veryVerbose)
        printf("\tWrapping a non const rvalue .\n");
    {
        TypeWithAllocator xBuffer(ValueType(3), &oa);
        TYPE&             x = xBuffer.object();
        ValueType         exp(3);
        ValueType&        val = x;
        val.resetMoveCopiedFlags();
        const WrapperType& wrappedX = Obj::wrap(MoveUtil::move(x));

        // If TYPE is const, wrap will perform a copy. Otherwise, it will do a
        // move
        ASSERT(wrappedX.unwrap() == exp);
        ASSERT(wrappedX.unwrap().isCopied() ==
               bsl::is_const<ValueType>::value);
        ASSERT(wrappedX.unwrap().isMoved() != bsl::is_const<ValueType>::value);
        ASSERT(checkAllocator(wrappedX.unwrap(),
                              (bsl::is_const<ValueType>::value ? &da : &oa)));
    }
    /* 
    // the following can not be made to work in C++03, and the questino is 
    // if we should have it working in C++11 ? If yes, we need a const T&& 
    // overload of wrap. The current overload set does not compile, as opposed
    // to does the wrong thing.
    {
        const TYPE x(ValueType(3)); 
        ValueType exp(3);
        const ValueType &val = x;
        val.resetMoveCopiedFlags(); 
        const WrapperType &wrappedX = Obj::wrap(MoveUtil::move(x));
        
        ASSERT(wrappedX.unwrap() == exp);
        ASSERT(wrappedX.unwrap().isCopied());
        ASSERT(!wrappedX.unwrap().isMoved());
    }
    */
}
template <class TYPE>
void TestDriver<TYPE>::testCase4()
{
    // --------------------------------------------------------------------
    // TESTING 'NothrowMovableUtil' TRAITS
    //
    // Concerns:
    //: 1 `IsWrapped`, `UnwrappedType`, and `WrappedType` trait correspond to
    //:    the same named member of `NothrowMovableUtil_Traits`
    //
    // Plan:
    //: 1 For given type `TYPE`, check that `IsWrapped` returns the same value
    //:   as `NothrowMovableUtil_Traits<TYPE>::IsWrapped`.  [C-1]
    //: 2 For given type `TYPE`, check that `UnwrappedType` and `WrappedType`
    //:   returns the same type as the same named member of
    //:   `NothrowMovableUtil_Traits<TYPE>`.  [C-1]
    //
    // Testing:
    //  NothrowMovableUtil::IsWrapped
    //  NothrowMovableUtil::UnwrappedType
    //  NothrowMovableUtil::WrappedType
    // --------------------------------------------------------------------
    if (verbose)
        printf("\nTESTING 'NothrowMovableUtil' TRAITS"
               "\n==================================\n");

    {
        ASSERT((bslalg::NothrowMovableUtil_Traits<TYPE>::IsWrapped::value ==
                bslalg::NothrowMovableUtil::IsWrapped<TYPE>::value));

        ASSERT(
            (bsl::is_same<typename bslalg::NothrowMovableUtil_Traits<
                              TYPE>::UnwrappedType,
                          typename bslalg::NothrowMovableUtil::UnwrappedType<
                              TYPE>::type>::value));

        ASSERT((bsl::is_same<
                typename bslalg::NothrowMovableUtil_Traits<TYPE>::WrappedType,
                typename bslalg::NothrowMovableUtil::WrappedType<TYPE>::type>::
                    value));
    }
}

//=============================================================================
//                              MAIN PROGRAM
//-----------------------------------------------------------------------------

int main(int argc, char *argv[])
{
    int test            = argc > 1 ? atoi(argv[1]) : 0;
    verbose             = argc > 2;
    veryVerbose         = argc > 3;
    veryVeryVerbose     = argc > 4;
    veryVeryVeryVerbose = argc > 5;

    printf("TEST " __FILE__ " CASE %d\n", test);

    switch (test) {
      case 0:  // Zero is always the leading case.
      case 6: {
        RUN_EACH_TYPE(
            TestDriver,
            testCase6,
            TrackableValue,
            TrackableValueWithAlloc,
            BloombergLP::bslalg::NothrowMovableWrapper<TrackableValue>,
            BloombergLP::bslalg::NothrowMovableWrapper<
                TrackableValueWithAlloc>,
            BloombergLP::bslalg::NothrowMovableWrapper<const TrackableValue>,
            BloombergLP::bslalg::NothrowMovableWrapper<
                const TrackableValueWithAlloc>);
      } break;
      case 5: {
        RUN_EACH_TYPE(TestDriver,
                      testCase5,
                      TrackableValue, TrackableValueWithAlloc,
                      BloombergLP::bslalg::NothrowMovableWrapper<TrackableValue>, 
                      BloombergLP::bslalg::NothrowMovableWrapper<TrackableValueWithAlloc>/*,
                      BloombergLP::bslalg::NothrowMovableWrapper<const TrackableValue>, 
                      BloombergLP::bslalg::NothrowMovableWrapper<const TrackableValueWithAlloc>*/);
      } break;
      case 4: {
        RUN_EACH_TYPE(TestDriver,
                      testCase4,
                      BSLALG_NOTHROWMOVABLEUTIL_TEST_TYPES);
      } break;
      case 3: {
        // --------------------------------------------------------------------
        // TESTING 'NothrowMovableUtil_Traits' TRAITS
        //
        //  This test checks the correct behaviour of
        //  'NothrowMovableUtil_Traits'
        //  traits.
        //
        // Concerns:
        //: 1 `IsWrapped` trait is satisfied if and only if 'TYPE' is a
        //:  specialization of 'NothrowMovableWrapper`.
        //: 2 `WrappedType::type` is `NothrowMovableWrapper<TYPE>' if `TYPE` is
        //:   not a specialization of `NothrowMovableWrapper`, and `TYPE'
        //:   otherwise.
        //: 3 `UnWrappedType::type` is `TYPE` if `TYPE` is not a specialization
        // of
        //:   `NothrowMovableWrapper` and `TYPE is not a funciton, `TYPE*` if
        // TYPE
        //:    is a function, and `TYPE::ValueType` otherwise.
        //
        // Plan:
        //: 1 Using a `TYPE` which is not a specialization of
        //:   'NothrowMovableWrapper` check that `IsWrapped` trait is not
        //:   satisfied.  [C-1]
        //: 2 In step 1, check that `WrappedType::type` is
        //:   `NothrowMovableWrapper<TYPE>'.  [C-2]
        //: 3 In step 1, check that `UnwrappedType::type` is
        //:   `TYPE`.  [C-3]
        //: 4 In step 3, using a function type `TYPE`, verify that
        // `UnwrappedType::type` is
        //:   `TYPE*`
        //: 5 Using a `NothrowMovableWrapper<TYPE> as test type, check that
        //:   `IsWrapped` trait is satisfied.  [C-1]
        //: 6 In step 5, check that `WrappedType::type` is `TYPE'.  [C-2]
        //: 7 In step 6, check that `UnwrappedType::type` is
        //:   `TYPE::ValueType`.  [C-3]
        //
        // Testing:
        //  NothrowMovableUtil_Traits::IsWrapped
        //  NothrowMovableUtil_Traits::UnwrappedType
        //  NothrowMovableUtil_Traits::WrappedType
        // --------------------------------------------------------------------
        if (verbose)
            printf("\nTESTING 'NothrowMovableUtil_Traits' TRAITS"
                   "\n=========================================\n");
        if (veryVerbose)
            printf("\tNon-wrapped types.\n");
        {
            ASSERT(
                !(bslalg::NothrowMovableUtil_Traits<int>::IsWrapped::value));
            ASSERT((bsl::is_same<
                    bslalg::NothrowMovableUtil_Traits<int>::UnwrappedType,
                    int>::value));
            ASSERT((bsl::is_same<
                    bslalg::NothrowMovableUtil_Traits<int>::WrappedType,
                    bslalg::NothrowMovableWrapper<int> >::value));

            ASSERT(!(bslalg::NothrowMovableUtil_Traits<
                     const int>::IsWrapped::value));
            ASSERT((bsl::is_same<bslalg::NothrowMovableUtil_Traits<
                                     const int>::UnwrappedType,
                                 const int>::value));
            ASSERT((bsl::is_same<
                    bslalg::NothrowMovableUtil_Traits<const int>::WrappedType,
                    bslalg::NothrowMovableWrapper<const int> >::value));

            ASSERT(!(bslalg::NothrowMovableUtil_Traits<
                     BSLALG_NOTHROWMOVABLEUTIL_FUNCTION_PTR_TYPE>::IsWrapped::
                         value));
            ASSERT((bsl::is_same<
                    bslalg::NothrowMovableUtil_Traits<
                        BSLALG_NOTHROWMOVABLEUTIL_FUNCTION_PTR_TYPE>::
                        UnwrappedType,
                    BSLALG_NOTHROWMOVABLEUTIL_FUNCTION_PTR_TYPE>::value));
            ASSERT((bsl::is_same<
                    bslalg::NothrowMovableUtil_Traits<
                        BSLALG_NOTHROWMOVABLEUTIL_FUNCTION_PTR_TYPE>::
                        WrappedType,
                    bslalg::NothrowMovableWrapper<
                        BSLALG_NOTHROWMOVABLEUTIL_FUNCTION_PTR_TYPE> >::
                        value));

            ASSERT(!(bslalg::NothrowMovableUtil_Traits<
                     BSLALG_NOTHROWMOVABLEUTIL_FUNCTION_TYPE>::IsWrapped::
                         value));
            ASSERT((bsl::is_same<
                    bslalg::NothrowMovableUtil_Traits<
                        BSLALG_NOTHROWMOVABLEUTIL_FUNCTION_TYPE>::
                        UnwrappedType,
                    BSLALG_NOTHROWMOVABLEUTIL_FUNCTION_TYPE *>::value));
            ASSERT((bsl::is_same<
                    bslalg::NothrowMovableUtil_Traits<
                        BSLALG_NOTHROWMOVABLEUTIL_FUNCTION_TYPE>::WrappedType,
                    bslalg::NothrowMovableWrapper<
                        BSLALG_NOTHROWMOVABLEUTIL_FUNCTION_TYPE *> >::value));
        }
        if (veryVerbose)
            printf("\tWrapped types.\n");
        {
            ASSERT(
              (bslalg::NothrowMovableUtil_Traits<NTMB_INT>::IsWrapped::value));
            ASSERT((bsl::is_same<
                    bslalg::NothrowMovableUtil_Traits<NTMB_INT>::UnwrappedType,
                    int>::value));
            ASSERT((bsl::is_same<
                    bslalg::NothrowMovableUtil_Traits<NTMB_INT>::WrappedType,
                    NTMB_INT>::value));

            ASSERT((bslalg::NothrowMovableUtil_Traits<
                    NTMB_CONST_INT>::IsWrapped::value));
            ASSERT((bsl::is_same<bslalg::NothrowMovableUtil_Traits<
                                     NTMB_CONST_INT>::UnwrappedType,
                                 const int>::value));
            ASSERT((bsl::is_same<bslalg::NothrowMovableUtil_Traits<
                                     NTMB_CONST_INT>::WrappedType,
                                 NTMB_CONST_INT>::value));

            ASSERT((bslalg::NothrowMovableUtil_Traits<
                    NTMB_FUNCTION_PTR_TYPE>::IsWrapped::value));
            ASSERT((bsl::is_same<
                    bslalg::NothrowMovableUtil_Traits<
                        NTMB_FUNCTION_PTR_TYPE>::UnwrappedType,
                    BSLALG_NOTHROWMOVABLEUTIL_FUNCTION_TYPE *>::value));
            ASSERT((bsl::is_same<bslalg::NothrowMovableUtil_Traits<
                                     NTMB_FUNCTION_PTR_TYPE>::WrappedType,
                                 NTMB_FUNCTION_PTR_TYPE>::value));
        }
      } break;
      case 2: {
        // --------------------------------------------------------------------
        // USAGE EXAMPLE
        // --------------------------------------------------------------------

        if (verbose)
            printf("\nUSAGE EXAMPLE"
                   "\n============\n");

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

        if (verbose)
            printf("\nBREATHING TEST"
                   "\n==============\n");

        bslalg::NothrowMovableUtil::WrappedType<int(int)>::type funcWrap2 =
            bslalg::NothrowMovableUtil::wrap(simpleFunction);
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
