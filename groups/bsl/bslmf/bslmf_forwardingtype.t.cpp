// bslmf_forwardingtype.t.cpp                                         -*-C++-*-
#include <bslmf_forwardingtype.h>

#include <bslmf_isarray.h>
#include <bslmf_issame.h>          // for testing only

#include <bsls_platform.h>

#include <stdlib.h>    // atoi()
#include <iostream>
#include <typeinfo>

using namespace BloombergLP;
using namespace std;

//=============================================================================
//                                TEST PLAN
//-----------------------------------------------------------------------------
//                                Overview
//                                --------
// This component provides a meta-function.  We simply verify that it returns
// the proper type for a list of suitably chosen arguments.  It also provides
// a utility function for which we verify that it returns the correct type and
// value
//-----------------------------------------------------------------------------
// [ 1] bslmf::ForwardingType<TYPE>::Type
// [ 2] bslmf::ForwardingTypeUtil<TYPE>::TargetType
// [ 2] bslmf::ForwardingTypeUtil<TYPE>::forwardToTarget(v)
// [ 3] USAGE EXAMPLES
//=============================================================================
//                  STANDARD BDE ASSERT TEST MACRO
//-----------------------------------------------------------------------------
static int testStatus = 0;

static void aSsErT(int c, const char *s, int i) {
    if (c) {
        cout << "Error " << __FILE__ << "(" << i << "): " << s
             << "    (failed)" << endl;
        if (testStatus >= 0 && testStatus <= 100) ++testStatus;
    }
}
#define ASSERT(X) { aSsErT(!(X), #X, __LINE__); }

//-----------------------------------------------------------------------------
#define LOOP_ASSERT(I,X) { \
    if (!(X)) { cout << #I << ": " << I << "\n"; aSsErT(1, #X, __LINE__);}}

#define LOOP2_ASSERT(I,J,X) { \
    if (!(X)) { cout << #I << ": " << I << "\t" << #J << ": " \
        << J << "\n"; aSsErT(1, #X, __LINE__); } }
//=============================================================================
#define P(X) cout << #X " = " << (X) << endl; // Print identifier and value.
#define Q(X) cout << "<| " #X " |>" << endl;  // Quote identifier literally.
#define P_(X) cout << #X " = " << (X) << ", " << flush; // P(X) without '\n'
#define L_ __LINE__                           // current Line number
#define T_() cout << '\t' << flush;           // Print tab w/o linefeed.

#define ASSERT_SAME(X, Y) ASSERT((bsl::is_same<X, Y>::value))

//=============================================================================
//                  GLOBAL TYPES/OBJECTS FOR TESTING
//-----------------------------------------------------------------------------

enum   Enum   { E_VAL1, E_VAL2 };

struct Struct {
    int d_data;
    Struct(int v) : d_data(v) { }
};

inline bool operator==(Struct a, Struct b) {
    return a.d_data == b.d_data;
}

union  Union  {
    int d_data;
    Union(int v) : d_data(v) { }
};

inline bool operator==(Union a, Union b) {
    return a.d_data == b.d_data;
}

class  Class  {
    int d_data;
public:
    Class(int v) : d_data(v) { }
    int value() const { return d_data; }
};

inline bool operator==(Class a, Class b) {
    return a.value() == b.value();
}

typedef void      F ();
typedef void ( & RF)();
typedef void (*  PF)();
typedef void (*&RPF)();

typedef void    Fi  (int);
typedef void (&RFi) (int);
typedef void (*PFi) (int);
typedef void    FRi (int&);
typedef void (&RFRi)(int&);

typedef char    A [5];
typedef char (&RA)[5];
typedef char   AU [];

typedef int Struct::*Pm;
typedef int (Class::*Pmf)() const;

void func() { }
void funcI(int) { }
void funcRi(int&) { }

//=============================================================================
//                           USAGE EXAMPLES
//-----------------------------------------------------------------------------

///Usage
///-----
// These examples demonstrate the expected use of this component.
// In this section we show intended use of this component.
//
///Example 1: Direct look at metafunction results
///- - - - - - - - - - - - - - - - - - - - - - -
// In this example, we invoke 'ForwardingType' on a variety of types and look
// at the resulting 'Type' member:
//..
    struct MyType {};
    typedef MyType& MyTypeRef;

    void usageExample1()
    {
        typedef int                    T1;
        typedef int&                   T2;
        typedef const volatile double& T3;
        typedef const double &         T4;
        typedef const float * &        T5;
        typedef const float * const &  T6;
        typedef MyType                 T7;
        typedef const MyType&          T8;
        typedef MyType&                T9;
        typedef MyType*                T10;

        typedef int                    EXP1;
        typedef int&                   EXP2;
        typedef const volatile double& EXP3;
        typedef const double &         EXP4;
        typedef const float * &        EXP5;
        typedef const float * const &  EXP6;
        typedef const MyType&          EXP7;
        typedef const MyType&          EXP8;
        typedef MyType&                EXP9;
        typedef MyType*                EXP10;

        ASSERT((bsl::is_same<bslmf::ForwardingType<T1>::Type, EXP1>::value));
        ASSERT((bsl::is_same<bslmf::ForwardingType<T2>::Type, EXP2>::value));
        ASSERT((bsl::is_same<bslmf::ForwardingType<T3>::Type, EXP3>::value));
        ASSERT((bsl::is_same<bslmf::ForwardingType<T4>::Type, EXP4>::value));
        ASSERT((bsl::is_same<bslmf::ForwardingType<T5>::Type, EXP5>::value));
        ASSERT((bsl::is_same<bslmf::ForwardingType<T6>::Type, EXP6>::value));
        ASSERT((bsl::is_same<bslmf::ForwardingType<T7>::Type, EXP7>::value));
        ASSERT((bsl::is_same<bslmf::ForwardingType<T8>::Type, EXP8>::value));
        ASSERT((bsl::is_same<bslmf::ForwardingType<T9>::Type, EXP9>::value));
        ASSERT((bsl::is_same<bslmf::ForwardingType<T10>::Type, EXP10>::value));
    }
//..
//
///Example 2: A logging invocation wrapper
///- - - - - - - - - - - - - - - - - - - - - - -
// This example illustrates the use of 'ForwardingType' to efficiently
// implement a wrapper class that holds a function pointer and logs
// information about each call to the pointed-to-function through the wrapper.
// The pointed-to-function takes three arguments whose types a4re specified
// via template arguments.  The first argument is required to be convertible
// to 'int'.  The class definition looks as follows:
//..
    // Primary template is never defined
    template <class PROTOTYPE> class LoggingWrapper;

    template <class RET, class ARG1, class ARG2, class ARG3>
    class LoggingWrapper<RET(ARG1, ARG2, ARG3)> {
        // Specialization of wrapper for specified function prototype.

        RET (*d_function_p)(ARG1, ARG2, ARG3);

    public:
        LoggingWrapper(RET (*function_p)(ARG1, ARG2, ARG3))
          : d_function_p(function_p) { }

        RET operator()(ARG1, ARG2, ARG3) const;
//..
// Next, we declare a private member function that actually invokes the
// function. This member function will be called by 'operator()' and must
// therefore receive arguments indirectly through 'operator()'. In order to
// avoid excessive copies of pass-by-value arguments, we use 'ForwardingType'
// to declare a more efficient intermediate argument type for our private
// member function:
//..
    private:
        RET invoke(typename bslmf::ForwardingType<ARG1>::Type a1,
                   typename bslmf::ForwardingType<ARG2>::Type a2,
                   typename bslmf::ForwardingType<ARG3>::Type a3) const;
    };
//..
// Next, we define logging functions that simply count the number of
// invocations and return from invocations (e.g., to count how may invocations
// completed without exceptions):
//..
    int invocations = 0, returns = 0;
    void logInvocation(int /* ignored */) { ++invocations; }
    void logReturn(int /* ignored */) { ++returns; }
//..
// Next, we implement 'operator()' to call the logging functions and call
// 'invoke()':
//..
    template <class RET, class ARG1, class ARG2, class ARG3>
    RET LoggingWrapper<RET(ARG1, ARG2, ARG3)>::operator()(ARG1 a1,
                                                          ARG2 a2,
                                                          ARG3 a3) const {
        logInvocation(a1);
        RET r = invoke(a1, a2, a3);
        logReturn(a1);
        return r;
    }
//..
// Next, we implement 'invoke()' to actually call the function through the
// pointer. To reconstitute the arguments to the function as close as possible
// to the types they were passed in as, we call the 'forwardToTarget' member
// of 'ForwardingTypeUtil':
//..
    template <class RET, class ARG1, class ARG2, class ARG3>
    RET LoggingWrapper<RET(ARG1,ARG2,ARG3)>::invoke(
        typename bslmf::ForwardingType<ARG1>::Type a1,
        typename bslmf::ForwardingType<ARG2>::Type a2,
        typename bslmf::ForwardingType<ARG3>::Type a3) const
    {
        return d_function_p(
            bslmf::ForwardingTypeUtil<ARG1>::forwardToTarget(a1),
            bslmf::ForwardingTypeUtil<ARG2>::forwardToTarget(a2),
            bslmf::ForwardingTypeUtil<ARG3>::forwardToTarget(a3));
    }
//..
// Next, in order to see this wrapper in action, we must define the function
// we wish to wrap.  This function will take an argument of type 'ArgType',
// which, among other things, keeps track of whether it has been directly
// constructed or copied from anther 'ArgType' object.  If it has been copied,
// it keeps track of how many "generations" of copy were done:
//..
    class ArgType {
        int d_value;
        int d_copies;
    public:
        ArgType(int v = 0) : d_value(v), d_copies(0) { }
        ArgType(const ArgType& other)
          : d_value(other.d_value)
          , d_copies(other.d_copies + 1) { }

        int value() const { return d_value; }
        int copies() const { return d_copies; }
    };

    int myFunc(const short& i, ArgType& x, ArgType y)
        // Assign 'x' the value of 'y' and return the 'value()' of 'x'.
    {
        ASSERT(i == y.copies());
        x = y;
        return x.value();
    }
//..
// Finally, we create a instance of 'LoggingWrapper' to wrap 'myFunc', and we
// invoke it.  Note that 'y' is copied into the second argument of
// 'operator()' and is copied again when 'myFunc' is invoked.  However, it is
// *not* copied when 'operator()' calls 'invoke()' because the 'ForwardType'
// of 'ArgType' is 'const ArgType&', which does not create another copy.  In
// C++11, if 'ArgType' had a move constructor, then the number of copies would
// be only 1, since the final forwarding would be a move instead of a copy.
//..
    void usageExample2() {
        ArgType x(0);
        ArgType y(99);

        LoggingWrapper<int(const short&, ArgType&, ArgType)> lw(myFunc);
        ASSERT(0 == invocations && 0 == returns);
        lw(2, x, y);  // Expect two copies of 'y'
        ASSERT(1 == invocations && 1 == returns);
        ASSERT(99 == x.value());
    }
//..    

//=============================================================================
//                              MAIN PROGRAM
//-----------------------------------------------------------------------------

template <class TYPE>
bool sameAddress(const TYPE& a, const TYPE& b)
    // Return true if address of the specified 'a' object matches the address
    // of the specified 'b' object.  Unlike a direct test, this function can
    // be called even if 'a' or 'b' is an rvalue or rvalue refearence.
{
    return &a == &b;
}

template <class TYPE>
void testForwardToTargetVal(TYPE obj)
{
    typedef typename bslmf::ForwardingType<TYPE>::Type FwdType;
    typedef typename bslmf::ForwardingTypeUtil<TYPE>::TargetType TargetType;

#if defined(BSLS_COMPILERFEATURES_SUPPORT_RVALUE_REFERENCES)
    ASSERT_SAME(TYPE, typename bsl::remove_reference<TargetType>::type);
#else
    ASSERT((bsl::is_same<TYPE,
                    typename bsl::remove_reference<TargetType>::type>::value ||
            bsl::is_same<const TYPE,
                    typename bsl::remove_reference<TargetType>::type>::value));
#endif

    FwdType fwdObj = obj;

    // For pass-by-value, compare original and final value.
    ASSERT(obj == bslmf::ForwardingTypeUtil<TYPE>::forwardToTarget(fwdObj));
}

template <class TYPE>
void testForwardToTargetArray(TYPE obj)
{
    typedef typename bslmf::ForwardingType<TYPE>::Type FwdType;
    typedef typename bslmf::ForwardingTypeUtil<TYPE>::TargetType TargetType;

    ASSERT_SAME(TYPE&, TargetType);

    FwdType fwdObj = obj;

    // For arrays,j compare address of first element of original and final
    // arrays.
    ASSERT(&obj[0] ==
           &bslmf::ForwardingTypeUtil<TYPE>::forwardToTarget(fwdObj)[0]);
}

template <class TYPE>
void testForwardToTargetRef(TYPE ref)
{
    typedef typename bslmf::ForwardingType<TYPE>::Type FwdType;
    typedef typename bslmf::ForwardingTypeUtil<TYPE>::TargetType TargetType;

    ASSERT_SAME(TYPE, TargetType);

    FwdType fwdRef = ref;

    // For pass-by-reference, compare addresses of original and final
    // references.
    ASSERT(sameAddress(ref,
                    bslmf::ForwardingTypeUtil<TYPE>::forwardToTarget(fwdRef)));
}

int main(int argc, char *argv[])
{
    int test = argc > 1 ? atoi(argv[1]) : 0;
    int verbose = argc > 2;
    int veryVerbose = argc > 3;

    (void) verbose;      // eliminate unused variable warning
    (void) veryVerbose;  // eliminate unused variable warning

    cout << "TEST " << __FILE__ << " CASE " << test << endl;

    switch (test) { case 0:  // Zero is always the leading case.
      case 3: {
        // --------------------------------------------------------------------
        // USAGE EXAMPLES
        //
        // Concerns: The usage examples in the component doc compile and run
        //   as promised.
        //
        // Plan: Copy the usage example from the component header, replace
        //   'assert' with 'ASSERT' and 'main' with 'usageExampleN' and verify
        //   that it compiles and runs succesfully.
        //
        // Testing:
        //   USAGE EXAMPLES
        // --------------------------------------------------------------------

        if (verbose) cout << "\nUSAGE EXAMPLES"
                          << "\n==============" << endl;

        usageExample1();
        usageExample2();

      } break;

      case 2: {
        // --------------------------------------------------------------------
        // TESTING bslmf::ForwardingTypeUtil
        //
        // Concerns:
        //: 1 For types that are neither references not arrays,
        //:   'ForwardingTypeUtil<TYPE>::TargetType' is similar to
        //:   'TYPE' except that 'TargetType' might be a const reference
        //:   (C++03) or rvalue reference (C++11+). An object of 'TYPE'
        //:   converted to 'ForwardingType<TYPE>::Type', then forwarded using
        //:   'ForwardingTypeUtil<TYPE>::forwardToTarget() will yield a value
        //:   equal to the original object.
        //: 2 For array types of (known or unknown) size,
        //:   'ForwardingTypeUtil<TYPE>::TargetType' yields a reference to
        //:   'TYPE'. An array object of 'TYPE' converted to
        //:   'ForwardingType<TYPE>::Type' then forwarded using 
        //:   'ForwardingTypeUtil<TYPE>::forwardToTarget() will yield a
        //:   reference to the original array.
        //: 3 For reference types, 'ForwardingTypeUtil<TYPE>::TargetType'
        //:   yields 'TYPE'.  A reference of 'TYPE' converted to
        //:   'ForwardingType<TYPE>::Type' then forwarded using 
        //:   'ForwardingTypeUtil<TYPE>::forwardToTarget() will yield a
        //:   a reference identical to the original.
        //: 4 All of the above concerns apply when 'TYPE' is
        //:   cv-qualified. Note that passing volatile-qualified objects by
        //:   value or by rvalue-reference does not really happen in real code
        //:   and need not be tested.
        //
        // Plan:
        //: 1 For concern 1, implement a function template,
        //:   'testForwardToTargetVal' that can be instantiated with a 'TYPE'
        //:   and which takes an argument 'obj' of 'TYPE'.  Instantiated
        //:   on a variety of basic and non-basic types,
        //:   'testForwardToTargetVal' performs the following operations: 
        //:   a Verify that 'TargetType' is the expected transformation of
        //:     'TYPE'
        //:   b Initialize a temporary variable of type
        //:     'ForwardingType<TYPE>::Type' using 'obj.
        //:   c Call 'forwardToTarget' on the temporary variable and verify
        //:     that the resulting object compares equal to 'obj'.
        //: 2 For concern 2, implement a function template,
        //    'testForwardToTargetArray' that can be instantiated with an
        //:   an array 'TYPE' (or reference-to-array 'TYPE') and which takes
        //:   an argument 'obj' of 'TYPE'.  Instantiated on a variety of array
        //:   types of known and unknown size as well a lvalue and rvalues to
        //:   such types, 'testForwardToTargetArray' performs the following
        //:   operations:
        //:   a Verify that 'TargetType' is the expected transformation of
        //:     'TYPE'
        //:   b Initialize a temporary variable of type
        //:     'ForwardingType<TYPE>::Type' using 'obj.
        //:   c Call 'forwardToTarget' on the temporary variable and verify
        //:     that the resulting object has the same address as 'obj'.
        //: 3 For concern 3, implement a function template,
        //:   'testForwardToTargetRef' that can be instantiated with a
        //:   reference 'TYPE' and which takes an argument 'ref' of 'TYPE'.
        //:   Instantiated on a variety of lvalue and rvalue reference types,
        //:   'testForwardToTargetRef' performs the following operations:
        //:   a Verify that 'TargetType' is the expected transformation of
        //:     'TYPE'
        //:   b Initialize a temporary variable of type
        //:     'ForwardingType<TYPE>::Type' using 'obj.
        //:   c Call 'forwardToTarget' on the temporary variable and verify
        //:     that the resulting object has the same address as 'obj'.
        //: 4 For concern 4, instantiate the templates defined in the previous
        //:   steps using cv-qualified template parameters.
        // 
        // Testing:
        //      bslmf::ForwardingTypeUtil<TYPE>::TargetType
        //      bslmf::ForwardingTypeUtil<TYPE>::forwardToTarget(v)
        // --------------------------------------------------------------------
          
        if (verbose) cout << "\nbslmf::ForwardingTypeUtil"
                          << "\n=========================" << endl;

        Enum    e = E_VAL2;
        Struct  s(99);
        Union   u(98);
        Class   c(97);
        double  d = 1.23;
        double *p = &d;
        char    a[5] = { '5', '4', '3', '2', '1' };
        char  (&au)[] = reinterpret_cast<AU&>(a);
        F      *f_p = func;
        Pm      m_p  = &Struct::d_data;
        Pmf     mf_p = &Class::value;

        testForwardToTargetVal<Enum    >(e);
        testForwardToTargetVal<Struct  >(s);
        testForwardToTargetVal<Union   >(u);
        testForwardToTargetVal<Class   >(c);
        testForwardToTargetVal<double  >(d);
        testForwardToTargetVal<double *>(p);
        testForwardToTargetVal<PF      >(f_p);
        testForwardToTargetVal<Pm      >(m_p);
        testForwardToTargetVal<Pmf     >(mf_p);
        
        testForwardToTargetVal<Enum    const>(e);
        testForwardToTargetVal<Struct  const>(s);
        testForwardToTargetVal<Union   const>(u);
        testForwardToTargetVal<Class   const>(c);
        testForwardToTargetVal<double  const>(d);
        testForwardToTargetVal<double *const>(p);
        testForwardToTargetVal<PF      const>(f_p);
        testForwardToTargetVal<Pm      const>(m_p);
        testForwardToTargetVal<Pmf     const>(mf_p);

        // Do note test volatile rvalues of class types. They have no real use
        // and require strange copy constructors and comparison operators to
        // test correctly.
        testForwardToTargetVal<Enum    volatile>(e);
        testForwardToTargetVal<double  volatile>(d);
        testForwardToTargetVal<double *volatile>(p);
        testForwardToTargetVal<A       volatile>(a);
        testForwardToTargetVal<AU      volatile>(au);
        testForwardToTargetVal<PF      volatile>(f_p);
        testForwardToTargetVal<Pm      volatile>(m_p);
        testForwardToTargetVal<Pmf     volatile>(mf_p);

        testForwardToTargetArray<A           >(a);
        testForwardToTargetArray<A  const    >(a);
        testForwardToTargetArray<A          &>(a);
        testForwardToTargetArray<A  const   &>(a);
        testForwardToTargetArray<A  volatile&>(a);
        testForwardToTargetArray<AU          >(au);
        testForwardToTargetArray<AU const    >(au);
        testForwardToTargetArray<AU         &>(au);
        testForwardToTargetArray<AU const   &>(au);
        testForwardToTargetArray<AU volatile&>(au);
#if defined(BSLS_COMPILERFEATURES_SUPPORT_RVALUE_REFERENCES)
        testForwardToTargetArray<A          &&>(std::move(a));
        testForwardToTargetArray<A  const   &&>(std::move(a));
        testForwardToTargetArray<AU         &&>(std::move(au));
        testForwardToTargetArray<AU const   &&>(std::move(au));
#endif

        testForwardToTargetRef<Enum    &>(e);
        testForwardToTargetRef<Struct  &>(s);
        testForwardToTargetRef<Union   &>(u);
        testForwardToTargetRef<Class   &>(c);
        testForwardToTargetRef<double  &>(d);
        testForwardToTargetRef<double *&>(p);
        testForwardToTargetRef<F       &>(func);
        testForwardToTargetRef<Fi      &>(funcI);
        testForwardToTargetRef<FRi     &>(funcRi);
        testForwardToTargetRef<PF      &>(f_p);
        testForwardToTargetRef<Pm      &>(m_p);
        testForwardToTargetRef<Pmf     &>(mf_p);

        testForwardToTargetRef<Enum    const&>(e);
        testForwardToTargetRef<Struct  const&>(s);
        testForwardToTargetRef<Union   const&>(u);
        testForwardToTargetRef<Class   const&>(c);
        testForwardToTargetRef<double  const&>(d);
        testForwardToTargetRef<double *const&>(p);
        testForwardToTargetRef<PF      const&>(f_p);
        testForwardToTargetRef<Pm      const&>(m_p);
        testForwardToTargetRef<Pmf     const&>(mf_p);
        
        testForwardToTargetRef<Enum    volatile&>(e);
        testForwardToTargetRef<Struct  volatile&>(s);
        testForwardToTargetRef<Union   volatile&>(u);
        testForwardToTargetRef<Class   volatile&>(c);
        testForwardToTargetRef<double  volatile&>(d);
        testForwardToTargetRef<double *volatile&>(p);
        testForwardToTargetRef<PF      volatile&>(f_p);
        testForwardToTargetRef<Pm      volatile&>(m_p);
        testForwardToTargetRef<Pmf     volatile&>(mf_p);
        
        testForwardToTargetRef<Enum    const volatile&>(e);
        testForwardToTargetRef<Struct  const volatile&>(s);
        testForwardToTargetRef<Union   const volatile&>(u);
        testForwardToTargetRef<Class   const volatile&>(c);
        testForwardToTargetRef<double  const volatile&>(d);
        testForwardToTargetRef<double *const volatile&>(p);
        testForwardToTargetRef<PF      const volatile&>(f_p);
        testForwardToTargetRef<Pm      const volatile&>(m_p);
        testForwardToTargetRef<Pmf     const volatile&>(mf_p);
        
#if defined(BSLS_COMPILERFEATURES_SUPPORT_RVALUE_REFERENCES)
        testForwardToTargetRef<Enum    &&>(std::move(e));
        testForwardToTargetRef<Struct  &&>(std::move(s));
        testForwardToTargetRef<Union   &&>(std::move(u));
        testForwardToTargetRef<Class   &&>(std::move(c));
        testForwardToTargetRef<double  &&>(std::move(d));
        testForwardToTargetRef<double *&&>(std::move(p));
        testForwardToTargetRef<PF      &&>(std::move(f_p));
        testForwardToTargetRef<Pm      &&>(std::move(m_p));
        testForwardToTargetRef<Pmf     &&>(std::move(mf_p));

        testForwardToTargetRef<Enum     const&&>(std::move(e));
        testForwardToTargetRef<Struct   const&&>(std::move(s));
        testForwardToTargetRef<Union    const&&>(std::move(u));
        testForwardToTargetRef<Class    const&&>(std::move(c));
        testForwardToTargetRef<double   const&&>(std::move(d));
        testForwardToTargetRef<double * const&&>(std::move(p));
        testForwardToTargetRef<PF       const&&>(std::move(f_p));
        testForwardToTargetRef<Pm       const&&>(std::move(m_p));
        testForwardToTargetRef<Pmf      const&&>(std::move(mf_p));

        // Do not test volatile rvalue references.  They have no real uses and
        // would require distortions in the test that could result in missing
        // actual errors.
#endif

      } break;

      case 1: {
        // --------------------------------------------------------------------
        // TESTING bslmf::ForwardingType<TYPE>::Type
        //
        // Concerns:
        //: 1 The forwarding type for "*basic* type cvq 'T'" is the same as
        //:   'T' with the cv-qualification removed.
        //: 2 The forwarding type for "*class* *or* *union* type cvq 'T'" is
        //:   'const vq T&', where 'vq' is 'volatile' if 'T' is
        //:   volatile-qualified and is empty otherwise.
        //: 3 The forwarding type for "function of type 'F'" or "reference to
        //:   function of type 'F'" is "reference to function of type
        //:   'F'". The forwarding type for "pointer to function of type 'F'"
        //:   is the same pointer type, 'F*'.
        //: 4 The forwarding type for "array of cvq 'T'" or "(lvalue or
        //:   rvalue) reference to
        //:   array of cvq 'T'" is "cvq 'T*'", regardless of whether
        //:   the array size is known.
        //: 5 The forwarding type for "lvalue reference to type cvq 'T'" is
        //:   the same "lvalue reference to cvq 'T', for non-function and
        //:   non-array types.
        //: 6 (C++11 and newer only) The forwarding type for "rvalue reference
        //:   to cvq type 'T'" for non-function and non-array 'T' is 'const vq
        //:   T&', where 'vq' is 'volatile' if 'T' is volatile-qualified and
        //:   'vq' is empty otherwise.
        //
        // Test Plan:
        //: 1 For concern 1, instantiate 'ForwardingType' for fundamental,
        //:   pointer, and enumeration types, both cv-qualified and
        //:   unqualified, and verify  that the resulting 'Type' member is
        //:   the parameter type without cv qualification.
        //: 2 For concern 2, instantiate 'ForwardingType' for class and union
        //:   type, both unqualified and cv-qualified, and verify that the
        //:   resulting 'Type' member is the expected const lvalue reference
        //:   type. 
        //: 3 For concern 3, instantiate 'ForwardingType' for a small number
        //:   of function, reference-to-function, and pointer-to-function
        //:   parameters and verify that the resulting 'Type' member is the
        //:   expected type.
        //: 3 For concern 4, instantiate 'ForwadingType' for a small number of
        //:   array types, lvalue-reference-to-array types, and
        //:   rvalue-reference-to-array types, both sized and unsized, and
        //:   both cv-qualified and unqualified, and verify that the resulting
        //:   'Type' member is the expected pointer type.
        //: 5 For concern 5, instantiate 'ForwardingType' for *lvalue*
        //:   reference to fundamental, pointer, enumeration, class, and union
        //:   types, both cv-qualified and unqualified, and verify that the
        //:   resulting 'Type' member is the same as the parameter type.
        //: 6 For concern 6, instantiate 'ForwardingType' for *rvalue* reference
        //:   to fundamental, pointer, enumeration, class, and union
        //:   types, both cv-qualified and unqualified, and
        //:   verify that the resulting 'Type' member is the expected const
        //:   lvalue reference type.
        //
        // Testing:
        //     bslmf::ForwardingType<TYPE>::Type
        // --------------------------------------------------------------------

        if (verbose) cout << "\nbslmf::ForwardingType<TYPE>::Type"
                          << "\n=================================" << endl;

#define TEST_FWD_TYPE(T, EXP) ASSERT_SAME(bslmf::ForwardingType<T>::Type, EXP)
#define TEST_FWD_TYPE_UNCHANGED(T) TEST_FWD_TYPE(T, T)

        if (veryVerbose) cout << "Basic types" << endl;

        TEST_FWD_TYPE(int                        , int);
        TEST_FWD_TYPE(int *                      , int *);
        TEST_FWD_TYPE(int Class::*               , int Class::*);
        TEST_FWD_TYPE(int (*)(float)             , int (*)(float));
        TEST_FWD_TYPE(int (Class::*)(char)       , int (Class::*)(char));
        TEST_FWD_TYPE(Enum                       , Enum);

        TEST_FWD_TYPE(const int                  , int);
        TEST_FWD_TYPE(int *const                 , int *);
        TEST_FWD_TYPE(const int *                , const int *);
        TEST_FWD_TYPE(int Class::* const         , int Class::*);
        TEST_FWD_TYPE(int (* const)(float)       , int (*)(float));
        TEST_FWD_TYPE(int (Class::* const)(char) , int (Class::*)(char));
        TEST_FWD_TYPE(const Enum                 , Enum);

        TEST_FWD_TYPE(volatile int               , int);
        TEST_FWD_TYPE(int *volatile              , int *);
        TEST_FWD_TYPE(volatile int *             , volatile int *);
        TEST_FWD_TYPE(int Class::* volatile      , int Class::*);
        TEST_FWD_TYPE(int (* volatile)(float)    , int (*)(float));
        TEST_FWD_TYPE(volatile Enum              , Enum);

        TEST_FWD_TYPE(const volatile int         , int);
        TEST_FWD_TYPE(int *const volatile        , int *);
        TEST_FWD_TYPE(const volatile int *       , const volatile int *);
        TEST_FWD_TYPE(int Class::* const volatile, int Class::*);
        TEST_FWD_TYPE(const volatile Enum        , Enum);

        if (veryVerbose) cout << "Class and union types" << endl;

        TEST_FWD_TYPE(Class                 , const Class&);
        TEST_FWD_TYPE(Struct                , const Struct&);
        TEST_FWD_TYPE(Union                 , const Union&);

        TEST_FWD_TYPE(const Class           , const Class&);
        TEST_FWD_TYPE(const Struct          , const Struct&);
        TEST_FWD_TYPE(const Union           , const Union&);

        TEST_FWD_TYPE(volatile Class        , const volatile Class&);
        TEST_FWD_TYPE(volatile Struct       , const volatile Struct&);
        TEST_FWD_TYPE(volatile Union        , const volatile Union&);

        TEST_FWD_TYPE(const volatile Class  , const volatile Class&);
        TEST_FWD_TYPE(const volatile Struct , const volatile Struct&);
        TEST_FWD_TYPE(const volatile Union  , const volatile Union&);

        if (veryVerbose) cout << "Function types" << endl;

#if !defined(BSLS_PLATFORM_CMP_MSVC) && \
    (!defined(BSLS_PLATFORM_CMP_IBM) || (BSLS_PLATFORM_CMP_VER_MAJOR < 0x0800))
        // xlc-8 and MSVC 2005 seem to have problems with function types.
        // Skip these tests for those compilers
        TEST_FWD_TYPE(void()        , void(&)());
        TEST_FWD_TYPE(int(int)      , int(&)(int));
        TEST_FWD_TYPE(void(int&)    , void(&)(int&));
#endif

        TEST_FWD_TYPE(void(&)()     , void(&)());
        TEST_FWD_TYPE(int(&)(int)   , int(&)(int));
        TEST_FWD_TYPE(void(&)(int&) , void(&)(int&));

#if defined(BSLS_COMPILERFEATURES_SUPPORT_RVALUE_REFERENCES)
        TEST_FWD_TYPE(void(&&)()    , void(&)());
        TEST_FWD_TYPE(int(&&)(int)  , int(&)(int));
        TEST_FWD_TYPE(void(&&)(int&), void(&)(int&));
#endif

        TEST_FWD_TYPE(void(*)()     , void(*)());
        TEST_FWD_TYPE(int(*)(int)   , int(*)(int));
        TEST_FWD_TYPE(void(*)(int&) , void(*)(int&));

        if (veryVerbose) cout << "Array types" << endl;

        TEST_FWD_TYPE(int[5]                   , int*                    );
        TEST_FWD_TYPE(int*[5]                  , int**                   );
        TEST_FWD_TYPE(int[5][6]                , int(*)[6]               );
        TEST_FWD_TYPE(Class[]                  , Class*                  );
        TEST_FWD_TYPE(Struct[][6]              , Struct(*)[6]            );
        TEST_FWD_TYPE(int(&)[5]                , int*                    );
        TEST_FWD_TYPE(int *const(&)[5]         , int *const *            );
        TEST_FWD_TYPE(int(&)[5][6]             , int(*)[6]               );
        TEST_FWD_TYPE(Class(&)[]               , Class*                  );
        TEST_FWD_TYPE(Struct(&)[][6]           , Struct(*)[6]            );
        TEST_FWD_TYPE(int *const[5]            , int *const *            );
        TEST_FWD_TYPE(const int[5][6]          , const int(*)[6]         );
        TEST_FWD_TYPE(const int[]              , const int*              );
        TEST_FWD_TYPE(const int[][6]           , const int(*)[6]         );
        TEST_FWD_TYPE(const int(&)[5]          , const int*              );
        TEST_FWD_TYPE(volatile int[5]          , volatile int*           );
        TEST_FWD_TYPE(volatile int[5][6]       , volatile int(*)[6]      );
        TEST_FWD_TYPE(volatile int[]           , volatile int*           );
        TEST_FWD_TYPE(volatile int[][6]        , volatile int(*)[6]      );
        TEST_FWD_TYPE(volatile int(&)[5]       , volatile int*           );
        TEST_FWD_TYPE(const volatile int[5]    , const volatile int*     );
        TEST_FWD_TYPE(const volatile int[5][6] , const volatile int(*)[6]);
        TEST_FWD_TYPE(const volatile int[]     , const volatile int*     );
        TEST_FWD_TYPE(const volatile int[][6]  , const volatile int(*)[6]);
        TEST_FWD_TYPE(const volatile int(&)[5] , const volatile int*     );
#if defined(BSLS_COMPILERFEATURES_SUPPORT_RVALUE_REFERENCES)
        TEST_FWD_TYPE(int *const(&&)[5]        , int *const *            );
        TEST_FWD_TYPE(int(&&)[5]               , int*                    );
        TEST_FWD_TYPE(int(&&)[5][6]            , int(*)[6]               );
        TEST_FWD_TYPE(Class(&&)[]              , Class*                  );
        TEST_FWD_TYPE(Struct(&&)[][6]          , Struct(*)[6]            );
        TEST_FWD_TYPE(const int(&&)[5]         , const int*              );
        TEST_FWD_TYPE(volatile int(&&)[5]      , volatile int*           );
        TEST_FWD_TYPE(const volatile int(&&)[5], const volatile int*     );
#endif

        if (veryVerbose) cout << "Lvalue references" << endl;

        TEST_FWD_TYPE_UNCHANGED(int&                        );
        TEST_FWD_TYPE_UNCHANGED(int *&                      );
        TEST_FWD_TYPE_UNCHANGED(int Class::*&               );
        TEST_FWD_TYPE_UNCHANGED(int (*&)(float)             );
        TEST_FWD_TYPE_UNCHANGED(int (Class::*&)(char)       );
        TEST_FWD_TYPE_UNCHANGED(Enum&                       );
        TEST_FWD_TYPE_UNCHANGED(Class&                      );
        TEST_FWD_TYPE_UNCHANGED(Struct&                     );
        TEST_FWD_TYPE_UNCHANGED(Union&                      );

        TEST_FWD_TYPE_UNCHANGED(const int&                  );
        TEST_FWD_TYPE_UNCHANGED(int *const&                 );
        TEST_FWD_TYPE_UNCHANGED(const int *&                );
        TEST_FWD_TYPE_UNCHANGED(int Class::* const&         );
        TEST_FWD_TYPE_UNCHANGED(int (* const&)(float)       );
        TEST_FWD_TYPE_UNCHANGED(int (Class::* const&)(char) );
        TEST_FWD_TYPE_UNCHANGED(const Enum&                 );
        TEST_FWD_TYPE_UNCHANGED(const Class&                );
        TEST_FWD_TYPE_UNCHANGED(const Struct&               );
        TEST_FWD_TYPE_UNCHANGED(const Union&                );

        TEST_FWD_TYPE_UNCHANGED(volatile int&               );
        TEST_FWD_TYPE_UNCHANGED(int *volatile&              );
        TEST_FWD_TYPE_UNCHANGED(volatile int *&             );
        TEST_FWD_TYPE_UNCHANGED(int Class::* volatile&      );
        TEST_FWD_TYPE_UNCHANGED(int (* volatile&)(float)    );
        TEST_FWD_TYPE_UNCHANGED(volatile Enum&              );
        TEST_FWD_TYPE_UNCHANGED(volatile Class&             );
        TEST_FWD_TYPE_UNCHANGED(volatile Struct&            );
        TEST_FWD_TYPE_UNCHANGED(volatile Union&             );

        TEST_FWD_TYPE_UNCHANGED(const volatile int&         );
        TEST_FWD_TYPE_UNCHANGED(int *const volatile&        );
        TEST_FWD_TYPE_UNCHANGED(const volatile int *&       );
        TEST_FWD_TYPE_UNCHANGED(int Class::* const volatile&);
        TEST_FWD_TYPE_UNCHANGED(const volatile Enum&        );
        TEST_FWD_TYPE_UNCHANGED(const volatile Class&       );
        TEST_FWD_TYPE_UNCHANGED(const volatile Struct&      );
        TEST_FWD_TYPE_UNCHANGED(const volatile Union&       );

#if defined(BSLS_COMPILERFEATURES_SUPPORT_RVALUE_REFERENCES)
        if (veryVerbose) cout << "Rvalue references" << endl;

        TEST_FWD_TYPE(int&&                  , const int&);
        TEST_FWD_TYPE(int *&&                , int *const &);
        TEST_FWD_TYPE(const int *&&          , const int *const &);
        TEST_FWD_TYPE(volatile int *&&       , volatile int *const &);
        TEST_FWD_TYPE(const volatile int *&& , const volatile int *const &);
        TEST_FWD_TYPE(int Class::*&&         , int Class::* const&);
        TEST_FWD_TYPE(int (*&&)(float)       , int (*const &)(float));
        TEST_FWD_TYPE(int (Class::*&&)(char) , int (Class::*const &)(char));
        TEST_FWD_TYPE(Enum&&                 , const Enum&);
        TEST_FWD_TYPE(Class&&                , const Class&);
        TEST_FWD_TYPE(Struct&&               , const Struct&);
        TEST_FWD_TYPE(Union&&                , const Union&);

        TEST_FWD_TYPE(const int&&            , const int&);
        TEST_FWD_TYPE(int *const&&           , int *const &);
        TEST_FWD_TYPE(const int *const &&    , const int *const &);
        TEST_FWD_TYPE(int Class::* const&&   , int Class::* const &);
        TEST_FWD_TYPE(int (* const&&)(float) , int (* const&)(float));
        TEST_FWD_TYPE(int (Class::* const&&)(char)
                                             , int (Class::* const&)(char));
        TEST_FWD_TYPE(const Enum&&           , const Enum&);
        TEST_FWD_TYPE(const Class&&          , const Class&);
        TEST_FWD_TYPE(const Struct&&         , const Struct&);
        TEST_FWD_TYPE(const Union&&          , const Union&);

        TEST_FWD_TYPE(volatile int&&         , const volatile int&);
        TEST_FWD_TYPE(int *volatile&&        , int *const volatile&);
        TEST_FWD_TYPE(int Class::* volatile&&, int Class::* const volatile&);
        TEST_FWD_TYPE(int (* volatile&&)(float)
                                             , int (* const volatile&)(float));
        TEST_FWD_TYPE(volatile Enum&&        , const volatile Enum&);
        TEST_FWD_TYPE(volatile Class&&       , const volatile Class&);
        TEST_FWD_TYPE(volatile Struct&&      , const volatile Struct&);
        TEST_FWD_TYPE(volatile Union&&       , const volatile Union&);

        TEST_FWD_TYPE(const volatile int&&   , const volatile int&);
        TEST_FWD_TYPE(int *const volatile&&  , int *const volatile&);
        TEST_FWD_TYPE(int Class::* const volatile&&
                                             , int Class::* const volatile&);
        TEST_FWD_TYPE(const volatile Enum&&  , const volatile Enum&);
        TEST_FWD_TYPE(const volatile Class&& , const volatile Class&);
        TEST_FWD_TYPE(const volatile Struct&&, const volatile Struct&);
        TEST_FWD_TYPE(const volatile Union&& , const volatile Union&);

#endif // defined(BSLS_COMPILERFEATURES_SUPPORT_RVALUE_REFERENCES)

      } break;

      default: {
        cerr << "WARNING: CASE `" << test << "' NOT FOUND." << endl;
        testStatus = -1;
      }
    }

    if (testStatus > 0) {
        cerr << "Error, non-zero test status = "
             << testStatus << "." << endl;
    }
    return testStatus;
}

// ----------------------------------------------------------------------------
// Copyright 2013 Bloomberg Finance L.P.
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
