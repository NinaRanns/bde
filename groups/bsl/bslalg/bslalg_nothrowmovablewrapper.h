// bslalg_nothrowmovablewrapper.h                                     -*-C++-*-

#ifndef INCLUDED_BSLALG_NOTHROWMOVABLEWRAPPER
#define INCLUDED_BSLALG_NOTHROWMOVABLEWRAPPER

#include <bsls_ident.h>
BSLS_IDENT("$Id: $")

//@PURPOSE: Provide a wrapper that asserts a noexcept move constructor.
//
//@CLASSES:
// bslalg::NothrowMovableWrapper: wrapper class with nexcept move constructor
// bslalg::NothrowMovableWrapperUtil: namespace for managing wrappers
//
//@SEE_ALSO: bslstl_function
//
//@AUTHOR: Pablo Halpern (phalpern)
//
//@DESCRIPTION:  This component provides a wrapper class template
// 'bslalg::NothrowMovableWrapper<TYPE>' holding an of 'TYPE', and providing no
// other functionality other than returning the wrapped object.  The use of
// this class communicates to specific clients (see 'bslstl_function') that
// the wrapped object should be treated as-if it has a 'noexcept' move
// constructor, even in C++03, where 'noexcept' does not exist.  The client
// might, for example, move the object using efficient, non-exception-safe
// logic rather than, e.g., copying the object or storing it in heap memory so
// that its pointer can be moved.  The behavior is undefined if the move
// constructor is invoked and *does* throw; typically resulting in 'terminate'
// being invoked.
//
// This component also provides a utility struct,
// 'bslalg::NothrowMovableWrapperUtil', that provides a namespace for static
// functions 'wrap' and 'unwrap' with a uniform interface such that unwrapping
// an object that is not wrapped or wrapping an object that is already wrapped
// are noops.  This utility struct also provides type traits for determining
// whether a type is wrapped and for deducing the type of the wrapped and
// unwrapped object.
//
///Usage
///-----
//
///Example 1
///- - - - -
// In this example, we define a class template, 'CountedType<TYPE>',
// that is little more than just a a wrapper around 'TYPE' that counts the
// number of extant 'CountedType' objects.  We begin by defining the static
// count member along with the single value member:
//..
//  template <class TYPE>
//  class CountedType {
//      // CLASS DATA
//      static int s_count;
//
//      // DATA
//      TYPE       d_value;
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
//      // PRIVATE CLASS FUNCTIONS
//      template <class TP>
//      static typename
//      bsl::conditional<bsl::is_nothrow_move_constructible<TP>::value,
//                       bslmf::MovableRef<TP>, const TP&>::type
//      MoveIfNoexcept(TP& x);
//..
// We next finish out the class definition with a constructor, copy
// constructor, move constructor, destructor, and member functions to retrieve
// the count and value:
//..
//  public:
//      // CLASS FUNCTIONS
//      static int count() { return s_count; }
//
//      // CREATORS
//      CountedType(const TYPE& val);
//          // Construct 'CountedType' from the specified 'val'.
//
//      CountedType(const CountedType& original);
//          // Copy construct '*this' from the specified 'original' object.
//
//      CountedType(bslmf::MovableRef<CountedType> original);
//          // Move construct '*this' from 'original'.  If an exception is
//          // thrown, by the constructor for 'TYPE' 'original' is unchanged.
//
//      ~CountedType() { --s_count; }
//          // Destroy this object.
//
//      // MANIPULATORS
//      TYPE& value() { return d_value; }
//
//      // ACCESSORS
//      const TYPE& value() const { return d_value; }
//  };
//..
// Next, we implement 'MoveIfNoexcept', which calls 'move' on its argument,
// allowing it to convert back to an lvalue if the return type is an lvalue
// reference:
//..
//  template <class TYPE>
//  template <class TP>
//  inline typename
//  bsl::conditional<bsl::is_nothrow_move_constructible<TP>::value,
//                   bslmf::MovableRef<TP>, const TP&>::type
//  CountedType<TYPE>::MoveIfNoexcept(TP& x)
//  {
//      return bslmf::MovableRefUtil::move(x);
//  }
//..
// Next, we implement the value constructor and move constructor, which simply
// copy their argument into the 'd_value' data members and increment the
// count:
//..
//  template <class TYPE>
//  CountedType<TYPE>::CountedType(const TYPE& val) : d_value(val)
//  {
//      ++s_count;
//  }
//
//  template <class TYPE>
//  CountedType<TYPE>::CountedType(const CountedType& original)
//      : d_value(original.d_value)
//  {
//      ++s_count;
//  }
//..
// We're now ready implement the move constructor.  Logically, we would simply
// move the value from 'original' into the 'd_value' member of '*this', but an
// exception thrown 'TYPE''s move constructor would leave 'original' in a
// (valid but) unspecified state, violating the strong guarantee.  Instead, we
// move the value only if we know that the move will succeed; otherwise, we
// copy it.  This behavior is facilitated by the 'MoveIfNoexcept' function
// defined above:
//..
//  template <class TYPE>
//  CountedType<TYPE>::CountedType(bslmf::MovableRef<CountedType> original)
//      : d_value(
//          MoveIfNoexcept(bslmf::MovableRefUtil::access(original).d_value))
//  {
//      ++s_count;
//  }
//..
// Finally, we define the 's_count' member to complete the class
// implementation:
//..
//  template <class TYPE>
//  int CountedType<TYPE>::s_count = 0;
//..
// To test the 'CountedType' class template, assume a simple client type,
// 'SomeType' that makes it easy to detect if it was move constructed.
// 'SomeType' holds an 'int' value which is set to -1 when it is moved from,
// as shown here:
//..
//  class SomeType {
//      int d_value;
//  public:
//      SomeType(int v = 0) : d_value(v) { }                        // IMPLICIT
//      SomeType(const SomeType& original) : d_value(original.d_value) { }
//      SomeType(bslmf::MovableRef<SomeType> original)
//          : d_value(bslmf::MovableRefUtil::access(original).d_value)
//          { bslmf::MovableRefUtil::access(original).d_value = -1; }
//
//      int value() const { return d_value; }
//  };
//..
// Notice that 'SomeType' neglected to declare its move constructor as
// 'noexcept'.  This might be an oversight or it could be an old class that
// predates both 'noexcept' and the 'bsl::is_nothrow_move_constructible'
// trait.  It is even be possible that the move constructor might throw
// (though, of course, it doesn't in this simplified example).  Regardless,
// the effect is that move-constructing a 'CountedType<SomeType>' will result
// in the move constructor actually performing a copy:
//..
//  void main()
//  {
//      CountedType<SomeType> obj1(1);
//      CountedType<SomeType> obj2(bslmf::MovableRefUtil::move(obj1));
//      assert(1 == obj1.value().value());  // Copied, not moved from
//      assert(1 == obj2.value().value());
//..
// For the purpose of this example, we can be sure that 'SomeThing' will not
// throw on move, at least not in our applcation.  In order to obtain the
// expected move optimiztion, we next wrap our 'SomeType in a
// 'bslalg::NothrowMovableWrapper':
//..
//      CountedType<bslalg::NothrowMovableWrapper<SomeType> >
//          obj3(SomeType(3));
//      CountedType<bslalg::NothrowMovableWrapper<SomeType> >
//          obj4(bslmf::MovableRefUtil::move(obj3));
//      assert(-1 == obj3.value().unwrap().value());  // moved from
//      assert(3 == obj4.value().unwrap().value());
//  }
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
//  template <class TYPE>
//  class CountedType2 {
//      CountedType<TYPE> d_data;
//..
// Next, for convenience, we add a public data type, 'ValueType' for the value
// stored within 'CountedType2'.  However, rather than defining 'ValueType'
// as simply 'TYPE', we unwrap it, in case 'TYPE' is an instantiation of
// 'NothrowMovableWrapper':
//..
//  public:
//      // TYPES
//      typedef typename
//      bslalg::NothrowMovableWrapperUtil::UnwrappedType<TYPE>::type ValueType;
//..
// Note that the 'UnwrappedType' metafunction has no affect of 'TYPE' is not
// wrapped.
//
// Next, we declare (and define) the class functions, constructors, and
// destructor, simply forwarding to the corresponding 'CountedType' function,
// constructor, or destructor:
//..
//      // CLASS FUNCTIONS
//      static int count() { return CountedType<TYPE>::count(); }
//
//      // CREATORS
//      CountedType2(const TYPE& val) : d_data(val) { }
//      CountedType2(const CountedType2& original)
//          : d_data(original.d_data) { }
//      CountedType2(bslmf::MovableRef<CountedType2> original)
//          : d_data(bslmf::MovableRefUtil::move(
//                       bslmf::MovableRefUtil::access(original).d_data)) { }
//..
// Finally, we implement the 'value()' members such that the returned values
// do not need to be unwrapped.  As in the case of the 'UnwrappedType'
// metafunction, the 'unwrap()' function in 'NothrowMovableWrapperUtil'
// handles both wrapped and unwrapped arguments, unwrapping the latter and
// returning an unmodified reference to the former:
//..
//      // MANIPULATORS
//      ValueType& value()
//      {
//          return bslalg::NothrowMovableWrapperUtil::unwrap(d_data.value());
//          // Alternatively: 'return d_data.value();'
//      }
//
//      // ACCESSORS
//      const ValueType& value() const
//      {
//          return bslalg::NothrowMovableWrapperUtil::unwrap(d_data.value());
//          // Alternatively: 'return d_data.value();'
//      }
//  };
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
//  void main()
//  {
//      CountedType2<SomeType> obj1(1);
//      CountedType2<SomeType> obj2(bslmf::MovableRefUtil::move(obj1));
//      assert(1 == obj1.value().value());  // Copied, not moved from
//      assert(1 == obj2.value().value());
//
//      CountedType2<bslalg::NothrowMovableWrapper<SomeType> >
//          obj3(SomeType(3));
//      CountedType2<bslalg::NothrowMovableWrapper<SomeType> >
//          obj4(bslmf::MovableRefUtil::move(obj3));
//      assert(-1 == obj3.value().value());  // moved from
//      assert(3 == obj4.value().value());   // No need to call 'unwrap'
//  }
//..

#include <bslscm_version.h>

#include <bslma_constructionutil.h>
#include <bslma_stdallocator.h>
#include <bslma_usesbslmaallocator.h>

#include <bslmf_allocatorargt.h>
#include <bslmf_assert.h>
#include <bslmf_conditional.h>
#include <bslmf_isarray.h>
#include <bslmf_isfunction.h>
#include <bslmf_isnothrowmoveconstructible.h>
#include <bslmf_movableref.h>
#include <bslmf_nestedtraitdeclaration.h>
#include <bslmf_removecv.h>
#include <bslmf_usesallocatorargt.h>

#include <bsls_keyword.h>
#include <bsls_objectbuffer.h>

namespace BloombergLP {

namespace bslalg {

// 'MovableRef<TYPE>' is defined such that 'TYPE' cannot be deduced directly
// from 'MovableRef<TYPE>' in C++11 mode.  Use
// 'BSLMF_NOTHROWMOVABLEWRAPPER_DEDUCE_RVREF(TYPE)' instead of
// 'MovableRef<TYPE>' in situations where 'TYPE' must be deduced.
#ifdef BSLS_COMPILERFEATURES_SUPPORT_RVALUE_REFERENCES
# define BSLMF_NOTHROWMOVABLEWRAPPER_DEDUCE_RVREF(TYPE) TYPE&&
#else
# define BSLMF_NOTHROWMOVABLEWRAPPER_DEDUCE_RVREF(TYPE) bslmf::MovableRef<TYPE>
#endif

template <class TYPE, bool = bsl::is_function<TYPE>::value>
struct NothrowMovableWrapper_Traits;

                        // ======================================
                        // class template NothrowMovableWrapper
                        // ======================================

template <class TYPE>
class NothrowMovableWrapper
{
    // An object of this type wraps a value of the specified 'TYPE', and
    // provides no other functionality other than returning the wrapped
    // object.  The move constructor is guaranteed not to throw, even if the
    // move constructor for 'TYPE' has no such guarantee.  The user is thus
    // asserting that the move constructor for the wrapped object *will not*
    // throw, even if it is allowed to.  Constraints: this class can be
    // instantiated on object types only, i.e., not references, arrays, or
    // function types (though function pointers are OK).

    // Cannot wrap reference types, array types, or function types.
    BSLMF_ASSERT(! bslmf::MovableRefUtil::IsReference<TYPE>::value);
    BSLMF_ASSERT(! bsl::is_array<TYPE>::value);
    BSLMF_ASSERT(! bsl::is_function<TYPE>::value);

    // PRIVATE TYPES
    struct DummyAllocator {
        // Private type that prevents allocator-argument overloads from
        // participating in overload resolution if 'TYPE' is not allocator
        // aware.  Does not meet the allocator requirements (or any other
        // requirements) and cannot be constructed by users.
    };

    typedef bslmf::MovableRefUtil MovableRefUtil;

    // DATA
    bsls::ObjectBuffer<TYPE> d_buffer;

    // NOT IMPLEMENTED
    NothrowMovableWrapper&
    operator=(const NothrowMovableWrapper&) BSLS_KEYWORD_DELETED;
        // Not assignable.

  public:
    // TRAITS
    BSLMF_NESTED_TRAIT_DECLARATION(NothrowMovableWrapper,
                                   bsl::is_nothrow_move_constructible);

    BSLMF_NESTED_TRAIT_DECLARATION_IF(NothrowMovableWrapper,
                                      bslma::UsesBslmaAllocator,
                                      bslma::UsesBslmaAllocator<TYPE>::value);

    // If this wrapper is allocator-aware (because 'TYPE' is allocator-aware),
    // then choose the leading-allocator convention.
    BSLMF_NESTED_TRAIT_DECLARATION_IF(NothrowMovableWrapper,
                                      bslmf::UsesAllocatorArgT,
                                      bslma::UsesBslmaAllocator<TYPE>::value);

    BSLMF_NESTED_TRAIT_DECLARATION_IF(NothrowMovableWrapper,
                                      bslmf::IsBitwiseMoveable,
                                      bslmf::IsBitwiseMoveable<TYPE>::value);

    // TYPES
    typedef typename bsl::conditional<bslma::UsesBslmaAllocator<TYPE>::value,
                                      bsl::allocator<char>,
                                      DummyAllocator>::type    allocator_type;
        // Type of allocator to use.  If 'TYPE' is not allocator-aware, then
        // this is a private dummy type that will disable use of any
        // constructor that takes an allocator.

    typedef TYPE ValueType;

    // CREATORS
    NothrowMovableWrapper();
    NothrowMovableWrapper(bsl::allocator_arg_t  ,
                          const allocator_type& allocator);
        // Value-initialize the object wrapped by '*this'.  For
        // allocator-aware 'TYPE', optionally specify an 'allocator' (e.g.,
        // the address of a 'bslma::Allocator' object) to supply memory;
        // otherwise, the default allocator is used.

    NothrowMovableWrapper(const TYPE& val);                         // IMPLICIT
    NothrowMovableWrapper(bsl::allocator_arg_t  ,
                          const allocator_type& allocator,
                          const TYPE&           val);
        // Wrap the specified 'val', using 'TYPE''s (possibly extended) copy
        // constructor.  For allocator-aware 'TYPE', optionally specify an
        // 'allocator' (e.g., the address of a 'bslma::Allocator' object) to
        // supply memory; otherwise, the default allocator is used.

    NothrowMovableWrapper(bslmf::MovableRef<TYPE> val);             // IMPLICIT
        // Wrap the specified 'val', using 'TYPE''s move constructor.  Note
        // that this move constructor is unconditionally 'noexcept', as that
        // is the entire purpose of this wrapper.

    NothrowMovableWrapper(bsl::allocator_arg_t    ,
                          const allocator_type&   allocator,
                          bslmf::MovableRef<TYPE> val);
        // Wrap the specified 'val', using 'TYPE''s extended move constructor.
        // Use the specified 'allocator' (e.g., the address of a
        // 'bslma::Allocator' object) to supply memory.  Note that this
        // constructor will not be selected by overload resolution unless
        // 'TYPE' is allocator aware.

    NothrowMovableWrapper(const NothrowMovableWrapper& original);
        // Copy construct from the specified 'original' wrapper using 'TYPE''s
        // copy constructor.

    NothrowMovableWrapper(bsl::allocator_arg_t      ,
                          const allocator_type&     alloc,
                          const NothrowMovableWrapper& original);
        // Copy construct from the specified 'original' wrapper using 'TYPE''s
        // extended copy constructor.  Use the specified 'allocator' (e.g.,
        // the address of a 'bslma::Allocator' object) to supply memory.  Note
        // that this constructor will not be selected by overload resolution
        // unless 'TYPE' is allocator aware.

    NothrowMovableWrapper(bslmf::MovableRef<NothrowMovableWrapper> original)
                                                         BSLS_KEYWORD_NOEXCEPT;
        // Move construct from the specified 'original' wrapper using 'TYPE''s
        // move constructor.

    NothrowMovableWrapper(bsl::allocator_arg_t                  ,
                          const allocator_type&                 alloc,
                          bslmf::MovableRef<NothrowMovableWrapper> original);
        // Move construct from the specified 'original' wrapper using 'TYPE''s
        // extended move constructor.  Use the specified 'allocator' (e.g.,
        // the address of a 'bslma::Allocator' object) to supply memory.  Note
        // that this constructor will not be selected by overload resolution
        // unless 'TYPE' is allocator aware.

    ~NothrowMovableWrapper();
        // Destroy this object, invoking 'TYPE''s destructor.

    // MANIPULATORS
    ValueType& unwrap();
    operator ValueType&();
        // Return a modifiable reference the wrapped object.

    // ACCESSORS
    ValueType const& unwrap() const;
    operator const ValueType&() const;
        // Return a const reference the wrapped object.

    allocator_type get_allocator() const;
        // Return the allocator used to construct this object.  Note that this
        // method will fail to instantiate unless 'TYPE' is allocator-aware.
};

                        // =============================
                        // struct NothrowMovableWrapperUtil
                        // =============================

struct NothrowMovableWrapperUtil {
    // Namesapace for 'NothrowMovableWrapper' traits and utilities.

    // TRAITS
    template <class TYPE>
    struct IsWrapped : NothrowMovableWrapper_Traits<TYPE>::IsWrapped {
        // Metafunction evaluating to 'true_type' if 'TYPE' is a
        // specialization of 'NothrowMovableWrapper'; otherwise, 'false_type'.
    };

    template <class TYPE>
    struct WrappedType {
        // Metafunction: If 'TYPE' is a specialization of
        // 'NothrowMovableWrapper', then 'type' is 'TYPE'; otherwise 'type' is
        // 'NothrowMovableWrapper<TYPE>'.

        typedef typename
        NothrowMovableWrapper_Traits<TYPE>::WrappedType type;
    };

    template <class TYPE>
    struct UnwrappedType {
        // Metafunction: If 'TYPE' is a specialization of
        // 'NothrowMovableWrapper', then 'type' is 'TYPE::ValueType'; otherwise
        // 'type' is 'TYPE'.

        typedef typename
        NothrowMovableWrapper_Traits<TYPE>::UnwrappedType type;
    };

    // CLASS METHODS
    template <class TYPE>
    static typename WrappedType<TYPE>::type wrap(TYPE&       f);
    template <class TYPE>
    static typename WrappedType<TYPE>::type wrap(TYPE const& f);
    template <class TYPE>
    static typename WrappedType<
        typename bslmf::MovableRefUtil::RemoveReference<TYPE>::type>::type
    wrap(BSLMF_NOTHROWMOVABLEWRAPPER_DEDUCE_RVREF(TYPE)         f);
        // Return a wrapped copy of the specified 'f' object.  If 'f' is
        // already wrapped, return a simple copy of 'f' without wrapping it
        // again.  Note that the overloads taking an lvalue argument prevent
        // the overload taking an rvalue argument from treating the arguement
        // as a forwarding reference.

    template <class TYPE>
    static typename UnwrappedType<TYPE>::type&       unwrap(TYPE&       f);
    template <class TYPE>
    static typename UnwrappedType<TYPE>::type const& unwrap(TYPE const& f);
    template <class TYPE>
    static bslmf::MovableRef<
        typename UnwrappedType<
            typename bslmf::MovableRefUtil::RemoveReference<TYPE>::type>::type>
    unwrap(BSLMF_NOTHROWMOVABLEWRAPPER_DEDUCE_RVREF(TYPE)                   f);
        // Return a reference to the object wrapped in the specified 'f'
        // object.  If 'f' is wrapped, simply return a reference to 'f'.  Note
        // that the overloads taking an lvalue argument prevent the overload
        // taking an rvalue argument from treating the arguement as a
        // forwarding reference.
};

}  // close package namespace

// ===========================================================================
//                TEMPLATE AND INLINE IMPLEMENTATIONS
// ===========================================================================

                        // ----------------------------------------
                        // class template NothrowMovableWrapper_Traits
                        // ----------------------------------------

namespace bslalg {

template <class TYPE>
struct NothrowMovableWrapper_Traits<TYPE, false /* is_function */>
{
    // Component-private class -- do not use.
    // This specialization of traits is for non-function types that are not
    // wrapped.

    // Cannot instantiate on reference types or array types.
    BSLMF_ASSERT(! bslmf::MovableRefUtil::IsReference<TYPE>::value);
    BSLMF_ASSERT(! bsl::is_array<TYPE>::value);

    typedef bsl::false_type                                    IsWrapped;
    typedef TYPE                                               UnwrappedType;
    typedef
    NothrowMovableWrapper<typename bsl::remove_cv<TYPE>::type> WrappedType;
};

template <class TYPE>
struct NothrowMovableWrapper_Traits<TYPE, true /* is_function */>
{
    // Component-private class -- do not use.
    // This specialization of traits is for function types that are not
    // wrapped.  This specialization is NOT for function pointers or function
    // references.
    //
    // 'UnwrappedType' is an object type, and is thus the decayed version of
    // 'TYPE', i.e., a function pointer.  Since 'unwrap' always returns a
    // reference, this decay means that, 'unwrap(f)' will not compile if 'f'
    // is a function or reference to function.  However
    // 'UnwrappedType pf = f;' will work whether 'f' is a function or not.

    typedef bsl::false_type              IsWrapped;
    typedef TYPE*                        UnwrappedType;
    typedef NothrowMovableWrapper<TYPE*> WrappedType;
};

template <class TYPE>
struct NothrowMovableWrapper_Traits<NothrowMovableWrapper<TYPE>, false>
{
    // Component-private class -- do not use.
    // This specialization is for wrapped types.

    typedef bsl::true_type              IsWrapped;
    typedef TYPE                        UnwrappedType;
    typedef NothrowMovableWrapper<TYPE> WrappedType;
};

}  // close package namespace

                        // ---------------------------------
                        // class template NothrowMovableWrapper
                        // ---------------------------------


// CREATORS
template <class TYPE>
inline
bslalg::NothrowMovableWrapper<TYPE>::NothrowMovableWrapper()
{
    bslma::ConstructionUtil::construct(d_buffer.address(), (void*) 0);
}

template <class TYPE>
inline
bslalg::NothrowMovableWrapper<TYPE>::
NothrowMovableWrapper(bsl::allocator_arg_t  ,
                      const allocator_type& allocator)
{
    bslma::ConstructionUtil::construct(d_buffer.address(),
                                       allocator.mechanism());
}

template <class TYPE>
inline
bslalg::NothrowMovableWrapper<TYPE>::NothrowMovableWrapper(const TYPE& val)
{
    bslma::ConstructionUtil::construct(d_buffer.address(), (void*) 0, val);
}

template <class TYPE>
inline
bslalg::NothrowMovableWrapper<TYPE>::
NothrowMovableWrapper(bsl::allocator_arg_t  ,
                      const allocator_type& allocator,
                      const TYPE&           val)
{
    bslma::ConstructionUtil::construct(d_buffer.address(),
                                       allocator.mechanism(),
                                       val);
}

template <class TYPE>
inline
bslalg::NothrowMovableWrapper<TYPE>::
NothrowMovableWrapper(bslmf::MovableRef<TYPE> val)
{
    bslma::ConstructionUtil::construct(d_buffer.address(), (void*) 0,
                                       bslmf::MovableRefUtil::move(val));
}

template <class TYPE>
inline
bslalg::NothrowMovableWrapper<TYPE>::
NothrowMovableWrapper(bsl::allocator_arg_t    ,
                      const allocator_type&   allocator,
                      bslmf::MovableRef<TYPE> val)
{
    bslma::ConstructionUtil::construct(d_buffer.address(),
                                       allocator.mechanism(),
                                       bslmf::MovableRefUtil::move(val));
}

template <class TYPE>
inline
bslalg::NothrowMovableWrapper<TYPE>::
NothrowMovableWrapper(const NothrowMovableWrapper& original)
{
    bslma::ConstructionUtil::construct(d_buffer.address(), (void*) 0,
                                       original.unwrap());

}

template <class TYPE>
inline
bslalg::NothrowMovableWrapper<TYPE>::
NothrowMovableWrapper(bsl::allocator_arg_t         ,
                      const allocator_type&        allocator,
                      const NothrowMovableWrapper& original)
{
    bslma::ConstructionUtil::construct(d_buffer.address(),
                                       allocator.mechanism(),
                                       original.unwrap());
}

template <class TYPE>
inline
bslalg::NothrowMovableWrapper<TYPE>::
NothrowMovableWrapper(bslmf::MovableRef<NothrowMovableWrapper> original)
                                                          BSLS_KEYWORD_NOEXCEPT
{
    bslma::ConstructionUtil::construct(d_buffer.address(), (void*) 0,
                        bslmf::MovableRefUtil::move(
                            bslmf::MovableRefUtil::access(original).unwrap()));
}

template <class TYPE>
inline
bslalg::NothrowMovableWrapper<TYPE>::
NothrowMovableWrapper(bsl::allocator_arg_t                     ,
                      const allocator_type&                    allocator,
                      bslmf::MovableRef<NothrowMovableWrapper> original)
{
    bslma::ConstructionUtil::construct(
        d_buffer.address(),
        allocator.mechanism(),
        bslmf::MovableRefUtil::move(
                            bslmf::MovableRefUtil::access(original).unwrap()));
}

template <class TYPE>
inline
bslalg::NothrowMovableWrapper<TYPE>::~NothrowMovableWrapper()
{
    d_buffer.object().~TYPE();
}

// MANIPULATORS
template <class TYPE>
inline
typename bslalg::NothrowMovableWrapper<TYPE>::ValueType&
bslalg::NothrowMovableWrapper<TYPE>::unwrap()
{
    return d_buffer.object();
}

template <class TYPE>
inline
bslalg::NothrowMovableWrapper<TYPE>::operator ValueType&()
{
    return unwrap();
}

// ACCESSORS
template <class TYPE>
inline
typename bslalg::NothrowMovableWrapper<TYPE>::ValueType const&
bslalg::NothrowMovableWrapper<TYPE>::unwrap() const
{
    return d_buffer.object();
}

template <class TYPE>
inline
bslalg::NothrowMovableWrapper<TYPE>::operator const ValueType&() const
{
    return unwrap();
}

template <class TYPE>
inline
typename bslalg::NothrowMovableWrapper<TYPE>::allocator_type
bslalg::NothrowMovableWrapper<TYPE>::get_allocator() const
{
    return d_buffer.object().allocator();
}

                        // -----------------------------------------
                        // struct template NothrowMovableWrapperUtil
                        // -----------------------------------------

// PUBLIC CLASS METHODS
template <class TYPE>
inline
typename bslalg::NothrowMovableWrapperUtil::WrappedType<TYPE>::type
bslalg::NothrowMovableWrapperUtil::wrap(TYPE& f)
{
    return f;
}

template <class TYPE>
inline
typename bslalg::NothrowMovableWrapperUtil::WrappedType<TYPE>::type
bslalg::NothrowMovableWrapperUtil::wrap(const TYPE& f)
{
    return f;
}

template <class TYPE>
inline
typename bslalg::NothrowMovableWrapperUtil::WrappedType<
    typename bslmf::MovableRefUtil::RemoveReference<TYPE>::type>::type
bslalg::NothrowMovableWrapperUtil::
wrap(BSLMF_NOTHROWMOVABLEWRAPPER_DEDUCE_RVREF(TYPE) f)
{
    return bslmf::MovableRefUtil::move(f);
}

template <class TYPE>
inline
typename bslalg::NothrowMovableWrapperUtil::UnwrappedType<TYPE>::type&
bslalg::NothrowMovableWrapperUtil::unwrap(TYPE& f)
{
    return f;
}

template <class TYPE>
inline
typename bslalg::NothrowMovableWrapperUtil::UnwrappedType<TYPE>::type const&
bslalg::NothrowMovableWrapperUtil::unwrap(TYPE const& f)
{
    return f;
}

template <class TYPE>
inline
bslmf::MovableRef<
    typename bslalg::NothrowMovableWrapperUtil::UnwrappedType<
        typename bslmf::MovableRefUtil::RemoveReference<TYPE>::type>::type>
unwrap(BSLMF_NOTHROWMOVABLEWRAPPER_DEDUCE_RVREF(TYPE) f)
{
    typename bslalg::NothrowMovableWrapperUtil::UnwrappedType<TYPE>::type& r =
        bslmf::MovableRefUtil::access(f);
    return bslmf::MovableRefUtil::move(r);
}

}  // close enterprise namespace

#endif // ! defined(INCLUDED_BSLALG_NOTHROWMOVABLEWRAPPER)

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
