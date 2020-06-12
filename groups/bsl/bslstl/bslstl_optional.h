// bslstl_optional.h                                                  -*-C++-*-

// ----------------------------------------------------------------------------
//                                   NOTICE
//
// This component is not up to date with current BDE coding standards, and
// should not be used as an example for new development.
// ----------------------------------------------------------------------------

#ifndef INCLUDED_BSLSTL_OPTIONAL
#define INCLUDED_BSLSTL_OPTIONAL

#include <bsls_ident.h>
BSLS_IDENT("$Id: $")

//@PURPOSE: Provide a standard-compliant allocator aware optional type.
//
//@CLASSES:
//  bsl::optional: template for nullable (in-place) objects
//
//@DESCRIPTION: This component provides a template class,
// 'bsl::optional<TYPE>', that can be used to augment an arbitrary
// value-semantic 'TYPE', such as 'int' or 'bsl::string', so that it also
// supports the notion of a "null" value.  That is, the set of values
// representable by the template parameter 'TYPE' is extended to include null.
// If the underlying 'TYPE' has value-semantics, then so will the augmented
// type 'bsl::optional<TYPE>'.  Two homogeneous nullable objects have the same
// value if their underlying (non-null) 'TYPE' values are the same, or both are
// null.
//
// Note that the object of template parameter 'TYPE' that is managed by a
// 'bsl:optional<TYPE>' object is created *in*-*place*.  Consequently, the
// template parameter 'TYPE' must be a complete type when the class is
// instantiated.  In contrast, 'bslstl::NullableAllocatedValue<TYPE>' (see
// 'bslstl_nullableallocatedvalue') does not require that 'TYPE' be complete
// when that class is instantiated, with the trade-off that the managed 'TYPE'
// object is always allocated out-of-place in that case.
//
// In addition to the standard homogeneous, value-semantic, operations such as
// copy/move construction, copy/move assignment, equality comparison, and
// relational operators, 'bsl::optional' also supports conversion between
// augmented types for which the underlying types are convertible, i.e., for
// heterogeneous copy construction, copy assignment, and equality comparison
// (e.g., between 'int' and 'double'); attempts at conversion between
// incompatible types, such as 'int' and 'bsl::string', will fail to compile.
// Note that these operational semantics are similar to those found in
// 'bsl::shared_ptr'.
//
// For allocator-aware types, bsl::optional uses the same allocator for all
// 'value_type' objects it manages during its lifetime.
//
///Usage
///-----
// The following snippets of code illustrate use of this component:
//
// First, create a nullable 'int' object:
//..
//  bsl::optional<int> optionalInt;
//  assert( !optionalInt.has_value());
//..
// Next, give the 'int' object the value 123 (making it non-null):
//..
//  optionalInt.emplace(123);
//  assert(optionalInt.has_value());
//  assert(123 == optionalInt.value());
//..
// Finally, reset the object to its default constructed state (i.e., null):
//..
//  optionalInt.reset();
//  assert( !optionalInt.has_value());
//..

//known limitations :
//  - For assignment/construction constraints, we use is_constructible
//    but the exact creation will be done using allocation construction
//    that will invoke an allocator extended constructor for allocator
//    aware types. If the 'value_type' is constructible from the
//    assignment/constructor parameter, but doesn't have a corresponding
//    allocator extended constructor, the overload selection may not be
//    be correct.
//  - 'optional<const TYPE>' is fully supported in C++11 and onwards. However,
//    due to limitations of 'MovableRef<const TYPE>', C++03 support for const
//    value_types is limited and move semantics of such an 'optional' in C++03
//    will not work.


#include <bslscm_version.h>
#include <bslalg_swaputil.h>

#include <bslma_constructionutil.h>
#include <bslma_default.h>
#include <bslma_destructionutil.h>
#include <bslma_stdallocator.h>
#include <bslma_usesbslmaallocator.h>

#include <bslmf_allocatorargt.h>
#include <bslmf_decay.h>
#include <bslmf_if.h>
#include <bslmf_integralconstant.h>
#include <bslmf_isbitwisemoveable.h>
#include <bslmf_isconvertible.h>
#include <bslmf_movableref.h>
#include <bslmf_nestedtraitdeclaration.h>
#include <bslmf_removeconst.h>
#include <bslmf_removecv.h>
#include <bslmf_removereference.h>

#include <bsls_assert.h>
#include <bsls_compilerfeatures.h>
#include <bsls_exceptionutil.h>
#include <bsls_keyword.h>
#include <bsls_objectbuffer.h>
#include <bsls_unspecifiedbool.h>
#include <bsls_util.h>

#include <bslstl_badoptionalaccess.h>

#include <stddef.h>

#ifdef BSLS_LIBRARYFEATURES_HAS_CPP11_BASELINE_LIBRARY
//#include <bsl_type_traits.h>
#include <type_traits>
#endif
#ifdef BSLS_LIBRARYFEATURES_HAS_CPP17_BASELINE_LIBRARY
//In C++17, bsl::optional for non-aa types inherits from std::optional
#include <optional>
#endif // BSLS_LIBRARYFEATURES_HAS_CPP17_BASELINE_LIBRARY

#if defined(BSLS_COMPILERFEATURES_SUPPORT_GENERALIZED_INITIALIZERS)
#include <initializer_list>
#endif

//#if BSLS_COMPILERFEATURES_SIMULATE_CPP11_FEATURES
//#error "BSLS_COMPILERFEATURES_SIMULATE_CPP11_FEATURES not supported"
//#endif

//TBD move these macros somewhere more appropriate
#if defined(BSLS_COMPILERFEATURES_SUPPORT_REF_QUALIFIERS)
#define BSLS_KEYWORD_LVREF_QUAL &
#define BSLS_KEYWORD_RVREF_QUAL &&
    // These macros represent ref-qualifiers on member functions in C++11
#else
#define BSLS_KEYWORD_LVREF_QUAL
//#define BSLS_KEYWORD_RVREF_QUAL
    // In C++03, there is no notion of rvalues, and all member functions are
    // effectively lvalue qualified.  'BSLS_KEYWORD_RVREF_QUAL' is deliberately
    // not defined in C++03, as any reasonable implementation of a member
    // function with this qualifier, i.e. one that takes advantage of the fact
    // it has been invoked on an rvalue, has no meaning in C++03.
#endif

namespace bsl {

#ifdef __cpp_lib_optional

using nullopt_t = std::nullopt_t;
using std::nullopt;

// 'in_place_t' doesn't have it's own feature test macro, but it will be
// available with '__cpp_lib_optional' because 'std::optional' uses it.
using in_place_t = std::in_place_t;
using std::in_place;
#else

                        // =========================
                        // class nullopt_t
                        // =========================
struct nullopt_t {
    // 'nullopt_t' is a tag type used to create 'optional' objects in a
    // disengaged state.  It should not be default constructible so the
    // following assignment isn't ambiguous :
    //..
    //   optional<SomeType> o;
    //   o = {};
    //..
    // where 'o' is an 'optional' object.


    // CREATORS
    explicit BSLS_KEYWORD_CONSTEXPR nullopt_t(int) BSLS_KEYWORD_NOEXCEPT;
        // constructor must be explicit to prevent accidental creation of
        // nullopt_t objects.
};

// CREATORS

// bde_verify requires an out of class definition.  It also requires the tag
// variable below not to be extern.  The variable itself requires a constructor
// definition before its own definition.
inline BSLS_KEYWORD_CONSTEXPR nullopt_t::nullopt_t(int) BSLS_KEYWORD_NOEXCEPT
{}

static const BSLS_KEYWORD_CONSTEXPR nullopt_t nullopt = nullopt_t(0);
    // Value of type 'nullopt_t' used as an argument to functions  that take
    // an 'nullopt_t' argument.

                        // =========================
                        // class in_place_t
                        // =========================
struct in_place_t {
    // 'in_place_t' is a tag type that is passed to the constructors of
    // 'optional' to indicate that the contained object should be constructed
    //  in-place.
};

static const BSLS_KEYWORD_CONSTEXPR in_place_t in_place = in_place_t();
    // Value of type 'in_place_t' used as an argument to functions  that take
    // an 'in_place_t' argument.

#endif //__cpp_lib_optional

template <class TYPE, bool USES_BSLMA_ALLOC =
                           BloombergLP::bslma::UsesBslmaAllocator<TYPE>::value>
class optional;

}  // close namespace bsl

namespace BloombergLP {
namespace bslstl {

                        // ============================
                        // class Optional_OptNoSuchType
                        // ============================

struct Optional_OptNoSuchType {
        // Type to distinguish between arguments passed by a user, and an
        // 'enable_if' argument.  It should not be default constructible so
        // the following construction never invokes a constrained single
        // parameter constructor :
        //..
        //   optional<SomeType> o(int, {});
        //..

    // CREATORS
    explicit BSLS_KEYWORD_CONSTEXPR Optional_OptNoSuchType(int)
        BSLS_KEYWORD_NOEXCEPT;
        // constructor must be explicit to prevent accidental creation of
        // Optional_OptNoSuchType objects.
};

// CREATORS

// bde_verify requires an out of class definition.  It also requires the tag
// variable below not to be extern.  The variable itself requires a constructor
// definition before its own definition.
inline BSLS_KEYWORD_CONSTEXPR
Optional_OptNoSuchType::Optional_OptNoSuchType(int) BSLS_KEYWORD_NOEXCEPT
{}

static const BSLS_KEYWORD_CONSTEXPR Optional_OptNoSuchType optNoSuchType =
                                                     Optional_OptNoSuchType(0);
    // Value of type 'Optional_OptNoSuchType' used as the default argument in
    // functions that are constrained using a function argument.

#ifdef BSLS_LIBRARYFEATURES_HAS_CPP11_BASELINE_LIBRARY

#define BSLSTL_OPTIONAL_OR_IS_CONSTRUCTIBLE_V(U,V)                            \
                                        || std::is_constructible<U, V>::value
#define BSLSTL_OPTIONAL_AND_IS_CONSTRUCTIBLE_V(U,V)                           \
                                        && std::is_constructible<U, V>::value
#define BSLSTL_OPTIONAL_AND_IS_ASSIGNABLE_V(U,V)                              \
                                        && std::is_assignable<U, V>::value
#define BSLSTL_OPTIONAL_IS_TRIVIALLY_DESTRUCTIBLE                             \
                                        std::is_trivially_destructible


#else

#define BSLSTL_OPTIONAL_OR_IS_CONSTRUCTIBLE_V(U,V)
#define BSLSTL_OPTIONAL_AND_IS_CONSTRUCTIBLE_V(U,V)
#define BSLSTL_OPTIONAL_AND_IS_ASSIGNABLE_V(U,V)
#define BSLSTL_OPTIONAL_IS_TRIVIALLY_DESTRUCTIBLE bsl::is_trivially_copyable
#endif // BSLS_LIBRARYFEATURES_HAS_CPP11_BASELINE_LIBRARY


// Type traits to assist in choosing the correct assignment and construction
// overload.  If the 'value_type' converts  or assigns from an
// 'optional<other_type>', then the overload passing the function parameter to
// the 'value_type' is preferred.  As in 'std' implementation, if the
// 'value_type' converts or assigns from any value category, we consider it
// convertible/assignable from optional.

#ifdef __cpp_lib_optional
template <class TYPE, class ANY_TYPE>
struct Optional_ConvertsFromStdOptional
: bsl::integral_constant< bool,
    bsl::is_convertible<const std::optional<ANY_TYPE>&, TYPE>::value
    || bsl::is_convertible<std::optional<ANY_TYPE>&, TYPE>::value
    || bsl::is_convertible<const std::optional<ANY_TYPE>, TYPE>::value
    || bsl::is_convertible<std::optional<ANY_TYPE>, TYPE>::value
    || std::is_constructible<TYPE, const std::optional<ANY_TYPE>&>::value
    || std::is_constructible<TYPE, std::optional<ANY_TYPE>&>::value
    || std::is_constructible<TYPE, const std::optional<ANY_TYPE>>::value
    || std::is_constructible<TYPE, std::optional<ANY_TYPE>>::value
    >
{};

template <class TYPE, class ANY_TYPE>
struct Optional_AssignsFromStdOptional
: bsl::integral_constant< bool,
            std::is_assignable<TYPE&, const std::optional<ANY_TYPE>&>::value
            || std::is_assignable<TYPE&, std::optional<ANY_TYPE>&>::value
            || std::is_assignable<TYPE&, const std::optional<ANY_TYPE>>::value
            || std::is_assignable<TYPE&, std::optional<ANY_TYPE>>::value>
{};

#else
  template <class TYPE, class ANY_TYPE>
  struct Optional_ConvertsFromStdOptional
  : bsl::integral_constant< bool, false>
  {};

  template <class TYPE, class ANY_TYPE>
  struct Optional_AssignsFromStdOptional
  : bsl::integral_constant< bool, false>
  {};
#endif // __cpp_lib_optional


template <class TYPE, class ANY_TYPE>
struct Optional_ConvertsFromOptional
: bsl::integral_constant< bool,
    bsl::is_convertible<const bsl::optional<ANY_TYPE>&, TYPE>::value
    || bsl::is_convertible<bsl::optional<ANY_TYPE>&, TYPE>::value
    || bsl::is_convertible<const bsl::optional<ANY_TYPE>, TYPE>::value
    || bsl::is_convertible<bsl::optional<ANY_TYPE>, TYPE>::value
    BSLSTL_OPTIONAL_OR_IS_CONSTRUCTIBLE_V(TYPE, const bsl::optional<ANY_TYPE>&)
    BSLSTL_OPTIONAL_OR_IS_CONSTRUCTIBLE_V(TYPE, bsl::optional<ANY_TYPE>&)
    BSLSTL_OPTIONAL_OR_IS_CONSTRUCTIBLE_V(TYPE, const bsl::optional<ANY_TYPE>)
    BSLSTL_OPTIONAL_OR_IS_CONSTRUCTIBLE_V(TYPE, bsl::optional<ANY_TYPE>)
    || Optional_ConvertsFromStdOptional<TYPE, ANY_TYPE>::value
    >
{};

#ifdef BSLS_LIBRARYFEATURES_HAS_CPP11_BASELINE_LIBRARY
template <class TYPE, class ANY_TYPE>
struct Optional_AssignsFromOptional
: bsl::integral_constant< bool,
            std::is_assignable<TYPE&, const bsl::optional<ANY_TYPE>&>::value
            || std::is_assignable<TYPE&, bsl::optional<ANY_TYPE>&>::value
            || std::is_assignable<TYPE&, const bsl::optional<ANY_TYPE>>::value
            || std::is_assignable<TYPE&, bsl::optional<ANY_TYPE>>::value
            || Optional_AssignsFromStdOptional<TYPE, ANY_TYPE>::value>
{};
#else
//We only use '|| BloombergLP::bslstl::Optional_AssignsFrom' in 'bsl::optional'
//constraints. In order to ignore Optional_AssignsFromOptional trait in C++03,
//we set it to false so it never affects the trait it appears in.
template <class TYPE, class ANY_TYPE>
struct Optional_AssignsFromOptional
: bsl::integral_constant< bool, false>
{};
#endif //BSLS_LIBRARYFEATURES_HAS_CPP11_BASELINE_LIBRARY


// Macros to define common constraints that enable a constructor or assignment
// operator.
#define BSLSTL_OPTIONAL_ENABLE_IF_CONSTRUCT_FROM_OPTIONAL_LVAL  ,             \
    typename bsl::enable_if<                                                  \
                 !bsl::is_same<ANY_TYPE, TYPE>::value  &&                     \
                 !BloombergLP::bslstl::Optional_ConvertsFromOptional          \
                                               <TYPE, ANY_TYPE>::value        \
                 BSLSTL_OPTIONAL_AND_IS_CONSTRUCTIBLE_V(TYPE,const ANY_TYPE&),\
                 BloombergLP::bslstl::Optional_OptNoSuchType>::type           \
                                           = BloombergLP::bslstl::optNoSuchType

#define BSLSTL_OPTIONAL_ENABLE_IF_CONSTRUCT_FROM_OPTIONAL_RVAL  ,             \
    typename bsl::enable_if<                                                  \
                       !bsl::is_same<ANY_TYPE, TYPE>::value  &&               \
                       !BloombergLP::bslstl::Optional_ConvertsFromOptional    \
                                                     <TYPE, ANY_TYPE>::value  \
                       BSLSTL_OPTIONAL_AND_IS_CONSTRUCTIBLE_V(TYPE, ANY_TYPE),\
                       BloombergLP::bslstl::Optional_OptNoSuchType>::type     \
                                         = BloombergLP::bslstl::optNoSuchType

#define BSLSTL_OPTIONAL_ENABLE_IF_CONSTRUCT_FROM_ANYTYPE  ,                   \
    typename bsl::enable_if<                                                  \
                      !bsl::is_same<ANY_TYPE, TYPE>::value &&                 \
                      !bsl::is_same<typename bsl::remove_cv<typename bsl::remove_reference<ANY_TYPE>::type>::type, optional >::value && \
                      !bsl::is_same<ANY_TYPE, bsl::nullopt_t>::value &&       \
                      !bsl::is_same<ANY_TYPE, bsl::in_place_t>::value &&      \
                      !bsl::is_same<ANY_TYPE, bsl::allocator_arg_t>::value    \
                      BSLSTL_OPTIONAL_AND_IS_CONSTRUCTIBLE_V(TYPE, ANY_TYPE), \
                      BloombergLP::bslstl::Optional_OptNoSuchType>::type      \
                                         = BloombergLP::bslstl::optNoSuchType

#define BSLSTL_OPTIONAL_ENABLE_IF_SAME(U,V)  ,                                \
    typename bsl::enable_if<bsl::is_same<V, U>::value,                        \
                            BloombergLP::bslstl::Optional_OptNoSuchType>::type\
                                          = BloombergLP::bslstl::optNoSuchType


#define BSLSTL_OPTIONAL_ENABLE_IF_EXPLICIT_CONSTRUCT(U,V)  ,                  \
    typename bsl::enable_if<!bsl::is_convertible<V, U>::value,                \
                            BloombergLP::bslstl::Optional_OptNoSuchType>::type\
                                         = BloombergLP::bslstl::optNoSuchType

#define BSLSTL_OPTIONAL_ENABLE_IF_NOT_EXPLICIT_CONSTRUCT(U,V)  ,              \
    typename bsl::enable_if<bsl::is_convertible<V, U>::value,                 \
                            BloombergLP::bslstl::Optional_OptNoSuchType>::type\
                                          = BloombergLP::bslstl::optNoSuchType

#define BSLSTL_OPTIONAL_ENABLE_ASSIGN_FROM_OPTIONAL_LVAL                      \
    typename bsl::enable_if<                                                  \
     !BloombergLP::bslstl::Optional_ConvertsFromOptional<TYPE,ANY_TYPE>::value\
     BSLSTL_OPTIONAL_AND_IS_CONSTRUCTIBLE_V(TYPE,const ANY_TYPE &)            \
     BSLSTL_OPTIONAL_AND_IS_ASSIGNABLE_V(TYPE&, ANY_TYPE) &&                  \
     !BloombergLP::bslstl::Optional_AssignsFromOptional<TYPE,ANY_TYPE>::value,\
     optional>::type                                                          \

#define BSLSTL_OPTIONAL_ENABLE_ASSIGN_FROM_OPTIONAL_RVAL                      \
    typename bsl::enable_if<                                                  \
     !BloombergLP::bslstl::Optional_ConvertsFromOptional<TYPE,ANY_TYPE>::value\
     BSLSTL_OPTIONAL_AND_IS_CONSTRUCTIBLE_V(TYPE, ANY_TYPE)                   \
     BSLSTL_OPTIONAL_AND_IS_ASSIGNABLE_V(TYPE&, ANY_TYPE) &&                  \
     !BloombergLP::bslstl::Optional_AssignsFromOptional<TYPE,ANY_TYPE>::value,\
     optional>::type                                                          \

#define BSLSTL_OPTIONAL_ENABLE_ASSIGN_FROM_ANYTYPE                            \
    typename bsl::enable_if<                                                  \
                        !bsl::is_same<ANY_TYPE, optional>::value              \
                        && !(bsl::is_same<ANY_TYPE,                           \
                                     typename bsl::decay<TYPE>::type >::value \
                             && std::is_scalar<TYPE>::value)                  \
                        BSLSTL_OPTIONAL_AND_IS_CONSTRUCTIBLE_V(TYPE, ANY_TYPE)\
                        BSLSTL_OPTIONAL_AND_IS_ASSIGNABLE_V(TYPE&, ANY_TYPE), \
                        optional>::type

#ifdef __cpp_lib_optional

#define BSLSTL_OPTIONAL_ENABLE_IF_CONSTRUCT_FROM_STD_OPTIONAL_LVAL  ,         \
    typename bsl::enable_if<                                                  \
                 !BloombergLP::bslstl::Optional_ConvertsFromOptional          \
                                               <TYPE, ANY_TYPE>::value        \
                 BSLSTL_OPTIONAL_AND_IS_CONSTRUCTIBLE_V(TYPE,const ANY_TYPE&),\
                 BloombergLP::bslstl::Optional_OptNoSuchType>::type           \
                                           = BloombergLP::bslstl::optNoSuchType

#define BSLSTL_OPTIONAL_ENABLE_IF_CONSTRUCT_FROM_STD_OPTIONAL_RVAL  ,         \
    typename bsl::enable_if<                                                  \
                       !BloombergLP::bslstl::Optional_ConvertsFromOptional    \
                                                     <TYPE, ANY_TYPE>::value  \
                       BSLSTL_OPTIONAL_AND_IS_CONSTRUCTIBLE_V(TYPE, ANY_TYPE),\
                       BloombergLP::bslstl::Optional_OptNoSuchType>::type     \
                                         = BloombergLP::bslstl::optNoSuchType

#define BSLSTL_OPTIONAL_ENABLE_ASSIGN_FROM_STD_OPTIONAL_LVAL                  \
    typename bsl::enable_if<                                                  \
     !BloombergLP::bslstl::Optional_ConvertsFromStdOptional<TYPE,ANY_TYPE>::value\
     BSLSTL_OPTIONAL_AND_IS_CONSTRUCTIBLE_V(TYPE,const ANY_TYPE &)            \
     BSLSTL_OPTIONAL_AND_IS_ASSIGNABLE_V(TYPE&, ANY_TYPE) &&                  \
     !BloombergLP::bslstl::Optional_AssignsFromStdOptional<TYPE,ANY_TYPE>::value,\
     optional>::type                                                          \

#define BSLSTL_OPTIONAL_ENABLE_ASSIGN_FROM_STD_OPTIONAL_RVAL                  \
    typename bsl::enable_if<                                                  \
     !BloombergLP::bslstl::Optional_ConvertsFromStdOptional<TYPE,ANY_TYPE>::value\
     BSLSTL_OPTIONAL_AND_IS_CONSTRUCTIBLE_V(TYPE, ANY_TYPE)                   \
     BSLSTL_OPTIONAL_AND_IS_ASSIGNABLE_V(TYPE&, ANY_TYPE) &&                  \
     !BloombergLP::bslstl::Optional_AssignsFromStdOptional<TYPE,ANY_TYPE>::value,\
     optional>::type

#endif //__cpp_lib_optional
                        // ======================
                        // class Optional_DataImp
                        // ======================

template <class TYPE>
struct Optional_DataImp {
    // Component-level class; DO NOT USE

    // Optional_Data is a type used to manage 'value_type' object in
    // 'optional'.  It allows 'optional<TYPE>' to be trivially destructible if
    // 'TYPE' is trivially destructible.  This class also provides an
    // abstraction for const value type.  An 'optional' may contain a 'const'
    // type object.  An assignment to such an 'optional' should not succeed.
    // However, unless the 'optional' itself is 'const', it should be possible
    // to change the value of the 'optional' using 'emplace'.  In order to
    // allow for that, this class manages a non-const object of 'value_type',
    // but all the observers return a 'const' adjusted reference to the managed
    // object.  This functionality is common for all value types and is
    // implemented in the Optional_DataImp base class.  The derived class,
    // 'Optional_Data', is specialised on 'value_type'
    // 'is_trivially_destructible' trait.  The main template is for non
    // trivially destructible types, and it provides a destructor that ensures
    // the value_type destructor is called if 'd_buffer' holds an object.  The
    // specialisation for 'is_trivially_destructible' types does not have a
    // user provided destructor and 'is_trivially_destructible' itself.

  private:
    // PRIVATE TYPES
    typedef typename bsl::remove_const<TYPE>::type StoredType;

    //DATA
    BloombergLP::bsls::ObjectBuffer<StoredType>  d_buffer;
                 // in-place 'TYPE' object
    bool           d_hasValue;       // 'true' if object has value, Otherwise
                 // 'false'
  public:

    // CREATORS
    Optional_DataImp() BSLS_KEYWORD_NOEXCEPT;
        // Creates an empty 'Optional_DataImp' object.

    // MANIPULATORS
#if !BSLS_COMPILERFEATURES_SIMULATE_CPP11_FEATURES
    template<class... ARGS>
    void emplace(bslma::Allocator *allocator,
                 BSLS_COMPILERFEATURES_FORWARD_REF(ARGS)...);
        // Create an object of 'StoredType' in 'd_buffer' using the specified
        // 'allocator' and arguments.

    #if defined(BSLS_COMPILERFEATURES_SUPPORT_GENERALIZED_INITIALIZERS)
    template<class IL_TYPE, class... ARGS>
    void emplace(bslma::Allocator              *allocator,
                 std::initializer_list<IL_TYPE> initializer_list,
                 BSLS_COMPILERFEATURES_FORWARD_REF(ARGS)...);
        // Create an object of 'StoredType' in 'd_buffer' using the specified
        // 'allocator', 'initializer_list', and arguments.

    #endif//BSLS_COMPILERFEATURES_SUPPORT_GENERALIZED_INITIALIZERS
#elif BSLS_COMPILERFEATURES_SIMULATE_VARIADIC_TEMPLATES
// {{{ BEGIN GENERATED CODE
// The following section is automatically generated.  **DO NOT EDIT**
// Generator command line: sim_cpp11_features.pl bslstl_optional.h
#ifndef BSLSTL_OPTIONAL_VARIADIC_LIMIT
#define BSLSTL_OPTIONAL_VARIADIC_LIMIT 10
#endif
#ifndef BSLSTL_OPTIONAL_VARIADIC_LIMIT_A
#define BSLSTL_OPTIONAL_VARIADIC_LIMIT_A BSLSTL_OPTIONAL_VARIADIC_LIMIT
#endif
#if BSLSTL_OPTIONAL_VARIADIC_LIMIT_A >= 0
    void emplace(bslma::Allocator *allocator);
#endif  // BSLSTL_OPTIONAL_VARIADIC_LIMIT_A >= 0

#if BSLSTL_OPTIONAL_VARIADIC_LIMIT_A >= 1
    template<class ARGS_01>
    void emplace(bslma::Allocator *allocator,
                 BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_01));
#endif  // BSLSTL_OPTIONAL_VARIADIC_LIMIT_A >= 1

#if BSLSTL_OPTIONAL_VARIADIC_LIMIT_A >= 2
    template<class ARGS_01,
             class ARGS_02>
    void emplace(bslma::Allocator *allocator,
                 BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_01),
                 BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_02));
#endif  // BSLSTL_OPTIONAL_VARIADIC_LIMIT_A >= 2

#if BSLSTL_OPTIONAL_VARIADIC_LIMIT_A >= 3
    template<class ARGS_01,
             class ARGS_02,
             class ARGS_03>
    void emplace(bslma::Allocator *allocator,
                 BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_01),
                 BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_02),
                 BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_03));
#endif  // BSLSTL_OPTIONAL_VARIADIC_LIMIT_A >= 3

#if BSLSTL_OPTIONAL_VARIADIC_LIMIT_A >= 4
    template<class ARGS_01,
             class ARGS_02,
             class ARGS_03,
             class ARGS_04>
    void emplace(bslma::Allocator *allocator,
                 BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_01),
                 BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_02),
                 BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_03),
                 BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_04));
#endif  // BSLSTL_OPTIONAL_VARIADIC_LIMIT_A >= 4

#if BSLSTL_OPTIONAL_VARIADIC_LIMIT_A >= 5
    template<class ARGS_01,
             class ARGS_02,
             class ARGS_03,
             class ARGS_04,
             class ARGS_05>
    void emplace(bslma::Allocator *allocator,
                 BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_01),
                 BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_02),
                 BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_03),
                 BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_04),
                 BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_05));
#endif  // BSLSTL_OPTIONAL_VARIADIC_LIMIT_A >= 5

#if BSLSTL_OPTIONAL_VARIADIC_LIMIT_A >= 6
    template<class ARGS_01,
             class ARGS_02,
             class ARGS_03,
             class ARGS_04,
             class ARGS_05,
             class ARGS_06>
    void emplace(bslma::Allocator *allocator,
                 BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_01),
                 BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_02),
                 BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_03),
                 BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_04),
                 BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_05),
                 BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_06));
#endif  // BSLSTL_OPTIONAL_VARIADIC_LIMIT_A >= 6

#if BSLSTL_OPTIONAL_VARIADIC_LIMIT_A >= 7
    template<class ARGS_01,
             class ARGS_02,
             class ARGS_03,
             class ARGS_04,
             class ARGS_05,
             class ARGS_06,
             class ARGS_07>
    void emplace(bslma::Allocator *allocator,
                 BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_01),
                 BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_02),
                 BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_03),
                 BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_04),
                 BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_05),
                 BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_06),
                 BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_07));
#endif  // BSLSTL_OPTIONAL_VARIADIC_LIMIT_A >= 7

#if BSLSTL_OPTIONAL_VARIADIC_LIMIT_A >= 8
    template<class ARGS_01,
             class ARGS_02,
             class ARGS_03,
             class ARGS_04,
             class ARGS_05,
             class ARGS_06,
             class ARGS_07,
             class ARGS_08>
    void emplace(bslma::Allocator *allocator,
                 BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_01),
                 BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_02),
                 BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_03),
                 BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_04),
                 BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_05),
                 BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_06),
                 BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_07),
                 BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_08));
#endif  // BSLSTL_OPTIONAL_VARIADIC_LIMIT_A >= 8

#if BSLSTL_OPTIONAL_VARIADIC_LIMIT_A >= 9
    template<class ARGS_01,
             class ARGS_02,
             class ARGS_03,
             class ARGS_04,
             class ARGS_05,
             class ARGS_06,
             class ARGS_07,
             class ARGS_08,
             class ARGS_09>
    void emplace(bslma::Allocator *allocator,
                 BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_01),
                 BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_02),
                 BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_03),
                 BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_04),
                 BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_05),
                 BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_06),
                 BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_07),
                 BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_08),
                 BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_09));
#endif  // BSLSTL_OPTIONAL_VARIADIC_LIMIT_A >= 9

#if BSLSTL_OPTIONAL_VARIADIC_LIMIT_A >= 10
    template<class ARGS_01,
             class ARGS_02,
             class ARGS_03,
             class ARGS_04,
             class ARGS_05,
             class ARGS_06,
             class ARGS_07,
             class ARGS_08,
             class ARGS_09,
             class ARGS_10>
    void emplace(bslma::Allocator *allocator,
                 BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_01),
                 BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_02),
                 BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_03),
                 BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_04),
                 BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_05),
                 BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_06),
                 BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_07),
                 BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_08),
                 BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_09),
                 BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_10));
#endif  // BSLSTL_OPTIONAL_VARIADIC_LIMIT_A >= 10


    #if defined(BSLS_COMPILERFEATURES_SUPPORT_GENERALIZED_INITIALIZERS)
#if BSLSTL_OPTIONAL_VARIADIC_LIMIT_A >= 0
    template<class IL_TYPE>
    void emplace(bslma::Allocator              *allocator,
                 std::initializer_list<IL_TYPE> initializer_list);
#endif  // BSLSTL_OPTIONAL_VARIADIC_LIMIT_A >= 0

#if BSLSTL_OPTIONAL_VARIADIC_LIMIT_A >= 1
    template<class IL_TYPE, class ARGS_01>
    void emplace(bslma::Allocator              *allocator,
                 std::initializer_list<IL_TYPE> initializer_list,
                 BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_01));
#endif  // BSLSTL_OPTIONAL_VARIADIC_LIMIT_A >= 1

#if BSLSTL_OPTIONAL_VARIADIC_LIMIT_A >= 2
    template<class IL_TYPE, class ARGS_01,
                            class ARGS_02>
    void emplace(bslma::Allocator              *allocator,
                 std::initializer_list<IL_TYPE> initializer_list,
                 BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_01),
                 BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_02));
#endif  // BSLSTL_OPTIONAL_VARIADIC_LIMIT_A >= 2

#if BSLSTL_OPTIONAL_VARIADIC_LIMIT_A >= 3
    template<class IL_TYPE, class ARGS_01,
                            class ARGS_02,
                            class ARGS_03>
    void emplace(bslma::Allocator              *allocator,
                 std::initializer_list<IL_TYPE> initializer_list,
                 BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_01),
                 BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_02),
                 BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_03));
#endif  // BSLSTL_OPTIONAL_VARIADIC_LIMIT_A >= 3

#if BSLSTL_OPTIONAL_VARIADIC_LIMIT_A >= 4
    template<class IL_TYPE, class ARGS_01,
                            class ARGS_02,
                            class ARGS_03,
                            class ARGS_04>
    void emplace(bslma::Allocator              *allocator,
                 std::initializer_list<IL_TYPE> initializer_list,
                 BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_01),
                 BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_02),
                 BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_03),
                 BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_04));
#endif  // BSLSTL_OPTIONAL_VARIADIC_LIMIT_A >= 4

#if BSLSTL_OPTIONAL_VARIADIC_LIMIT_A >= 5
    template<class IL_TYPE, class ARGS_01,
                            class ARGS_02,
                            class ARGS_03,
                            class ARGS_04,
                            class ARGS_05>
    void emplace(bslma::Allocator              *allocator,
                 std::initializer_list<IL_TYPE> initializer_list,
                 BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_01),
                 BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_02),
                 BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_03),
                 BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_04),
                 BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_05));
#endif  // BSLSTL_OPTIONAL_VARIADIC_LIMIT_A >= 5

#if BSLSTL_OPTIONAL_VARIADIC_LIMIT_A >= 6
    template<class IL_TYPE, class ARGS_01,
                            class ARGS_02,
                            class ARGS_03,
                            class ARGS_04,
                            class ARGS_05,
                            class ARGS_06>
    void emplace(bslma::Allocator              *allocator,
                 std::initializer_list<IL_TYPE> initializer_list,
                 BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_01),
                 BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_02),
                 BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_03),
                 BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_04),
                 BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_05),
                 BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_06));
#endif  // BSLSTL_OPTIONAL_VARIADIC_LIMIT_A >= 6

#if BSLSTL_OPTIONAL_VARIADIC_LIMIT_A >= 7
    template<class IL_TYPE, class ARGS_01,
                            class ARGS_02,
                            class ARGS_03,
                            class ARGS_04,
                            class ARGS_05,
                            class ARGS_06,
                            class ARGS_07>
    void emplace(bslma::Allocator              *allocator,
                 std::initializer_list<IL_TYPE> initializer_list,
                 BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_01),
                 BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_02),
                 BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_03),
                 BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_04),
                 BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_05),
                 BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_06),
                 BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_07));
#endif  // BSLSTL_OPTIONAL_VARIADIC_LIMIT_A >= 7

#if BSLSTL_OPTIONAL_VARIADIC_LIMIT_A >= 8
    template<class IL_TYPE, class ARGS_01,
                            class ARGS_02,
                            class ARGS_03,
                            class ARGS_04,
                            class ARGS_05,
                            class ARGS_06,
                            class ARGS_07,
                            class ARGS_08>
    void emplace(bslma::Allocator              *allocator,
                 std::initializer_list<IL_TYPE> initializer_list,
                 BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_01),
                 BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_02),
                 BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_03),
                 BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_04),
                 BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_05),
                 BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_06),
                 BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_07),
                 BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_08));
#endif  // BSLSTL_OPTIONAL_VARIADIC_LIMIT_A >= 8

#if BSLSTL_OPTIONAL_VARIADIC_LIMIT_A >= 9
    template<class IL_TYPE, class ARGS_01,
                            class ARGS_02,
                            class ARGS_03,
                            class ARGS_04,
                            class ARGS_05,
                            class ARGS_06,
                            class ARGS_07,
                            class ARGS_08,
                            class ARGS_09>
    void emplace(bslma::Allocator              *allocator,
                 std::initializer_list<IL_TYPE> initializer_list,
                 BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_01),
                 BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_02),
                 BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_03),
                 BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_04),
                 BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_05),
                 BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_06),
                 BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_07),
                 BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_08),
                 BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_09));
#endif  // BSLSTL_OPTIONAL_VARIADIC_LIMIT_A >= 9

#if BSLSTL_OPTIONAL_VARIADIC_LIMIT_A >= 10
    template<class IL_TYPE, class ARGS_01,
                            class ARGS_02,
                            class ARGS_03,
                            class ARGS_04,
                            class ARGS_05,
                            class ARGS_06,
                            class ARGS_07,
                            class ARGS_08,
                            class ARGS_09,
                            class ARGS_10>
    void emplace(bslma::Allocator              *allocator,
                 std::initializer_list<IL_TYPE> initializer_list,
                 BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_01),
                 BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_02),
                 BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_03),
                 BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_04),
                 BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_05),
                 BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_06),
                 BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_07),
                 BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_08),
                 BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_09),
                 BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_10));
#endif  // BSLSTL_OPTIONAL_VARIADIC_LIMIT_A >= 10


    #endif
#else
// The generated code below is a workaround for the absence of perfect
// forwarding in some compilers.
    template<class... ARGS>
    void emplace(bslma::Allocator *allocator,
                 BSLS_COMPILERFEATURES_FORWARD_REF(ARGS)...);

    #if defined(BSLS_COMPILERFEATURES_SUPPORT_GENERALIZED_INITIALIZERS)
    template<class IL_TYPE, class... ARGS>
    void emplace(bslma::Allocator              *allocator,
                 std::initializer_list<IL_TYPE> initializer_list,
                 BSLS_COMPILERFEATURES_FORWARD_REF(ARGS)...);

    #endif
// }}} END GENERATED CODE
#endif

    void reset() BSLS_KEYWORD_NOEXCEPT;
        // Destroy the 'value_type' object in 'd_buffer', if any.

    TYPE& value() BSLS_KEYWORD_LVREF_QUAL;
        // Return the 'value_type' object in 'd_buffer' with const
        // qualification adjusted to match that of 'TYPE'.  The behavior is
        // undefined unless 'this->hasValue() == true'

#if defined(BSLS_COMPILERFEATURES_SUPPORT_REF_QUALIFIERS)
    TYPE&& value() BSLS_KEYWORD_RVREF_QUAL;
        // Return the 'value_type' object in 'd_buffer' with const
        // qualification adjusted to match that of 'TYPE'.  The behavior is
        // undefined unless 'this->hasValue() == true'
#endif // BSLS_COMPILERFEATURES_SUPPORT_REF_QUALIFIERS

    //ACCESSORS
    bool hasValue() const BSLS_KEYWORD_NOEXCEPT;
        // return 'true' if there is a value in 'd_buffer', and 'false'
        //otherwise.

    const TYPE& value() const BSLS_KEYWORD_LVREF_QUAL;
        // Return the 'value_type' object in 'd_buffer' with const
        // qualification adjusted to match that of 'TYPE'.  The behavior is
        // undefined unless 'this->hasValue() == true'

#if defined(BSLS_COMPILERFEATURES_SUPPORT_REF_QUALIFIERS)
    const TYPE&& value() const BSLS_KEYWORD_RVREF_QUAL;
        // Return the 'value_type' object in 'd_buffer' with const
        // qualification adjusted to match that of 'TYPE'.  The behavior is
        // undefined unless 'this->hasValue() == true'
#endif // BSLS_COMPILERFEATURES_SUPPORT_REF_QUALIFIERS

};

template <class TYPE>
Optional_DataImp<TYPE>::Optional_DataImp()
BSLS_KEYWORD_NOEXCEPT
: d_hasValue(false)
{}

#if !BSLS_COMPILERFEATURES_SIMULATE_CPP11_FEATURES
template <class TYPE>
template <class... ARGS>
inline
void Optional_DataImp<TYPE>::emplace(
                          bslma::Allocator                          *allocator,
                          BSLS_COMPILERFEATURES_FORWARD_REF(ARGS)... args)
{
    reset();
    BloombergLP::bslma::ConstructionUtil::construct(
                                  d_buffer.address(),
                                  allocator,
                                  BSLS_COMPILERFEATURES_FORWARD(ARGS, args)...
                                  );
    d_hasValue = true;
}

#if defined(BSLS_COMPILERFEATURES_SUPPORT_GENERALIZED_INITIALIZERS)
template <class TYPE>
template<class IL_TYPE, class... ARGS>
void Optional_DataImp<TYPE>::emplace(
                          bslma::Allocator                          *allocator,
                          std::initializer_list<IL_TYPE>             il,
                          BSLS_COMPILERFEATURES_FORWARD_REF(ARGS)... args)
{
    reset();
    BloombergLP::bslma::ConstructionUtil::construct(
                                  d_buffer.address(),
                                  allocator,
                                  il,
                                  BSLS_COMPILERFEATURES_FORWARD(ARGS, args)...
                                  );
    d_hasValue = true;
}
#endif//BSLS_COMPILERFEATURES_SUPPORT_GENERALIZED_INITIALIZERS
#elif BSLS_COMPILERFEATURES_SIMULATE_VARIADIC_TEMPLATES
// {{{ BEGIN GENERATED CODE
// The following section is automatically generated.  **DO NOT EDIT**
// Generator command line: sim_cpp11_features.pl bslstl_optional.h
#ifndef BSLSTL_OPTIONAL_VARIADIC_LIMIT
#define BSLSTL_OPTIONAL_VARIADIC_LIMIT 10
#endif
#ifndef BSLSTL_OPTIONAL_VARIADIC_LIMIT_B
#define BSLSTL_OPTIONAL_VARIADIC_LIMIT_B BSLSTL_OPTIONAL_VARIADIC_LIMIT
#endif
#if BSLSTL_OPTIONAL_VARIADIC_LIMIT_B >= 0
template <class TYPE>
inline
void Optional_DataImp<TYPE>::emplace(
                          bslma::Allocator                          *allocator)
{
    reset();
    BloombergLP::bslma::ConstructionUtil::construct(
                                  d_buffer.address(),
                                  allocator
                                  );
    d_hasValue = true;
}
#endif  // BSLSTL_OPTIONAL_VARIADIC_LIMIT_B >= 0

#if BSLSTL_OPTIONAL_VARIADIC_LIMIT_B >= 1
template <class TYPE>
template <class ARGS_01>
inline
void Optional_DataImp<TYPE>::emplace(
                          bslma::Allocator                          *allocator,
                          BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_01) args_01)
{
    reset();
    BloombergLP::bslma::ConstructionUtil::construct(
                                  d_buffer.address(),
                                  allocator,
                                BSLS_COMPILERFEATURES_FORWARD(ARGS_01, args_01)
                                  );
    d_hasValue = true;
}
#endif  // BSLSTL_OPTIONAL_VARIADIC_LIMIT_B >= 1

#if BSLSTL_OPTIONAL_VARIADIC_LIMIT_B >= 2
template <class TYPE>
template <class ARGS_01,
          class ARGS_02>
inline
void Optional_DataImp<TYPE>::emplace(
                          bslma::Allocator                          *allocator,
                          BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_01) args_01,
                          BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_02) args_02)
{
    reset();
    BloombergLP::bslma::ConstructionUtil::construct(
                                  d_buffer.address(),
                                  allocator,
                               BSLS_COMPILERFEATURES_FORWARD(ARGS_01, args_01),
                               BSLS_COMPILERFEATURES_FORWARD(ARGS_02, args_02)
                                  );
    d_hasValue = true;
}
#endif  // BSLSTL_OPTIONAL_VARIADIC_LIMIT_B >= 2

#if BSLSTL_OPTIONAL_VARIADIC_LIMIT_B >= 3
template <class TYPE>
template <class ARGS_01,
          class ARGS_02,
          class ARGS_03>
inline
void Optional_DataImp<TYPE>::emplace(
                          bslma::Allocator                          *allocator,
                          BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_01) args_01,
                          BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_02) args_02,
                          BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_03) args_03)
{
    reset();
    BloombergLP::bslma::ConstructionUtil::construct(
                                  d_buffer.address(),
                                  allocator,
                               BSLS_COMPILERFEATURES_FORWARD(ARGS_01, args_01),
                               BSLS_COMPILERFEATURES_FORWARD(ARGS_02, args_02),
                               BSLS_COMPILERFEATURES_FORWARD(ARGS_03, args_03)
                                  );
    d_hasValue = true;
}
#endif  // BSLSTL_OPTIONAL_VARIADIC_LIMIT_B >= 3

#if BSLSTL_OPTIONAL_VARIADIC_LIMIT_B >= 4
template <class TYPE>
template <class ARGS_01,
          class ARGS_02,
          class ARGS_03,
          class ARGS_04>
inline
void Optional_DataImp<TYPE>::emplace(
                          bslma::Allocator                          *allocator,
                          BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_01) args_01,
                          BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_02) args_02,
                          BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_03) args_03,
                          BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_04) args_04)
{
    reset();
    BloombergLP::bslma::ConstructionUtil::construct(
                                  d_buffer.address(),
                                  allocator,
                               BSLS_COMPILERFEATURES_FORWARD(ARGS_01, args_01),
                               BSLS_COMPILERFEATURES_FORWARD(ARGS_02, args_02),
                               BSLS_COMPILERFEATURES_FORWARD(ARGS_03, args_03),
                               BSLS_COMPILERFEATURES_FORWARD(ARGS_04, args_04)
                                  );
    d_hasValue = true;
}
#endif  // BSLSTL_OPTIONAL_VARIADIC_LIMIT_B >= 4

#if BSLSTL_OPTIONAL_VARIADIC_LIMIT_B >= 5
template <class TYPE>
template <class ARGS_01,
          class ARGS_02,
          class ARGS_03,
          class ARGS_04,
          class ARGS_05>
inline
void Optional_DataImp<TYPE>::emplace(
                          bslma::Allocator                          *allocator,
                          BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_01) args_01,
                          BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_02) args_02,
                          BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_03) args_03,
                          BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_04) args_04,
                          BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_05) args_05)
{
    reset();
    BloombergLP::bslma::ConstructionUtil::construct(
                                  d_buffer.address(),
                                  allocator,
                               BSLS_COMPILERFEATURES_FORWARD(ARGS_01, args_01),
                               BSLS_COMPILERFEATURES_FORWARD(ARGS_02, args_02),
                               BSLS_COMPILERFEATURES_FORWARD(ARGS_03, args_03),
                               BSLS_COMPILERFEATURES_FORWARD(ARGS_04, args_04),
                               BSLS_COMPILERFEATURES_FORWARD(ARGS_05, args_05)
                                  );
    d_hasValue = true;
}
#endif  // BSLSTL_OPTIONAL_VARIADIC_LIMIT_B >= 5

#if BSLSTL_OPTIONAL_VARIADIC_LIMIT_B >= 6
template <class TYPE>
template <class ARGS_01,
          class ARGS_02,
          class ARGS_03,
          class ARGS_04,
          class ARGS_05,
          class ARGS_06>
inline
void Optional_DataImp<TYPE>::emplace(
                          bslma::Allocator                          *allocator,
                          BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_01) args_01,
                          BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_02) args_02,
                          BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_03) args_03,
                          BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_04) args_04,
                          BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_05) args_05,
                          BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_06) args_06)
{
    reset();
    BloombergLP::bslma::ConstructionUtil::construct(
                                  d_buffer.address(),
                                  allocator,
                               BSLS_COMPILERFEATURES_FORWARD(ARGS_01, args_01),
                               BSLS_COMPILERFEATURES_FORWARD(ARGS_02, args_02),
                               BSLS_COMPILERFEATURES_FORWARD(ARGS_03, args_03),
                               BSLS_COMPILERFEATURES_FORWARD(ARGS_04, args_04),
                               BSLS_COMPILERFEATURES_FORWARD(ARGS_05, args_05),
                               BSLS_COMPILERFEATURES_FORWARD(ARGS_06, args_06)
                                  );
    d_hasValue = true;
}
#endif  // BSLSTL_OPTIONAL_VARIADIC_LIMIT_B >= 6

#if BSLSTL_OPTIONAL_VARIADIC_LIMIT_B >= 7
template <class TYPE>
template <class ARGS_01,
          class ARGS_02,
          class ARGS_03,
          class ARGS_04,
          class ARGS_05,
          class ARGS_06,
          class ARGS_07>
inline
void Optional_DataImp<TYPE>::emplace(
                          bslma::Allocator                          *allocator,
                          BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_01) args_01,
                          BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_02) args_02,
                          BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_03) args_03,
                          BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_04) args_04,
                          BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_05) args_05,
                          BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_06) args_06,
                          BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_07) args_07)
{
    reset();
    BloombergLP::bslma::ConstructionUtil::construct(
                                  d_buffer.address(),
                                  allocator,
                               BSLS_COMPILERFEATURES_FORWARD(ARGS_01, args_01),
                               BSLS_COMPILERFEATURES_FORWARD(ARGS_02, args_02),
                               BSLS_COMPILERFEATURES_FORWARD(ARGS_03, args_03),
                               BSLS_COMPILERFEATURES_FORWARD(ARGS_04, args_04),
                               BSLS_COMPILERFEATURES_FORWARD(ARGS_05, args_05),
                               BSLS_COMPILERFEATURES_FORWARD(ARGS_06, args_06),
                               BSLS_COMPILERFEATURES_FORWARD(ARGS_07, args_07)
                                  );
    d_hasValue = true;
}
#endif  // BSLSTL_OPTIONAL_VARIADIC_LIMIT_B >= 7

#if BSLSTL_OPTIONAL_VARIADIC_LIMIT_B >= 8
template <class TYPE>
template <class ARGS_01,
          class ARGS_02,
          class ARGS_03,
          class ARGS_04,
          class ARGS_05,
          class ARGS_06,
          class ARGS_07,
          class ARGS_08>
inline
void Optional_DataImp<TYPE>::emplace(
                          bslma::Allocator                          *allocator,
                          BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_01) args_01,
                          BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_02) args_02,
                          BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_03) args_03,
                          BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_04) args_04,
                          BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_05) args_05,
                          BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_06) args_06,
                          BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_07) args_07,
                          BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_08) args_08)
{
    reset();
    BloombergLP::bslma::ConstructionUtil::construct(
                                  d_buffer.address(),
                                  allocator,
                               BSLS_COMPILERFEATURES_FORWARD(ARGS_01, args_01),
                               BSLS_COMPILERFEATURES_FORWARD(ARGS_02, args_02),
                               BSLS_COMPILERFEATURES_FORWARD(ARGS_03, args_03),
                               BSLS_COMPILERFEATURES_FORWARD(ARGS_04, args_04),
                               BSLS_COMPILERFEATURES_FORWARD(ARGS_05, args_05),
                               BSLS_COMPILERFEATURES_FORWARD(ARGS_06, args_06),
                               BSLS_COMPILERFEATURES_FORWARD(ARGS_07, args_07),
                               BSLS_COMPILERFEATURES_FORWARD(ARGS_08, args_08)
                                  );
    d_hasValue = true;
}
#endif  // BSLSTL_OPTIONAL_VARIADIC_LIMIT_B >= 8

#if BSLSTL_OPTIONAL_VARIADIC_LIMIT_B >= 9
template <class TYPE>
template <class ARGS_01,
          class ARGS_02,
          class ARGS_03,
          class ARGS_04,
          class ARGS_05,
          class ARGS_06,
          class ARGS_07,
          class ARGS_08,
          class ARGS_09>
inline
void Optional_DataImp<TYPE>::emplace(
                          bslma::Allocator                          *allocator,
                          BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_01) args_01,
                          BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_02) args_02,
                          BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_03) args_03,
                          BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_04) args_04,
                          BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_05) args_05,
                          BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_06) args_06,
                          BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_07) args_07,
                          BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_08) args_08,
                          BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_09) args_09)
{
    reset();
    BloombergLP::bslma::ConstructionUtil::construct(
                                  d_buffer.address(),
                                  allocator,
                               BSLS_COMPILERFEATURES_FORWARD(ARGS_01, args_01),
                               BSLS_COMPILERFEATURES_FORWARD(ARGS_02, args_02),
                               BSLS_COMPILERFEATURES_FORWARD(ARGS_03, args_03),
                               BSLS_COMPILERFEATURES_FORWARD(ARGS_04, args_04),
                               BSLS_COMPILERFEATURES_FORWARD(ARGS_05, args_05),
                               BSLS_COMPILERFEATURES_FORWARD(ARGS_06, args_06),
                               BSLS_COMPILERFEATURES_FORWARD(ARGS_07, args_07),
                               BSLS_COMPILERFEATURES_FORWARD(ARGS_08, args_08),
                               BSLS_COMPILERFEATURES_FORWARD(ARGS_09, args_09)
                                  );
    d_hasValue = true;
}
#endif  // BSLSTL_OPTIONAL_VARIADIC_LIMIT_B >= 9

#if BSLSTL_OPTIONAL_VARIADIC_LIMIT_B >= 10
template <class TYPE>
template <class ARGS_01,
          class ARGS_02,
          class ARGS_03,
          class ARGS_04,
          class ARGS_05,
          class ARGS_06,
          class ARGS_07,
          class ARGS_08,
          class ARGS_09,
          class ARGS_10>
inline
void Optional_DataImp<TYPE>::emplace(
                          bslma::Allocator                          *allocator,
                          BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_01) args_01,
                          BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_02) args_02,
                          BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_03) args_03,
                          BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_04) args_04,
                          BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_05) args_05,
                          BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_06) args_06,
                          BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_07) args_07,
                          BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_08) args_08,
                          BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_09) args_09,
                          BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_10) args_10)
{
    reset();
    BloombergLP::bslma::ConstructionUtil::construct(
                                  d_buffer.address(),
                                  allocator,
                               BSLS_COMPILERFEATURES_FORWARD(ARGS_01, args_01),
                               BSLS_COMPILERFEATURES_FORWARD(ARGS_02, args_02),
                               BSLS_COMPILERFEATURES_FORWARD(ARGS_03, args_03),
                               BSLS_COMPILERFEATURES_FORWARD(ARGS_04, args_04),
                               BSLS_COMPILERFEATURES_FORWARD(ARGS_05, args_05),
                               BSLS_COMPILERFEATURES_FORWARD(ARGS_06, args_06),
                               BSLS_COMPILERFEATURES_FORWARD(ARGS_07, args_07),
                               BSLS_COMPILERFEATURES_FORWARD(ARGS_08, args_08),
                               BSLS_COMPILERFEATURES_FORWARD(ARGS_09, args_09),
                               BSLS_COMPILERFEATURES_FORWARD(ARGS_10, args_10)
                                  );
    d_hasValue = true;
}
#endif  // BSLSTL_OPTIONAL_VARIADIC_LIMIT_B >= 10


#if defined(BSLS_COMPILERFEATURES_SUPPORT_GENERALIZED_INITIALIZERS)
#if BSLSTL_OPTIONAL_VARIADIC_LIMIT_B >= 0
template <class TYPE>
template<class IL_TYPE>
void Optional_DataImp<TYPE>::emplace(
                          bslma::Allocator                          *allocator,
                          std::initializer_list<IL_TYPE>             il)
{
    reset();
    BloombergLP::bslma::ConstructionUtil::construct(
                                  d_buffer.address(),
                                  allocator,
                                  il
                                  );
    d_hasValue = true;
}
#endif  // BSLSTL_OPTIONAL_VARIADIC_LIMIT_B >= 0

#if BSLSTL_OPTIONAL_VARIADIC_LIMIT_B >= 1
template <class TYPE>
template<class IL_TYPE, class ARGS_01>
void Optional_DataImp<TYPE>::emplace(
                          bslma::Allocator                          *allocator,
                          std::initializer_list<IL_TYPE>             il,
                          BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_01) args_01)
{
    reset();
    BloombergLP::bslma::ConstructionUtil::construct(
                                  d_buffer.address(),
                                  allocator,
                                  il,
                                BSLS_COMPILERFEATURES_FORWARD(ARGS_01, args_01)
                                  );
    d_hasValue = true;
}
#endif  // BSLSTL_OPTIONAL_VARIADIC_LIMIT_B >= 1

#if BSLSTL_OPTIONAL_VARIADIC_LIMIT_B >= 2
template <class TYPE>
template<class IL_TYPE, class ARGS_01,
                        class ARGS_02>
void Optional_DataImp<TYPE>::emplace(
                          bslma::Allocator                          *allocator,
                          std::initializer_list<IL_TYPE>             il,
                          BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_01) args_01,
                          BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_02) args_02)
{
    reset();
    BloombergLP::bslma::ConstructionUtil::construct(
                                  d_buffer.address(),
                                  allocator,
                                  il,
                               BSLS_COMPILERFEATURES_FORWARD(ARGS_01, args_01),
                               BSLS_COMPILERFEATURES_FORWARD(ARGS_02, args_02)
                                  );
    d_hasValue = true;
}
#endif  // BSLSTL_OPTIONAL_VARIADIC_LIMIT_B >= 2

#if BSLSTL_OPTIONAL_VARIADIC_LIMIT_B >= 3
template <class TYPE>
template<class IL_TYPE, class ARGS_01,
                        class ARGS_02,
                        class ARGS_03>
void Optional_DataImp<TYPE>::emplace(
                          bslma::Allocator                          *allocator,
                          std::initializer_list<IL_TYPE>             il,
                          BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_01) args_01,
                          BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_02) args_02,
                          BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_03) args_03)
{
    reset();
    BloombergLP::bslma::ConstructionUtil::construct(
                                  d_buffer.address(),
                                  allocator,
                                  il,
                               BSLS_COMPILERFEATURES_FORWARD(ARGS_01, args_01),
                               BSLS_COMPILERFEATURES_FORWARD(ARGS_02, args_02),
                               BSLS_COMPILERFEATURES_FORWARD(ARGS_03, args_03)
                                  );
    d_hasValue = true;
}
#endif  // BSLSTL_OPTIONAL_VARIADIC_LIMIT_B >= 3

#if BSLSTL_OPTIONAL_VARIADIC_LIMIT_B >= 4
template <class TYPE>
template<class IL_TYPE, class ARGS_01,
                        class ARGS_02,
                        class ARGS_03,
                        class ARGS_04>
void Optional_DataImp<TYPE>::emplace(
                          bslma::Allocator                          *allocator,
                          std::initializer_list<IL_TYPE>             il,
                          BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_01) args_01,
                          BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_02) args_02,
                          BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_03) args_03,
                          BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_04) args_04)
{
    reset();
    BloombergLP::bslma::ConstructionUtil::construct(
                                  d_buffer.address(),
                                  allocator,
                                  il,
                               BSLS_COMPILERFEATURES_FORWARD(ARGS_01, args_01),
                               BSLS_COMPILERFEATURES_FORWARD(ARGS_02, args_02),
                               BSLS_COMPILERFEATURES_FORWARD(ARGS_03, args_03),
                               BSLS_COMPILERFEATURES_FORWARD(ARGS_04, args_04)
                                  );
    d_hasValue = true;
}
#endif  // BSLSTL_OPTIONAL_VARIADIC_LIMIT_B >= 4

#if BSLSTL_OPTIONAL_VARIADIC_LIMIT_B >= 5
template <class TYPE>
template<class IL_TYPE, class ARGS_01,
                        class ARGS_02,
                        class ARGS_03,
                        class ARGS_04,
                        class ARGS_05>
void Optional_DataImp<TYPE>::emplace(
                          bslma::Allocator                          *allocator,
                          std::initializer_list<IL_TYPE>             il,
                          BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_01) args_01,
                          BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_02) args_02,
                          BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_03) args_03,
                          BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_04) args_04,
                          BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_05) args_05)
{
    reset();
    BloombergLP::bslma::ConstructionUtil::construct(
                                  d_buffer.address(),
                                  allocator,
                                  il,
                               BSLS_COMPILERFEATURES_FORWARD(ARGS_01, args_01),
                               BSLS_COMPILERFEATURES_FORWARD(ARGS_02, args_02),
                               BSLS_COMPILERFEATURES_FORWARD(ARGS_03, args_03),
                               BSLS_COMPILERFEATURES_FORWARD(ARGS_04, args_04),
                               BSLS_COMPILERFEATURES_FORWARD(ARGS_05, args_05)
                                  );
    d_hasValue = true;
}
#endif  // BSLSTL_OPTIONAL_VARIADIC_LIMIT_B >= 5

#if BSLSTL_OPTIONAL_VARIADIC_LIMIT_B >= 6
template <class TYPE>
template<class IL_TYPE, class ARGS_01,
                        class ARGS_02,
                        class ARGS_03,
                        class ARGS_04,
                        class ARGS_05,
                        class ARGS_06>
void Optional_DataImp<TYPE>::emplace(
                          bslma::Allocator                          *allocator,
                          std::initializer_list<IL_TYPE>             il,
                          BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_01) args_01,
                          BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_02) args_02,
                          BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_03) args_03,
                          BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_04) args_04,
                          BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_05) args_05,
                          BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_06) args_06)
{
    reset();
    BloombergLP::bslma::ConstructionUtil::construct(
                                  d_buffer.address(),
                                  allocator,
                                  il,
                               BSLS_COMPILERFEATURES_FORWARD(ARGS_01, args_01),
                               BSLS_COMPILERFEATURES_FORWARD(ARGS_02, args_02),
                               BSLS_COMPILERFEATURES_FORWARD(ARGS_03, args_03),
                               BSLS_COMPILERFEATURES_FORWARD(ARGS_04, args_04),
                               BSLS_COMPILERFEATURES_FORWARD(ARGS_05, args_05),
                               BSLS_COMPILERFEATURES_FORWARD(ARGS_06, args_06)
                                  );
    d_hasValue = true;
}
#endif  // BSLSTL_OPTIONAL_VARIADIC_LIMIT_B >= 6

#if BSLSTL_OPTIONAL_VARIADIC_LIMIT_B >= 7
template <class TYPE>
template<class IL_TYPE, class ARGS_01,
                        class ARGS_02,
                        class ARGS_03,
                        class ARGS_04,
                        class ARGS_05,
                        class ARGS_06,
                        class ARGS_07>
void Optional_DataImp<TYPE>::emplace(
                          bslma::Allocator                          *allocator,
                          std::initializer_list<IL_TYPE>             il,
                          BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_01) args_01,
                          BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_02) args_02,
                          BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_03) args_03,
                          BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_04) args_04,
                          BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_05) args_05,
                          BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_06) args_06,
                          BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_07) args_07)
{
    reset();
    BloombergLP::bslma::ConstructionUtil::construct(
                                  d_buffer.address(),
                                  allocator,
                                  il,
                               BSLS_COMPILERFEATURES_FORWARD(ARGS_01, args_01),
                               BSLS_COMPILERFEATURES_FORWARD(ARGS_02, args_02),
                               BSLS_COMPILERFEATURES_FORWARD(ARGS_03, args_03),
                               BSLS_COMPILERFEATURES_FORWARD(ARGS_04, args_04),
                               BSLS_COMPILERFEATURES_FORWARD(ARGS_05, args_05),
                               BSLS_COMPILERFEATURES_FORWARD(ARGS_06, args_06),
                               BSLS_COMPILERFEATURES_FORWARD(ARGS_07, args_07)
                                  );
    d_hasValue = true;
}
#endif  // BSLSTL_OPTIONAL_VARIADIC_LIMIT_B >= 7

#if BSLSTL_OPTIONAL_VARIADIC_LIMIT_B >= 8
template <class TYPE>
template<class IL_TYPE, class ARGS_01,
                        class ARGS_02,
                        class ARGS_03,
                        class ARGS_04,
                        class ARGS_05,
                        class ARGS_06,
                        class ARGS_07,
                        class ARGS_08>
void Optional_DataImp<TYPE>::emplace(
                          bslma::Allocator                          *allocator,
                          std::initializer_list<IL_TYPE>             il,
                          BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_01) args_01,
                          BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_02) args_02,
                          BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_03) args_03,
                          BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_04) args_04,
                          BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_05) args_05,
                          BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_06) args_06,
                          BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_07) args_07,
                          BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_08) args_08)
{
    reset();
    BloombergLP::bslma::ConstructionUtil::construct(
                                  d_buffer.address(),
                                  allocator,
                                  il,
                               BSLS_COMPILERFEATURES_FORWARD(ARGS_01, args_01),
                               BSLS_COMPILERFEATURES_FORWARD(ARGS_02, args_02),
                               BSLS_COMPILERFEATURES_FORWARD(ARGS_03, args_03),
                               BSLS_COMPILERFEATURES_FORWARD(ARGS_04, args_04),
                               BSLS_COMPILERFEATURES_FORWARD(ARGS_05, args_05),
                               BSLS_COMPILERFEATURES_FORWARD(ARGS_06, args_06),
                               BSLS_COMPILERFEATURES_FORWARD(ARGS_07, args_07),
                               BSLS_COMPILERFEATURES_FORWARD(ARGS_08, args_08)
                                  );
    d_hasValue = true;
}
#endif  // BSLSTL_OPTIONAL_VARIADIC_LIMIT_B >= 8

#if BSLSTL_OPTIONAL_VARIADIC_LIMIT_B >= 9
template <class TYPE>
template<class IL_TYPE, class ARGS_01,
                        class ARGS_02,
                        class ARGS_03,
                        class ARGS_04,
                        class ARGS_05,
                        class ARGS_06,
                        class ARGS_07,
                        class ARGS_08,
                        class ARGS_09>
void Optional_DataImp<TYPE>::emplace(
                          bslma::Allocator                          *allocator,
                          std::initializer_list<IL_TYPE>             il,
                          BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_01) args_01,
                          BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_02) args_02,
                          BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_03) args_03,
                          BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_04) args_04,
                          BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_05) args_05,
                          BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_06) args_06,
                          BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_07) args_07,
                          BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_08) args_08,
                          BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_09) args_09)
{
    reset();
    BloombergLP::bslma::ConstructionUtil::construct(
                                  d_buffer.address(),
                                  allocator,
                                  il,
                               BSLS_COMPILERFEATURES_FORWARD(ARGS_01, args_01),
                               BSLS_COMPILERFEATURES_FORWARD(ARGS_02, args_02),
                               BSLS_COMPILERFEATURES_FORWARD(ARGS_03, args_03),
                               BSLS_COMPILERFEATURES_FORWARD(ARGS_04, args_04),
                               BSLS_COMPILERFEATURES_FORWARD(ARGS_05, args_05),
                               BSLS_COMPILERFEATURES_FORWARD(ARGS_06, args_06),
                               BSLS_COMPILERFEATURES_FORWARD(ARGS_07, args_07),
                               BSLS_COMPILERFEATURES_FORWARD(ARGS_08, args_08),
                               BSLS_COMPILERFEATURES_FORWARD(ARGS_09, args_09)
                                  );
    d_hasValue = true;
}
#endif  // BSLSTL_OPTIONAL_VARIADIC_LIMIT_B >= 9

#if BSLSTL_OPTIONAL_VARIADIC_LIMIT_B >= 10
template <class TYPE>
template<class IL_TYPE, class ARGS_01,
                        class ARGS_02,
                        class ARGS_03,
                        class ARGS_04,
                        class ARGS_05,
                        class ARGS_06,
                        class ARGS_07,
                        class ARGS_08,
                        class ARGS_09,
                        class ARGS_10>
void Optional_DataImp<TYPE>::emplace(
                          bslma::Allocator                          *allocator,
                          std::initializer_list<IL_TYPE>             il,
                          BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_01) args_01,
                          BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_02) args_02,
                          BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_03) args_03,
                          BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_04) args_04,
                          BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_05) args_05,
                          BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_06) args_06,
                          BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_07) args_07,
                          BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_08) args_08,
                          BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_09) args_09,
                          BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_10) args_10)
{
    reset();
    BloombergLP::bslma::ConstructionUtil::construct(
                                  d_buffer.address(),
                                  allocator,
                                  il,
                               BSLS_COMPILERFEATURES_FORWARD(ARGS_01, args_01),
                               BSLS_COMPILERFEATURES_FORWARD(ARGS_02, args_02),
                               BSLS_COMPILERFEATURES_FORWARD(ARGS_03, args_03),
                               BSLS_COMPILERFEATURES_FORWARD(ARGS_04, args_04),
                               BSLS_COMPILERFEATURES_FORWARD(ARGS_05, args_05),
                               BSLS_COMPILERFEATURES_FORWARD(ARGS_06, args_06),
                               BSLS_COMPILERFEATURES_FORWARD(ARGS_07, args_07),
                               BSLS_COMPILERFEATURES_FORWARD(ARGS_08, args_08),
                               BSLS_COMPILERFEATURES_FORWARD(ARGS_09, args_09),
                               BSLS_COMPILERFEATURES_FORWARD(ARGS_10, args_10)
                                  );
    d_hasValue = true;
}
#endif  // BSLSTL_OPTIONAL_VARIADIC_LIMIT_B >= 10

#endif
#else
// The generated code below is a workaround for the absence of perfect
// forwarding in some compilers.
template <class TYPE>
template <class... ARGS>
inline
void Optional_DataImp<TYPE>::emplace(
                          bslma::Allocator                          *allocator,
                          BSLS_COMPILERFEATURES_FORWARD_REF(ARGS)... args)
{
    reset();
    BloombergLP::bslma::ConstructionUtil::construct(
                                  d_buffer.address(),
                                  allocator,
                                  BSLS_COMPILERFEATURES_FORWARD(ARGS, args)...
                                  );
    d_hasValue = true;
}

#if defined(BSLS_COMPILERFEATURES_SUPPORT_GENERALIZED_INITIALIZERS)
template <class TYPE>
template<class IL_TYPE, class... ARGS>
void Optional_DataImp<TYPE>::emplace(
                          bslma::Allocator                          *allocator,
                          std::initializer_list<IL_TYPE>             il,
                          BSLS_COMPILERFEATURES_FORWARD_REF(ARGS)... args)
{
    reset();
    BloombergLP::bslma::ConstructionUtil::construct(
                                  d_buffer.address(),
                                  allocator,
                                  il,
                                  BSLS_COMPILERFEATURES_FORWARD(ARGS, args)...
                                  );
    d_hasValue = true;
}
#endif
// }}} END GENERATED CODE
#endif

template <class TYPE>
void Optional_DataImp<TYPE>::reset()
BSLS_KEYWORD_NOEXCEPT
{
    if (d_hasValue) {
        BloombergLP::bslma::DestructionUtil::destroy(&(d_buffer.object()));
    }
    d_hasValue = false;
}

//ACCESSORS
template <class TYPE>
inline
bool
Optional_DataImp<TYPE>::hasValue()
    const BSLS_KEYWORD_NOEXCEPT
{
    if (d_hasValue) {
        return true;                                                  // RETURN
    }
    return false;
}

template <class TYPE>
inline
TYPE&
Optional_DataImp<TYPE>::value()
    BSLS_KEYWORD_LVREF_QUAL
{
    BSLS_ASSERT(d_hasValue);

    return d_buffer.object();
}

template <class TYPE>
inline
const TYPE&
Optional_DataImp<TYPE>::value()
    const BSLS_KEYWORD_LVREF_QUAL
{
    BSLS_ASSERT(d_hasValue);

    return d_buffer.object();
}

#if defined(BSLS_COMPILERFEATURES_SUPPORT_REF_QUALIFIERS)
template <class TYPE>
inline
TYPE&&
Optional_DataImp<TYPE>::value()
    BSLS_KEYWORD_RVREF_QUAL
{
    BSLS_ASSERT(d_hasValue);

    return std::move(d_buffer.object());
}

template <class TYPE>
inline
const TYPE&&
Optional_DataImp<TYPE>::value()
    const BSLS_KEYWORD_RVREF_QUAL
{
    BSLS_ASSERT(d_hasValue);

    return std::move(d_buffer.object());
}
#endif //defined(BSLS_COMPILERFEATURES_SUPPORT_REF_QUALIFIERS)

                        // ===================
                        // class Optional_Data
                        // ===================

template <class TYPE, bool IS_TRIVIALLY_DESTRUCTIBLE =
                        BSLSTL_OPTIONAL_IS_TRIVIALLY_DESTRUCTIBLE<TYPE>::value>
struct Optional_Data : public Optional_DataImp<TYPE>{
    // Component-level class; DO NOT USE

    // Optional_Data is a type used to manage 'value_type' object in
    // 'optional'.  It allows 'optional<TYPE>' to be trivially destructible if
    // 'TYPE' is trivially destructible.  This class also provides an
    // abstraction for const value type.  An 'optional' may contain a 'const'
    // type object.  An assignment to such an 'optional' should not succeed.
    // However, unless the 'optional' itself is 'const', it should be possible
    // to change the value of the 'optional' using 'emplace'.  In order to
    // allow for that, this class manages a non-const object of 'value_type',
    // but all the observers return a 'const' adjusted reference to the managed
    // object.  This functionality is common for all value types and is
    // implemented in the Optional_DataImp base class.  The derived class,
    // 'Optional_Data', is specialised on 'value_type'
    // 'is_trivially_destructible' trait.  The main template is for non
    // trivially destructible types, and it provides a destructor that ensures
    // the value_type destructor is called if 'd_buffer' holds an object.  The
    // specialisation for 'is_trivially_destructible' types does not have a
    // user provided destructor and 'is_trivially_destructible' itself.

  public:
    // CREATORS
    ~Optional_Data();
        // Destroy the managed 'value_type' object, if it exists.

};

                        // ===================
                        // class Optional_Data
                        // ===================

template <class TYPE>
struct Optional_Data<TYPE, true> : public Optional_DataImp<TYPE> {
    // Component-level class; DO NOT USE

    // Optional_Data is a type used to manage 'value_type' object in
    // 'optional'.  It allows 'optional<TYPE>' to be trivially destructible if
    // 'TYPE' is trivially destructible.  This class also provides an
    // abstraction for const value type.  An 'optional' may contain a 'const'
    // type object.  An assignment to such an 'optional' should not succeed.
    // However, unless the 'optional' itself is 'const', it should be possible
    // to change the value of the 'optional' using 'emplace'.  In order to
    // allow for that, this class manages a non-const object of 'value_type',
    // but all the observers return a 'const' adjusted reference to the managed
    // object.  This functionality is common for all value types and is
    // implemented in the Optional_DataImp base class.  The derived class,
    // 'Optional_Data', is specialised on 'value_type'
    // 'is_trivially_destructible' trait.  The main template is for non
    // trivially destructible types, and it provides a destructor that ensures
    // the value_type destructor is called if 'd_buffer' holds an object.  This
    // specialisation is for 'is_trivially_destructible' types, and does not
    // have a user provided destructor, which makes it
    // 'is_trivially_destructible' itself.
};


template <class TYPE, bool IS_TRIVIALLY_DESTRUCTIBLE>
Optional_Data<TYPE, IS_TRIVIALLY_DESTRUCTIBLE>::~Optional_Data()
{
    this->reset();
}

}  // close package namespace
}  // close enterprise namespace

namespace bsl {

                        // =========================
                        // class optional<TYPE>
                        // =========================

template <class TYPE, bool USES_BSLMA_ALLOC>
class optional {
    // This class template provides an STL-compliant implementation of
    // 'optional' type.  The main template is instantiated for allocator-aware
    // types and holds an allocator used to create all objects of 'value_type'
    // managed by the 'optional' object.

  public :
    // PUBLIC TYPES
    typedef TYPE value_type;
        // 'value_type' is an alias for the underlying 'TYPE' upon which this
        // template class is instantiated, and represents the type of the
        // managed object.  The name is chosen so it is compatible with the
        // 'std::optional' implementation.

    typedef typename bsl::allocator<char> allocator_type;

  private:
    // PRIVATE TYPES
    typedef BloombergLP::bslmf::MovableRefUtil MoveUtil;

#ifndef BSLS_COMPILERFEATURES_SUPPORT_OPERATOR_EXPLICIT
    // UNSPECIFIED BOOL

    // This type is needed only in C++03 mode, where 'explicit' conversion
    // operators are not supported.  A 'function' is implicitly converted to
    // 'UnspecifiedBool' when used in 'if' statements, but is not implicitly
    // convertible to 'bool'.
    typedef BloombergLP::bsls::UnspecifiedBool<optional>
                                                          UnspecifiedBoolUtil;
    typedef typename UnspecifiedBoolUtil::BoolType        UnspecifiedBool;

#endif

    // DATA
    BloombergLP::bslstl::Optional_Data<TYPE>  d_value;
                                // in-place 'TYPE' object
    allocator_type d_allocator; // allocator to be used for all in-place 'TYPE'
                                // objects

  public:
    // TRAITS
    BSLMF_NESTED_TRAIT_DECLARATION(optional,
                                   BloombergLP::bslma::UsesBslmaAllocator);
    BSLMF_NESTED_TRAIT_DECLARATION(optional,
                                   BloombergLP::bslmf::UsesAllocatorArgT);
    BSLMF_NESTED_TRAIT_DECLARATION_IF(optional,
                           BloombergLP::bslmf::IsBitwiseMoveable,
                           BloombergLP::bslmf::IsBitwiseMoveable<TYPE>::value);

    // CREATORS
    optional();
        // Create a disengaged 'optional' object.  Use the currently installed
        // default allocator to supply memory for future 'value_type' objects.

    optional(bsl::nullopt_t );                                      // IMPLICIT
        // Create a disengaged 'optional' object.  Use the currently installed
        // default allocator to supply memory for future 'value_type' objects.

    optional(const optional& rhs);                                  // IMPLICIT
        // If 'rhs' contains a value, initialize the contained value using
        // '*rhs'. Otherwise, create a disengaged 'optional'.  Use the
        // currently installed default allocator to supply memory for this and
        // any future 'value_type' objects.

    optional(BloombergLP::bslmf::MovableRef<optional> rhs);         // IMPLICIT
        // If 'rhs' contains a value, initialize the contained 'value_type'
        // object by move construction from '*rhs'. Otherwise, create a
        // disengaged 'optional'.  Use the allocator from rhs to supply memory
        // for this and any future 'value_type' objects.  'rhs' is left in a
        // valid, but unspecified state.


    // Because there are no default arguments in C++03, the case of
    // ANYTYPE==TYPE is written out separately.
    template<class ANY_TYPE>
    optional(BSLS_COMPILERFEATURES_FORWARD_REF(ANY_TYPE) rhs
             BSLSTL_OPTIONAL_ENABLE_IF_SAME(TYPE, ANY_TYPE)
             BSLSTL_OPTIONAL_ENABLE_IF_NOT_EXPLICIT_CONSTRUCT(TYPE, ANY_TYPE))
        // Create an 'optional' object having the value of the specified 'rhs'
        // object.  Use the currently installed default allocator to supply
        // memory for future 'value_type' objects.
    {
        // Must be in-place inline because the use of 'enable_if' will
        // otherwise break the MSVC 2010 compiler.
        this->emplace(BSLS_COMPILERFEATURES_FORWARD(ANY_TYPE, rhs));
    }

    template<class ANY_TYPE>
    explicit
    optional(BSLS_COMPILERFEATURES_FORWARD_REF(ANY_TYPE) rhs
             BSLSTL_OPTIONAL_ENABLE_IF_SAME(TYPE, ANY_TYPE)
             BSLSTL_OPTIONAL_ENABLE_IF_EXPLICIT_CONSTRUCT(TYPE, ANY_TYPE))
        // Create an 'optional' object having the value of the specified 'rhs'
        // object.  Use the currently installed default allocator to supply
        // memory for future 'value_type' objects.
    {
        // Must be in-place inline because the use of 'enable_if' will
        // otherwise break the MSVC 2010 compiler.
        this->emplace(BSLS_COMPILERFEATURES_FORWARD(ANY_TYPE, rhs));
    }

    template<class ANY_TYPE>
    optional(BSLS_COMPILERFEATURES_FORWARD_REF(ANY_TYPE) rhs
             BSLSTL_OPTIONAL_ENABLE_IF_CONSTRUCT_FROM_ANYTYPE
             BSLSTL_OPTIONAL_ENABLE_IF_NOT_EXPLICIT_CONSTRUCT(TYPE, ANY_TYPE))
        // Create an 'optional' object having the value of the specified 'rhs'
        // object.  Use the currently installed default allocator to supply
        // memory for future 'value_type' objects.
    {
        // Must be in-place inline because the use of 'enable_if' will
        // otherwise break the MSVC 2010 compiler.
        this->emplace(BSLS_COMPILERFEATURES_FORWARD(ANY_TYPE, rhs));
    }

    template<class ANY_TYPE>
    explicit
    optional(BSLS_COMPILERFEATURES_FORWARD_REF(ANY_TYPE) rhs
             BSLSTL_OPTIONAL_ENABLE_IF_CONSTRUCT_FROM_ANYTYPE
             BSLSTL_OPTIONAL_ENABLE_IF_EXPLICIT_CONSTRUCT(TYPE, ANY_TYPE))
        // Create an 'optional' object having the value of the specified 'rhs'
        // object.  Use the currently installed default allocator to supply
        // memory for future 'value_type' objects.
    {
        // Must be in-place inline because the use of 'enable_if' will
        // otherwise break the MSVC 2010 compiler.
        this->emplace(BSLS_COMPILERFEATURES_FORWARD(ANY_TYPE, rhs));
    }

    template<class ANY_TYPE>
    optional(const optional<ANY_TYPE>& rhs
             BSLSTL_OPTIONAL_ENABLE_IF_CONSTRUCT_FROM_OPTIONAL_LVAL
             BSLSTL_OPTIONAL_ENABLE_IF_NOT_EXPLICIT_CONSTRUCT(TYPE,
                                                              const ANY_TYPE&))
        // If specified 'rhs' contains a value, initialize the 'value_type'
        // object using '*rhs'.  Otherwise, create a disengaged optional.  Use
        // the currently installed default allocator to supply memory for this
        // and any future 'value_type' objects.
    {
        // Must be in-place inline because the use of 'enable_if' will
        // otherwise break the MSVC 2010 compiler.
        if (rhs.has_value()) {
            emplace(rhs.value());
        }
    }

    template<class ANY_TYPE>
    explicit
    optional(const optional<ANY_TYPE>& rhs
             BSLSTL_OPTIONAL_ENABLE_IF_CONSTRUCT_FROM_OPTIONAL_LVAL
             BSLSTL_OPTIONAL_ENABLE_IF_EXPLICIT_CONSTRUCT(TYPE,
                                                          const ANY_TYPE&))
        // If specified 'rhs' contains a value, initialize the 'value_type'
        // object using '*rhs'.  Otherwise, create a disengaged optional.  Use
        // the currently installed default allocator to supply memory for this
        // and any future 'value_type' objects.
    {
        // Must be in-place inline because the use of 'enable_if' will
        // otherwise break the MSVC 2010 compiler.
        if (rhs.has_value()) {
            emplace(rhs.value());
        }
    }

#if defined(BSLS_COMPILERFEATURES_SUPPORT_RVALUE_REFERENCES )
    // 'MovableRef' prevents correct type deduction in C++11 when used with
    // 'optional<ANY_TYPE>'.  These constructors need to be defined in terms
    // of rvalue reference in C++11.  In C++03, this type deduction issue does
    // not exist due to the nature of C++03 MovableRef implementation and
    // usage.  Consequently, a 'MovableRef' equivalent constructors needs to be
    // provided in C++03 (see below).
    template<class ANY_TYPE>
    optional(optional<ANY_TYPE>&& rhs
             BSLSTL_OPTIONAL_ENABLE_IF_CONSTRUCT_FROM_OPTIONAL_RVAL
             BSLSTL_OPTIONAL_ENABLE_IF_NOT_EXPLICIT_CONSTRUCT(TYPE, ANY_TYPE))
        // If specified 'rhs' contains a value, initialize the 'value_type'
        // object by moving from '*rhs'.  Otherwise, create a disengaged
        // 'optional'.  Use the currently installed default allocator to supply
        // memory for this and any future 'value_type' objects.
    {
        // Must be in-place inline because the use of 'enable_if' will
        // otherwise break the MSVC 2010 compiler.
        if (rhs.has_value()) {
            emplace(MoveUtil::move(rhs.value()));
        }
    }

    template<class ANY_TYPE>
    explicit
    optional(optional<ANY_TYPE>&& rhs
             BSLSTL_OPTIONAL_ENABLE_IF_CONSTRUCT_FROM_OPTIONAL_RVAL
             BSLSTL_OPTIONAL_ENABLE_IF_EXPLICIT_CONSTRUCT(TYPE, ANY_TYPE))
        // If specified 'rhs' contains a value, initialize the 'value_type'
        // object by moving from '*rhs'.  Otherwise, create a disengaged
        // 'optional'.  Use the currently installed default allocator to supply
        // memory for this and any future 'value_type' objects.
    {
        // Must be in-place inline because the use of 'enable_if' will
        // otherwise break the MSVC 2010 compiler.
        if (rhs.has_value()) {
            emplace(MoveUtil::move(rhs.value()));
        }
    }

#else
    template<class ANY_TYPE>
    optional(BloombergLP::bslmf::MovableRef<optional<ANY_TYPE> > rhs
             BSLSTL_OPTIONAL_ENABLE_IF_CONSTRUCT_FROM_OPTIONAL_RVAL
             BSLSTL_OPTIONAL_ENABLE_IF_NOT_EXPLICIT_CONSTRUCT(TYPE, ANY_TYPE))
        // If specified 'rhs' contains a value, initialize the 'value_type'
        // object by moving from '*rhs'.  Otherwise, create a disengaged
        // 'optional'.  Use the currently installed default allocator to supply
        // memory for this and any future 'value_type' objects.
    {
        // Must be in-place inline because the use of 'enable_if' will
        // otherwise break the MSVC 2010 compiler.
        optional<ANY_TYPE> lvalue = rhs;
        if (lvalue.has_value()) {
            emplace(MoveUtil::move(lvalue.value()));
        }
    }

    template<class ANY_TYPE>
    explicit
    optional(BloombergLP::bslmf::MovableRef<optional<ANY_TYPE> > rhs
             BSLSTL_OPTIONAL_ENABLE_IF_CONSTRUCT_FROM_OPTIONAL_RVAL
             BSLSTL_OPTIONAL_ENABLE_IF_EXPLICIT_CONSTRUCT(TYPE, ANY_TYPE))
        // If specified 'rhs' contains a value, initialize the 'value_type'
        // object by moving from '*rhs'.  Otherwise, create a disengaged
        // 'optional'.  Use the currently installed default allocator to supply
        // memory for this and any future 'value_type' objects.
    {
        // Must be in-place inline because the use of 'enable_if' will
        // otherwise break the MSVC 2010 compiler.
        optional<ANY_TYPE> lvalue = rhs;
        if (lvalue.has_value()) {
            emplace(MoveUtil::move(lvalue.value()));
        }
    }
#endif //defined(BSLS_COMPILERFEATURES_SUPPORT_RVALUE_REFERENCES )


#ifdef __cpp_lib_optional
    template<class ANY_TYPE = TYPE>
    optional(const std::optional<ANY_TYPE>& rhs
             BSLSTL_OPTIONAL_ENABLE_IF_CONSTRUCT_FROM_STD_OPTIONAL_LVAL
             BSLSTL_OPTIONAL_ENABLE_IF_NOT_EXPLICIT_CONSTRUCT(TYPE, ANY_TYPE))
        // If specified 'rhs' contains a value, initialize the 'value_type'
        // object by moving from '*rhs'.  Otherwise, create a disengaged
        // 'optional'.  Use the currently installed default allocator to supply
        // memory for this and any future 'value_type' objects.
    {
        // Must be in-place inline because the use of 'enable_if' will
        // otherwise break the MSVC 2010 compiler.
        if (rhs.has_value()) {
            emplace(rhs.value());
        }
    }

    template<class ANY_TYPE = TYPE>
    explicit
    optional(const std::optional<ANY_TYPE>& rhs
             BSLSTL_OPTIONAL_ENABLE_IF_CONSTRUCT_FROM_STD_OPTIONAL_LVAL
             BSLSTL_OPTIONAL_ENABLE_IF_EXPLICIT_CONSTRUCT(TYPE, ANY_TYPE))
        // If specified 'rhs' contains a value, initialize the 'value_type'
        // object by moving from '*rhs'.  Otherwise, create a disengaged
        // 'optional'.  Use the currently installed default allocator to supply
        // memory for this and any future 'value_type' objects.
    {
        // Must be in-place inline because the use of 'enable_if' will
        // otherwise break the MSVC 2010 compiler.
        if (rhs.has_value()) {
            emplace(rhs.value());
        }
    }

    template<class ANY_TYPE = TYPE>
    optional(std::optional<ANY_TYPE>&& rhs
             BSLSTL_OPTIONAL_ENABLE_IF_CONSTRUCT_FROM_STD_OPTIONAL_RVAL
             BSLSTL_OPTIONAL_ENABLE_IF_NOT_EXPLICIT_CONSTRUCT(TYPE, ANY_TYPE))
        // If specified 'rhs' contains a value, initialize the 'value_type'
        // object by moving from '*rhs'.  Otherwise, create a disengaged
        // 'optional'.  Use the currently installed default allocator to supply
        // memory for this and any future 'value_type' objects.
    {
        // Must be in-place inline because the use of 'enable_if' will
        // otherwise break the MSVC 2010 compiler.
        if (rhs.has_value()) {
            emplace(std::move(rhs.value()));
        }
    }

    template<class ANY_TYPE = TYPE>
    explicit
    optional(std::optional<ANY_TYPE>&& rhs
             BSLSTL_OPTIONAL_ENABLE_IF_CONSTRUCT_FROM_STD_OPTIONAL_RVAL
             BSLSTL_OPTIONAL_ENABLE_IF_EXPLICIT_CONSTRUCT(TYPE, ANY_TYPE))
        // If specified 'rhs' contains a value, initialize the 'value_type'
        // object by moving from '*rhs'.  Otherwise, create a disengaged
        // 'optional'.  Use the currently installed default allocator to supply
        // memory for this and any future 'value_type' objects.
    {
        // Must be in-place inline because the use of 'enable_if' will
        // otherwise break the MSVC 2010 compiler.
        if (rhs.has_value()) {
            emplace(std::move(rhs.value()));
        }
    }
#endif //__cpp_lib_optional

#if !BSLS_COMPILERFEATURES_SIMULATE_CPP11_FEATURES
    template<class... ARGS>
    explicit optional(bsl::in_place_t,
                      BSLS_COMPILERFEATURES_FORWARD_REF(ARGS)...);
#if defined(BSLS_COMPILERFEATURES_SUPPORT_GENERALIZED_INITIALIZERS)
    template<class IL_TYPE, class... ARGS>
    explicit optional(bsl::in_place_t,
                      std::initializer_list<IL_TYPE>,
                      BSLS_COMPILERFEATURES_FORWARD_REF(ARGS)...);
#endif//BSLS_COMPILERFEATURES_SUPPORT_GENERALIZED_INITIALIZERS
#elif BSLS_COMPILERFEATURES_SIMULATE_VARIADIC_TEMPLATES
// {{{ BEGIN GENERATED CODE
// The following section is automatically generated.  **DO NOT EDIT**
// Generator command line: sim_cpp11_features.pl bslstl_optional.h
#ifndef BSLSTL_OPTIONAL_VARIADIC_LIMIT
#define BSLSTL_OPTIONAL_VARIADIC_LIMIT 10
#endif
#ifndef BSLSTL_OPTIONAL_VARIADIC_LIMIT_C
#define BSLSTL_OPTIONAL_VARIADIC_LIMIT_C BSLSTL_OPTIONAL_VARIADIC_LIMIT
#endif
#if BSLSTL_OPTIONAL_VARIADIC_LIMIT_C >= 0
    explicit optional(bsl::in_place_t);
#endif  // BSLSTL_OPTIONAL_VARIADIC_LIMIT_C >= 0

#if BSLSTL_OPTIONAL_VARIADIC_LIMIT_C >= 1
    template<class ARGS_01>
    explicit optional(bsl::in_place_t,
                      BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_01));
#endif  // BSLSTL_OPTIONAL_VARIADIC_LIMIT_C >= 1

#if BSLSTL_OPTIONAL_VARIADIC_LIMIT_C >= 2
    template<class ARGS_01,
             class ARGS_02>
    explicit optional(bsl::in_place_t,
                      BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_01),
                      BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_02));
#endif  // BSLSTL_OPTIONAL_VARIADIC_LIMIT_C >= 2

#if BSLSTL_OPTIONAL_VARIADIC_LIMIT_C >= 3
    template<class ARGS_01,
             class ARGS_02,
             class ARGS_03>
    explicit optional(bsl::in_place_t,
                      BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_01),
                      BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_02),
                      BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_03));
#endif  // BSLSTL_OPTIONAL_VARIADIC_LIMIT_C >= 3

#if BSLSTL_OPTIONAL_VARIADIC_LIMIT_C >= 4
    template<class ARGS_01,
             class ARGS_02,
             class ARGS_03,
             class ARGS_04>
    explicit optional(bsl::in_place_t,
                      BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_01),
                      BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_02),
                      BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_03),
                      BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_04));
#endif  // BSLSTL_OPTIONAL_VARIADIC_LIMIT_C >= 4

#if BSLSTL_OPTIONAL_VARIADIC_LIMIT_C >= 5
    template<class ARGS_01,
             class ARGS_02,
             class ARGS_03,
             class ARGS_04,
             class ARGS_05>
    explicit optional(bsl::in_place_t,
                      BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_01),
                      BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_02),
                      BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_03),
                      BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_04),
                      BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_05));
#endif  // BSLSTL_OPTIONAL_VARIADIC_LIMIT_C >= 5

#if BSLSTL_OPTIONAL_VARIADIC_LIMIT_C >= 6
    template<class ARGS_01,
             class ARGS_02,
             class ARGS_03,
             class ARGS_04,
             class ARGS_05,
             class ARGS_06>
    explicit optional(bsl::in_place_t,
                      BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_01),
                      BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_02),
                      BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_03),
                      BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_04),
                      BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_05),
                      BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_06));
#endif  // BSLSTL_OPTIONAL_VARIADIC_LIMIT_C >= 6

#if BSLSTL_OPTIONAL_VARIADIC_LIMIT_C >= 7
    template<class ARGS_01,
             class ARGS_02,
             class ARGS_03,
             class ARGS_04,
             class ARGS_05,
             class ARGS_06,
             class ARGS_07>
    explicit optional(bsl::in_place_t,
                      BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_01),
                      BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_02),
                      BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_03),
                      BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_04),
                      BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_05),
                      BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_06),
                      BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_07));
#endif  // BSLSTL_OPTIONAL_VARIADIC_LIMIT_C >= 7

#if BSLSTL_OPTIONAL_VARIADIC_LIMIT_C >= 8
    template<class ARGS_01,
             class ARGS_02,
             class ARGS_03,
             class ARGS_04,
             class ARGS_05,
             class ARGS_06,
             class ARGS_07,
             class ARGS_08>
    explicit optional(bsl::in_place_t,
                      BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_01),
                      BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_02),
                      BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_03),
                      BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_04),
                      BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_05),
                      BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_06),
                      BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_07),
                      BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_08));
#endif  // BSLSTL_OPTIONAL_VARIADIC_LIMIT_C >= 8

#if BSLSTL_OPTIONAL_VARIADIC_LIMIT_C >= 9
    template<class ARGS_01,
             class ARGS_02,
             class ARGS_03,
             class ARGS_04,
             class ARGS_05,
             class ARGS_06,
             class ARGS_07,
             class ARGS_08,
             class ARGS_09>
    explicit optional(bsl::in_place_t,
                      BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_01),
                      BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_02),
                      BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_03),
                      BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_04),
                      BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_05),
                      BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_06),
                      BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_07),
                      BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_08),
                      BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_09));
#endif  // BSLSTL_OPTIONAL_VARIADIC_LIMIT_C >= 9

#if BSLSTL_OPTIONAL_VARIADIC_LIMIT_C >= 10
    template<class ARGS_01,
             class ARGS_02,
             class ARGS_03,
             class ARGS_04,
             class ARGS_05,
             class ARGS_06,
             class ARGS_07,
             class ARGS_08,
             class ARGS_09,
             class ARGS_10>
    explicit optional(bsl::in_place_t,
                      BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_01),
                      BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_02),
                      BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_03),
                      BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_04),
                      BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_05),
                      BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_06),
                      BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_07),
                      BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_08),
                      BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_09),
                      BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_10));
#endif  // BSLSTL_OPTIONAL_VARIADIC_LIMIT_C >= 10

#if defined(BSLS_COMPILERFEATURES_SUPPORT_GENERALIZED_INITIALIZERS)
#if BSLSTL_OPTIONAL_VARIADIC_LIMIT_C >= 0
    template<class IL_TYPE>
    explicit optional(bsl::in_place_t,
                      std::initializer_list<IL_TYPE>);
#endif  // BSLSTL_OPTIONAL_VARIADIC_LIMIT_C >= 0

#if BSLSTL_OPTIONAL_VARIADIC_LIMIT_C >= 1
    template<class IL_TYPE, class ARGS_01>
    explicit optional(bsl::in_place_t,
                      std::initializer_list<IL_TYPE>,
                      BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_01));
#endif  // BSLSTL_OPTIONAL_VARIADIC_LIMIT_C >= 1

#if BSLSTL_OPTIONAL_VARIADIC_LIMIT_C >= 2
    template<class IL_TYPE, class ARGS_01,
                            class ARGS_02>
    explicit optional(bsl::in_place_t,
                      std::initializer_list<IL_TYPE>,
                      BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_01),
                      BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_02));
#endif  // BSLSTL_OPTIONAL_VARIADIC_LIMIT_C >= 2

#if BSLSTL_OPTIONAL_VARIADIC_LIMIT_C >= 3
    template<class IL_TYPE, class ARGS_01,
                            class ARGS_02,
                            class ARGS_03>
    explicit optional(bsl::in_place_t,
                      std::initializer_list<IL_TYPE>,
                      BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_01),
                      BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_02),
                      BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_03));
#endif  // BSLSTL_OPTIONAL_VARIADIC_LIMIT_C >= 3

#if BSLSTL_OPTIONAL_VARIADIC_LIMIT_C >= 4
    template<class IL_TYPE, class ARGS_01,
                            class ARGS_02,
                            class ARGS_03,
                            class ARGS_04>
    explicit optional(bsl::in_place_t,
                      std::initializer_list<IL_TYPE>,
                      BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_01),
                      BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_02),
                      BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_03),
                      BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_04));
#endif  // BSLSTL_OPTIONAL_VARIADIC_LIMIT_C >= 4

#if BSLSTL_OPTIONAL_VARIADIC_LIMIT_C >= 5
    template<class IL_TYPE, class ARGS_01,
                            class ARGS_02,
                            class ARGS_03,
                            class ARGS_04,
                            class ARGS_05>
    explicit optional(bsl::in_place_t,
                      std::initializer_list<IL_TYPE>,
                      BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_01),
                      BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_02),
                      BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_03),
                      BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_04),
                      BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_05));
#endif  // BSLSTL_OPTIONAL_VARIADIC_LIMIT_C >= 5

#if BSLSTL_OPTIONAL_VARIADIC_LIMIT_C >= 6
    template<class IL_TYPE, class ARGS_01,
                            class ARGS_02,
                            class ARGS_03,
                            class ARGS_04,
                            class ARGS_05,
                            class ARGS_06>
    explicit optional(bsl::in_place_t,
                      std::initializer_list<IL_TYPE>,
                      BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_01),
                      BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_02),
                      BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_03),
                      BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_04),
                      BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_05),
                      BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_06));
#endif  // BSLSTL_OPTIONAL_VARIADIC_LIMIT_C >= 6

#if BSLSTL_OPTIONAL_VARIADIC_LIMIT_C >= 7
    template<class IL_TYPE, class ARGS_01,
                            class ARGS_02,
                            class ARGS_03,
                            class ARGS_04,
                            class ARGS_05,
                            class ARGS_06,
                            class ARGS_07>
    explicit optional(bsl::in_place_t,
                      std::initializer_list<IL_TYPE>,
                      BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_01),
                      BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_02),
                      BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_03),
                      BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_04),
                      BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_05),
                      BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_06),
                      BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_07));
#endif  // BSLSTL_OPTIONAL_VARIADIC_LIMIT_C >= 7

#if BSLSTL_OPTIONAL_VARIADIC_LIMIT_C >= 8
    template<class IL_TYPE, class ARGS_01,
                            class ARGS_02,
                            class ARGS_03,
                            class ARGS_04,
                            class ARGS_05,
                            class ARGS_06,
                            class ARGS_07,
                            class ARGS_08>
    explicit optional(bsl::in_place_t,
                      std::initializer_list<IL_TYPE>,
                      BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_01),
                      BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_02),
                      BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_03),
                      BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_04),
                      BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_05),
                      BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_06),
                      BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_07),
                      BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_08));
#endif  // BSLSTL_OPTIONAL_VARIADIC_LIMIT_C >= 8

#if BSLSTL_OPTIONAL_VARIADIC_LIMIT_C >= 9
    template<class IL_TYPE, class ARGS_01,
                            class ARGS_02,
                            class ARGS_03,
                            class ARGS_04,
                            class ARGS_05,
                            class ARGS_06,
                            class ARGS_07,
                            class ARGS_08,
                            class ARGS_09>
    explicit optional(bsl::in_place_t,
                      std::initializer_list<IL_TYPE>,
                      BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_01),
                      BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_02),
                      BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_03),
                      BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_04),
                      BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_05),
                      BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_06),
                      BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_07),
                      BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_08),
                      BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_09));
#endif  // BSLSTL_OPTIONAL_VARIADIC_LIMIT_C >= 9

#if BSLSTL_OPTIONAL_VARIADIC_LIMIT_C >= 10
    template<class IL_TYPE, class ARGS_01,
                            class ARGS_02,
                            class ARGS_03,
                            class ARGS_04,
                            class ARGS_05,
                            class ARGS_06,
                            class ARGS_07,
                            class ARGS_08,
                            class ARGS_09,
                            class ARGS_10>
    explicit optional(bsl::in_place_t,
                      std::initializer_list<IL_TYPE>,
                      BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_01),
                      BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_02),
                      BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_03),
                      BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_04),
                      BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_05),
                      BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_06),
                      BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_07),
                      BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_08),
                      BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_09),
                      BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_10));
#endif  // BSLSTL_OPTIONAL_VARIADIC_LIMIT_C >= 10

#endif
#else
// The generated code below is a workaround for the absence of perfect
// forwarding in some compilers.
    template<class... ARGS>
    explicit optional(bsl::in_place_t,
                      BSLS_COMPILERFEATURES_FORWARD_REF(ARGS)...);
#if defined(BSLS_COMPILERFEATURES_SUPPORT_GENERALIZED_INITIALIZERS)
    template<class IL_TYPE, class... ARGS>
    explicit optional(bsl::in_place_t,
                      std::initializer_list<IL_TYPE>,
                      BSLS_COMPILERFEATURES_FORWARD_REF(ARGS)...);
#endif
// }}} END GENERATED CODE
#endif

    optional(bsl::allocator_arg_t, allocator_type allocator);
        // Create a disengaged 'optional' object.  Use the specified
        // 'allocator' to supply memory for future objects.

    optional(bsl::allocator_arg_t,
             allocator_type allocator,
             bsl::nullopt_t);
        // Create a disengaged 'optional' object.  Use the specified
        // 'allocator' to supply memory for future objects.

    optional(bsl::allocator_arg_t,
             allocator_type allocator,
             const optional&);
        // If specified 'rhs' contains a value, initialize the contained
        //  'value_type' object with '*rhs'.  Otherwise, create a disengaged
        // 'optional'.  Use the specified 'allocator' to supply memory.

    optional(bsl::allocator_arg_t,
             allocator_type                           allocator,
             BloombergLP::bslmf::MovableRef<optional> rhs);
        // If specified 'rhs' contains a value, initialize the contained
        // 'value_type' object by move construction from '*rhs'.  Otherwise,
        // create a disengaged 'optional'.  Use the specified 'allocator' to
        // supply memory.

    template<class ANY_TYPE>
    explicit
    optional(bsl::allocator_arg_t,
             allocator_type                              allocator,
             BSLS_COMPILERFEATURES_FORWARD_REF(ANY_TYPE) rhs
             BSLSTL_OPTIONAL_ENABLE_IF_SAME(ANY_TYPE, TYPE))
    : d_allocator(allocator)
        // Create an 'optional' object having the same value as the specified
        // 'rhs' object by forwarding the contents of 'rhs' to the
        // newly-created object.  Use the specified 'allocator' to supply
        // memory.  'rhs' is left in a valid but unspecified state.
    {
        // Must be in-place inline because the use of 'enable_if' will
        // otherwise break the MSVC 2010 compiler.
        this->emplace(BSLS_COMPILERFEATURES_FORWARD(ANY_TYPE, rhs));
    }

    template<class ANY_TYPE>
    explicit
    optional(bsl::allocator_arg_t,
             allocator_type                              allocator,
             BSLS_COMPILERFEATURES_FORWARD_REF(ANY_TYPE) rhs
             BSLSTL_OPTIONAL_ENABLE_IF_CONSTRUCT_FROM_ANYTYPE)
    : d_allocator(allocator)
        // Create an 'optional' object having the same value as the specified
        // 'rhs' object by forwarding the contents of 'rhs' to the
        // newly-created object.  Use the specified 'allocator' to supply
        // memory.  'rhs' is left in a valid but unspecified state.
    {
        // Must be in-place inline because the use of 'enable_if' will
        // otherwise break the MSVC 2010 compiler.
        this->emplace(BSLS_COMPILERFEATURES_FORWARD(ANY_TYPE, rhs));
    }

    template<class ANY_TYPE>
    explicit
    optional(bsl::allocator_arg_t,
             allocator_type            allocator,
             const optional<ANY_TYPE>& rhs
             BSLSTL_OPTIONAL_ENABLE_IF_CONSTRUCT_FROM_OPTIONAL_LVAL)
    : d_allocator(allocator)
        // If specified 'rhs' contains a value, initialize the 'value_type'
        // object with '*rhs'.  Otherwise, create a disengaged 'optional'.  Use
        // the specified 'allocator' to supply memory.
    {
        // Must be in-place inline because the use of 'enable_if' will
        // otherwise break the MSVC 2010 compiler.
        if (rhs.has_value()) {
            this->emplace(rhs.value());
        }
    }

#ifdef BSLS_COMPILERFEATURES_SUPPORT_RVALUE_REFERENCES
    // 'MovableRef' prevents correct type deduction in C++11 when used with
    // 'optional<ANY_TYPE>'.  This constructor needs to be defined in terms of
    // rvalue reference in C++11.  In C++03, this type deduction issue does not
    // exist due to the nature of C++03 MovableRef implementation and usage.
    // Consequently, a 'MovableRef' equivalent constructor needs to be
    // provided in C++03 (see below).
    template<class ANY_TYPE>
    explicit
    optional(bsl::allocator_arg_t,
             allocator_type       allocator,
             optional<ANY_TYPE>&& rhs
             BSLSTL_OPTIONAL_ENABLE_IF_CONSTRUCT_FROM_OPTIONAL_RVAL)

    : d_allocator(allocator)
        // If specified 'rhs' contains a value, initialize the 'value_type'
        // object by move construction from '*rhs'.  Otherwise, create a
        // disengaged 'optional'.  Use the specified 'allocator' to supply
        // memory.
    {
        // Must be in-place inline because the use of 'enable_if' will
        // otherwise break the MSVC 2010 compiler.
        optional<ANY_TYPE>& lvalue = rhs;

        if (lvalue.has_value()) {
            emplace(MoveUtil::move(lvalue.value()));
        }
    }

#else
    template<class ANY_TYPE>
    explicit
    optional(bsl::allocator_arg_t,
             allocator_type allocator,
             BloombergLP::bslmf::MovableRef<optional<ANY_TYPE> > rhs
             BSLSTL_OPTIONAL_ENABLE_IF_CONSTRUCT_FROM_OPTIONAL_RVAL)
    : d_allocator(allocator)
        // If specified 'rhs' contains a value, initialize the 'value_type'
        // object by move construction from '*rhs'.  Otherwise, create a
        // disengaged 'optional'.  Use the specified 'allocator' to supply
        // memory.
    {
        // Must be in-place inline because the use of 'enable_if' will
        // otherwise break the MSVC 2010 compiler.
        optional<ANY_TYPE>& lvalue = rhs;

        if (lvalue.has_value()) {
            emplace(MoveUtil::move(lvalue.value()));
        }
    }

#endif //BSLS_COMPILERFEATURES_SUPPORT_RVALUE_REFERENCES

#ifdef __cpp_lib_optional
    template<class ANY_TYPE = TYPE>
    explicit
    optional(bsl::allocator_arg_t,
             allocator_type                  allocator,
             const std::optional<ANY_TYPE>&  rhs
             BSLSTL_OPTIONAL_ENABLE_IF_CONSTRUCT_FROM_STD_OPTIONAL_LVAL)
    : d_allocator(allocator)
        // If specified 'rhs' contains a value, initialize the 'value_type'
        // object with '*rhs'.  Otherwise, create a disengaged 'optional'.  Use
        // the specified 'allocator' to supply memory.
    {
        // Must be in-place inline because the use of 'enable_if' will
        // otherwise break the MSVC 2010 compiler.
        if (rhs.has_value()) {
            this->emplace(rhs.value());
        }
    }

    template<class ANY_TYPE = TYPE>
    explicit
    optional(bsl::allocator_arg_t,
             allocator_type             allocator,
             std::optional<ANY_TYPE>&&  rhs
             BSLSTL_OPTIONAL_ENABLE_IF_CONSTRUCT_FROM_STD_OPTIONAL_RVAL)

    : d_allocator(allocator)
        // If specified 'rhs' contains a value, initialize the 'value_type'
        // object by move construction from '*rhs'.  Otherwise, create a
        // disengaged 'optional'.  Use the specified 'allocator' to supply
        // memory.
    {
        // Must be in-place inline because the use of 'enable_if' will
        // otherwise break the MSVC 2010 compiler.
        std::optional<ANY_TYPE>& lvalue = rhs;

        if (lvalue.has_value()) {
            emplace(MoveUtil::move(lvalue.value()));
        }
    }

#endif //__cpp_lib_optional

#if !BSLS_COMPILERFEATURES_SIMULATE_CPP11_FEATURES
    template<class... ARGS>
    explicit optional(bsl::allocator_arg_t,
                      allocator_type allocator,
                      bsl::in_place_t,
                      BSLS_COMPILERFEATURES_FORWARD_REF(ARGS)...);
        // Create the 'value_type' object using the specified arguments.  Use
        // the specified 'allocator' to supply memory for this and any future
        // 'value_type' objects.

#if defined(BSLS_COMPILERFEATURES_SUPPORT_GENERALIZED_INITIALIZERS)
    template<class IL_TYPE, class... ARGS>
    explicit optional(bsl::allocator_arg_t,
                      allocator_type                 allocator,
                      bsl::in_place_t,
                      std::initializer_list<IL_TYPE> initializer_list,
                      BSLS_COMPILERFEATURES_FORWARD_REF(ARGS)...);
        // Create the 'value_type' object using the specified
        // 'initializer_list' and arguments.  Use the specified 'allocator' to
        // supply memory for this and any future 'value_type' objects.
#endif//BSLS_COMPILERFEATURES_SUPPORT_GENERALIZED_INITIALIZERS
#elif BSLS_COMPILERFEATURES_SIMULATE_VARIADIC_TEMPLATES
// {{{ BEGIN GENERATED CODE
// The following section is automatically generated.  **DO NOT EDIT**
// Generator command line: sim_cpp11_features.pl bslstl_optional.h
#ifndef BSLSTL_OPTIONAL_VARIADIC_LIMIT
#define BSLSTL_OPTIONAL_VARIADIC_LIMIT 10
#endif
#ifndef BSLSTL_OPTIONAL_VARIADIC_LIMIT_D
#define BSLSTL_OPTIONAL_VARIADIC_LIMIT_D BSLSTL_OPTIONAL_VARIADIC_LIMIT
#endif
#if BSLSTL_OPTIONAL_VARIADIC_LIMIT_D >= 0
    explicit optional(bsl::allocator_arg_t,
                      allocator_type allocator,
                      bsl::in_place_t);
#endif  // BSLSTL_OPTIONAL_VARIADIC_LIMIT_D >= 0

#if BSLSTL_OPTIONAL_VARIADIC_LIMIT_D >= 1
    template<class ARGS_01>
    explicit optional(bsl::allocator_arg_t,
                      allocator_type allocator,
                      bsl::in_place_t,
                      BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_01));
#endif  // BSLSTL_OPTIONAL_VARIADIC_LIMIT_D >= 1

#if BSLSTL_OPTIONAL_VARIADIC_LIMIT_D >= 2
    template<class ARGS_01,
             class ARGS_02>
    explicit optional(bsl::allocator_arg_t,
                      allocator_type allocator,
                      bsl::in_place_t,
                      BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_01),
                      BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_02));
#endif  // BSLSTL_OPTIONAL_VARIADIC_LIMIT_D >= 2

#if BSLSTL_OPTIONAL_VARIADIC_LIMIT_D >= 3
    template<class ARGS_01,
             class ARGS_02,
             class ARGS_03>
    explicit optional(bsl::allocator_arg_t,
                      allocator_type allocator,
                      bsl::in_place_t,
                      BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_01),
                      BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_02),
                      BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_03));
#endif  // BSLSTL_OPTIONAL_VARIADIC_LIMIT_D >= 3

#if BSLSTL_OPTIONAL_VARIADIC_LIMIT_D >= 4
    template<class ARGS_01,
             class ARGS_02,
             class ARGS_03,
             class ARGS_04>
    explicit optional(bsl::allocator_arg_t,
                      allocator_type allocator,
                      bsl::in_place_t,
                      BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_01),
                      BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_02),
                      BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_03),
                      BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_04));
#endif  // BSLSTL_OPTIONAL_VARIADIC_LIMIT_D >= 4

#if BSLSTL_OPTIONAL_VARIADIC_LIMIT_D >= 5
    template<class ARGS_01,
             class ARGS_02,
             class ARGS_03,
             class ARGS_04,
             class ARGS_05>
    explicit optional(bsl::allocator_arg_t,
                      allocator_type allocator,
                      bsl::in_place_t,
                      BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_01),
                      BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_02),
                      BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_03),
                      BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_04),
                      BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_05));
#endif  // BSLSTL_OPTIONAL_VARIADIC_LIMIT_D >= 5

#if BSLSTL_OPTIONAL_VARIADIC_LIMIT_D >= 6
    template<class ARGS_01,
             class ARGS_02,
             class ARGS_03,
             class ARGS_04,
             class ARGS_05,
             class ARGS_06>
    explicit optional(bsl::allocator_arg_t,
                      allocator_type allocator,
                      bsl::in_place_t,
                      BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_01),
                      BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_02),
                      BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_03),
                      BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_04),
                      BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_05),
                      BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_06));
#endif  // BSLSTL_OPTIONAL_VARIADIC_LIMIT_D >= 6

#if BSLSTL_OPTIONAL_VARIADIC_LIMIT_D >= 7
    template<class ARGS_01,
             class ARGS_02,
             class ARGS_03,
             class ARGS_04,
             class ARGS_05,
             class ARGS_06,
             class ARGS_07>
    explicit optional(bsl::allocator_arg_t,
                      allocator_type allocator,
                      bsl::in_place_t,
                      BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_01),
                      BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_02),
                      BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_03),
                      BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_04),
                      BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_05),
                      BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_06),
                      BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_07));
#endif  // BSLSTL_OPTIONAL_VARIADIC_LIMIT_D >= 7

#if BSLSTL_OPTIONAL_VARIADIC_LIMIT_D >= 8
    template<class ARGS_01,
             class ARGS_02,
             class ARGS_03,
             class ARGS_04,
             class ARGS_05,
             class ARGS_06,
             class ARGS_07,
             class ARGS_08>
    explicit optional(bsl::allocator_arg_t,
                      allocator_type allocator,
                      bsl::in_place_t,
                      BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_01),
                      BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_02),
                      BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_03),
                      BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_04),
                      BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_05),
                      BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_06),
                      BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_07),
                      BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_08));
#endif  // BSLSTL_OPTIONAL_VARIADIC_LIMIT_D >= 8

#if BSLSTL_OPTIONAL_VARIADIC_LIMIT_D >= 9
    template<class ARGS_01,
             class ARGS_02,
             class ARGS_03,
             class ARGS_04,
             class ARGS_05,
             class ARGS_06,
             class ARGS_07,
             class ARGS_08,
             class ARGS_09>
    explicit optional(bsl::allocator_arg_t,
                      allocator_type allocator,
                      bsl::in_place_t,
                      BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_01),
                      BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_02),
                      BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_03),
                      BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_04),
                      BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_05),
                      BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_06),
                      BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_07),
                      BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_08),
                      BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_09));
#endif  // BSLSTL_OPTIONAL_VARIADIC_LIMIT_D >= 9

#if BSLSTL_OPTIONAL_VARIADIC_LIMIT_D >= 10
    template<class ARGS_01,
             class ARGS_02,
             class ARGS_03,
             class ARGS_04,
             class ARGS_05,
             class ARGS_06,
             class ARGS_07,
             class ARGS_08,
             class ARGS_09,
             class ARGS_10>
    explicit optional(bsl::allocator_arg_t,
                      allocator_type allocator,
                      bsl::in_place_t,
                      BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_01),
                      BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_02),
                      BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_03),
                      BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_04),
                      BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_05),
                      BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_06),
                      BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_07),
                      BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_08),
                      BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_09),
                      BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_10));
#endif  // BSLSTL_OPTIONAL_VARIADIC_LIMIT_D >= 10


#if defined(BSLS_COMPILERFEATURES_SUPPORT_GENERALIZED_INITIALIZERS)
#if BSLSTL_OPTIONAL_VARIADIC_LIMIT_D >= 0
    template<class IL_TYPE>
    explicit optional(bsl::allocator_arg_t,
                      allocator_type                 allocator,
                      bsl::in_place_t,
                      std::initializer_list<IL_TYPE> initializer_list);
#endif  // BSLSTL_OPTIONAL_VARIADIC_LIMIT_D >= 0

#if BSLSTL_OPTIONAL_VARIADIC_LIMIT_D >= 1
    template<class IL_TYPE, class ARGS_01>
    explicit optional(bsl::allocator_arg_t,
                      allocator_type                 allocator,
                      bsl::in_place_t,
                      std::initializer_list<IL_TYPE> initializer_list,
                      BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_01));
#endif  // BSLSTL_OPTIONAL_VARIADIC_LIMIT_D >= 1

#if BSLSTL_OPTIONAL_VARIADIC_LIMIT_D >= 2
    template<class IL_TYPE, class ARGS_01,
                            class ARGS_02>
    explicit optional(bsl::allocator_arg_t,
                      allocator_type                 allocator,
                      bsl::in_place_t,
                      std::initializer_list<IL_TYPE> initializer_list,
                      BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_01),
                      BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_02));
#endif  // BSLSTL_OPTIONAL_VARIADIC_LIMIT_D >= 2

#if BSLSTL_OPTIONAL_VARIADIC_LIMIT_D >= 3
    template<class IL_TYPE, class ARGS_01,
                            class ARGS_02,
                            class ARGS_03>
    explicit optional(bsl::allocator_arg_t,
                      allocator_type                 allocator,
                      bsl::in_place_t,
                      std::initializer_list<IL_TYPE> initializer_list,
                      BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_01),
                      BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_02),
                      BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_03));
#endif  // BSLSTL_OPTIONAL_VARIADIC_LIMIT_D >= 3

#if BSLSTL_OPTIONAL_VARIADIC_LIMIT_D >= 4
    template<class IL_TYPE, class ARGS_01,
                            class ARGS_02,
                            class ARGS_03,
                            class ARGS_04>
    explicit optional(bsl::allocator_arg_t,
                      allocator_type                 allocator,
                      bsl::in_place_t,
                      std::initializer_list<IL_TYPE> initializer_list,
                      BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_01),
                      BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_02),
                      BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_03),
                      BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_04));
#endif  // BSLSTL_OPTIONAL_VARIADIC_LIMIT_D >= 4

#if BSLSTL_OPTIONAL_VARIADIC_LIMIT_D >= 5
    template<class IL_TYPE, class ARGS_01,
                            class ARGS_02,
                            class ARGS_03,
                            class ARGS_04,
                            class ARGS_05>
    explicit optional(bsl::allocator_arg_t,
                      allocator_type                 allocator,
                      bsl::in_place_t,
                      std::initializer_list<IL_TYPE> initializer_list,
                      BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_01),
                      BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_02),
                      BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_03),
                      BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_04),
                      BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_05));
#endif  // BSLSTL_OPTIONAL_VARIADIC_LIMIT_D >= 5

#if BSLSTL_OPTIONAL_VARIADIC_LIMIT_D >= 6
    template<class IL_TYPE, class ARGS_01,
                            class ARGS_02,
                            class ARGS_03,
                            class ARGS_04,
                            class ARGS_05,
                            class ARGS_06>
    explicit optional(bsl::allocator_arg_t,
                      allocator_type                 allocator,
                      bsl::in_place_t,
                      std::initializer_list<IL_TYPE> initializer_list,
                      BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_01),
                      BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_02),
                      BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_03),
                      BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_04),
                      BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_05),
                      BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_06));
#endif  // BSLSTL_OPTIONAL_VARIADIC_LIMIT_D >= 6

#if BSLSTL_OPTIONAL_VARIADIC_LIMIT_D >= 7
    template<class IL_TYPE, class ARGS_01,
                            class ARGS_02,
                            class ARGS_03,
                            class ARGS_04,
                            class ARGS_05,
                            class ARGS_06,
                            class ARGS_07>
    explicit optional(bsl::allocator_arg_t,
                      allocator_type                 allocator,
                      bsl::in_place_t,
                      std::initializer_list<IL_TYPE> initializer_list,
                      BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_01),
                      BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_02),
                      BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_03),
                      BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_04),
                      BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_05),
                      BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_06),
                      BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_07));
#endif  // BSLSTL_OPTIONAL_VARIADIC_LIMIT_D >= 7

#if BSLSTL_OPTIONAL_VARIADIC_LIMIT_D >= 8
    template<class IL_TYPE, class ARGS_01,
                            class ARGS_02,
                            class ARGS_03,
                            class ARGS_04,
                            class ARGS_05,
                            class ARGS_06,
                            class ARGS_07,
                            class ARGS_08>
    explicit optional(bsl::allocator_arg_t,
                      allocator_type                 allocator,
                      bsl::in_place_t,
                      std::initializer_list<IL_TYPE> initializer_list,
                      BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_01),
                      BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_02),
                      BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_03),
                      BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_04),
                      BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_05),
                      BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_06),
                      BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_07),
                      BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_08));
#endif  // BSLSTL_OPTIONAL_VARIADIC_LIMIT_D >= 8

#if BSLSTL_OPTIONAL_VARIADIC_LIMIT_D >= 9
    template<class IL_TYPE, class ARGS_01,
                            class ARGS_02,
                            class ARGS_03,
                            class ARGS_04,
                            class ARGS_05,
                            class ARGS_06,
                            class ARGS_07,
                            class ARGS_08,
                            class ARGS_09>
    explicit optional(bsl::allocator_arg_t,
                      allocator_type                 allocator,
                      bsl::in_place_t,
                      std::initializer_list<IL_TYPE> initializer_list,
                      BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_01),
                      BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_02),
                      BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_03),
                      BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_04),
                      BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_05),
                      BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_06),
                      BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_07),
                      BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_08),
                      BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_09));
#endif  // BSLSTL_OPTIONAL_VARIADIC_LIMIT_D >= 9

#if BSLSTL_OPTIONAL_VARIADIC_LIMIT_D >= 10
    template<class IL_TYPE, class ARGS_01,
                            class ARGS_02,
                            class ARGS_03,
                            class ARGS_04,
                            class ARGS_05,
                            class ARGS_06,
                            class ARGS_07,
                            class ARGS_08,
                            class ARGS_09,
                            class ARGS_10>
    explicit optional(bsl::allocator_arg_t,
                      allocator_type                 allocator,
                      bsl::in_place_t,
                      std::initializer_list<IL_TYPE> initializer_list,
                      BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_01),
                      BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_02),
                      BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_03),
                      BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_04),
                      BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_05),
                      BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_06),
                      BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_07),
                      BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_08),
                      BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_09),
                      BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_10));
#endif  // BSLSTL_OPTIONAL_VARIADIC_LIMIT_D >= 10

#endif
#else
// The generated code below is a workaround for the absence of perfect
// forwarding in some compilers.
    template<class... ARGS>
    explicit optional(bsl::allocator_arg_t,
                      allocator_type allocator,
                      bsl::in_place_t,
                      BSLS_COMPILERFEATURES_FORWARD_REF(ARGS)...);

#if defined(BSLS_COMPILERFEATURES_SUPPORT_GENERALIZED_INITIALIZERS)
    template<class IL_TYPE, class... ARGS>
    explicit optional(bsl::allocator_arg_t,
                      allocator_type                 allocator,
                      bsl::in_place_t,
                      std::initializer_list<IL_TYPE> initializer_list,
                      BSLS_COMPILERFEATURES_FORWARD_REF(ARGS)...);
#endif
// }}} END GENERATED CODE
#endif

    //MANIPULATORS
#if !BSLS_COMPILERFEATURES_SIMULATE_CPP11_FEATURES
    template<class... ARGS>
    void emplace(ARGS&&...);
        // Destroy the current 'value_type' object, if any, and create a new
        // one using the stored allocator and the provided arguments.

#if defined(BSLS_COMPILERFEATURES_SUPPORT_GENERALIZED_INITIALIZERS)
    template<class IL_TYPE, class... ARGS>
    void emplace(std::initializer_list<IL_TYPE>, ARGS&&...);
        // Destroy the current 'value_type' object, if any, and create a new
        // one using the stored allocator and the provided arguments.
#endif//BSLS_COMPILERFEATURES_SUPPORT_GENERALIZED_INITIALIZERS
#elif BSLS_COMPILERFEATURES_SIMULATE_VARIADIC_TEMPLATES
// {{{ BEGIN GENERATED CODE
// The following section is automatically generated.  **DO NOT EDIT**
// Generator command line: sim_cpp11_features.pl bslstl_optional.h
#ifndef BSLSTL_OPTIONAL_VARIADIC_LIMIT
#define BSLSTL_OPTIONAL_VARIADIC_LIMIT 10
#endif
#ifndef BSLSTL_OPTIONAL_VARIADIC_LIMIT_E
#define BSLSTL_OPTIONAL_VARIADIC_LIMIT_E BSLSTL_OPTIONAL_VARIADIC_LIMIT
#endif
#if BSLSTL_OPTIONAL_VARIADIC_LIMIT_E >= 0
    void emplace();
#endif  // BSLSTL_OPTIONAL_VARIADIC_LIMIT_E >= 0

#if BSLSTL_OPTIONAL_VARIADIC_LIMIT_E >= 1
    template<class ARGS_01>
    void emplace(BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_01));
#endif  // BSLSTL_OPTIONAL_VARIADIC_LIMIT_E >= 1

#if BSLSTL_OPTIONAL_VARIADIC_LIMIT_E >= 2
    template<class ARGS_01,
             class ARGS_02>
    void emplace(BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_01),
                 BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_02));
#endif  // BSLSTL_OPTIONAL_VARIADIC_LIMIT_E >= 2

#if BSLSTL_OPTIONAL_VARIADIC_LIMIT_E >= 3
    template<class ARGS_01,
             class ARGS_02,
             class ARGS_03>
    void emplace(BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_01),
                 BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_02),
                 BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_03));
#endif  // BSLSTL_OPTIONAL_VARIADIC_LIMIT_E >= 3

#if BSLSTL_OPTIONAL_VARIADIC_LIMIT_E >= 4
    template<class ARGS_01,
             class ARGS_02,
             class ARGS_03,
             class ARGS_04>
    void emplace(BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_01),
                 BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_02),
                 BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_03),
                 BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_04));
#endif  // BSLSTL_OPTIONAL_VARIADIC_LIMIT_E >= 4

#if BSLSTL_OPTIONAL_VARIADIC_LIMIT_E >= 5
    template<class ARGS_01,
             class ARGS_02,
             class ARGS_03,
             class ARGS_04,
             class ARGS_05>
    void emplace(BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_01),
                 BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_02),
                 BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_03),
                 BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_04),
                 BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_05));
#endif  // BSLSTL_OPTIONAL_VARIADIC_LIMIT_E >= 5

#if BSLSTL_OPTIONAL_VARIADIC_LIMIT_E >= 6
    template<class ARGS_01,
             class ARGS_02,
             class ARGS_03,
             class ARGS_04,
             class ARGS_05,
             class ARGS_06>
    void emplace(BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_01),
                 BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_02),
                 BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_03),
                 BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_04),
                 BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_05),
                 BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_06));
#endif  // BSLSTL_OPTIONAL_VARIADIC_LIMIT_E >= 6

#if BSLSTL_OPTIONAL_VARIADIC_LIMIT_E >= 7
    template<class ARGS_01,
             class ARGS_02,
             class ARGS_03,
             class ARGS_04,
             class ARGS_05,
             class ARGS_06,
             class ARGS_07>
    void emplace(BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_01),
                 BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_02),
                 BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_03),
                 BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_04),
                 BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_05),
                 BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_06),
                 BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_07));
#endif  // BSLSTL_OPTIONAL_VARIADIC_LIMIT_E >= 7

#if BSLSTL_OPTIONAL_VARIADIC_LIMIT_E >= 8
    template<class ARGS_01,
             class ARGS_02,
             class ARGS_03,
             class ARGS_04,
             class ARGS_05,
             class ARGS_06,
             class ARGS_07,
             class ARGS_08>
    void emplace(BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_01),
                 BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_02),
                 BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_03),
                 BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_04),
                 BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_05),
                 BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_06),
                 BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_07),
                 BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_08));
#endif  // BSLSTL_OPTIONAL_VARIADIC_LIMIT_E >= 8

#if BSLSTL_OPTIONAL_VARIADIC_LIMIT_E >= 9
    template<class ARGS_01,
             class ARGS_02,
             class ARGS_03,
             class ARGS_04,
             class ARGS_05,
             class ARGS_06,
             class ARGS_07,
             class ARGS_08,
             class ARGS_09>
    void emplace(BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_01),
                 BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_02),
                 BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_03),
                 BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_04),
                 BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_05),
                 BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_06),
                 BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_07),
                 BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_08),
                 BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_09));
#endif  // BSLSTL_OPTIONAL_VARIADIC_LIMIT_E >= 9

#if BSLSTL_OPTIONAL_VARIADIC_LIMIT_E >= 10
    template<class ARGS_01,
             class ARGS_02,
             class ARGS_03,
             class ARGS_04,
             class ARGS_05,
             class ARGS_06,
             class ARGS_07,
             class ARGS_08,
             class ARGS_09,
             class ARGS_10>
    void emplace(BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_01),
                 BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_02),
                 BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_03),
                 BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_04),
                 BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_05),
                 BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_06),
                 BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_07),
                 BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_08),
                 BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_09),
                 BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_10));
#endif  // BSLSTL_OPTIONAL_VARIADIC_LIMIT_E >= 10


#if defined(BSLS_COMPILERFEATURES_SUPPORT_GENERALIZED_INITIALIZERS)
#if BSLSTL_OPTIONAL_VARIADIC_LIMIT_E >= 0
    template<class IL_TYPE>
    void emplace(std::initializer_list<IL_TYPE>);
#endif  // BSLSTL_OPTIONAL_VARIADIC_LIMIT_E >= 0

#if BSLSTL_OPTIONAL_VARIADIC_LIMIT_E >= 1
    template<class IL_TYPE, class ARGS_01>
    void emplace(std::initializer_list<IL_TYPE>,
                                   BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_01)
                                   );
#endif  // BSLSTL_OPTIONAL_VARIADIC_LIMIT_E >= 1

#if BSLSTL_OPTIONAL_VARIADIC_LIMIT_E >= 2
    template<class IL_TYPE, class ARGS_01,
                            class ARGS_02>
    void emplace(std::initializer_list<IL_TYPE>,
                                   BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_01)
                                   ,
                                   BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_02)
                                   );
#endif  // BSLSTL_OPTIONAL_VARIADIC_LIMIT_E >= 2

#if BSLSTL_OPTIONAL_VARIADIC_LIMIT_E >= 3
    template<class IL_TYPE, class ARGS_01,
                            class ARGS_02,
                            class ARGS_03>
    void emplace(std::initializer_list<IL_TYPE>,
                                   BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_01)
                                   ,
                                   BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_02)
                                   ,
                                   BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_03)
                                   );
#endif  // BSLSTL_OPTIONAL_VARIADIC_LIMIT_E >= 3

#if BSLSTL_OPTIONAL_VARIADIC_LIMIT_E >= 4
    template<class IL_TYPE, class ARGS_01,
                            class ARGS_02,
                            class ARGS_03,
                            class ARGS_04>
    void emplace(std::initializer_list<IL_TYPE>,
                                   BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_01)
                                   ,
                                   BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_02)
                                   ,
                                   BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_03)
                                   ,
                                   BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_04)
                                   );
#endif  // BSLSTL_OPTIONAL_VARIADIC_LIMIT_E >= 4

#if BSLSTL_OPTIONAL_VARIADIC_LIMIT_E >= 5
    template<class IL_TYPE, class ARGS_01,
                            class ARGS_02,
                            class ARGS_03,
                            class ARGS_04,
                            class ARGS_05>
    void emplace(std::initializer_list<IL_TYPE>,
                                   BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_01)
                                   ,
                                   BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_02)
                                   ,
                                   BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_03)
                                   ,
                                   BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_04)
                                   ,
                                   BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_05)
                                   );
#endif  // BSLSTL_OPTIONAL_VARIADIC_LIMIT_E >= 5

#if BSLSTL_OPTIONAL_VARIADIC_LIMIT_E >= 6
    template<class IL_TYPE, class ARGS_01,
                            class ARGS_02,
                            class ARGS_03,
                            class ARGS_04,
                            class ARGS_05,
                            class ARGS_06>
    void emplace(std::initializer_list<IL_TYPE>,
                                   BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_01)
                                   ,
                                   BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_02)
                                   ,
                                   BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_03)
                                   ,
                                   BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_04)
                                   ,
                                   BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_05)
                                   ,
                                   BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_06)
                                   );
#endif  // BSLSTL_OPTIONAL_VARIADIC_LIMIT_E >= 6

#if BSLSTL_OPTIONAL_VARIADIC_LIMIT_E >= 7
    template<class IL_TYPE, class ARGS_01,
                            class ARGS_02,
                            class ARGS_03,
                            class ARGS_04,
                            class ARGS_05,
                            class ARGS_06,
                            class ARGS_07>
    void emplace(std::initializer_list<IL_TYPE>,
                                   BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_01)
                                   ,
                                   BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_02)
                                   ,
                                   BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_03)
                                   ,
                                   BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_04)
                                   ,
                                   BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_05)
                                   ,
                                   BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_06)
                                   ,
                                   BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_07)
                                   );
#endif  // BSLSTL_OPTIONAL_VARIADIC_LIMIT_E >= 7

#if BSLSTL_OPTIONAL_VARIADIC_LIMIT_E >= 8
    template<class IL_TYPE, class ARGS_01,
                            class ARGS_02,
                            class ARGS_03,
                            class ARGS_04,
                            class ARGS_05,
                            class ARGS_06,
                            class ARGS_07,
                            class ARGS_08>
    void emplace(std::initializer_list<IL_TYPE>,
                                   BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_01)
                                   ,
                                   BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_02)
                                   ,
                                   BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_03)
                                   ,
                                   BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_04)
                                   ,
                                   BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_05)
                                   ,
                                   BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_06)
                                   ,
                                   BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_07)
                                   ,
                                   BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_08)
                                   );
#endif  // BSLSTL_OPTIONAL_VARIADIC_LIMIT_E >= 8

#if BSLSTL_OPTIONAL_VARIADIC_LIMIT_E >= 9
    template<class IL_TYPE, class ARGS_01,
                            class ARGS_02,
                            class ARGS_03,
                            class ARGS_04,
                            class ARGS_05,
                            class ARGS_06,
                            class ARGS_07,
                            class ARGS_08,
                            class ARGS_09>
    void emplace(std::initializer_list<IL_TYPE>,
                                   BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_01)
                                   ,
                                   BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_02)
                                   ,
                                   BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_03)
                                   ,
                                   BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_04)
                                   ,
                                   BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_05)
                                   ,
                                   BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_06)
                                   ,
                                   BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_07)
                                   ,
                                   BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_08)
                                   ,
                                   BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_09)
                                   );
#endif  // BSLSTL_OPTIONAL_VARIADIC_LIMIT_E >= 9

#if BSLSTL_OPTIONAL_VARIADIC_LIMIT_E >= 10
    template<class IL_TYPE, class ARGS_01,
                            class ARGS_02,
                            class ARGS_03,
                            class ARGS_04,
                            class ARGS_05,
                            class ARGS_06,
                            class ARGS_07,
                            class ARGS_08,
                            class ARGS_09,
                            class ARGS_10>
    void emplace(std::initializer_list<IL_TYPE>,
                                   BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_01)
                                   ,
                                   BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_02)
                                   ,
                                   BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_03)
                                   ,
                                   BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_04)
                                   ,
                                   BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_05)
                                   ,
                                   BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_06)
                                   ,
                                   BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_07)
                                   ,
                                   BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_08)
                                   ,
                                   BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_09)
                                   ,
                                   BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_10)
                                   );
#endif  // BSLSTL_OPTIONAL_VARIADIC_LIMIT_E >= 10

#endif
#else
// The generated code below is a workaround for the absence of perfect
// forwarding in some compilers.
    template<class... ARGS>
    void emplace(BSLS_COMPILERFEATURES_FORWARD_REF(ARGS)...);

#if defined(BSLS_COMPILERFEATURES_SUPPORT_GENERALIZED_INITIALIZERS)
    template<class IL_TYPE, class... ARGS>
    void emplace(std::initializer_list<IL_TYPE>,
                                        BSLS_COMPILERFEATURES_FORWARD_REF(ARGS)
                                        ...);
#endif
// }}} END GENERATED CODE
#endif
    void reset() BSLS_KEYWORD_NOEXCEPT;
        // Reset this object to the default constructed state (i.e., to a
        // disengaged state).

    //OBSERVERS
    TYPE& value() BSLS_KEYWORD_LVREF_QUAL;
        // Return a reference providing modifiable access to the underlying
        // 'TYPE' object.  Throws a 'bsl::bad_optional_access' if the
        // 'optional' object is disengaged.

#if defined(BSLS_COMPILERFEATURES_SUPPORT_REF_QUALIFIERS)
    TYPE&& value() BSLS_KEYWORD_RVREF_QUAL;
        // Return a reference providing modifiable access to the underlying
        // 'TYPE' object.  Throws a 'bsl::bad_optional_access' if the
        // 'optional' object is disengaged.
#endif


    optional& operator=(bsl::nullopt_t) BSLS_KEYWORD_NOEXCEPT;
        // Reset this object to be disengaged and return a reference providing
        // modifiable access to this object.

    optional& operator=(const optional& rhs);
    optional& operator=(BloombergLP::bslmf::MovableRef<optional> rhs);
        // If specified 'rhs' is engaged, assign its value to this object.
        // Otherwise, reset this object to a disengaged state.  Return a
        // reference providing modifiable access to this object.  Note that
        // this method may invoke assignment from 'rhs', or construction from
        // 'rhs', depending on whether this 'optional' object is engaged.

    template<class ANY_TYPE>
    BSLSTL_OPTIONAL_ENABLE_ASSIGN_FROM_OPTIONAL_LVAL &
    operator=(const optional<ANY_TYPE> &rhs)
        // If specified 'rhs' is engaged, assign its value to this object.
        // Otherwise, reset this object to a disengaged state.  Return a
        // reference providing modifiable access to this object.  Note that
        // this method may invoke assignment from 'rhs', or construction from
        // 'rhs', depending on whether this 'optional' object is engaged.
    {
        // Must be in-place inline because the use of 'enable_if' will
        // otherwise break the MSVC 2010 compiler.
        if (rhs.has_value()) {
            if (this->has_value()) {
                this->value() = rhs.value();
            }
            else {
                this->emplace(rhs.value());
            }
        }
        else {
            this->reset();
        }
        return *this;
    }

#ifdef BSLS_COMPILERFEATURES_SUPPORT_RVALUE_REFERENCES
    template<class ANY_TYPE>
    BSLSTL_OPTIONAL_ENABLE_ASSIGN_FROM_OPTIONAL_RVAL &
    operator=(optional<ANY_TYPE>&& rhs)
        // If specified 'rhs' is engaged, assign its value to this object.
        // Otherwise, reset this object to a disengaged state.  Return a
        // reference providing modifiable access to this object.  Note that
        // this method may invoke assignment from 'rhs', or construction from
        // 'rhs', depending on whether this 'optional' object is engaged.
    {
        // Must be in-place inline because the use of 'enable_if' will
        // otherwise break the MSVC 2010 compiler.
        if (rhs.has_value()) {
            if (this->has_value()) {
                this->value() = MoveUtil::move(rhs.value());
            }
            else {
                this->emplace(MoveUtil::move(rhs.value()));
            }
        }
        else {
            this->reset();
        }
        return *this;
    }

    template<class ANY_TYPE = TYPE>
    BSLSTL_OPTIONAL_ENABLE_ASSIGN_FROM_ANYTYPE &
    operator=(ANY_TYPE&& rhs)
        // Assign to this object the value of the specified 'rhs' object
        // converted to 'TYPE', and return a reference providing modifiable
        // access to this object.  Note that this method may invoke assignment
        // from 'rhs', or construction from 'rhs', depending on whether this
        // object is engaged.  BSLSTL_OPTIONAL_ENABLE_ASSIGN_FROM_ANYTYPE
        // contains a check that disables this overload if 'ANY_TYPE' is
        // 'optional<TYPE>'.  This is needed to prevent this assignment
        // operator being a better match for non const 'optional<TYPE>' lvalues
        // than 'operator=(const optional& rhs)'.  This function needs to be a
        // worse match than 'operator=(optional)' so cases like :
        //..
        //      bsl::optional<int> oi;
        //      oi = {};
        //..
        // represent assignment from a default constructed 'optional', as
        // opposed to assignment from default constructed 'value_type'.  Note
        // that in C++03, where there is no concept of perfect forwarding,
        // this is not a concern.
    {
        // Must be in-place inline because the use of 'enable_if' will
        // otherwise break the MSVC 2010 compiler.
        if (this->has_value()) {
            this->value() = std::forward<ANY_TYPE>(rhs);
        }
        else {
            this->emplace(std::forward<ANY_TYPE>(rhs));
        }
        return *this;
    }
#else
    // The existence of MovableRef in C++11 affects the above functions,
    // and they need to be defined in terms of rvalue references and perfect
    // forwarding. For C++03, the MovableRef overloads are provided below.

    optional& operator=(const TYPE& rhs);
    optional& operator=(BloombergLP::bslmf::MovableRef<TYPE> rhs);

    template<class ANY_TYPE>
    BSLSTL_OPTIONAL_ENABLE_ASSIGN_FROM_OPTIONAL_RVAL &
    operator=(BloombergLP::bslmf::MovableRef< optional<ANY_TYPE> > rhs)
        // If 'rhs' is engaged, assign its value to this object. Otherwise,
        // reset this object to a disengaged state. Return a reference
        // providing modifiable access to this object.  Note that this
        // method may invoke assignment from 'rhs', or construction from 'rhs',
        // depending on whether this 'optional' object is engaged.
    {
        // Must be in-place inline because the use of 'enable_if' will
        // otherwise break the MSVC 2010 compiler.
        optional<ANY_TYPE> lvalue = rhs;

        if (lvalue.has_value()) {
            if (this->has_value()) {
                this->value() = MoveUtil::move(lvalue.value());
            }
            else {
                this->emplace(MoveUtil::move(lvalue.value()));
            }
        }
        else {
            this->reset();
        }
        return *this;
    }

    template<class ANY_TYPE>
    optional&  operator=(const ANY_TYPE& rhs);
        // Assign to this object the value of the specified 'rhs' object (of
        // 'ANY_TYPE') converted to 'TYPE', and return a reference
        // providing modifiable access to this object.  Note that this
        // method may invoke assignment from 'rhs', or construction from 'rhs',
        // depending on whether this 'optional' object is engaged.


    template<class ANY_TYPE>
    optional&
    operator=(BloombergLP::bslmf::MovableRef< ANY_TYPE> rhs);
        // Assign to this object the value of the specified 'rhs' object (of
        // 'ANY_TYPE') converted to 'TYPE', and return a reference
        // providing modifiable access to this object.  Note that this
        // method may invoke assignment from 'rhs', or construction from 'rhs',
        // depending on whether this 'optional' object is engaged.

#endif

#ifdef __cpp_lib_optional
    template<class ANY_TYPE = TYPE>
    BSLSTL_OPTIONAL_ENABLE_ASSIGN_FROM_STD_OPTIONAL_LVAL &
    operator=(const std::optional<ANY_TYPE> &rhs)
        // If specified 'rhs' object is engaged, assign to this object the
        // result of 'rhs.value()' converted to 'TYPE'. Otherwise, disengage
        // this object.  Note that this method may invoke assignment from 'rhs'
        // , or construction from 'rhs', depending on whether this object is
        // engaged.
    {
        // Must be in-place inline because the use of 'enable_if' will
        // otherwise break the MSVC 2010 compiler.
        if (rhs.has_value()) {
            if (this->has_value()) {
                this->value() = rhs.value();
            }
            else {
                this->emplace(rhs.value());
            }
        }
        else {
            this->reset();
        }
        return *this;
    }

    template<class ANY_TYPE = TYPE>
    BSLSTL_OPTIONAL_ENABLE_ASSIGN_FROM_STD_OPTIONAL_RVAL &
    operator=(std::optional<ANY_TYPE>&& rhs)
        // If specified 'rhs' object is engaged, assign to this object the
        // result of 'rhs.value()' converted to 'TYPE'. Otherwise, disengage
        // this object.  Note that this method may invoke assignment from 'rhs'
        // , or construction from 'rhs', depending on whether this object is
        // engaged.  Using rvalue reference instead of movableRef ensures this
        // overload is considered a better match over 'ANY_TYPE' overloads for
        // optional types.
    {
        // Must be in-place inline because the use of 'enable_if' will
        // otherwise break the MSVC 2010 compiler.
        if (rhs.has_value()) {
            if (this->has_value()) {
                this->value() = std::move(rhs.value());
            }
            else {
                this->emplace(std::move(rhs.value()));
            }
        }
        else {
            this->reset();
        }
        return *this;
    }
#endif // __cpp_lib_optional

    TYPE* operator->();
        // Return a pointer providing modifiable access to the underlying
        // 'TYPE' object.  The behaviour is undefined if the 'optional' object
        // is disengaged.

    TYPE& operator*() BSLS_KEYWORD_LVREF_QUAL;
        // Return a reference providing modifiable access to the underlying
        // 'TYPE' object.  The behavior is undefined if the 'optional' object
        // is disengaged.

#if defined(BSLS_COMPILERFEATURES_SUPPORT_REF_QUALIFIERS)
    TYPE&& operator*() BSLS_KEYWORD_RVREF_QUAL;
        // Return a reference providing modifiable access to the underlying
        // 'TYPE' object.  The behavior is undefined if the 'optional' object
        // is disengaged.
#endif

    void swap(optional& other);
        // Efficiently exchange the value of this object with the value of the
        // specified 'other' object.  In effect, performs
        // 'using bsl::swap; swap(c, other.c);'.

#if defined(BSLS_COMPILERFEATURES_SUPPORT_REF_QUALIFIERS)
    template<class ANY_TYPE>
    TYPE value_or(ANY_TYPE&& rhs) BSLS_KEYWORD_RVREF_QUAL;
        // If 'this->has_value() == true', return a copy of the contained
        // 'value_type' object.  Otherwise, return the value of specified 'rhs'
        // converted to 'value_type'.
#endif

#ifdef  BSL_COMPILERFEATURES_GUARANTEED_COPY_ELISION
#if defined(BSLS_COMPILERFEATURES_SUPPORT_REF_QUALIFIERS)
    template<class ANY_TYPE>
    TYPE value_or(bsl::allocator_arg_t,
                  allocator_type,
                  ANY_TYPE&&) BSLS_KEYWORD_RVREF_QUAL;
        // If 'this->has_value() == true', return a copy of the contained
        // 'value_type' object created using the provided allocator.
        // Otherwise, return the value of specified 'rhs' converted to
        // 'value_type' and use the specified 'allocator' for the returned
        // object.
#endif
#endif //BSL_COMPILERFEATURES_GUARANTEED_COPY_ELISION

    // ACCESSORS

    const TYPE& value() const BSLS_KEYWORD_LVREF_QUAL;
        // Return a reference providing non-modifiable access to the underlying
        // 'TYPE' object.  Throws a 'bsl::bad_optional_access' if the
        // 'optional' object is disengaged.

#if defined(BSLS_COMPILERFEATURES_SUPPORT_REF_QUALIFIERS)
    const TYPE&& value() const BSLS_KEYWORD_RVREF_QUAL;
        // Return a reference providing non-modifiable access to the underlying
        // 'TYPE' object.  Throws a 'bsl::bad_optional_access' if the
        // 'optional' object is disengaged.
#endif

    const TYPE* operator->() const;
        // Return a pointer providing non-modifiable access to the underlying
        // 'TYPE' object.  The behaviour is undefined if the 'optional' object
        // is disengaged.

    const TYPE& operator*() const BSLS_KEYWORD_LVREF_QUAL;
        // Return a reference providing non-modifiable access to the underlying
        // 'TYPE' object.  The behavior is undefined if the 'optional' object
        // is disengaged.

#if defined(BSLS_COMPILERFEATURES_SUPPORT_REF_QUALIFIERS)
    const TYPE&& operator*() const BSLS_KEYWORD_RVREF_QUAL;
        // Return a reference providing non-modifiable access to the underlying
        // 'TYPE' object.  The behavior is undefined if the 'optional' object
        // is disengaged.
#endif

    allocator_type get_allocator() const BSLS_KEYWORD_NOEXCEPT;
        // Return allocator used for construction of 'value_type'.

    bool has_value() const BSLS_KEYWORD_NOEXCEPT;
        // Return 'true' if this object is disengaged, and 'false' otherwise.

#ifdef BSLS_COMPILERFEATURES_SUPPORT_OPERATOR_EXPLICIT
    explicit operator bool()  const  BSLS_KEYWORD_NOEXCEPT;
        // Return 'true' if this object is disengaged, and 'false' otherwise.
#else
    // Simulation of explicit conversion to bool.
    // Inlined to work around xlC bug when out-of-line.
    operator UnspecifiedBool() const BSLS_NOTHROW_SPEC
    {
        return UnspecifiedBoolUtil::makeValue(has_value());
    }
#endif //#ifdef BSLS_COMPILERFEATURES_SUPPORT_OPERATOR_EXPLICIT


    template<class ANY_TYPE>
    TYPE value_or(BSLS_COMPILERFEATURES_FORWARD_REF(ANY_TYPE) rhs) const
    BSLS_KEYWORD_LVREF_QUAL;
        // If 'this->has_value() == true', return a copy of the contained
        // 'value_type' object.  Otherwise, return the value of specified 'rhs'
        // converted to 'value_type'.

#ifdef  BSL_COMPILERFEATURES_GUARANTEED_COPY_ELISION
    template<class ANY_TYPE>
    TYPE value_or(bsl::allocator_arg_t,
                  allocator_type,
                  BSLS_COMPILERFEATURES_FORWARD_REF(ANY_TYPE)
                  ) const BSLS_KEYWORD_LVREF_QUAL;
        // If 'this->has_value() == true', return a copy of the contained
        // 'value_type' object created using the provided allocator.
        // Otherwise, return the value of specified 'rhs' converted to
        // 'value_type' and  use the specified 'allocator' for the returned
        // object.
#endif //BSL_COMPILERFEATURES_GUARANTEED_COPY_ELISION
  private :

};

                        // ====================
                        // class optional<TYPE>
                        // ====================

#ifdef __cpp_lib_optional
template <class TYPE>
class optional<TYPE, false> : public std::optional<TYPE> {

    typedef std::optional<TYPE> optionalBase;
  public :
    using optionalBase::optionalBase;

    optional(const optional& rhs);                                  // IMPLICIT
        // If 'rhs' contains a value, initialize the 'value_type' object using
        // '*rhs'. Otherwise, create a disengaged 'optional'.  Needed because
        // we declare a move assignment operator.

    optional(optional&& rhs);          // IMPLICIT
        // If 'rhs' contains a value, initialize the 'value_type' object by
        // move construction from '*rhs'. Otherwise, create a disengaged
        // 'optional'. 'rhs' is left in a valid, but unspecified state.


    template<class ANY_TYPE = TYPE>
    optional(const std::optional<ANY_TYPE>& rhs
            BSLSTL_OPTIONAL_ENABLE_IF_CONSTRUCT_FROM_STD_OPTIONAL_LVAL
            BSLSTL_OPTIONAL_ENABLE_IF_NOT_EXPLICIT_CONSTRUCT(TYPE, ANY_TYPE))
       // If specified 'rhs' contains a value, initialize the 'value_type'
       // object by moving from '*rhs'.  Otherwise, create a disengaged
       // 'optional'.  Use the currently installed default allocator to supply
       // memory for this and any future 'value_type' objects.
    : optionalBase(rhs)
    {
       // Must be in-place inline because the use of 'enable_if' will
       // otherwise break the MSVC 2010 compiler.
    }

    template<class ANY_TYPE = TYPE>
    explicit
    optional(const std::optional<ANY_TYPE>& rhs
            BSLSTL_OPTIONAL_ENABLE_IF_CONSTRUCT_FROM_STD_OPTIONAL_LVAL
            BSLSTL_OPTIONAL_ENABLE_IF_EXPLICIT_CONSTRUCT(TYPE, ANY_TYPE))
       // If specified 'rhs' contains a value, initialize the 'value_type'
       // object by moving from '*rhs'.  Otherwise, create a disengaged
       // 'optional'.  Use the currently installed default allocator to supply
       // memory for this and any future 'value_type' objects.
    : optionalBase(rhs)
    {
       // Must be in-place inline because the use of 'enable_if' will
       // otherwise break the MSVC 2010 compiler.
    }


    template<class ANY_TYPE = TYPE>
    optional(std::optional<ANY_TYPE>&& rhs
            BSLSTL_OPTIONAL_ENABLE_IF_CONSTRUCT_FROM_STD_OPTIONAL_RVAL
            BSLSTL_OPTIONAL_ENABLE_IF_NOT_EXPLICIT_CONSTRUCT(TYPE, ANY_TYPE))
       // If specified 'rhs' contains a value, initialize the 'value_type'
       // object by moving from '*rhs'.  Otherwise, create a disengaged
       // 'optional'.  Use the currently installed default allocator to supply
       // memory for this and any future 'value_type' objects.
    : optionalBase(std::forward<std::optional<ANY_TYPE>>(rhs))
    {
       // Must be in-place inline because the use of 'enable_if' will
       // otherwise break the MSVC 2010 compiler.
    }

    template<class ANY_TYPE = TYPE>
    explicit
    optional(std::optional<ANY_TYPE>&& rhs
            BSLSTL_OPTIONAL_ENABLE_IF_CONSTRUCT_FROM_STD_OPTIONAL_RVAL
            BSLSTL_OPTIONAL_ENABLE_IF_EXPLICIT_CONSTRUCT(TYPE, ANY_TYPE))
       // If specified 'rhs' contains a value, initialize the 'value_type'
       // object by moving from '*rhs'.  Otherwise, create a disengaged
       // 'optional'.  Use the currently installed default allocator to supply
       // memory for this and any future 'value_type' objects.
    : optionalBase(std::forward<std::optional<ANY_TYPE>>(rhs))
    {
       // Must be in-place inline because the use of 'enable_if' will
       // otherwise break the MSVC 2010 compiler.
    }

    optional& operator=(bsl::nullopt_t) BSLS_KEYWORD_NOEXCEPT;
        // reset the optional to a disengaged state.

    optional& operator=(const optional& rhs);
    optional& operator=(optional&& rhs);
        // Assign to this object the value of the specified 'rhs' object.  Note
        // that this method may invoke assignment from 'rhs', or construction
        // from 'rhs', depending on whether the current object is engaged.

    template<class ANY_TYPE>
    BSLSTL_OPTIONAL_ENABLE_ASSIGN_FROM_OPTIONAL_LVAL &
    operator=(const optional<ANY_TYPE> &rhs)
        // If specified 'rhs' object is engaged, assign to this object the
        // result of 'rhs.value()' converted to 'TYPE'. Otherwise, disengage
        // this object.  Note that this method may invoke assignment from 'rhs'
        // , or construction from 'rhs', depending on whether this object is
        // engaged.
    {
        // Must be in-place inline because the use of 'enable_if' will
        // otherwise break the MSVC 2010 compiler.
        if (rhs.has_value()) {
            if (this->has_value()) {
                this->value() = rhs.value();
            }
            else {
                this->emplace(rhs.value());
            }
        }
        else {
            this->reset();
        }
        return *this;
    }

    template<class ANY_TYPE>
    BSLSTL_OPTIONAL_ENABLE_ASSIGN_FROM_OPTIONAL_RVAL &
    operator=(optional<ANY_TYPE>&& rhs)
        // If specified 'rhs' object is engaged, assign to this object the
        // result of 'rhs.value()' converted to 'TYPE'. Otherwise, disengage
        // this object.  Note that this method may invoke assignment from 'rhs'
        // , or construction from 'rhs', depending on whether this object is
        // engaged.  Using rvalue reference instead of movableRef ensures this
        // overload is considered a better match over 'ANY_TYPE' overloads for
        // optional types.
    {
        // Must be in-place inline because the use of 'enable_if' will
        // otherwise break the MSVC 2010 compiler.
        if (rhs.has_value()) {
            if (this->has_value()) {
                this->value() = std::move(rhs.value());
            }
            else {
                this->emplace(std::move(rhs.value()));
            }
        }
        else {
            this->reset();
        }
        return *this;
    }

    template<class ANY_TYPE = TYPE>
    BSLSTL_OPTIONAL_ENABLE_ASSIGN_FROM_STD_OPTIONAL_LVAL &
    operator=(const std::optional<ANY_TYPE> &rhs)
        // If specified 'rhs' object is engaged, assign to this object the
        // result of 'rhs.value()' converted to 'TYPE'. Otherwise, disengage
        // this object.  Note that this method may invoke assignment from 'rhs'
        // , or construction from 'rhs', depending on whether this object is
        // engaged.
    {
        // Must be in-place inline because the use of 'enable_if' will
        // otherwise break the MSVC 2010 compiler.
        if (rhs.has_value()) {
            if (this->has_value()) {
                this->value() = rhs.value();
            }
            else {
                this->emplace(rhs.value());
            }
        }
        else {
            this->reset();
        }
        return *this;
    }

    template<class ANY_TYPE = TYPE>
    BSLSTL_OPTIONAL_ENABLE_ASSIGN_FROM_STD_OPTIONAL_RVAL &
    operator=(std::optional<ANY_TYPE>&& rhs)
        // If specified 'rhs' object is engaged, assign to this object the
        // result of 'rhs.value()' converted to 'TYPE'. Otherwise, disengage
        // this object.  Note that this method may invoke assignment from 'rhs'
        // , or construction from 'rhs', depending on whether this object is
        // engaged.  Using rvalue reference instead of movableRef ensures this
        // overload is considered a better match over 'ANY_TYPE' overloads for
        // optional types.
    {
        // Must be in-place inline because the use of 'enable_if' will
        // otherwise break the MSVC 2010 compiler.
        if (rhs.has_value()) {
            if (this->has_value()) {
                this->value() = std::move(rhs.value());
            }
            else {
                this->emplace(std::move(rhs.value()));
            }
        }
        else {
            this->reset();
        }
        return *this;
    }

    template<class ANY_TYPE = TYPE>
    BSLSTL_OPTIONAL_ENABLE_ASSIGN_FROM_ANYTYPE &
    operator=(ANY_TYPE&& rhs)
        // Assign to this object the value of the specified 'rhs' object
        // converted to 'TYPE', and return a reference providing modifiable
        // access to this object.  Note that this method may invoke assignment
        // from 'rhs', or construction from 'rhs', depending on whether this
        // object is engaged.  BSLSTL_OPTIONAL_ENABLE_ASSIGN_FROM_ANYTYPE
        // contains a check that disables this overload if 'ANY_TYPE' is
        // 'optional<TYPE>'.  This is needed to prevent this assignment
        // operator being a better match for non const 'optional<TYPE>' lvalues
        // than 'operator=(const optional& rhs)'.  This function needs to be a
        // worse match than 'operator=(optional)' so cases like :
        //..
        //      bsl::optional<int> oi;
        //      oi = {};
        //..
        // represent assignment from a default constructed 'optional', as
        // opposed to assignment from default constructed 'value_type'.  Note
        // that in C++03, where there is no concept of perfect forwarding,
        // this is not a concern.
    {
        // Must be in-place inline because the use of 'enable_if' will
        // otherwise break the MSVC 2010 compiler.
        if (this->has_value()) {
            this->value() = std::forward<ANY_TYPE>(rhs);
        }
        else {
            this->emplace(std::forward<ANY_TYPE>(rhs));
        }
        return *this;
    }

};
#else

template <class TYPE>
class optional<TYPE, false> {
    // specialization of 'optional' for 'value_type' that is not allocator
    // aware.
  public :
    // PUBLIC TYPES
    typedef TYPE value_type;
    // 'value_type' is an alias for the underlying 'TYPE' upon which this
    // template class is instantiated, and represents the type of the managed
    // object.  The name is chosen so it is compatible with the 'std::optional'
    // implementation.

  private:
    // PRIVATE TYPES
    typedef BloombergLP::bslmf::MovableRefUtil MoveUtil;

#ifndef BSLS_COMPILERFEATURES_SUPPORT_OPERATOR_EXPLICIT
    // UNSPECIFIED BOOL

    // This type is needed only in C++03 mode, where 'explicit' conversion
    // operators are not supported.  A 'function' is implicitly converted to
    // 'UnspecifiedBool' when used in 'if' statements, but is not implicitly
    // convertible to 'bool'.
    typedef BloombergLP::bsls::UnspecifiedBool<optional>
                                                          UnspecifiedBoolUtil;
    typedef typename UnspecifiedBoolUtil::BoolType        UnspecifiedBool;

#endif

    // DATA
    BloombergLP::bslstl::Optional_Data<TYPE>  d_value;
                                                 // in-place 'TYPE' object

  public:

    // TRAITS
    BSLMF_NESTED_TRAIT_DECLARATION_IF(optional,
        BloombergLP::bslmf::IsBitwiseMoveable,
        BloombergLP::bslmf::IsBitwiseMoveable<TYPE>::value);

    // CREATORS
    optional();
        // Create a disengaged 'optional' object.

    optional(bsl::nullopt_t );                                      // IMPLICIT
        // Create a disengaged 'optional' object.

    optional(const optional& rhs);                                  // IMPLICIT
        // If 'rhs' contains a value, initialize the 'value_type' object using
        // '*rhs'. Otherwise, create a disengaged 'optional'.

    optional(BloombergLP::bslmf::MovableRef<optional> rhs);         // IMPLICIT
        // If 'rhs' contains a value, initialize the 'value_type' object by
        // move construction from '*rhs'. Otherwise, create a disengaged
        // 'optional'. 'rhs' is left in a valid, but unspecified state.

    template<class ANY_TYPE>
    optional(BSLS_COMPILERFEATURES_FORWARD_REF(ANY_TYPE) rhs
             BSLSTL_OPTIONAL_ENABLE_IF_SAME(ANY_TYPE, TYPE)
             BSLSTL_OPTIONAL_ENABLE_IF_NOT_EXPLICIT_CONSTRUCT(TYPE, ANY_TYPE))
        // Create an 'optional' object having the same value as the specified
        // 'rhs' object by forwarding the contents of 'rhs' to the
        // newly-created object.
    {
        // Must be in-place inline because the use of 'enable_if' will
        // otherwise break the MSVC 2010 compiler.
        this->emplace(BSLS_COMPILERFEATURES_FORWARD(ANY_TYPE, rhs));
    }

    template<class ANY_TYPE>
    explicit
    optional(BSLS_COMPILERFEATURES_FORWARD_REF(ANY_TYPE) rhs
             BSLSTL_OPTIONAL_ENABLE_IF_SAME(ANY_TYPE, TYPE)
             BSLSTL_OPTIONAL_ENABLE_IF_EXPLICIT_CONSTRUCT(TYPE, ANY_TYPE))
        // Create an 'optional' object having the same value as the specified
        // 'rhs' object by forwarding the contents of 'rhs' to the
        // newly-created object.
    {
        // Must be in-place inline because the use of 'enable_if' will
        // otherwise break the MSVC 2010 compiler.
        this->emplace(BSLS_COMPILERFEATURES_FORWARD(ANY_TYPE, rhs));
    }

    template<class ANY_TYPE>
    optional(BSLS_COMPILERFEATURES_FORWARD_REF(ANY_TYPE) rhs
             BSLSTL_OPTIONAL_ENABLE_IF_CONSTRUCT_FROM_ANYTYPE
             BSLSTL_OPTIONAL_ENABLE_IF_NOT_EXPLICIT_CONSTRUCT(TYPE, ANY_TYPE)
)
        // Create an 'optional' object having the same value as the specified
        // 'rhs' object by forwarding the contents of 'rhs' to the
        // newly-created object.
    {
        // Must be in-place inline because the use of 'enable_if' will
        // otherwise break the MSVC 2010 compiler.
        this->emplace(BSLS_COMPILERFEATURES_FORWARD(ANY_TYPE, rhs));
    }

    template<class ANY_TYPE>
    explicit
    optional(BSLS_COMPILERFEATURES_FORWARD_REF(ANY_TYPE) rhs
             BSLSTL_OPTIONAL_ENABLE_IF_CONSTRUCT_FROM_ANYTYPE
             BSLSTL_OPTIONAL_ENABLE_IF_EXPLICIT_CONSTRUCT(TYPE, ANY_TYPE))
        // Create an 'optional' object having the same value as the specified
        // 'rhs' object by forwarding the contents of 'rhs' to the
        // newly-created object.
    {
        // Must be in-place inline because the use of 'enable_if' will
        // otherwise break the MSVC 2010 compiler.
        this->emplace(BSLS_COMPILERFEATURES_FORWARD(ANY_TYPE, rhs));
    }

    template<class ANY_TYPE>
    optional(const optional<ANY_TYPE>& rhs
             BSLSTL_OPTIONAL_ENABLE_IF_CONSTRUCT_FROM_OPTIONAL_LVAL
             BSLSTL_OPTIONAL_ENABLE_IF_NOT_EXPLICIT_CONSTRUCT(TYPE,
                                                              const ANY_TYPE&))
        // If specified 'rhs' contains a value, initialize the 'value_type'
        // object using '*rhs'.  Otherwise, create a disengaged 'optional'.
    {
        // Must be in-place inline because the use of 'enable_if' will
        // otherwise break the MSVC 2010 compiler.
        if (rhs.has_value()) {
            emplace(rhs.value());
        }
    }

    template<class ANY_TYPE>
    explicit
    optional(const optional<ANY_TYPE>& rhs
             BSLSTL_OPTIONAL_ENABLE_IF_CONSTRUCT_FROM_OPTIONAL_LVAL
             BSLSTL_OPTIONAL_ENABLE_IF_EXPLICIT_CONSTRUCT(TYPE,
                                                          const ANY_TYPE&))
        // If specified 'rhs' contains a value, initialize the 'value_type'
        // object using '*rhs'.  Otherwise, create a disengaged 'optional'.
    {
        // Must be in-place inline because the use of 'enable_if' will
        // otherwise break the MSVC 2010 compiler.
        if (rhs.has_value()) {
            emplace(rhs.value());
        }
    }

#ifdef BSLS_COMPILERFEATURES_SUPPORT_RVALUE_REFERENCES
    // 'MovableRef' prevents correct type deduction in C++11 when used with
    // 'optional<ANY_TYPE>'.  These constructors needs to be defined in terms
    // of rvalue reference in C++11.  In C++03, this type deduction issue does
    // not exist due to the nature of C++03 MovableRef implementation and
    // usage.  Consequently, a 'MovableRef' equivalent constructors needs to be
    // provided in C++03 (see below).
    template<class ANY_TYPE>
    optional(optional<ANY_TYPE>&& rhs
             BSLSTL_OPTIONAL_ENABLE_IF_CONSTRUCT_FROM_OPTIONAL_RVAL
             BSLSTL_OPTIONAL_ENABLE_IF_NOT_EXPLICIT_CONSTRUCT(TYPE, ANY_TYPE))
        // If specified 'rhs' contains a value, initialize the 'value_type'
        // object by moving from '*rhs'.  Otherwise, create a disengaged
        // 'optional'.  The 'rhs' parameter can not be specified in terms of
        // MovableRef as that prevents making this overload being a better
        // match for optional types.
    {
        // Must be in-place inline because the use of 'enable_if' will
        // otherwise break the MSVC 2010 compiler.
        if (rhs.has_value()) {
            emplace(MoveUtil::move(rhs.value()));
        }
    }

    template<class ANY_TYPE>
    explicit
    optional(optional<ANY_TYPE>&& rhs
             BSLSTL_OPTIONAL_ENABLE_IF_CONSTRUCT_FROM_OPTIONAL_RVAL
             BSLSTL_OPTIONAL_ENABLE_IF_EXPLICIT_CONSTRUCT(TYPE, ANY_TYPE))
        // If specified 'rhs' contains a value, initialize the 'value_type'
        // object by moving from '*rhs'.  Otherwise, create a disengaged
        // 'optional'.  The 'rhs' parameter can not be specified in terms of
        // MovableRef as that prevents making this overload being a better
        // match for optional types.
    {
        // Must be in-place inline because the use of 'enable_if' will
        // otherwise break the MSVC 2010 compiler.
        if (rhs.has_value()) {
            emplace(MoveUtil::move(rhs.value()));
        }
    }
#else
    template<class ANY_TYPE>
    optional(BloombergLP::bslmf::MovableRef<optional<ANY_TYPE> > rhs
             BSLSTL_OPTIONAL_ENABLE_IF_CONSTRUCT_FROM_OPTIONAL_RVAL
             BSLSTL_OPTIONAL_ENABLE_IF_NOT_EXPLICIT_CONSTRUCT(TYPE, ANY_TYPE))
        // If specified 'rhs' contains a value, initialize the 'value_type'
        // object by moving from '*rhs'.  Otherwise, create a disengaged
        // 'optional'.
    {
        // Must be in-place inline because the use of 'enable_if' will
        // otherwise break the MSVC 2010 compiler.
        optional<ANY_TYPE> lvalue = rhs;
        if (lvalue.has_value()) {
            emplace(MoveUtil::move(lvalue.value()));
        }
    }

    template<class ANY_TYPE>
    explicit
    optional(BloombergLP::bslmf::MovableRef<optional<ANY_TYPE> > rhs
             BSLSTL_OPTIONAL_ENABLE_IF_CONSTRUCT_FROM_OPTIONAL_RVAL
             BSLSTL_OPTIONAL_ENABLE_IF_EXPLICIT_CONSTRUCT(TYPE, ANY_TYPE))
        // If specified 'rhs' contains a value, initialize the 'value_type'
        // object by moving from '*rhs'.  Otherwise, create a disengaged
        // 'optional'.
    {
        // Must be in-place inline because the use of 'enable_if' will
        // otherwise break the MSVC 2010 compiler.
        optional<ANY_TYPE> lvalue = rhs;
        if (lvalue.has_value()) {
            emplace(MoveUtil::move(lvalue.value()));
        }
    }
#endif //BSLS_COMPILERFEATURES_SUPPORT_RVALUE_REFERENCES


#if !BSLS_COMPILERFEATURES_SIMULATE_CPP11_FEATURES
    template<class... ARGS>
    explicit optional(bsl::in_place_t,
                      BSLS_COMPILERFEATURES_FORWARD_REF(ARGS)...);
#if defined(BSLS_COMPILERFEATURES_SUPPORT_GENERALIZED_INITIALIZERS)
    template<class IL_TYPE, class... ARGS>
    explicit optional(bsl::in_place_t,
                      std::initializer_list<IL_TYPE>,
                      BSLS_COMPILERFEATURES_FORWARD_REF(ARGS)...);
#endif //BSLS_COMPILERFEATURES_SUPPORT_GENERALIZED_INITIALIZERS
#elif BSLS_COMPILERFEATURES_SIMULATE_VARIADIC_TEMPLATES
// {{{ BEGIN GENERATED CODE
// The following section is automatically generated.  **DO NOT EDIT**
// Generator command line: sim_cpp11_features.pl bslstl_optional.h
#ifndef BSLSTL_OPTIONAL_VARIADIC_LIMIT
#define BSLSTL_OPTIONAL_VARIADIC_LIMIT 10
#endif
#ifndef BSLSTL_OPTIONAL_VARIADIC_LIMIT_F
#define BSLSTL_OPTIONAL_VARIADIC_LIMIT_F BSLSTL_OPTIONAL_VARIADIC_LIMIT
#endif
#if BSLSTL_OPTIONAL_VARIADIC_LIMIT_F >= 0
    explicit optional(bsl::in_place_t);
#endif  // BSLSTL_OPTIONAL_VARIADIC_LIMIT_F >= 0

#if BSLSTL_OPTIONAL_VARIADIC_LIMIT_F >= 1
    template<class ARGS_01>
    explicit optional(bsl::in_place_t,
                      BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_01));
#endif  // BSLSTL_OPTIONAL_VARIADIC_LIMIT_F >= 1

#if BSLSTL_OPTIONAL_VARIADIC_LIMIT_F >= 2
    template<class ARGS_01,
             class ARGS_02>
    explicit optional(bsl::in_place_t,
                      BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_01),
                      BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_02));
#endif  // BSLSTL_OPTIONAL_VARIADIC_LIMIT_F >= 2

#if BSLSTL_OPTIONAL_VARIADIC_LIMIT_F >= 3
    template<class ARGS_01,
             class ARGS_02,
             class ARGS_03>
    explicit optional(bsl::in_place_t,
                      BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_01),
                      BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_02),
                      BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_03));
#endif  // BSLSTL_OPTIONAL_VARIADIC_LIMIT_F >= 3

#if BSLSTL_OPTIONAL_VARIADIC_LIMIT_F >= 4
    template<class ARGS_01,
             class ARGS_02,
             class ARGS_03,
             class ARGS_04>
    explicit optional(bsl::in_place_t,
                      BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_01),
                      BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_02),
                      BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_03),
                      BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_04));
#endif  // BSLSTL_OPTIONAL_VARIADIC_LIMIT_F >= 4

#if BSLSTL_OPTIONAL_VARIADIC_LIMIT_F >= 5
    template<class ARGS_01,
             class ARGS_02,
             class ARGS_03,
             class ARGS_04,
             class ARGS_05>
    explicit optional(bsl::in_place_t,
                      BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_01),
                      BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_02),
                      BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_03),
                      BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_04),
                      BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_05));
#endif  // BSLSTL_OPTIONAL_VARIADIC_LIMIT_F >= 5

#if BSLSTL_OPTIONAL_VARIADIC_LIMIT_F >= 6
    template<class ARGS_01,
             class ARGS_02,
             class ARGS_03,
             class ARGS_04,
             class ARGS_05,
             class ARGS_06>
    explicit optional(bsl::in_place_t,
                      BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_01),
                      BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_02),
                      BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_03),
                      BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_04),
                      BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_05),
                      BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_06));
#endif  // BSLSTL_OPTIONAL_VARIADIC_LIMIT_F >= 6

#if BSLSTL_OPTIONAL_VARIADIC_LIMIT_F >= 7
    template<class ARGS_01,
             class ARGS_02,
             class ARGS_03,
             class ARGS_04,
             class ARGS_05,
             class ARGS_06,
             class ARGS_07>
    explicit optional(bsl::in_place_t,
                      BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_01),
                      BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_02),
                      BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_03),
                      BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_04),
                      BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_05),
                      BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_06),
                      BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_07));
#endif  // BSLSTL_OPTIONAL_VARIADIC_LIMIT_F >= 7

#if BSLSTL_OPTIONAL_VARIADIC_LIMIT_F >= 8
    template<class ARGS_01,
             class ARGS_02,
             class ARGS_03,
             class ARGS_04,
             class ARGS_05,
             class ARGS_06,
             class ARGS_07,
             class ARGS_08>
    explicit optional(bsl::in_place_t,
                      BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_01),
                      BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_02),
                      BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_03),
                      BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_04),
                      BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_05),
                      BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_06),
                      BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_07),
                      BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_08));
#endif  // BSLSTL_OPTIONAL_VARIADIC_LIMIT_F >= 8

#if BSLSTL_OPTIONAL_VARIADIC_LIMIT_F >= 9
    template<class ARGS_01,
             class ARGS_02,
             class ARGS_03,
             class ARGS_04,
             class ARGS_05,
             class ARGS_06,
             class ARGS_07,
             class ARGS_08,
             class ARGS_09>
    explicit optional(bsl::in_place_t,
                      BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_01),
                      BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_02),
                      BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_03),
                      BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_04),
                      BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_05),
                      BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_06),
                      BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_07),
                      BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_08),
                      BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_09));
#endif  // BSLSTL_OPTIONAL_VARIADIC_LIMIT_F >= 9

#if BSLSTL_OPTIONAL_VARIADIC_LIMIT_F >= 10
    template<class ARGS_01,
             class ARGS_02,
             class ARGS_03,
             class ARGS_04,
             class ARGS_05,
             class ARGS_06,
             class ARGS_07,
             class ARGS_08,
             class ARGS_09,
             class ARGS_10>
    explicit optional(bsl::in_place_t,
                      BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_01),
                      BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_02),
                      BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_03),
                      BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_04),
                      BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_05),
                      BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_06),
                      BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_07),
                      BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_08),
                      BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_09),
                      BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_10));
#endif  // BSLSTL_OPTIONAL_VARIADIC_LIMIT_F >= 10

#if defined(BSLS_COMPILERFEATURES_SUPPORT_GENERALIZED_INITIALIZERS)
#if BSLSTL_OPTIONAL_VARIADIC_LIMIT_F >= 0
    template<class IL_TYPE>
    explicit optional(bsl::in_place_t,
                      std::initializer_list<IL_TYPE>);
#endif  // BSLSTL_OPTIONAL_VARIADIC_LIMIT_F >= 0

#if BSLSTL_OPTIONAL_VARIADIC_LIMIT_F >= 1
    template<class IL_TYPE, class ARGS_01>
    explicit optional(bsl::in_place_t,
                      std::initializer_list<IL_TYPE>,
                      BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_01));
#endif  // BSLSTL_OPTIONAL_VARIADIC_LIMIT_F >= 1

#if BSLSTL_OPTIONAL_VARIADIC_LIMIT_F >= 2
    template<class IL_TYPE, class ARGS_01,
                            class ARGS_02>
    explicit optional(bsl::in_place_t,
                      std::initializer_list<IL_TYPE>,
                      BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_01),
                      BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_02));
#endif  // BSLSTL_OPTIONAL_VARIADIC_LIMIT_F >= 2

#if BSLSTL_OPTIONAL_VARIADIC_LIMIT_F >= 3
    template<class IL_TYPE, class ARGS_01,
                            class ARGS_02,
                            class ARGS_03>
    explicit optional(bsl::in_place_t,
                      std::initializer_list<IL_TYPE>,
                      BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_01),
                      BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_02),
                      BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_03));
#endif  // BSLSTL_OPTIONAL_VARIADIC_LIMIT_F >= 3

#if BSLSTL_OPTIONAL_VARIADIC_LIMIT_F >= 4
    template<class IL_TYPE, class ARGS_01,
                            class ARGS_02,
                            class ARGS_03,
                            class ARGS_04>
    explicit optional(bsl::in_place_t,
                      std::initializer_list<IL_TYPE>,
                      BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_01),
                      BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_02),
                      BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_03),
                      BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_04));
#endif  // BSLSTL_OPTIONAL_VARIADIC_LIMIT_F >= 4

#if BSLSTL_OPTIONAL_VARIADIC_LIMIT_F >= 5
    template<class IL_TYPE, class ARGS_01,
                            class ARGS_02,
                            class ARGS_03,
                            class ARGS_04,
                            class ARGS_05>
    explicit optional(bsl::in_place_t,
                      std::initializer_list<IL_TYPE>,
                      BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_01),
                      BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_02),
                      BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_03),
                      BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_04),
                      BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_05));
#endif  // BSLSTL_OPTIONAL_VARIADIC_LIMIT_F >= 5

#if BSLSTL_OPTIONAL_VARIADIC_LIMIT_F >= 6
    template<class IL_TYPE, class ARGS_01,
                            class ARGS_02,
                            class ARGS_03,
                            class ARGS_04,
                            class ARGS_05,
                            class ARGS_06>
    explicit optional(bsl::in_place_t,
                      std::initializer_list<IL_TYPE>,
                      BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_01),
                      BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_02),
                      BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_03),
                      BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_04),
                      BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_05),
                      BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_06));
#endif  // BSLSTL_OPTIONAL_VARIADIC_LIMIT_F >= 6

#if BSLSTL_OPTIONAL_VARIADIC_LIMIT_F >= 7
    template<class IL_TYPE, class ARGS_01,
                            class ARGS_02,
                            class ARGS_03,
                            class ARGS_04,
                            class ARGS_05,
                            class ARGS_06,
                            class ARGS_07>
    explicit optional(bsl::in_place_t,
                      std::initializer_list<IL_TYPE>,
                      BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_01),
                      BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_02),
                      BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_03),
                      BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_04),
                      BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_05),
                      BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_06),
                      BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_07));
#endif  // BSLSTL_OPTIONAL_VARIADIC_LIMIT_F >= 7

#if BSLSTL_OPTIONAL_VARIADIC_LIMIT_F >= 8
    template<class IL_TYPE, class ARGS_01,
                            class ARGS_02,
                            class ARGS_03,
                            class ARGS_04,
                            class ARGS_05,
                            class ARGS_06,
                            class ARGS_07,
                            class ARGS_08>
    explicit optional(bsl::in_place_t,
                      std::initializer_list<IL_TYPE>,
                      BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_01),
                      BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_02),
                      BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_03),
                      BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_04),
                      BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_05),
                      BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_06),
                      BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_07),
                      BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_08));
#endif  // BSLSTL_OPTIONAL_VARIADIC_LIMIT_F >= 8

#if BSLSTL_OPTIONAL_VARIADIC_LIMIT_F >= 9
    template<class IL_TYPE, class ARGS_01,
                            class ARGS_02,
                            class ARGS_03,
                            class ARGS_04,
                            class ARGS_05,
                            class ARGS_06,
                            class ARGS_07,
                            class ARGS_08,
                            class ARGS_09>
    explicit optional(bsl::in_place_t,
                      std::initializer_list<IL_TYPE>,
                      BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_01),
                      BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_02),
                      BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_03),
                      BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_04),
                      BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_05),
                      BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_06),
                      BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_07),
                      BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_08),
                      BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_09));
#endif  // BSLSTL_OPTIONAL_VARIADIC_LIMIT_F >= 9

#if BSLSTL_OPTIONAL_VARIADIC_LIMIT_F >= 10
    template<class IL_TYPE, class ARGS_01,
                            class ARGS_02,
                            class ARGS_03,
                            class ARGS_04,
                            class ARGS_05,
                            class ARGS_06,
                            class ARGS_07,
                            class ARGS_08,
                            class ARGS_09,
                            class ARGS_10>
    explicit optional(bsl::in_place_t,
                      std::initializer_list<IL_TYPE>,
                      BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_01),
                      BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_02),
                      BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_03),
                      BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_04),
                      BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_05),
                      BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_06),
                      BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_07),
                      BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_08),
                      BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_09),
                      BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_10));
#endif  // BSLSTL_OPTIONAL_VARIADIC_LIMIT_F >= 10

#endif
#else
// The generated code below is a workaround for the absence of perfect
// forwarding in some compilers.
    template<class... ARGS>
    explicit optional(bsl::in_place_t,
                      BSLS_COMPILERFEATURES_FORWARD_REF(ARGS)...);
#if defined(BSLS_COMPILERFEATURES_SUPPORT_GENERALIZED_INITIALIZERS)
    template<class IL_TYPE, class... ARGS>
    explicit optional(bsl::in_place_t,
                      std::initializer_list<IL_TYPE>,
                      BSLS_COMPILERFEATURES_FORWARD_REF(ARGS)...);
#endif
// }}} END GENERATED CODE
#endif

    //MANIPULATORS
#if !BSLS_COMPILERFEATURES_SIMULATE_CPP11_FEATURES
    template<class... ARGS>
    void emplace(BSLS_COMPILERFEATURES_FORWARD_REF(ARGS)...);
        // Destroy the current 'value_type' object, if any, and create a new
        // one using the provided arguments.

#if defined(BSLS_COMPILERFEATURES_SUPPORT_GENERALIZED_INITIALIZERS)
    template<class IL_TYPE, class... ARGS>
    void emplace(std::initializer_list<IL_TYPE>,
                 BSLS_COMPILERFEATURES_FORWARD_REF(ARGS)...);
        // Destroy the current 'value_type' object, if any, and create a new
        // one using the provided arguments.
#endif//BSLS_COMPILERFEATURES_SUPPORT_GENERALIZED_INITIALIZERS
#elif BSLS_COMPILERFEATURES_SIMULATE_VARIADIC_TEMPLATES
// {{{ BEGIN GENERATED CODE
// The following section is automatically generated.  **DO NOT EDIT**
// Generator command line: sim_cpp11_features.pl bslstl_optional.h
#ifndef BSLSTL_OPTIONAL_VARIADIC_LIMIT
#define BSLSTL_OPTIONAL_VARIADIC_LIMIT 10
#endif
#ifndef BSLSTL_OPTIONAL_VARIADIC_LIMIT_G
#define BSLSTL_OPTIONAL_VARIADIC_LIMIT_G BSLSTL_OPTIONAL_VARIADIC_LIMIT
#endif
#if BSLSTL_OPTIONAL_VARIADIC_LIMIT_G >= 0
    void emplace();
#endif  // BSLSTL_OPTIONAL_VARIADIC_LIMIT_G >= 0

#if BSLSTL_OPTIONAL_VARIADIC_LIMIT_G >= 1
    template<class ARGS_01>
    void emplace(BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_01));
#endif  // BSLSTL_OPTIONAL_VARIADIC_LIMIT_G >= 1

#if BSLSTL_OPTIONAL_VARIADIC_LIMIT_G >= 2
    template<class ARGS_01,
             class ARGS_02>
    void emplace(BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_01),
                 BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_02));
#endif  // BSLSTL_OPTIONAL_VARIADIC_LIMIT_G >= 2

#if BSLSTL_OPTIONAL_VARIADIC_LIMIT_G >= 3
    template<class ARGS_01,
             class ARGS_02,
             class ARGS_03>
    void emplace(BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_01),
                 BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_02),
                 BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_03));
#endif  // BSLSTL_OPTIONAL_VARIADIC_LIMIT_G >= 3

#if BSLSTL_OPTIONAL_VARIADIC_LIMIT_G >= 4
    template<class ARGS_01,
             class ARGS_02,
             class ARGS_03,
             class ARGS_04>
    void emplace(BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_01),
                 BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_02),
                 BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_03),
                 BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_04));
#endif  // BSLSTL_OPTIONAL_VARIADIC_LIMIT_G >= 4

#if BSLSTL_OPTIONAL_VARIADIC_LIMIT_G >= 5
    template<class ARGS_01,
             class ARGS_02,
             class ARGS_03,
             class ARGS_04,
             class ARGS_05>
    void emplace(BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_01),
                 BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_02),
                 BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_03),
                 BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_04),
                 BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_05));
#endif  // BSLSTL_OPTIONAL_VARIADIC_LIMIT_G >= 5

#if BSLSTL_OPTIONAL_VARIADIC_LIMIT_G >= 6
    template<class ARGS_01,
             class ARGS_02,
             class ARGS_03,
             class ARGS_04,
             class ARGS_05,
             class ARGS_06>
    void emplace(BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_01),
                 BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_02),
                 BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_03),
                 BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_04),
                 BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_05),
                 BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_06));
#endif  // BSLSTL_OPTIONAL_VARIADIC_LIMIT_G >= 6

#if BSLSTL_OPTIONAL_VARIADIC_LIMIT_G >= 7
    template<class ARGS_01,
             class ARGS_02,
             class ARGS_03,
             class ARGS_04,
             class ARGS_05,
             class ARGS_06,
             class ARGS_07>
    void emplace(BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_01),
                 BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_02),
                 BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_03),
                 BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_04),
                 BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_05),
                 BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_06),
                 BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_07));
#endif  // BSLSTL_OPTIONAL_VARIADIC_LIMIT_G >= 7

#if BSLSTL_OPTIONAL_VARIADIC_LIMIT_G >= 8
    template<class ARGS_01,
             class ARGS_02,
             class ARGS_03,
             class ARGS_04,
             class ARGS_05,
             class ARGS_06,
             class ARGS_07,
             class ARGS_08>
    void emplace(BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_01),
                 BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_02),
                 BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_03),
                 BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_04),
                 BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_05),
                 BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_06),
                 BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_07),
                 BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_08));
#endif  // BSLSTL_OPTIONAL_VARIADIC_LIMIT_G >= 8

#if BSLSTL_OPTIONAL_VARIADIC_LIMIT_G >= 9
    template<class ARGS_01,
             class ARGS_02,
             class ARGS_03,
             class ARGS_04,
             class ARGS_05,
             class ARGS_06,
             class ARGS_07,
             class ARGS_08,
             class ARGS_09>
    void emplace(BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_01),
                 BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_02),
                 BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_03),
                 BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_04),
                 BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_05),
                 BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_06),
                 BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_07),
                 BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_08),
                 BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_09));
#endif  // BSLSTL_OPTIONAL_VARIADIC_LIMIT_G >= 9

#if BSLSTL_OPTIONAL_VARIADIC_LIMIT_G >= 10
    template<class ARGS_01,
             class ARGS_02,
             class ARGS_03,
             class ARGS_04,
             class ARGS_05,
             class ARGS_06,
             class ARGS_07,
             class ARGS_08,
             class ARGS_09,
             class ARGS_10>
    void emplace(BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_01),
                 BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_02),
                 BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_03),
                 BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_04),
                 BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_05),
                 BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_06),
                 BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_07),
                 BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_08),
                 BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_09),
                 BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_10));
#endif  // BSLSTL_OPTIONAL_VARIADIC_LIMIT_G >= 10


#if defined(BSLS_COMPILERFEATURES_SUPPORT_GENERALIZED_INITIALIZERS)
#if BSLSTL_OPTIONAL_VARIADIC_LIMIT_G >= 0
    template<class IL_TYPE>
    void emplace(std::initializer_list<IL_TYPE>);
#endif  // BSLSTL_OPTIONAL_VARIADIC_LIMIT_G >= 0

#if BSLSTL_OPTIONAL_VARIADIC_LIMIT_G >= 1
    template<class IL_TYPE, class ARGS_01>
    void emplace(std::initializer_list<IL_TYPE>,
                 BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_01));
#endif  // BSLSTL_OPTIONAL_VARIADIC_LIMIT_G >= 1

#if BSLSTL_OPTIONAL_VARIADIC_LIMIT_G >= 2
    template<class IL_TYPE, class ARGS_01,
                            class ARGS_02>
    void emplace(std::initializer_list<IL_TYPE>,
                 BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_01),
                 BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_02));
#endif  // BSLSTL_OPTIONAL_VARIADIC_LIMIT_G >= 2

#if BSLSTL_OPTIONAL_VARIADIC_LIMIT_G >= 3
    template<class IL_TYPE, class ARGS_01,
                            class ARGS_02,
                            class ARGS_03>
    void emplace(std::initializer_list<IL_TYPE>,
                 BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_01),
                 BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_02),
                 BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_03));
#endif  // BSLSTL_OPTIONAL_VARIADIC_LIMIT_G >= 3

#if BSLSTL_OPTIONAL_VARIADIC_LIMIT_G >= 4
    template<class IL_TYPE, class ARGS_01,
                            class ARGS_02,
                            class ARGS_03,
                            class ARGS_04>
    void emplace(std::initializer_list<IL_TYPE>,
                 BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_01),
                 BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_02),
                 BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_03),
                 BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_04));
#endif  // BSLSTL_OPTIONAL_VARIADIC_LIMIT_G >= 4

#if BSLSTL_OPTIONAL_VARIADIC_LIMIT_G >= 5
    template<class IL_TYPE, class ARGS_01,
                            class ARGS_02,
                            class ARGS_03,
                            class ARGS_04,
                            class ARGS_05>
    void emplace(std::initializer_list<IL_TYPE>,
                 BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_01),
                 BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_02),
                 BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_03),
                 BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_04),
                 BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_05));
#endif  // BSLSTL_OPTIONAL_VARIADIC_LIMIT_G >= 5

#if BSLSTL_OPTIONAL_VARIADIC_LIMIT_G >= 6
    template<class IL_TYPE, class ARGS_01,
                            class ARGS_02,
                            class ARGS_03,
                            class ARGS_04,
                            class ARGS_05,
                            class ARGS_06>
    void emplace(std::initializer_list<IL_TYPE>,
                 BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_01),
                 BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_02),
                 BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_03),
                 BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_04),
                 BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_05),
                 BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_06));
#endif  // BSLSTL_OPTIONAL_VARIADIC_LIMIT_G >= 6

#if BSLSTL_OPTIONAL_VARIADIC_LIMIT_G >= 7
    template<class IL_TYPE, class ARGS_01,
                            class ARGS_02,
                            class ARGS_03,
                            class ARGS_04,
                            class ARGS_05,
                            class ARGS_06,
                            class ARGS_07>
    void emplace(std::initializer_list<IL_TYPE>,
                 BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_01),
                 BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_02),
                 BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_03),
                 BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_04),
                 BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_05),
                 BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_06),
                 BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_07));
#endif  // BSLSTL_OPTIONAL_VARIADIC_LIMIT_G >= 7

#if BSLSTL_OPTIONAL_VARIADIC_LIMIT_G >= 8
    template<class IL_TYPE, class ARGS_01,
                            class ARGS_02,
                            class ARGS_03,
                            class ARGS_04,
                            class ARGS_05,
                            class ARGS_06,
                            class ARGS_07,
                            class ARGS_08>
    void emplace(std::initializer_list<IL_TYPE>,
                 BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_01),
                 BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_02),
                 BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_03),
                 BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_04),
                 BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_05),
                 BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_06),
                 BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_07),
                 BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_08));
#endif  // BSLSTL_OPTIONAL_VARIADIC_LIMIT_G >= 8

#if BSLSTL_OPTIONAL_VARIADIC_LIMIT_G >= 9
    template<class IL_TYPE, class ARGS_01,
                            class ARGS_02,
                            class ARGS_03,
                            class ARGS_04,
                            class ARGS_05,
                            class ARGS_06,
                            class ARGS_07,
                            class ARGS_08,
                            class ARGS_09>
    void emplace(std::initializer_list<IL_TYPE>,
                 BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_01),
                 BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_02),
                 BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_03),
                 BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_04),
                 BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_05),
                 BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_06),
                 BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_07),
                 BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_08),
                 BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_09));
#endif  // BSLSTL_OPTIONAL_VARIADIC_LIMIT_G >= 9

#if BSLSTL_OPTIONAL_VARIADIC_LIMIT_G >= 10
    template<class IL_TYPE, class ARGS_01,
                            class ARGS_02,
                            class ARGS_03,
                            class ARGS_04,
                            class ARGS_05,
                            class ARGS_06,
                            class ARGS_07,
                            class ARGS_08,
                            class ARGS_09,
                            class ARGS_10>
    void emplace(std::initializer_list<IL_TYPE>,
                 BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_01),
                 BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_02),
                 BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_03),
                 BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_04),
                 BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_05),
                 BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_06),
                 BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_07),
                 BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_08),
                 BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_09),
                 BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_10));
#endif  // BSLSTL_OPTIONAL_VARIADIC_LIMIT_G >= 10

#endif
#else
// The generated code below is a workaround for the absence of perfect
// forwarding in some compilers.
    template<class... ARGS>
    void emplace(BSLS_COMPILERFEATURES_FORWARD_REF(ARGS)...);

#if defined(BSLS_COMPILERFEATURES_SUPPORT_GENERALIZED_INITIALIZERS)
    template<class IL_TYPE, class... ARGS>
    void emplace(std::initializer_list<IL_TYPE>,
                 BSLS_COMPILERFEATURES_FORWARD_REF(ARGS)...);
#endif
// }}} END GENERATED CODE
#endif

    void reset() BSLS_KEYWORD_NOEXCEPT;
        // Reset this object to the default constructed state (i.e., to be in a
        // disengaged state).

    TYPE& value() BSLS_KEYWORD_LVREF_QUAL;
        // Return a reference providing modifiable access to the underlying
        // 'TYPE' object.  Throws a 'bsl::bad_optional_access' if the
        // 'optional' object is disengaged.

#if defined(BSLS_COMPILERFEATURES_SUPPORT_REF_QUALIFIERS)
    TYPE&& value() BSLS_KEYWORD_RVREF_QUAL;
        // Return a reference providing modifiable access to the underlying
        // 'TYPE' object.  Throws a 'bsl::bad_optional_access' if the
        // 'optional' object is disengaged.
#endif


    optional& operator=(bsl::nullopt_t) BSLS_KEYWORD_NOEXCEPT;
        // reset the 'optional' to a disengaged state.

    optional& operator=(const optional& rhs);
    optional& operator=(BloombergLP::bslmf::MovableRef<optional> rhs);
        // Assign to this object the value of the specified 'rhs' object.  Note
        // that this method may invoke assignment from 'rhs', or construction
        // from 'rhs', depending on whether the current object is engaged.

    template<class ANY_TYPE>
    BSLSTL_OPTIONAL_ENABLE_ASSIGN_FROM_OPTIONAL_LVAL &
    operator=(const optional<ANY_TYPE> &rhs)
        // If specified 'rhs' object is engaged, assign to this object the
        // result of 'rhs.value()' converted to 'TYPE'. Otherwise, disengage
        // this object.  Note that this method may invoke assignment from 'rhs'
        // , or construction from 'rhs', depending on whether this object is
        // engaged.
    {
        // Must be in-place inline because the use of 'enable_if' will
        // otherwise break the MSVC 2010 compiler.
        if (rhs.has_value()) {
            if (this->has_value()) {
                this->value() = rhs.value();
            }
            else {
                this->emplace(rhs.value());
            }
        }
        else {
            this->reset();
        }
        return *this;
    }

#if defined(BSLS_COMPILERFEATURES_SUPPORT_RVALUE_REFERENCES )
    template<class ANY_TYPE>
    BSLSTL_OPTIONAL_ENABLE_ASSIGN_FROM_OPTIONAL_RVAL &
    operator=(optional<ANY_TYPE>&& rhs)
        // If specified 'rhs' object is engaged, assign to this object the
        // result of 'rhs.value()' converted to 'TYPE'. Otherwise, disengage
        // this object.  Note that this method may invoke assignment from 'rhs'
        // , or construction from 'rhs', depending on whether this object is
        // engaged.  Using rvalue reference instead of movableRef ensures this
        // overload is considered a better match over 'ANY_TYPE' overloads for
        // optional types.
    {
        // Must be in-place inline because the use of 'enable_if' will
        // otherwise break the MSVC 2010 compiler.
        if (rhs.has_value()) {
            if (this->has_value()) {
                this->value() = MoveUtil::move(rhs.value());
            }
            else {
                this->emplace(MoveUtil::move(rhs.value()));
            }
        }
        else {
            this->reset();
        }
        return *this;
    }

    template<class ANY_TYPE = TYPE>
    BSLSTL_OPTIONAL_ENABLE_ASSIGN_FROM_ANYTYPE &
    operator=(ANY_TYPE&& rhs)
        // Assign to this object the value of the specified 'rhs' object
        // converted to 'TYPE', and return a reference providing modifiable
        // access to this object.  Note that this method may invoke assignment
        // from 'rhs', or construction from 'rhs', depending on whether this
        // object is engaged.  BSLSTL_OPTIONAL_ENABLE_ASSIGN_FROM_ANYTYPE
        // contains a check that disables this overload if 'ANY_TYPE' is
        // 'optional<TYPE>'.  This is needed to prevent this assignment
        // operator being a better match for non const 'optional<TYPE>' lvalues
        // than 'operator=(const optional& rhs)'.  This function needs to be a
        // worse match than 'operator=(optional)' so cases like :
        //..
        //      bsl::optional<int> oi;
        //      oi = {};
        //..
        // represent assignment from a default constructed 'optional', as
        // opposed to assignment from default constructed 'value_type'.  Note
        // that in C++03, where there is no concept of perfect forwarding,
        // this is not a concern.
    {
        // Must be in-place inline because the use of 'enable_if' will
        // otherwise break the MSVC 2010 compiler.
        if (this->has_value()) {
            this->value() = std::forward<ANY_TYPE>(rhs);
        }
        else {
            this->emplace(std::forward<ANY_TYPE>(rhs));
        }
        return *this;
    }

#else
    // MovableRef and rvalue give different semantics in template functions.
    // For C++03, we need to specify different overloads.

    optional& operator=(const TYPE& rhs);
    optional& operator=(BloombergLP::bslmf::MovableRef<TYPE> rhs);

    template<class ANY_TYPE>
    BSLSTL_OPTIONAL_ENABLE_ASSIGN_FROM_OPTIONAL_RVAL &
    operator=(BloombergLP::bslmf::MovableRef<optional<ANY_TYPE> > rhs)
        // If 'rhs' object is engaged, assign to this object the result of
        // 'rhs.value()' converted to 'TYPE'. Otherwise, disengage this object.
        // Note that this method may invoke assignment from 'rhs', or
        // construction from 'rhs', depending on whether this object is
        // engaged.
    {
        // Must be in-place inline because the use of 'enable_if' will
        // otherwise break the MSVC 2010 compiler.
        optional<ANY_TYPE> lvalue = rhs;
        if (lvalue.has_value()) {
            if (this->has_value()) {
                this->value() = MoveUtil::move(lvalue.value());
            }
            else {
                this->emplace(MoveUtil::move(lvalue.value()));
            }
        }
        else {
            this->reset();
        }
        return *this;
    }

    template<class ANY_TYPE>
    optional& operator=(const ANY_TYPE& rhs);
        // Assign to this object the value of the specified 'rhs' object
        // converted to 'TYPE', and return a reference providing modifiable
        // access to this object.  Note that this method may invoke assignment
        // from 'rhs', or construction from 'rhs', depending on whether this
        // object is engaged.

    template<class ANY_TYPE>
    optional&  operator=(BloombergLP::bslmf::MovableRef<ANY_TYPE> rhs);
        // Assign to this object the value of the specified 'rhs' object
        // converted to 'TYPE', and return a reference providing modifiable
        // access to this object.  Note that this method may invoke assignment
        // from 'rhs', or construction from 'rhs', depending on whether this
        // object is engaged. These overloads need to exist in C++03 because
        // the perfect forwarding 'operator=' needs to be specified in terms
        // of rvalues

#endif

    TYPE* operator->();
        // Return a pointer providing modifiable access to the underlying
        // 'TYPE' object.  The behavior is undefined if this object is
        // disengaged.

    TYPE& operator*() BSLS_KEYWORD_LVREF_QUAL;
        // Return a reference providing modifiable access to the underlying
        // 'TYPE' object.  The behavior is undefined if this object is
        // disengaged.

#if defined(BSLS_COMPILERFEATURES_SUPPORT_REF_QUALIFIERS)
    TYPE&& operator*() BSLS_KEYWORD_RVREF_QUAL;
        // Return a reference providing modifiable access to the underlying
        // 'TYPE' object.  The behavior is undefined if this object is
        // disengaged.
#endif

    void swap(optional& other);
        // Efficiently exchange the value of this object with the value of the
        // specified 'other' object.  In effect, performs
        // 'using bsl::swap; swap(c, other.c);'.

#if defined(BSLS_COMPILERFEATURES_SUPPORT_REF_QUALIFIERS)
    template<class ANY_TYPE>
    TYPE value_or(ANY_TYPE&&) BSLS_KEYWORD_RVREF_QUAL;
        // If 'this->has_value() == true', return a copy of the contained
        // 'value_type' object.  Otherwise, return the value of 'rhs' converted
        // to 'value_type'.
#endif

    //ACCESSORS
    const TYPE& value() const BSLS_KEYWORD_LVREF_QUAL;
        // Return a reference providing non-modifiable access to the underlying
        // 'TYPE' object.  Throws a 'bsl::bad_optional_access' if the
        // 'optional' object is disengaged.

#if defined(BSLS_COMPILERFEATURES_SUPPORT_REF_QUALIFIERS)
    const TYPE&& value() const BSLS_KEYWORD_RVREF_QUAL;
        // Return a reference providing non-modifiable access to the underlying
        // 'TYPE' object.  Throws a 'bsl::bad_optional_access' if the
        // 'optional' object is disengaged.
#endif

    const TYPE* operator->() const;
        // Return a pointer providing non-modifiable access to the underlying
        // 'TYPE' object.  The behaviour is undefined if this object is
        // disengaged.

    const TYPE& operator*() const BSLS_KEYWORD_LVREF_QUAL;
        // Return a reference providing non-modifiable access to the underlying
        // 'TYPE' object.  The behavior is undefined if this object is
        // disengaged.

#if defined(BSLS_COMPILERFEATURES_SUPPORT_REF_QUALIFIERS)
    const TYPE&& operator*() const BSLS_KEYWORD_RVREF_QUAL;
        // Return a reference providing non-modifiable access to the underlying
        // 'TYPE' object.  The behavior is undefined if this object is
        // disengaged.
#endif

    bool has_value() const BSLS_KEYWORD_NOEXCEPT;
        // Return 'true' if this object is disengaged, and 'false' otherwise.

#ifdef BSLS_COMPILERFEATURES_SUPPORT_OPERATOR_EXPLICIT
    BSLS_KEYWORD_EXPLICIT operator bool()  const  BSLS_KEYWORD_NOEXCEPT;
        // Return 'true' if this object is disengaged, and 'false' otherwise.
#else
    // Simulation of explicit conversion to bool.
    // Inlined to work around xlC bug when out-of-line.
    operator UnspecifiedBool() const BSLS_NOTHROW_SPEC
    {
        return UnspecifiedBoolUtil::makeValue(has_value());
    }
#endif //#ifdef BSLS_COMPILERFEATURES_SUPPORT_OPERATOR_EXPLICIT

    template<class ANY_TYPE>
    TYPE value_or(BSLS_COMPILERFEATURES_FORWARD_REF(ANY_TYPE)) const
    BSLS_KEYWORD_LVREF_QUAL;

};
#endif // __cpp_lib_optional

// FREE FUNCTIONS
template <class TYPE>
void swap(bsl::optional<TYPE>& lhs,
          bsl::optional<TYPE>& rhs);
    // Swap the value of the specified 'lhs' optional with the value of the
    // specified 'rhs' optional.

// HASH SPECIALIZATIONS
template <class HASHALG, class TYPE>
void hashAppend(HASHALG& hashAlg, const optional<TYPE>&  input);
    // Pass the specified 'input' to the specified 'hashAlg', where 'hashAlg'
    // is a hashing algorithm.

// FREE OPERATORS

// comparison with optional
template <class LHS_TYPE, class RHS_TYPE>
bool operator==(const bsl::optional<LHS_TYPE>& lhs,
                const bsl::optional<RHS_TYPE>& rhs);
    // If, for specified 'lhs' and 'rhs', 'bool(lhs) != bool(rhs)', return
    // 'false'; otherwise if 'bool(lhs) == false', return 'true'; otherwise
    // return '*lhs == *rhs'. Note that this function will fail to compile if
    // 'LHS_TYPE' and 'RHS_TYPE' are not compatible.

template <class LHS_TYPE, class RHS_TYPE>
bool operator!=(const bsl::optional<LHS_TYPE>& lhs,
                const bsl::optional<RHS_TYPE>& rhs);
    // If, for specified 'lhs' and 'rhs', 'bool(lhs) != bool(rhs)', return
    // 'true'; otherwise, if 'bool(lhs) == false', return 'false'; otherwise
    // return '*lhs != *rhs'.  Note that this function will fail to compile if
    // 'LHS_TYPE' and 'RHS_TYPE' are not compatible.

template <class LHS_TYPE, class RHS_TYPE>
bool operator<(const bsl::optional<LHS_TYPE>& lhs,
               const bsl::optional<RHS_TYPE>& rhs);
    // If specified 'rhs' object is not engaged, return 'false'; otherwise, if
    // specified 'lhs' is not engaged, return 'true'; otherwise return the
    // value of '*lhs < *rhs'. Note that this function will fail to compile if
    // specified 'LHS_TYPE' and 'RHS_TYPE' are not compatible.

template <class LHS_TYPE, class RHS_TYPE>
bool operator>(const bsl::optional<LHS_TYPE>& lhs,
               const bsl::optional<RHS_TYPE>& rhs);
    // If specified 'lhs' object is not engaged, return 'false'; otherwise, if
    // specified 'rhs' is not engaged, return 'true'; otherwise return the
    // value of '*lhs > *rhs'. Note that this function will fail to compile if
    // specified 'LHS_TYPE' and 'RHS_TYPE' are not compatible.

template <class LHS_TYPE, class RHS_TYPE>
bool operator<=(const bsl::optional<LHS_TYPE>& lhs,
                const bsl::optional<RHS_TYPE>& rhs);
    // If specified 'lhs' object is not engaged, return 'true'; otherwise, if
    // specified 'rhs' is not engaged, return 'false'; otherwise return the
    // value of '*lhs <= *rhs'. Note that this function will fail to compile if
    // specified 'LHS_TYPE' and 'RHS_TYPE' are not compatible.

template <class LHS_TYPE, class RHS_TYPE>
bool operator>=(const bsl::optional<LHS_TYPE>& lhs,
                const bsl::optional<RHS_TYPE>& rhs);
    // If specified 'rhs' object is not engaged, return 'true'; otherwise, if
    // specified 'lhs' is not engaged, return 'false'; otherwise return the
    // value of '*lhs >= *rhs'. Note that this function will fail to compile if
    // specified 'LHS_TYPE' and 'RHS_TYPE' are not compatible.

// comparison with nullopt_t
template <class TYPE>
BSLS_KEYWORD_CONSTEXPR
bool operator==(const bsl::optional<TYPE>& value,
                const bsl::nullopt_t&) BSLS_KEYWORD_NOEXCEPT;
template <class TYPE>
BSLS_KEYWORD_CONSTEXPR
bool operator==(const bsl::nullopt_t&,
                const bsl::optional<TYPE>& value) BSLS_KEYWORD_NOEXCEPT;
    // Return 'true' if specified 'value' is disengaged, and 'false' otherwise.

template <class TYPE>
BSLS_KEYWORD_CONSTEXPR
bool operator!=(const bsl::optional<TYPE>& value,
                const bsl::nullopt_t&) BSLS_KEYWORD_NOEXCEPT;
template <class TYPE>
BSLS_KEYWORD_CONSTEXPR
bool operator!=(const bsl::nullopt_t&,
                const bsl::optional<TYPE>& value) BSLS_KEYWORD_NOEXCEPT;
    // Return 'true' if specified 'value' is engaged, and 'false' otherwise.

template <class TYPE>
BSLS_KEYWORD_CONSTEXPR
bool operator<(const bsl::optional<TYPE>&,
               const bsl::nullopt_t&) BSLS_KEYWORD_NOEXCEPT;
    // Return 'false'.

template <class TYPE>
BSLS_KEYWORD_CONSTEXPR
bool operator<(const bsl::nullopt_t&,
               const bsl::optional<TYPE>& value) BSLS_KEYWORD_NOEXCEPT;
    // Return 'true' if specified 'value' is engaged, and 'false' otherwise.

template <class TYPE>
BSLS_KEYWORD_CONSTEXPR
bool operator>(const bsl::optional<TYPE>& value,
               const bsl::nullopt_t&) BSLS_KEYWORD_NOEXCEPT;
    // Return 'true' if specified 'value' is engaged, and 'false' otherwise.

template <class TYPE>
BSLS_KEYWORD_CONSTEXPR
bool operator>(const bsl::nullopt_t&,
               const bsl::optional<TYPE>&) BSLS_KEYWORD_NOEXCEPT;
    // Return 'false'.

template <class TYPE>
BSLS_KEYWORD_CONSTEXPR
bool operator<=(const bsl::optional<TYPE>& value,
                const bsl::nullopt_t&) BSLS_KEYWORD_NOEXCEPT;
    // Return 'true' if specified 'value' is disengaged, and 'false' otherwise.

template <class TYPE>
BSLS_KEYWORD_CONSTEXPR
bool operator<=(const bsl::nullopt_t&,
                const bsl::optional<TYPE>&) BSLS_KEYWORD_NOEXCEPT;
    // Return 'true'.

template <class TYPE>
BSLS_KEYWORD_CONSTEXPR
bool operator>=(const bsl::optional<TYPE>&,
                const bsl::nullopt_t&) BSLS_KEYWORD_NOEXCEPT;
    // Return 'true'.

template <class TYPE>
BSLS_KEYWORD_CONSTEXPR
bool operator>=(const bsl::nullopt_t&,
                const bsl::optional<TYPE>& value) BSLS_KEYWORD_NOEXCEPT;
    // Return 'true' if specified 'value' is disengaged, and 'false' otherwise.

// comparison with T
template <class LHS_TYPE, class RHS_TYPE>
bool operator!=(const bsl::optional<LHS_TYPE>& lhs,
                const RHS_TYPE&                rhs);
template <class LHS_TYPE, class RHS_TYPE>
bool operator!=(const LHS_TYPE&                lhs,
                const bsl::optional<RHS_TYPE>& rhs);
    // Return 'true' if the specified 'lhs' and 'rhs' objects do not have the
    // same value, and 'false' otherwise.  An 'optional' object and a value of
    // some type do not have the same value if either the optional object is
    // null, or its underlying value does not compare equal to the other value.
    // Note that this function will fail to compile if 'LHS_TYPE' and
    // 'RHS_TYPE' are not compatible.

template <class LHS_TYPE, class RHS_TYPE>
bool operator==(const bsl::optional<LHS_TYPE>& lhs,
                const RHS_TYPE&                rhs);
template <class LHS_TYPE, class RHS_TYPE>
bool operator==(const LHS_TYPE&                lhs,
                const bsl::optional<RHS_TYPE>& rhs);
    // Return 'true' if the specified 'lhs' and 'rhs' objects have the same
    // value, and 'false' otherwise.  An 'optional' object and a value of some
    // type have the same value if the optional object is non-null and its
    // underlying value compares equal to the other value.  Note that this
    // function will fail to compile if 'LHS_TYPE' and 'RHS_TYPE' are not
    // compatible.


template <class LHS_TYPE, class RHS_TYPE>
bool operator<(const bsl::optional<LHS_TYPE>& lhs,
               const RHS_TYPE&                rhs);
    // Return 'true' if the specified 'lhs' optional object is ordered before
    // the specified 'rhs', and 'false' otherwise.  'lhs' is ordered before
    // 'rhs' if 'lhs' is null or 'lhs.value()' is ordered before 'rhs'.

template <class LHS_TYPE, class RHS_TYPE>
bool operator<(const LHS_TYPE&                lhs,
               const bsl::optional<RHS_TYPE>& rhs);
    // Return 'true' if the specified 'lhs' is ordered before the specified
    // 'rhs' optional object, and 'false' otherwise.  'lhs' is ordered before
    // 'rhs' if 'rhs' is not null and 'lhs' is ordered before 'rhs.value()'.


template <class LHS_TYPE, class RHS_TYPE>
bool operator>(const bsl::optional<LHS_TYPE>& lhs,
               const RHS_TYPE&                rhs);
    // Return 'true' if the specified 'lhs' optional object is ordered after
    // the specified 'rhs', and 'false' otherwise.  'lhs' is ordered after
    // 'rhs' if 'lhs' is not null and 'lhs.value()' is ordered after 'rhs'.
    // Note that this operator returns 'rhs < lhs'.

template <class LHS_TYPE, class RHS_TYPE>
bool operator>(const LHS_TYPE&                lhs,
               const bsl::optional<RHS_TYPE>& rhs);
    // Return 'true' if the specified 'lhs' is ordered after the specified
    // 'rhs' optional object, and 'false' otherwise.  'lhs' is ordered after
    // 'rhs' if 'rhs' is null or 'lhs' is ordered after 'rhs.value()'.  Note
    // that this operator returns 'rhs < lhs'.


template <class LHS_TYPE, class RHS_TYPE>
bool operator<=(const bsl::optional<LHS_TYPE>& lhs,
                const RHS_TYPE&                rhs);
    // Return 'true' if the specified 'lhs' optional object is ordered before
    // the specified 'rhs' or 'lhs' and 'rhs' have the same value, and 'false'
    // otherwise.  (See 'operator<' and 'operator=='.)  Note that this operator
    // returns '!(rhs < lhs)'.

template <class LHS_TYPE, class RHS_TYPE>
bool operator<=(const LHS_TYPE&                lhs,
                const bsl::optional<RHS_TYPE>& rhs);
    // Return 'true' if the specified 'lhs' is ordered before the specified
    // 'rhs' optional object or 'lhs' and 'rhs' have the same value, and
    // 'false' otherwise.  (See 'operator<' and 'operator=='.)  Note that this
    // operator returns '!(rhs < lhs)'.

template <class LHS_TYPE, class RHS_TYPE>
bool operator>=(const bsl::optional<LHS_TYPE>& lhs,
                const bsl::optional<RHS_TYPE>& rhs);
    // Return 'true' if the specified 'lhs' optional object is ordered after
    // the specified 'rhs' optional object or 'lhs' and 'rhs' have the same
    // value, and 'false' otherwise.  (See 'operator>' and 'operator=='.)  Note
    // that this operator returns '!(lhs < rhs)'.  Also note that this function
    // will fail to compile if 'LHS_TYPE' and 'RHS_TYPE' are not compatible.

template <class LHS_TYPE, class RHS_TYPE>
bool operator>=(const bsl::optional<LHS_TYPE>& lhs,
                const RHS_TYPE&                rhs);
    // Return 'true' if the specified 'lhs' optional object is ordered after
    // the specified 'rhs' or 'lhs' and 'rhs' have the same value, and 'false'
    // otherwise.  (See 'operator>' and 'operator=='.)  Note that this operator
    // returns '!(lhs < rhs)'.

template <class LHS_TYPE, class RHS_TYPE>
bool operator>=(const LHS_TYPE&                lhs,
                const bsl::optional<RHS_TYPE>& rhs);
    // Return 'true' if the specified 'lhs' is ordered after the specified
    // 'rhs' optional object or 'lhs' and 'rhs' have the same value, and
    // 'false' otherwise.  (See 'operator>' and 'operator=='.)  Note that this
    // operator returns '!(lhs < rhs)'.

#ifdef __cpp_lib_optional
template <class TYPE>
void swap(bsl::optional<TYPE>& lhs,
          std::optional<TYPE>& rhs);
template <class TYPE>
void swap(std::optional<TYPE>& lhs,
          bsl::optional<TYPE>& rhs);
    // Swap the value of the specified 'lhs' optional with the value of the
    // specified 'rhs' optional.

// comparison with optional
template <class LHS_TYPE, class RHS_TYPE>
bool operator==(const std::optional<LHS_TYPE>& lhs,
                const bsl::optional<RHS_TYPE>& rhs);
template <class LHS_TYPE, class RHS_TYPE>
bool operator==(const bsl::optional<LHS_TYPE>& lhs,
                const std::optional<RHS_TYPE>& rhs);
    // If bool(x) != bool(y), false; otherwise if bool(x) == false, true;
    // otherwise *x == *y. Note that this function will fail to compile if
    // 'LHS_TYPE' and 'RHS_TYPE' are not compatible.

template <class LHS_TYPE, class RHS_TYPE>
bool operator!=(const bsl::optional<LHS_TYPE>& lhs,
                const std::optional<RHS_TYPE>& rhs);
template <class LHS_TYPE, class RHS_TYPE>
bool operator!=(const std::optional<LHS_TYPE>& lhs,
                const bsl::optional<RHS_TYPE>& rhs);
    // If bool(x) != bool(y), true; otherwise, if bool(x) == false, false;
    // otherwise *x != *y.  Note that this function will fail to compile if
    // 'LHS_TYPE' and 'RHS_TYPE' are not compatible.

template <class LHS_TYPE, class RHS_TYPE>
bool operator<(const bsl::optional<LHS_TYPE>& lhs,
               const std::optional<RHS_TYPE>& rhs);
template <class LHS_TYPE, class RHS_TYPE>
bool operator<(const std::optional<LHS_TYPE>& lhs,
               const bsl::optional<RHS_TYPE>& rhs);
    // If !y, false; otherwise, if !x, true; otherwise *x < *y.  Note that
    // this function will fail to compile if 'LHS_TYPE' and 'RHS_TYPE' are
    // not compatible.

template <class LHS_TYPE, class RHS_TYPE>
bool operator>(const bsl::optional<LHS_TYPE>& lhs,
               const std::optional<RHS_TYPE>& rhs);
template <class LHS_TYPE, class RHS_TYPE>
bool operator>(const std::optional<LHS_TYPE>& lhs,
               const bsl::optional<RHS_TYPE>& rhs);
    // If !x, false; otherwise, if !y, true; otherwise *x > *y. note that this
    // function will fail to compile if 'LHS_TYPE' and 'RHS_TYPE' are not
    // compatible.

template <class LHS_TYPE, class RHS_TYPE>
bool operator<=(const bsl::optional<LHS_TYPE>& lhs,
                const std::optional<RHS_TYPE>& rhs);
template <class LHS_TYPE, class RHS_TYPE>
bool operator<=(const std::optional<LHS_TYPE>& lhs,
                const bsl::optional<RHS_TYPE>& rhs);
    // If !x, true; otherwise, if !y, false; otherwise *x <= *y. Note that this
    // function will fail to compile if 'LHS_TYPE' and 'RHS_TYPE' are not
    // compatible.

template <class LHS_TYPE, class RHS_TYPE>
bool operator>=(const bsl::optional<LHS_TYPE>& lhs,
                const std::optional<RHS_TYPE>& rhs);
template <class LHS_TYPE, class RHS_TYPE>
bool operator>=(const std::optional<LHS_TYPE>& lhs,
                const bsl::optional<RHS_TYPE>& rhs);
    // If !y, true; otherwise, if !x, false; otherwise *x >= *y. Note that this
    // function will fail to compile if 'LHS_TYPE' and 'RHS_TYPE' are not
    // compatible.

#endif // __cpp_lib_optional

template<class TYPE>
BSLS_KEYWORD_CONSTEXPR
bsl::optional<typename bsl::decay<TYPE>::type >
alloc_optional(typename bsl::optional<typename bsl::decay<TYPE>::type
                                 >::allocator_type const&,
               BSLS_COMPILERFEATURES_FORWARD_REF(TYPE) rhs);
    // Return an 'optional' object containing a 'TYPE' object created  by
    // invoking a 'bsl::optional' allocator extended 'in_place_t' constructor
    // with the specified 'alloc' as the allocator argument, and specified
    // 'rhs' as the constructor argument.  Note that this function will fail
    // to compile if TYPE doesn't use allocators.

#if !BSLS_COMPILERFEATURES_SIMULATE_CPP11_FEATURES
template<class TYPE, class... ARGS>
BSLS_KEYWORD_CONSTEXPR
bsl::optional<TYPE>
alloc_optional(typename bsl::optional<TYPE>::allocator_type const& alloc,
               BSLS_COMPILERFEATURES_FORWARD_REF(ARGS)...          args);
    // Return an 'optional' object containing a 'TYPE' object created by
    // invoking a 'bsl::optional' allocator extended 'in_place_t' constructor
    // with the specified 'alloc' as the allocator argument, and specified
    // 'args' as constructor arguments.  Note that this function will fail to
    // compile if TYPE doesn't use allocators.

#if defined(BSLS_COMPILERFEATURES_SUPPORT_GENERALIZED_INITIALIZERS)
template<class TYPE, class IL_TYPE, class... ARGS>
BSLS_KEYWORD_CONSTEXPR
bsl::optional<TYPE>
alloc_optional(typename bsl::optional<TYPE>::allocator_type const& alloc,
               std::initializer_list<IL_TYPE>                      il,
               BSLS_COMPILERFEATURES_FORWARD_REF(ARGS)...          args);
    // Return an 'optional' object containing a 'TYPE' object created by
    // invoking a 'bsl::optional' allocator extended 'in_place_t' constructor
    // with the specified 'alloc' as the allocator argument, and specified 'il'
    // and 'args' as the constructor arguments.  Note that this function will
    // fail to compile if TYPE doesn't use allocators.
#endif // defined(BSLS_COMPILERFEATURES_SUPPORT_GENERALIZED_INITIALIZERS)
#elif BSLS_COMPILERFEATURES_SIMULATE_VARIADIC_TEMPLATES
// {{{ BEGIN GENERATED CODE
// The following section is automatically generated.  **DO NOT EDIT**
// Generator command line: sim_cpp11_features.pl bslstl_optional.h
#ifndef BSLSTL_OPTIONAL_VARIADIC_LIMIT
#define BSLSTL_OPTIONAL_VARIADIC_LIMIT 10
#endif
#ifndef BSLSTL_OPTIONAL_VARIADIC_LIMIT_H
#define BSLSTL_OPTIONAL_VARIADIC_LIMIT_H BSLSTL_OPTIONAL_VARIADIC_LIMIT
#endif
#if BSLSTL_OPTIONAL_VARIADIC_LIMIT_H >= 0
template<class TYPE>
BSLS_KEYWORD_CONSTEXPR
bsl::optional<TYPE>
alloc_optional(typename bsl::optional<TYPE>::allocator_type const& alloc);
#endif  // BSLSTL_OPTIONAL_VARIADIC_LIMIT_H >= 0

#if BSLSTL_OPTIONAL_VARIADIC_LIMIT_H >= 1
template<class TYPE, class ARGS_01>
BSLS_KEYWORD_CONSTEXPR
bsl::optional<TYPE>
alloc_optional(typename bsl::optional<TYPE>::allocator_type const& alloc,
               BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_01) args_01);
#endif  // BSLSTL_OPTIONAL_VARIADIC_LIMIT_H >= 1

#if BSLSTL_OPTIONAL_VARIADIC_LIMIT_H >= 2
template<class TYPE, class ARGS_01,
                     class ARGS_02>
BSLS_KEYWORD_CONSTEXPR
bsl::optional<TYPE>
alloc_optional(typename bsl::optional<TYPE>::allocator_type const& alloc,
               BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_01) args_01,
               BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_02) args_02);
#endif  // BSLSTL_OPTIONAL_VARIADIC_LIMIT_H >= 2

#if BSLSTL_OPTIONAL_VARIADIC_LIMIT_H >= 3
template<class TYPE, class ARGS_01,
                     class ARGS_02,
                     class ARGS_03>
BSLS_KEYWORD_CONSTEXPR
bsl::optional<TYPE>
alloc_optional(typename bsl::optional<TYPE>::allocator_type const& alloc,
               BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_01) args_01,
               BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_02) args_02,
               BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_03) args_03);
#endif  // BSLSTL_OPTIONAL_VARIADIC_LIMIT_H >= 3

#if BSLSTL_OPTIONAL_VARIADIC_LIMIT_H >= 4
template<class TYPE, class ARGS_01,
                     class ARGS_02,
                     class ARGS_03,
                     class ARGS_04>
BSLS_KEYWORD_CONSTEXPR
bsl::optional<TYPE>
alloc_optional(typename bsl::optional<TYPE>::allocator_type const& alloc,
               BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_01) args_01,
               BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_02) args_02,
               BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_03) args_03,
               BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_04) args_04);
#endif  // BSLSTL_OPTIONAL_VARIADIC_LIMIT_H >= 4

#if BSLSTL_OPTIONAL_VARIADIC_LIMIT_H >= 5
template<class TYPE, class ARGS_01,
                     class ARGS_02,
                     class ARGS_03,
                     class ARGS_04,
                     class ARGS_05>
BSLS_KEYWORD_CONSTEXPR
bsl::optional<TYPE>
alloc_optional(typename bsl::optional<TYPE>::allocator_type const& alloc,
               BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_01) args_01,
               BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_02) args_02,
               BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_03) args_03,
               BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_04) args_04,
               BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_05) args_05);
#endif  // BSLSTL_OPTIONAL_VARIADIC_LIMIT_H >= 5

#if BSLSTL_OPTIONAL_VARIADIC_LIMIT_H >= 6
template<class TYPE, class ARGS_01,
                     class ARGS_02,
                     class ARGS_03,
                     class ARGS_04,
                     class ARGS_05,
                     class ARGS_06>
BSLS_KEYWORD_CONSTEXPR
bsl::optional<TYPE>
alloc_optional(typename bsl::optional<TYPE>::allocator_type const& alloc,
               BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_01) args_01,
               BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_02) args_02,
               BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_03) args_03,
               BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_04) args_04,
               BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_05) args_05,
               BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_06) args_06);
#endif  // BSLSTL_OPTIONAL_VARIADIC_LIMIT_H >= 6

#if BSLSTL_OPTIONAL_VARIADIC_LIMIT_H >= 7
template<class TYPE, class ARGS_01,
                     class ARGS_02,
                     class ARGS_03,
                     class ARGS_04,
                     class ARGS_05,
                     class ARGS_06,
                     class ARGS_07>
BSLS_KEYWORD_CONSTEXPR
bsl::optional<TYPE>
alloc_optional(typename bsl::optional<TYPE>::allocator_type const& alloc,
               BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_01) args_01,
               BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_02) args_02,
               BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_03) args_03,
               BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_04) args_04,
               BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_05) args_05,
               BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_06) args_06,
               BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_07) args_07);
#endif  // BSLSTL_OPTIONAL_VARIADIC_LIMIT_H >= 7

#if BSLSTL_OPTIONAL_VARIADIC_LIMIT_H >= 8
template<class TYPE, class ARGS_01,
                     class ARGS_02,
                     class ARGS_03,
                     class ARGS_04,
                     class ARGS_05,
                     class ARGS_06,
                     class ARGS_07,
                     class ARGS_08>
BSLS_KEYWORD_CONSTEXPR
bsl::optional<TYPE>
alloc_optional(typename bsl::optional<TYPE>::allocator_type const& alloc,
               BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_01) args_01,
               BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_02) args_02,
               BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_03) args_03,
               BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_04) args_04,
               BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_05) args_05,
               BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_06) args_06,
               BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_07) args_07,
               BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_08) args_08);
#endif  // BSLSTL_OPTIONAL_VARIADIC_LIMIT_H >= 8

#if BSLSTL_OPTIONAL_VARIADIC_LIMIT_H >= 9
template<class TYPE, class ARGS_01,
                     class ARGS_02,
                     class ARGS_03,
                     class ARGS_04,
                     class ARGS_05,
                     class ARGS_06,
                     class ARGS_07,
                     class ARGS_08,
                     class ARGS_09>
BSLS_KEYWORD_CONSTEXPR
bsl::optional<TYPE>
alloc_optional(typename bsl::optional<TYPE>::allocator_type const& alloc,
               BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_01) args_01,
               BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_02) args_02,
               BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_03) args_03,
               BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_04) args_04,
               BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_05) args_05,
               BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_06) args_06,
               BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_07) args_07,
               BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_08) args_08,
               BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_09) args_09);
#endif  // BSLSTL_OPTIONAL_VARIADIC_LIMIT_H >= 9

#if BSLSTL_OPTIONAL_VARIADIC_LIMIT_H >= 10
template<class TYPE, class ARGS_01,
                     class ARGS_02,
                     class ARGS_03,
                     class ARGS_04,
                     class ARGS_05,
                     class ARGS_06,
                     class ARGS_07,
                     class ARGS_08,
                     class ARGS_09,
                     class ARGS_10>
BSLS_KEYWORD_CONSTEXPR
bsl::optional<TYPE>
alloc_optional(typename bsl::optional<TYPE>::allocator_type const& alloc,
               BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_01) args_01,
               BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_02) args_02,
               BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_03) args_03,
               BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_04) args_04,
               BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_05) args_05,
               BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_06) args_06,
               BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_07) args_07,
               BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_08) args_08,
               BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_09) args_09,
               BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_10) args_10);
#endif  // BSLSTL_OPTIONAL_VARIADIC_LIMIT_H >= 10


#if defined(BSLS_COMPILERFEATURES_SUPPORT_GENERALIZED_INITIALIZERS)
#if BSLSTL_OPTIONAL_VARIADIC_LIMIT_H >= 0
template<class TYPE, class IL_TYPE>
BSLS_KEYWORD_CONSTEXPR
bsl::optional<TYPE>
alloc_optional(typename bsl::optional<TYPE>::allocator_type const& alloc,
               std::initializer_list<IL_TYPE>                      il);
#endif  // BSLSTL_OPTIONAL_VARIADIC_LIMIT_H >= 0

#if BSLSTL_OPTIONAL_VARIADIC_LIMIT_H >= 1
template<class TYPE, class IL_TYPE, class ARGS_01>
BSLS_KEYWORD_CONSTEXPR
bsl::optional<TYPE>
alloc_optional(typename bsl::optional<TYPE>::allocator_type const& alloc,
               std::initializer_list<IL_TYPE>                      il,
               BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_01) args_01);
#endif  // BSLSTL_OPTIONAL_VARIADIC_LIMIT_H >= 1

#if BSLSTL_OPTIONAL_VARIADIC_LIMIT_H >= 2
template<class TYPE, class IL_TYPE, class ARGS_01,
                                    class ARGS_02>
BSLS_KEYWORD_CONSTEXPR
bsl::optional<TYPE>
alloc_optional(typename bsl::optional<TYPE>::allocator_type const& alloc,
               std::initializer_list<IL_TYPE>                      il,
               BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_01) args_01,
               BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_02) args_02);
#endif  // BSLSTL_OPTIONAL_VARIADIC_LIMIT_H >= 2

#if BSLSTL_OPTIONAL_VARIADIC_LIMIT_H >= 3
template<class TYPE, class IL_TYPE, class ARGS_01,
                                    class ARGS_02,
                                    class ARGS_03>
BSLS_KEYWORD_CONSTEXPR
bsl::optional<TYPE>
alloc_optional(typename bsl::optional<TYPE>::allocator_type const& alloc,
               std::initializer_list<IL_TYPE>                      il,
               BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_01) args_01,
               BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_02) args_02,
               BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_03) args_03);
#endif  // BSLSTL_OPTIONAL_VARIADIC_LIMIT_H >= 3

#if BSLSTL_OPTIONAL_VARIADIC_LIMIT_H >= 4
template<class TYPE, class IL_TYPE, class ARGS_01,
                                    class ARGS_02,
                                    class ARGS_03,
                                    class ARGS_04>
BSLS_KEYWORD_CONSTEXPR
bsl::optional<TYPE>
alloc_optional(typename bsl::optional<TYPE>::allocator_type const& alloc,
               std::initializer_list<IL_TYPE>                      il,
               BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_01) args_01,
               BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_02) args_02,
               BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_03) args_03,
               BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_04) args_04);
#endif  // BSLSTL_OPTIONAL_VARIADIC_LIMIT_H >= 4

#if BSLSTL_OPTIONAL_VARIADIC_LIMIT_H >= 5
template<class TYPE, class IL_TYPE, class ARGS_01,
                                    class ARGS_02,
                                    class ARGS_03,
                                    class ARGS_04,
                                    class ARGS_05>
BSLS_KEYWORD_CONSTEXPR
bsl::optional<TYPE>
alloc_optional(typename bsl::optional<TYPE>::allocator_type const& alloc,
               std::initializer_list<IL_TYPE>                      il,
               BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_01) args_01,
               BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_02) args_02,
               BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_03) args_03,
               BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_04) args_04,
               BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_05) args_05);
#endif  // BSLSTL_OPTIONAL_VARIADIC_LIMIT_H >= 5

#if BSLSTL_OPTIONAL_VARIADIC_LIMIT_H >= 6
template<class TYPE, class IL_TYPE, class ARGS_01,
                                    class ARGS_02,
                                    class ARGS_03,
                                    class ARGS_04,
                                    class ARGS_05,
                                    class ARGS_06>
BSLS_KEYWORD_CONSTEXPR
bsl::optional<TYPE>
alloc_optional(typename bsl::optional<TYPE>::allocator_type const& alloc,
               std::initializer_list<IL_TYPE>                      il,
               BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_01) args_01,
               BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_02) args_02,
               BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_03) args_03,
               BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_04) args_04,
               BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_05) args_05,
               BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_06) args_06);
#endif  // BSLSTL_OPTIONAL_VARIADIC_LIMIT_H >= 6

#if BSLSTL_OPTIONAL_VARIADIC_LIMIT_H >= 7
template<class TYPE, class IL_TYPE, class ARGS_01,
                                    class ARGS_02,
                                    class ARGS_03,
                                    class ARGS_04,
                                    class ARGS_05,
                                    class ARGS_06,
                                    class ARGS_07>
BSLS_KEYWORD_CONSTEXPR
bsl::optional<TYPE>
alloc_optional(typename bsl::optional<TYPE>::allocator_type const& alloc,
               std::initializer_list<IL_TYPE>                      il,
               BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_01) args_01,
               BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_02) args_02,
               BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_03) args_03,
               BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_04) args_04,
               BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_05) args_05,
               BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_06) args_06,
               BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_07) args_07);
#endif  // BSLSTL_OPTIONAL_VARIADIC_LIMIT_H >= 7

#if BSLSTL_OPTIONAL_VARIADIC_LIMIT_H >= 8
template<class TYPE, class IL_TYPE, class ARGS_01,
                                    class ARGS_02,
                                    class ARGS_03,
                                    class ARGS_04,
                                    class ARGS_05,
                                    class ARGS_06,
                                    class ARGS_07,
                                    class ARGS_08>
BSLS_KEYWORD_CONSTEXPR
bsl::optional<TYPE>
alloc_optional(typename bsl::optional<TYPE>::allocator_type const& alloc,
               std::initializer_list<IL_TYPE>                      il,
               BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_01) args_01,
               BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_02) args_02,
               BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_03) args_03,
               BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_04) args_04,
               BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_05) args_05,
               BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_06) args_06,
               BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_07) args_07,
               BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_08) args_08);
#endif  // BSLSTL_OPTIONAL_VARIADIC_LIMIT_H >= 8

#if BSLSTL_OPTIONAL_VARIADIC_LIMIT_H >= 9
template<class TYPE, class IL_TYPE, class ARGS_01,
                                    class ARGS_02,
                                    class ARGS_03,
                                    class ARGS_04,
                                    class ARGS_05,
                                    class ARGS_06,
                                    class ARGS_07,
                                    class ARGS_08,
                                    class ARGS_09>
BSLS_KEYWORD_CONSTEXPR
bsl::optional<TYPE>
alloc_optional(typename bsl::optional<TYPE>::allocator_type const& alloc,
               std::initializer_list<IL_TYPE>                      il,
               BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_01) args_01,
               BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_02) args_02,
               BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_03) args_03,
               BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_04) args_04,
               BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_05) args_05,
               BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_06) args_06,
               BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_07) args_07,
               BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_08) args_08,
               BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_09) args_09);
#endif  // BSLSTL_OPTIONAL_VARIADIC_LIMIT_H >= 9

#if BSLSTL_OPTIONAL_VARIADIC_LIMIT_H >= 10
template<class TYPE, class IL_TYPE, class ARGS_01,
                                    class ARGS_02,
                                    class ARGS_03,
                                    class ARGS_04,
                                    class ARGS_05,
                                    class ARGS_06,
                                    class ARGS_07,
                                    class ARGS_08,
                                    class ARGS_09,
                                    class ARGS_10>
BSLS_KEYWORD_CONSTEXPR
bsl::optional<TYPE>
alloc_optional(typename bsl::optional<TYPE>::allocator_type const& alloc,
               std::initializer_list<IL_TYPE>                      il,
               BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_01) args_01,
               BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_02) args_02,
               BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_03) args_03,
               BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_04) args_04,
               BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_05) args_05,
               BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_06) args_06,
               BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_07) args_07,
               BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_08) args_08,
               BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_09) args_09,
               BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_10) args_10);
#endif  // BSLSTL_OPTIONAL_VARIADIC_LIMIT_H >= 10

#endif
#else
// The generated code below is a workaround for the absence of perfect
// forwarding in some compilers.
template<class TYPE, class... ARGS>
BSLS_KEYWORD_CONSTEXPR
bsl::optional<TYPE>
alloc_optional(typename bsl::optional<TYPE>::allocator_type const& alloc,
               BSLS_COMPILERFEATURES_FORWARD_REF(ARGS)...          args);

#if defined(BSLS_COMPILERFEATURES_SUPPORT_GENERALIZED_INITIALIZERS)
template<class TYPE, class IL_TYPE, class... ARGS>
BSLS_KEYWORD_CONSTEXPR
bsl::optional<TYPE>
alloc_optional(typename bsl::optional<TYPE>::allocator_type const& alloc,
               std::initializer_list<IL_TYPE>                      il,
               BSLS_COMPILERFEATURES_FORWARD_REF(ARGS)...          args);
#endif
// }}} END GENERATED CODE
#endif

template<class TYPE>
BSLS_KEYWORD_CONSTEXPR
bsl::optional<typename bsl::decay<TYPE>::type >
make_optional(BSLS_COMPILERFEATURES_FORWARD_REF(TYPE) rhs);
    // Return an 'optional' object containing a 'TYPE' object created by
    // invoking a 'bsl::optional' constructor with the specified 'rhs' as the
    // constructor argument.  If TYPE uses an allocator, the default allocator
    // will be used for the 'optional' object.

#if !BSLS_COMPILERFEATURES_SIMULATE_CPP11_FEATURES
template<class TYPE, class... ARGS>
BSLS_KEYWORD_CONSTEXPR
bsl::optional<TYPE>
make_optional(BSLS_COMPILERFEATURES_FORWARD_REF(ARGS)... args);
    // Return an 'optional' object containing a 'TYPE' object created by
    // invoking a 'bsl::optional' 'in_place_t' constructor with the specified
    // 'args' as the constructor arguments.  If TYPE uses an allocator, the
    // default allocator will be used for the 'optional' object.

#if defined(BSLS_COMPILERFEATURES_SUPPORT_GENERALIZED_INITIALIZERS)
template<class TYPE, class IL_TYPE, class... ARGS>
BSLS_KEYWORD_CONSTEXPR
bsl::optional<TYPE>
make_optional(std::initializer_list<IL_TYPE>             il,
              BSLS_COMPILERFEATURES_FORWARD_REF(ARGS)... args);
    // Return an 'optional' object containing a 'TYPE' object created by
    // invoking a 'bsl::optional' 'in_place_t' constructor with the specified
    // 'il' and 'args' as the constructor arguments.  If TYPE uses an
    // allocator, the default allocator will be used for the 'optional' object.

#endif // defined(BSLS_COMPILERFEATURES_SUPPORT_GENERALIZED_INITIALIZERS)
#elif BSLS_COMPILERFEATURES_SIMULATE_VARIADIC_TEMPLATES
// {{{ BEGIN GENERATED CODE
// The following section is automatically generated.  **DO NOT EDIT**
// Generator command line: sim_cpp11_features.pl bslstl_optional.h
#ifndef BSLSTL_OPTIONAL_VARIADIC_LIMIT
#define BSLSTL_OPTIONAL_VARIADIC_LIMIT 10
#endif
#ifndef BSLSTL_OPTIONAL_VARIADIC_LIMIT_I
#define BSLSTL_OPTIONAL_VARIADIC_LIMIT_I BSLSTL_OPTIONAL_VARIADIC_LIMIT
#endif
#if BSLSTL_OPTIONAL_VARIADIC_LIMIT_I >= 0
template<class TYPE>
BSLS_KEYWORD_CONSTEXPR
bsl::optional<TYPE>
make_optional();
#endif  // BSLSTL_OPTIONAL_VARIADIC_LIMIT_I >= 0

#if BSLSTL_OPTIONAL_VARIADIC_LIMIT_I >= 1
template<class TYPE, class ARGS_01>
BSLS_KEYWORD_CONSTEXPR
bsl::optional<TYPE>
make_optional(BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_01) args_01);
#endif  // BSLSTL_OPTIONAL_VARIADIC_LIMIT_I >= 1

#if BSLSTL_OPTIONAL_VARIADIC_LIMIT_I >= 2
template<class TYPE, class ARGS_01,
                     class ARGS_02>
BSLS_KEYWORD_CONSTEXPR
bsl::optional<TYPE>
make_optional(BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_01) args_01,
              BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_02) args_02);
#endif  // BSLSTL_OPTIONAL_VARIADIC_LIMIT_I >= 2

#if BSLSTL_OPTIONAL_VARIADIC_LIMIT_I >= 3
template<class TYPE, class ARGS_01,
                     class ARGS_02,
                     class ARGS_03>
BSLS_KEYWORD_CONSTEXPR
bsl::optional<TYPE>
make_optional(BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_01) args_01,
              BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_02) args_02,
              BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_03) args_03);
#endif  // BSLSTL_OPTIONAL_VARIADIC_LIMIT_I >= 3

#if BSLSTL_OPTIONAL_VARIADIC_LIMIT_I >= 4
template<class TYPE, class ARGS_01,
                     class ARGS_02,
                     class ARGS_03,
                     class ARGS_04>
BSLS_KEYWORD_CONSTEXPR
bsl::optional<TYPE>
make_optional(BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_01) args_01,
              BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_02) args_02,
              BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_03) args_03,
              BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_04) args_04);
#endif  // BSLSTL_OPTIONAL_VARIADIC_LIMIT_I >= 4

#if BSLSTL_OPTIONAL_VARIADIC_LIMIT_I >= 5
template<class TYPE, class ARGS_01,
                     class ARGS_02,
                     class ARGS_03,
                     class ARGS_04,
                     class ARGS_05>
BSLS_KEYWORD_CONSTEXPR
bsl::optional<TYPE>
make_optional(BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_01) args_01,
              BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_02) args_02,
              BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_03) args_03,
              BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_04) args_04,
              BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_05) args_05);
#endif  // BSLSTL_OPTIONAL_VARIADIC_LIMIT_I >= 5

#if BSLSTL_OPTIONAL_VARIADIC_LIMIT_I >= 6
template<class TYPE, class ARGS_01,
                     class ARGS_02,
                     class ARGS_03,
                     class ARGS_04,
                     class ARGS_05,
                     class ARGS_06>
BSLS_KEYWORD_CONSTEXPR
bsl::optional<TYPE>
make_optional(BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_01) args_01,
              BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_02) args_02,
              BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_03) args_03,
              BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_04) args_04,
              BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_05) args_05,
              BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_06) args_06);
#endif  // BSLSTL_OPTIONAL_VARIADIC_LIMIT_I >= 6

#if BSLSTL_OPTIONAL_VARIADIC_LIMIT_I >= 7
template<class TYPE, class ARGS_01,
                     class ARGS_02,
                     class ARGS_03,
                     class ARGS_04,
                     class ARGS_05,
                     class ARGS_06,
                     class ARGS_07>
BSLS_KEYWORD_CONSTEXPR
bsl::optional<TYPE>
make_optional(BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_01) args_01,
              BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_02) args_02,
              BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_03) args_03,
              BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_04) args_04,
              BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_05) args_05,
              BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_06) args_06,
              BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_07) args_07);
#endif  // BSLSTL_OPTIONAL_VARIADIC_LIMIT_I >= 7

#if BSLSTL_OPTIONAL_VARIADIC_LIMIT_I >= 8
template<class TYPE, class ARGS_01,
                     class ARGS_02,
                     class ARGS_03,
                     class ARGS_04,
                     class ARGS_05,
                     class ARGS_06,
                     class ARGS_07,
                     class ARGS_08>
BSLS_KEYWORD_CONSTEXPR
bsl::optional<TYPE>
make_optional(BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_01) args_01,
              BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_02) args_02,
              BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_03) args_03,
              BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_04) args_04,
              BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_05) args_05,
              BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_06) args_06,
              BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_07) args_07,
              BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_08) args_08);
#endif  // BSLSTL_OPTIONAL_VARIADIC_LIMIT_I >= 8

#if BSLSTL_OPTIONAL_VARIADIC_LIMIT_I >= 9
template<class TYPE, class ARGS_01,
                     class ARGS_02,
                     class ARGS_03,
                     class ARGS_04,
                     class ARGS_05,
                     class ARGS_06,
                     class ARGS_07,
                     class ARGS_08,
                     class ARGS_09>
BSLS_KEYWORD_CONSTEXPR
bsl::optional<TYPE>
make_optional(BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_01) args_01,
              BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_02) args_02,
              BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_03) args_03,
              BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_04) args_04,
              BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_05) args_05,
              BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_06) args_06,
              BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_07) args_07,
              BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_08) args_08,
              BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_09) args_09);
#endif  // BSLSTL_OPTIONAL_VARIADIC_LIMIT_I >= 9

#if BSLSTL_OPTIONAL_VARIADIC_LIMIT_I >= 10
template<class TYPE, class ARGS_01,
                     class ARGS_02,
                     class ARGS_03,
                     class ARGS_04,
                     class ARGS_05,
                     class ARGS_06,
                     class ARGS_07,
                     class ARGS_08,
                     class ARGS_09,
                     class ARGS_10>
BSLS_KEYWORD_CONSTEXPR
bsl::optional<TYPE>
make_optional(BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_01) args_01,
              BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_02) args_02,
              BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_03) args_03,
              BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_04) args_04,
              BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_05) args_05,
              BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_06) args_06,
              BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_07) args_07,
              BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_08) args_08,
              BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_09) args_09,
              BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_10) args_10);
#endif  // BSLSTL_OPTIONAL_VARIADIC_LIMIT_I >= 10


#if defined(BSLS_COMPILERFEATURES_SUPPORT_GENERALIZED_INITIALIZERS)
#if BSLSTL_OPTIONAL_VARIADIC_LIMIT_I >= 0
template<class TYPE, class IL_TYPE>
BSLS_KEYWORD_CONSTEXPR
bsl::optional<TYPE>
make_optional(std::initializer_list<IL_TYPE>             il);
#endif  // BSLSTL_OPTIONAL_VARIADIC_LIMIT_I >= 0

#if BSLSTL_OPTIONAL_VARIADIC_LIMIT_I >= 1
template<class TYPE, class IL_TYPE, class ARGS_01>
BSLS_KEYWORD_CONSTEXPR
bsl::optional<TYPE>
make_optional(std::initializer_list<IL_TYPE>             il,
              BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_01) args_01);
#endif  // BSLSTL_OPTIONAL_VARIADIC_LIMIT_I >= 1

#if BSLSTL_OPTIONAL_VARIADIC_LIMIT_I >= 2
template<class TYPE, class IL_TYPE, class ARGS_01,
                                    class ARGS_02>
BSLS_KEYWORD_CONSTEXPR
bsl::optional<TYPE>
make_optional(std::initializer_list<IL_TYPE>             il,
              BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_01) args_01,
              BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_02) args_02);
#endif  // BSLSTL_OPTIONAL_VARIADIC_LIMIT_I >= 2

#if BSLSTL_OPTIONAL_VARIADIC_LIMIT_I >= 3
template<class TYPE, class IL_TYPE, class ARGS_01,
                                    class ARGS_02,
                                    class ARGS_03>
BSLS_KEYWORD_CONSTEXPR
bsl::optional<TYPE>
make_optional(std::initializer_list<IL_TYPE>             il,
              BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_01) args_01,
              BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_02) args_02,
              BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_03) args_03);
#endif  // BSLSTL_OPTIONAL_VARIADIC_LIMIT_I >= 3

#if BSLSTL_OPTIONAL_VARIADIC_LIMIT_I >= 4
template<class TYPE, class IL_TYPE, class ARGS_01,
                                    class ARGS_02,
                                    class ARGS_03,
                                    class ARGS_04>
BSLS_KEYWORD_CONSTEXPR
bsl::optional<TYPE>
make_optional(std::initializer_list<IL_TYPE>             il,
              BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_01) args_01,
              BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_02) args_02,
              BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_03) args_03,
              BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_04) args_04);
#endif  // BSLSTL_OPTIONAL_VARIADIC_LIMIT_I >= 4

#if BSLSTL_OPTIONAL_VARIADIC_LIMIT_I >= 5
template<class TYPE, class IL_TYPE, class ARGS_01,
                                    class ARGS_02,
                                    class ARGS_03,
                                    class ARGS_04,
                                    class ARGS_05>
BSLS_KEYWORD_CONSTEXPR
bsl::optional<TYPE>
make_optional(std::initializer_list<IL_TYPE>             il,
              BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_01) args_01,
              BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_02) args_02,
              BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_03) args_03,
              BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_04) args_04,
              BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_05) args_05);
#endif  // BSLSTL_OPTIONAL_VARIADIC_LIMIT_I >= 5

#if BSLSTL_OPTIONAL_VARIADIC_LIMIT_I >= 6
template<class TYPE, class IL_TYPE, class ARGS_01,
                                    class ARGS_02,
                                    class ARGS_03,
                                    class ARGS_04,
                                    class ARGS_05,
                                    class ARGS_06>
BSLS_KEYWORD_CONSTEXPR
bsl::optional<TYPE>
make_optional(std::initializer_list<IL_TYPE>             il,
              BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_01) args_01,
              BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_02) args_02,
              BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_03) args_03,
              BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_04) args_04,
              BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_05) args_05,
              BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_06) args_06);
#endif  // BSLSTL_OPTIONAL_VARIADIC_LIMIT_I >= 6

#if BSLSTL_OPTIONAL_VARIADIC_LIMIT_I >= 7
template<class TYPE, class IL_TYPE, class ARGS_01,
                                    class ARGS_02,
                                    class ARGS_03,
                                    class ARGS_04,
                                    class ARGS_05,
                                    class ARGS_06,
                                    class ARGS_07>
BSLS_KEYWORD_CONSTEXPR
bsl::optional<TYPE>
make_optional(std::initializer_list<IL_TYPE>             il,
              BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_01) args_01,
              BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_02) args_02,
              BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_03) args_03,
              BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_04) args_04,
              BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_05) args_05,
              BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_06) args_06,
              BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_07) args_07);
#endif  // BSLSTL_OPTIONAL_VARIADIC_LIMIT_I >= 7

#if BSLSTL_OPTIONAL_VARIADIC_LIMIT_I >= 8
template<class TYPE, class IL_TYPE, class ARGS_01,
                                    class ARGS_02,
                                    class ARGS_03,
                                    class ARGS_04,
                                    class ARGS_05,
                                    class ARGS_06,
                                    class ARGS_07,
                                    class ARGS_08>
BSLS_KEYWORD_CONSTEXPR
bsl::optional<TYPE>
make_optional(std::initializer_list<IL_TYPE>             il,
              BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_01) args_01,
              BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_02) args_02,
              BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_03) args_03,
              BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_04) args_04,
              BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_05) args_05,
              BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_06) args_06,
              BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_07) args_07,
              BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_08) args_08);
#endif  // BSLSTL_OPTIONAL_VARIADIC_LIMIT_I >= 8

#if BSLSTL_OPTIONAL_VARIADIC_LIMIT_I >= 9
template<class TYPE, class IL_TYPE, class ARGS_01,
                                    class ARGS_02,
                                    class ARGS_03,
                                    class ARGS_04,
                                    class ARGS_05,
                                    class ARGS_06,
                                    class ARGS_07,
                                    class ARGS_08,
                                    class ARGS_09>
BSLS_KEYWORD_CONSTEXPR
bsl::optional<TYPE>
make_optional(std::initializer_list<IL_TYPE>             il,
              BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_01) args_01,
              BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_02) args_02,
              BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_03) args_03,
              BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_04) args_04,
              BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_05) args_05,
              BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_06) args_06,
              BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_07) args_07,
              BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_08) args_08,
              BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_09) args_09);
#endif  // BSLSTL_OPTIONAL_VARIADIC_LIMIT_I >= 9

#if BSLSTL_OPTIONAL_VARIADIC_LIMIT_I >= 10
template<class TYPE, class IL_TYPE, class ARGS_01,
                                    class ARGS_02,
                                    class ARGS_03,
                                    class ARGS_04,
                                    class ARGS_05,
                                    class ARGS_06,
                                    class ARGS_07,
                                    class ARGS_08,
                                    class ARGS_09,
                                    class ARGS_10>
BSLS_KEYWORD_CONSTEXPR
bsl::optional<TYPE>
make_optional(std::initializer_list<IL_TYPE>             il,
              BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_01) args_01,
              BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_02) args_02,
              BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_03) args_03,
              BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_04) args_04,
              BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_05) args_05,
              BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_06) args_06,
              BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_07) args_07,
              BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_08) args_08,
              BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_09) args_09,
              BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_10) args_10);
#endif  // BSLSTL_OPTIONAL_VARIADIC_LIMIT_I >= 10


#endif
#else
// The generated code below is a workaround for the absence of perfect
// forwarding in some compilers.
template<class TYPE, class... ARGS>
BSLS_KEYWORD_CONSTEXPR
bsl::optional<TYPE>
make_optional(BSLS_COMPILERFEATURES_FORWARD_REF(ARGS)... args);

#if defined(BSLS_COMPILERFEATURES_SUPPORT_GENERALIZED_INITIALIZERS)
template<class TYPE, class IL_TYPE, class... ARGS>
BSLS_KEYWORD_CONSTEXPR
bsl::optional<TYPE>
make_optional(std::initializer_list<IL_TYPE>             il,
              BSLS_COMPILERFEATURES_FORWARD_REF(ARGS)... args);

#endif
// }}} END GENERATED CODE
#endif

// ============================================================================
//                           INLINE DEFINITIONS
// ============================================================================

                      // -------------------------
                      // class optional<TYPE>
                      // -------------------------

// CREATORS
template <class TYPE, bool USES_BSLMA_ALLOC>
inline
optional<TYPE, USES_BSLMA_ALLOC>::optional()
{}

template <class TYPE, bool USES_BSLMA_ALLOC>
inline
optional<TYPE, USES_BSLMA_ALLOC>::optional(bsl::nullopt_t)
{}

template <class TYPE, bool USES_BSLMA_ALLOC>
inline
optional<TYPE, USES_BSLMA_ALLOC>::optional(const optional& rhs)
{
    if (rhs.has_value()) {
        emplace(rhs.value());
    }
}

template <class TYPE, bool USES_BSLMA_ALLOC>
inline
optional<TYPE, USES_BSLMA_ALLOC>::optional(
                                  BloombergLP::bslmf::MovableRef<optional> rhs)
: d_allocator(MoveUtil::access(rhs).get_allocator())
{
    optional& lvalue = rhs;

    if (lvalue.has_value()) {
        emplace(MoveUtil::move(lvalue.value()));
    }
}

#if !BSLS_COMPILERFEATURES_SIMULATE_CPP11_FEATURES
template <class TYPE, bool USES_BSLMA_ALLOC>
template<class... ARGS>
inline
optional<TYPE, USES_BSLMA_ALLOC>::optional(
                               bsl::in_place_t,
                               BSLS_COMPILERFEATURES_FORWARD_REF(ARGS)... args)
{
    emplace(BSLS_COMPILERFEATURES_FORWARD(ARGS, args)...);
}

#if defined(BSLS_COMPILERFEATURES_SUPPORT_GENERALIZED_INITIALIZERS)
template <class TYPE, bool USES_BSLMA_ALLOC>
template<class IL_TYPE, class... ARGS>
inline
optional<TYPE, USES_BSLMA_ALLOC>::optional(
                               bsl::in_place_t,
                               std::initializer_list<IL_TYPE>             it,
                               BSLS_COMPILERFEATURES_FORWARD_REF(ARGS)... args)
{
    emplace(it, BSLS_COMPILERFEATURES_FORWARD(ARGS, args)...);
}
#endif//BSLS_COMPILERFEATURES_SUPPORT_GENERALIZED_INITIALIZERS
#elif BSLS_COMPILERFEATURES_SIMULATE_VARIADIC_TEMPLATES
// {{{ BEGIN GENERATED CODE
// The following section is automatically generated.  **DO NOT EDIT**
// Generator command line: sim_cpp11_features.pl bslstl_optional.h
#ifndef BSLSTL_OPTIONAL_VARIADIC_LIMIT
#define BSLSTL_OPTIONAL_VARIADIC_LIMIT 10
#endif
#ifndef BSLSTL_OPTIONAL_VARIADIC_LIMIT_J
#define BSLSTL_OPTIONAL_VARIADIC_LIMIT_J BSLSTL_OPTIONAL_VARIADIC_LIMIT
#endif
#if BSLSTL_OPTIONAL_VARIADIC_LIMIT_J >= 0
template <class TYPE, bool USES_BSLMA_ALLOC>
inline
optional<TYPE, USES_BSLMA_ALLOC>::optional(
                               bsl::in_place_t)
{
    emplace();
}
#endif  // BSLSTL_OPTIONAL_VARIADIC_LIMIT_J >= 0

#if BSLSTL_OPTIONAL_VARIADIC_LIMIT_J >= 1
template <class TYPE, bool USES_BSLMA_ALLOC>
template<class ARGS_01>
inline
optional<TYPE, USES_BSLMA_ALLOC>::optional(
                               bsl::in_place_t,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_01) args_01)
{
    emplace(BSLS_COMPILERFEATURES_FORWARD(ARGS_01, args_01));
}
#endif  // BSLSTL_OPTIONAL_VARIADIC_LIMIT_J >= 1

#if BSLSTL_OPTIONAL_VARIADIC_LIMIT_J >= 2
template <class TYPE, bool USES_BSLMA_ALLOC>
template<class ARGS_01,
         class ARGS_02>
inline
optional<TYPE, USES_BSLMA_ALLOC>::optional(
                               bsl::in_place_t,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_01) args_01,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_02) args_02)
{
    emplace(BSLS_COMPILERFEATURES_FORWARD(ARGS_01, args_01),
            BSLS_COMPILERFEATURES_FORWARD(ARGS_02, args_02));
}
#endif  // BSLSTL_OPTIONAL_VARIADIC_LIMIT_J >= 2

#if BSLSTL_OPTIONAL_VARIADIC_LIMIT_J >= 3
template <class TYPE, bool USES_BSLMA_ALLOC>
template<class ARGS_01,
         class ARGS_02,
         class ARGS_03>
inline
optional<TYPE, USES_BSLMA_ALLOC>::optional(
                               bsl::in_place_t,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_01) args_01,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_02) args_02,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_03) args_03)
{
    emplace(BSLS_COMPILERFEATURES_FORWARD(ARGS_01, args_01),
            BSLS_COMPILERFEATURES_FORWARD(ARGS_02, args_02),
            BSLS_COMPILERFEATURES_FORWARD(ARGS_03, args_03));
}
#endif  // BSLSTL_OPTIONAL_VARIADIC_LIMIT_J >= 3

#if BSLSTL_OPTIONAL_VARIADIC_LIMIT_J >= 4
template <class TYPE, bool USES_BSLMA_ALLOC>
template<class ARGS_01,
         class ARGS_02,
         class ARGS_03,
         class ARGS_04>
inline
optional<TYPE, USES_BSLMA_ALLOC>::optional(
                               bsl::in_place_t,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_01) args_01,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_02) args_02,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_03) args_03,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_04) args_04)
{
    emplace(BSLS_COMPILERFEATURES_FORWARD(ARGS_01, args_01),
            BSLS_COMPILERFEATURES_FORWARD(ARGS_02, args_02),
            BSLS_COMPILERFEATURES_FORWARD(ARGS_03, args_03),
            BSLS_COMPILERFEATURES_FORWARD(ARGS_04, args_04));
}
#endif  // BSLSTL_OPTIONAL_VARIADIC_LIMIT_J >= 4

#if BSLSTL_OPTIONAL_VARIADIC_LIMIT_J >= 5
template <class TYPE, bool USES_BSLMA_ALLOC>
template<class ARGS_01,
         class ARGS_02,
         class ARGS_03,
         class ARGS_04,
         class ARGS_05>
inline
optional<TYPE, USES_BSLMA_ALLOC>::optional(
                               bsl::in_place_t,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_01) args_01,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_02) args_02,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_03) args_03,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_04) args_04,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_05) args_05)
{
    emplace(BSLS_COMPILERFEATURES_FORWARD(ARGS_01, args_01),
            BSLS_COMPILERFEATURES_FORWARD(ARGS_02, args_02),
            BSLS_COMPILERFEATURES_FORWARD(ARGS_03, args_03),
            BSLS_COMPILERFEATURES_FORWARD(ARGS_04, args_04),
            BSLS_COMPILERFEATURES_FORWARD(ARGS_05, args_05));
}
#endif  // BSLSTL_OPTIONAL_VARIADIC_LIMIT_J >= 5

#if BSLSTL_OPTIONAL_VARIADIC_LIMIT_J >= 6
template <class TYPE, bool USES_BSLMA_ALLOC>
template<class ARGS_01,
         class ARGS_02,
         class ARGS_03,
         class ARGS_04,
         class ARGS_05,
         class ARGS_06>
inline
optional<TYPE, USES_BSLMA_ALLOC>::optional(
                               bsl::in_place_t,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_01) args_01,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_02) args_02,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_03) args_03,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_04) args_04,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_05) args_05,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_06) args_06)
{
    emplace(BSLS_COMPILERFEATURES_FORWARD(ARGS_01, args_01),
            BSLS_COMPILERFEATURES_FORWARD(ARGS_02, args_02),
            BSLS_COMPILERFEATURES_FORWARD(ARGS_03, args_03),
            BSLS_COMPILERFEATURES_FORWARD(ARGS_04, args_04),
            BSLS_COMPILERFEATURES_FORWARD(ARGS_05, args_05),
            BSLS_COMPILERFEATURES_FORWARD(ARGS_06, args_06));
}
#endif  // BSLSTL_OPTIONAL_VARIADIC_LIMIT_J >= 6

#if BSLSTL_OPTIONAL_VARIADIC_LIMIT_J >= 7
template <class TYPE, bool USES_BSLMA_ALLOC>
template<class ARGS_01,
         class ARGS_02,
         class ARGS_03,
         class ARGS_04,
         class ARGS_05,
         class ARGS_06,
         class ARGS_07>
inline
optional<TYPE, USES_BSLMA_ALLOC>::optional(
                               bsl::in_place_t,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_01) args_01,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_02) args_02,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_03) args_03,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_04) args_04,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_05) args_05,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_06) args_06,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_07) args_07)
{
    emplace(BSLS_COMPILERFEATURES_FORWARD(ARGS_01, args_01),
            BSLS_COMPILERFEATURES_FORWARD(ARGS_02, args_02),
            BSLS_COMPILERFEATURES_FORWARD(ARGS_03, args_03),
            BSLS_COMPILERFEATURES_FORWARD(ARGS_04, args_04),
            BSLS_COMPILERFEATURES_FORWARD(ARGS_05, args_05),
            BSLS_COMPILERFEATURES_FORWARD(ARGS_06, args_06),
            BSLS_COMPILERFEATURES_FORWARD(ARGS_07, args_07));
}
#endif  // BSLSTL_OPTIONAL_VARIADIC_LIMIT_J >= 7

#if BSLSTL_OPTIONAL_VARIADIC_LIMIT_J >= 8
template <class TYPE, bool USES_BSLMA_ALLOC>
template<class ARGS_01,
         class ARGS_02,
         class ARGS_03,
         class ARGS_04,
         class ARGS_05,
         class ARGS_06,
         class ARGS_07,
         class ARGS_08>
inline
optional<TYPE, USES_BSLMA_ALLOC>::optional(
                               bsl::in_place_t,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_01) args_01,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_02) args_02,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_03) args_03,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_04) args_04,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_05) args_05,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_06) args_06,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_07) args_07,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_08) args_08)
{
    emplace(BSLS_COMPILERFEATURES_FORWARD(ARGS_01, args_01),
            BSLS_COMPILERFEATURES_FORWARD(ARGS_02, args_02),
            BSLS_COMPILERFEATURES_FORWARD(ARGS_03, args_03),
            BSLS_COMPILERFEATURES_FORWARD(ARGS_04, args_04),
            BSLS_COMPILERFEATURES_FORWARD(ARGS_05, args_05),
            BSLS_COMPILERFEATURES_FORWARD(ARGS_06, args_06),
            BSLS_COMPILERFEATURES_FORWARD(ARGS_07, args_07),
            BSLS_COMPILERFEATURES_FORWARD(ARGS_08, args_08));
}
#endif  // BSLSTL_OPTIONAL_VARIADIC_LIMIT_J >= 8

#if BSLSTL_OPTIONAL_VARIADIC_LIMIT_J >= 9
template <class TYPE, bool USES_BSLMA_ALLOC>
template<class ARGS_01,
         class ARGS_02,
         class ARGS_03,
         class ARGS_04,
         class ARGS_05,
         class ARGS_06,
         class ARGS_07,
         class ARGS_08,
         class ARGS_09>
inline
optional<TYPE, USES_BSLMA_ALLOC>::optional(
                               bsl::in_place_t,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_01) args_01,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_02) args_02,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_03) args_03,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_04) args_04,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_05) args_05,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_06) args_06,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_07) args_07,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_08) args_08,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_09) args_09)
{
    emplace(BSLS_COMPILERFEATURES_FORWARD(ARGS_01, args_01),
            BSLS_COMPILERFEATURES_FORWARD(ARGS_02, args_02),
            BSLS_COMPILERFEATURES_FORWARD(ARGS_03, args_03),
            BSLS_COMPILERFEATURES_FORWARD(ARGS_04, args_04),
            BSLS_COMPILERFEATURES_FORWARD(ARGS_05, args_05),
            BSLS_COMPILERFEATURES_FORWARD(ARGS_06, args_06),
            BSLS_COMPILERFEATURES_FORWARD(ARGS_07, args_07),
            BSLS_COMPILERFEATURES_FORWARD(ARGS_08, args_08),
            BSLS_COMPILERFEATURES_FORWARD(ARGS_09, args_09));
}
#endif  // BSLSTL_OPTIONAL_VARIADIC_LIMIT_J >= 9

#if BSLSTL_OPTIONAL_VARIADIC_LIMIT_J >= 10
template <class TYPE, bool USES_BSLMA_ALLOC>
template<class ARGS_01,
         class ARGS_02,
         class ARGS_03,
         class ARGS_04,
         class ARGS_05,
         class ARGS_06,
         class ARGS_07,
         class ARGS_08,
         class ARGS_09,
         class ARGS_10>
inline
optional<TYPE, USES_BSLMA_ALLOC>::optional(
                               bsl::in_place_t,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_01) args_01,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_02) args_02,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_03) args_03,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_04) args_04,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_05) args_05,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_06) args_06,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_07) args_07,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_08) args_08,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_09) args_09,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_10) args_10)
{
    emplace(BSLS_COMPILERFEATURES_FORWARD(ARGS_01, args_01),
            BSLS_COMPILERFEATURES_FORWARD(ARGS_02, args_02),
            BSLS_COMPILERFEATURES_FORWARD(ARGS_03, args_03),
            BSLS_COMPILERFEATURES_FORWARD(ARGS_04, args_04),
            BSLS_COMPILERFEATURES_FORWARD(ARGS_05, args_05),
            BSLS_COMPILERFEATURES_FORWARD(ARGS_06, args_06),
            BSLS_COMPILERFEATURES_FORWARD(ARGS_07, args_07),
            BSLS_COMPILERFEATURES_FORWARD(ARGS_08, args_08),
            BSLS_COMPILERFEATURES_FORWARD(ARGS_09, args_09),
            BSLS_COMPILERFEATURES_FORWARD(ARGS_10, args_10));
}
#endif  // BSLSTL_OPTIONAL_VARIADIC_LIMIT_J >= 10


#if defined(BSLS_COMPILERFEATURES_SUPPORT_GENERALIZED_INITIALIZERS)
#if BSLSTL_OPTIONAL_VARIADIC_LIMIT_J >= 0
template <class TYPE, bool USES_BSLMA_ALLOC>
template<class IL_TYPE>
inline
optional<TYPE, USES_BSLMA_ALLOC>::optional(
                               bsl::in_place_t,
                               std::initializer_list<IL_TYPE>             it)
{
    emplace(it);
}
#endif  // BSLSTL_OPTIONAL_VARIADIC_LIMIT_J >= 0

#if BSLSTL_OPTIONAL_VARIADIC_LIMIT_J >= 1
template <class TYPE, bool USES_BSLMA_ALLOC>
template<class IL_TYPE, class ARGS_01>
inline
optional<TYPE, USES_BSLMA_ALLOC>::optional(
                               bsl::in_place_t,
                               std::initializer_list<IL_TYPE>             it,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_01) args_01)
{
    emplace(it, BSLS_COMPILERFEATURES_FORWARD(ARGS_01, args_01));
}
#endif  // BSLSTL_OPTIONAL_VARIADIC_LIMIT_J >= 1

#if BSLSTL_OPTIONAL_VARIADIC_LIMIT_J >= 2
template <class TYPE, bool USES_BSLMA_ALLOC>
template<class IL_TYPE, class ARGS_01,
                        class ARGS_02>
inline
optional<TYPE, USES_BSLMA_ALLOC>::optional(
                               bsl::in_place_t,
                               std::initializer_list<IL_TYPE>             it,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_01) args_01,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_02) args_02)
{
    emplace(it, BSLS_COMPILERFEATURES_FORWARD(ARGS_01, args_01),
                BSLS_COMPILERFEATURES_FORWARD(ARGS_02, args_02));
}
#endif  // BSLSTL_OPTIONAL_VARIADIC_LIMIT_J >= 2

#if BSLSTL_OPTIONAL_VARIADIC_LIMIT_J >= 3
template <class TYPE, bool USES_BSLMA_ALLOC>
template<class IL_TYPE, class ARGS_01,
                        class ARGS_02,
                        class ARGS_03>
inline
optional<TYPE, USES_BSLMA_ALLOC>::optional(
                               bsl::in_place_t,
                               std::initializer_list<IL_TYPE>             it,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_01) args_01,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_02) args_02,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_03) args_03)
{
    emplace(it, BSLS_COMPILERFEATURES_FORWARD(ARGS_01, args_01),
                BSLS_COMPILERFEATURES_FORWARD(ARGS_02, args_02),
                BSLS_COMPILERFEATURES_FORWARD(ARGS_03, args_03));
}
#endif  // BSLSTL_OPTIONAL_VARIADIC_LIMIT_J >= 3

#if BSLSTL_OPTIONAL_VARIADIC_LIMIT_J >= 4
template <class TYPE, bool USES_BSLMA_ALLOC>
template<class IL_TYPE, class ARGS_01,
                        class ARGS_02,
                        class ARGS_03,
                        class ARGS_04>
inline
optional<TYPE, USES_BSLMA_ALLOC>::optional(
                               bsl::in_place_t,
                               std::initializer_list<IL_TYPE>             it,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_01) args_01,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_02) args_02,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_03) args_03,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_04) args_04)
{
    emplace(it, BSLS_COMPILERFEATURES_FORWARD(ARGS_01, args_01),
                BSLS_COMPILERFEATURES_FORWARD(ARGS_02, args_02),
                BSLS_COMPILERFEATURES_FORWARD(ARGS_03, args_03),
                BSLS_COMPILERFEATURES_FORWARD(ARGS_04, args_04));
}
#endif  // BSLSTL_OPTIONAL_VARIADIC_LIMIT_J >= 4

#if BSLSTL_OPTIONAL_VARIADIC_LIMIT_J >= 5
template <class TYPE, bool USES_BSLMA_ALLOC>
template<class IL_TYPE, class ARGS_01,
                        class ARGS_02,
                        class ARGS_03,
                        class ARGS_04,
                        class ARGS_05>
inline
optional<TYPE, USES_BSLMA_ALLOC>::optional(
                               bsl::in_place_t,
                               std::initializer_list<IL_TYPE>             it,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_01) args_01,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_02) args_02,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_03) args_03,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_04) args_04,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_05) args_05)
{
    emplace(it, BSLS_COMPILERFEATURES_FORWARD(ARGS_01, args_01),
                BSLS_COMPILERFEATURES_FORWARD(ARGS_02, args_02),
                BSLS_COMPILERFEATURES_FORWARD(ARGS_03, args_03),
                BSLS_COMPILERFEATURES_FORWARD(ARGS_04, args_04),
                BSLS_COMPILERFEATURES_FORWARD(ARGS_05, args_05));
}
#endif  // BSLSTL_OPTIONAL_VARIADIC_LIMIT_J >= 5

#if BSLSTL_OPTIONAL_VARIADIC_LIMIT_J >= 6
template <class TYPE, bool USES_BSLMA_ALLOC>
template<class IL_TYPE, class ARGS_01,
                        class ARGS_02,
                        class ARGS_03,
                        class ARGS_04,
                        class ARGS_05,
                        class ARGS_06>
inline
optional<TYPE, USES_BSLMA_ALLOC>::optional(
                               bsl::in_place_t,
                               std::initializer_list<IL_TYPE>             it,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_01) args_01,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_02) args_02,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_03) args_03,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_04) args_04,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_05) args_05,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_06) args_06)
{
    emplace(it, BSLS_COMPILERFEATURES_FORWARD(ARGS_01, args_01),
                BSLS_COMPILERFEATURES_FORWARD(ARGS_02, args_02),
                BSLS_COMPILERFEATURES_FORWARD(ARGS_03, args_03),
                BSLS_COMPILERFEATURES_FORWARD(ARGS_04, args_04),
                BSLS_COMPILERFEATURES_FORWARD(ARGS_05, args_05),
                BSLS_COMPILERFEATURES_FORWARD(ARGS_06, args_06));
}
#endif  // BSLSTL_OPTIONAL_VARIADIC_LIMIT_J >= 6

#if BSLSTL_OPTIONAL_VARIADIC_LIMIT_J >= 7
template <class TYPE, bool USES_BSLMA_ALLOC>
template<class IL_TYPE, class ARGS_01,
                        class ARGS_02,
                        class ARGS_03,
                        class ARGS_04,
                        class ARGS_05,
                        class ARGS_06,
                        class ARGS_07>
inline
optional<TYPE, USES_BSLMA_ALLOC>::optional(
                               bsl::in_place_t,
                               std::initializer_list<IL_TYPE>             it,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_01) args_01,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_02) args_02,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_03) args_03,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_04) args_04,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_05) args_05,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_06) args_06,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_07) args_07)
{
    emplace(it, BSLS_COMPILERFEATURES_FORWARD(ARGS_01, args_01),
                BSLS_COMPILERFEATURES_FORWARD(ARGS_02, args_02),
                BSLS_COMPILERFEATURES_FORWARD(ARGS_03, args_03),
                BSLS_COMPILERFEATURES_FORWARD(ARGS_04, args_04),
                BSLS_COMPILERFEATURES_FORWARD(ARGS_05, args_05),
                BSLS_COMPILERFEATURES_FORWARD(ARGS_06, args_06),
                BSLS_COMPILERFEATURES_FORWARD(ARGS_07, args_07));
}
#endif  // BSLSTL_OPTIONAL_VARIADIC_LIMIT_J >= 7

#if BSLSTL_OPTIONAL_VARIADIC_LIMIT_J >= 8
template <class TYPE, bool USES_BSLMA_ALLOC>
template<class IL_TYPE, class ARGS_01,
                        class ARGS_02,
                        class ARGS_03,
                        class ARGS_04,
                        class ARGS_05,
                        class ARGS_06,
                        class ARGS_07,
                        class ARGS_08>
inline
optional<TYPE, USES_BSLMA_ALLOC>::optional(
                               bsl::in_place_t,
                               std::initializer_list<IL_TYPE>             it,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_01) args_01,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_02) args_02,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_03) args_03,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_04) args_04,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_05) args_05,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_06) args_06,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_07) args_07,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_08) args_08)
{
    emplace(it, BSLS_COMPILERFEATURES_FORWARD(ARGS_01, args_01),
                BSLS_COMPILERFEATURES_FORWARD(ARGS_02, args_02),
                BSLS_COMPILERFEATURES_FORWARD(ARGS_03, args_03),
                BSLS_COMPILERFEATURES_FORWARD(ARGS_04, args_04),
                BSLS_COMPILERFEATURES_FORWARD(ARGS_05, args_05),
                BSLS_COMPILERFEATURES_FORWARD(ARGS_06, args_06),
                BSLS_COMPILERFEATURES_FORWARD(ARGS_07, args_07),
                BSLS_COMPILERFEATURES_FORWARD(ARGS_08, args_08));
}
#endif  // BSLSTL_OPTIONAL_VARIADIC_LIMIT_J >= 8

#if BSLSTL_OPTIONAL_VARIADIC_LIMIT_J >= 9
template <class TYPE, bool USES_BSLMA_ALLOC>
template<class IL_TYPE, class ARGS_01,
                        class ARGS_02,
                        class ARGS_03,
                        class ARGS_04,
                        class ARGS_05,
                        class ARGS_06,
                        class ARGS_07,
                        class ARGS_08,
                        class ARGS_09>
inline
optional<TYPE, USES_BSLMA_ALLOC>::optional(
                               bsl::in_place_t,
                               std::initializer_list<IL_TYPE>             it,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_01) args_01,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_02) args_02,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_03) args_03,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_04) args_04,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_05) args_05,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_06) args_06,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_07) args_07,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_08) args_08,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_09) args_09)
{
    emplace(it, BSLS_COMPILERFEATURES_FORWARD(ARGS_01, args_01),
                BSLS_COMPILERFEATURES_FORWARD(ARGS_02, args_02),
                BSLS_COMPILERFEATURES_FORWARD(ARGS_03, args_03),
                BSLS_COMPILERFEATURES_FORWARD(ARGS_04, args_04),
                BSLS_COMPILERFEATURES_FORWARD(ARGS_05, args_05),
                BSLS_COMPILERFEATURES_FORWARD(ARGS_06, args_06),
                BSLS_COMPILERFEATURES_FORWARD(ARGS_07, args_07),
                BSLS_COMPILERFEATURES_FORWARD(ARGS_08, args_08),
                BSLS_COMPILERFEATURES_FORWARD(ARGS_09, args_09));
}
#endif  // BSLSTL_OPTIONAL_VARIADIC_LIMIT_J >= 9

#if BSLSTL_OPTIONAL_VARIADIC_LIMIT_J >= 10
template <class TYPE, bool USES_BSLMA_ALLOC>
template<class IL_TYPE, class ARGS_01,
                        class ARGS_02,
                        class ARGS_03,
                        class ARGS_04,
                        class ARGS_05,
                        class ARGS_06,
                        class ARGS_07,
                        class ARGS_08,
                        class ARGS_09,
                        class ARGS_10>
inline
optional<TYPE, USES_BSLMA_ALLOC>::optional(
                               bsl::in_place_t,
                               std::initializer_list<IL_TYPE>             it,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_01) args_01,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_02) args_02,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_03) args_03,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_04) args_04,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_05) args_05,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_06) args_06,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_07) args_07,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_08) args_08,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_09) args_09,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_10) args_10)
{
    emplace(it, BSLS_COMPILERFEATURES_FORWARD(ARGS_01, args_01),
                BSLS_COMPILERFEATURES_FORWARD(ARGS_02, args_02),
                BSLS_COMPILERFEATURES_FORWARD(ARGS_03, args_03),
                BSLS_COMPILERFEATURES_FORWARD(ARGS_04, args_04),
                BSLS_COMPILERFEATURES_FORWARD(ARGS_05, args_05),
                BSLS_COMPILERFEATURES_FORWARD(ARGS_06, args_06),
                BSLS_COMPILERFEATURES_FORWARD(ARGS_07, args_07),
                BSLS_COMPILERFEATURES_FORWARD(ARGS_08, args_08),
                BSLS_COMPILERFEATURES_FORWARD(ARGS_09, args_09),
                BSLS_COMPILERFEATURES_FORWARD(ARGS_10, args_10));
}
#endif  // BSLSTL_OPTIONAL_VARIADIC_LIMIT_J >= 10

#endif
#else
// The generated code below is a workaround for the absence of perfect
// forwarding in some compilers.
template <class TYPE, bool USES_BSLMA_ALLOC>
template<class... ARGS>
inline
optional<TYPE, USES_BSLMA_ALLOC>::optional(
                               bsl::in_place_t,
                               BSLS_COMPILERFEATURES_FORWARD_REF(ARGS)... args)
{
    emplace(BSLS_COMPILERFEATURES_FORWARD(ARGS, args)...);
}

#if defined(BSLS_COMPILERFEATURES_SUPPORT_GENERALIZED_INITIALIZERS)
template <class TYPE, bool USES_BSLMA_ALLOC>
template<class IL_TYPE, class... ARGS>
inline
optional<TYPE, USES_BSLMA_ALLOC>::optional(
                               bsl::in_place_t,
                               std::initializer_list<IL_TYPE>             it,
                               BSLS_COMPILERFEATURES_FORWARD_REF(ARGS)... args)
{
    emplace(it, BSLS_COMPILERFEATURES_FORWARD(ARGS, args)...);
}
#endif
// }}} END GENERATED CODE
#endif

template <class TYPE, bool USES_BSLMA_ALLOC>
inline
optional<TYPE, USES_BSLMA_ALLOC>::optional(bsl::allocator_arg_t,
                                           allocator_type allocator)

:d_allocator(allocator)
{
}

template <class TYPE, bool USES_BSLMA_ALLOC>
inline
optional<TYPE, USES_BSLMA_ALLOC>::optional(bsl::allocator_arg_t,
                                           allocator_type allocator,
                                           bsl::nullopt_t)

:d_allocator(allocator)
{
}

template <class TYPE, bool USES_BSLMA_ALLOC>
inline
optional<TYPE, USES_BSLMA_ALLOC>::optional(bsl::allocator_arg_t,
                                           allocator_type   allocator,
                                           const optional&  rhs)
: d_allocator(allocator)
{
    if (rhs.has_value()) {
        this->emplace(rhs.value());
    }
}

template <class TYPE, bool USES_BSLMA_ALLOC>
inline
optional<TYPE, USES_BSLMA_ALLOC>::optional(
                            bsl::allocator_arg_t,
                            allocator_type                           allocator,
                            BloombergLP::bslmf::MovableRef<optional> rhs)
: d_allocator(allocator)
{
    optional& lvalue = rhs;

    if (lvalue.has_value()) {
        emplace(MoveUtil::move(lvalue.value()));
    }
}

#if !BSLS_COMPILERFEATURES_SIMULATE_CPP11_FEATURES
template <class TYPE, bool USES_BSLMA_ALLOC>
template<class... ARGS>
inline
optional<TYPE, USES_BSLMA_ALLOC>::optional(
                              bsl::allocator_arg_t,
                              allocator_type                             alloc,
                              bsl::in_place_t,
                              BSLS_COMPILERFEATURES_FORWARD_REF(ARGS)... args)
: d_allocator(alloc)
{
    emplace(BSLS_COMPILERFEATURES_FORWARD(ARGS, args)...);
}

#if defined(BSLS_COMPILERFEATURES_SUPPORT_GENERALIZED_INITIALIZERS)
template <class TYPE, bool USES_BSLMA_ALLOC>
template<class IL_TYPE, class... ARGS>
inline
optional<TYPE, USES_BSLMA_ALLOC>::optional(
                              bsl::allocator_arg_t,
                              allocator_type                             alloc,
                              bsl::in_place_t,
                              std::initializer_list<IL_TYPE>             il,
                              BSLS_COMPILERFEATURES_FORWARD_REF(ARGS)... args)
: d_allocator(alloc)
{
    emplace(il, BSLS_COMPILERFEATURES_FORWARD(ARGS, args)...);
}
#endif//BSLS_COMPILERFEATURES_SUPPORT_GENERALIZED_INITIALIZERS
#elif BSLS_COMPILERFEATURES_SIMULATE_VARIADIC_TEMPLATES
// {{{ BEGIN GENERATED CODE
// The following section is automatically generated.  **DO NOT EDIT**
// Generator command line: sim_cpp11_features.pl bslstl_optional.h
#ifndef BSLSTL_OPTIONAL_VARIADIC_LIMIT
#define BSLSTL_OPTIONAL_VARIADIC_LIMIT 10
#endif
#ifndef BSLSTL_OPTIONAL_VARIADIC_LIMIT_K
#define BSLSTL_OPTIONAL_VARIADIC_LIMIT_K BSLSTL_OPTIONAL_VARIADIC_LIMIT
#endif
#if BSLSTL_OPTIONAL_VARIADIC_LIMIT_K >= 0
template <class TYPE, bool USES_BSLMA_ALLOC>
inline
optional<TYPE, USES_BSLMA_ALLOC>::optional(
                              bsl::allocator_arg_t,
                              allocator_type                             alloc,
                              bsl::in_place_t)
: d_allocator(alloc)
{
    emplace();
}
#endif  // BSLSTL_OPTIONAL_VARIADIC_LIMIT_K >= 0

#if BSLSTL_OPTIONAL_VARIADIC_LIMIT_K >= 1
template <class TYPE, bool USES_BSLMA_ALLOC>
template<class ARGS_01>
inline
optional<TYPE, USES_BSLMA_ALLOC>::optional(
                              bsl::allocator_arg_t,
                              allocator_type                             alloc,
                              bsl::in_place_t,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_01) args_01)
: d_allocator(alloc)
{
    emplace(BSLS_COMPILERFEATURES_FORWARD(ARGS_01, args_01));
}
#endif  // BSLSTL_OPTIONAL_VARIADIC_LIMIT_K >= 1

#if BSLSTL_OPTIONAL_VARIADIC_LIMIT_K >= 2
template <class TYPE, bool USES_BSLMA_ALLOC>
template<class ARGS_01,
         class ARGS_02>
inline
optional<TYPE, USES_BSLMA_ALLOC>::optional(
                              bsl::allocator_arg_t,
                              allocator_type                             alloc,
                              bsl::in_place_t,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_01) args_01,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_02) args_02)
: d_allocator(alloc)
{
    emplace(BSLS_COMPILERFEATURES_FORWARD(ARGS_01, args_01),
            BSLS_COMPILERFEATURES_FORWARD(ARGS_02, args_02));
}
#endif  // BSLSTL_OPTIONAL_VARIADIC_LIMIT_K >= 2

#if BSLSTL_OPTIONAL_VARIADIC_LIMIT_K >= 3
template <class TYPE, bool USES_BSLMA_ALLOC>
template<class ARGS_01,
         class ARGS_02,
         class ARGS_03>
inline
optional<TYPE, USES_BSLMA_ALLOC>::optional(
                              bsl::allocator_arg_t,
                              allocator_type                             alloc,
                              bsl::in_place_t,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_01) args_01,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_02) args_02,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_03) args_03)
: d_allocator(alloc)
{
    emplace(BSLS_COMPILERFEATURES_FORWARD(ARGS_01, args_01),
            BSLS_COMPILERFEATURES_FORWARD(ARGS_02, args_02),
            BSLS_COMPILERFEATURES_FORWARD(ARGS_03, args_03));
}
#endif  // BSLSTL_OPTIONAL_VARIADIC_LIMIT_K >= 3

#if BSLSTL_OPTIONAL_VARIADIC_LIMIT_K >= 4
template <class TYPE, bool USES_BSLMA_ALLOC>
template<class ARGS_01,
         class ARGS_02,
         class ARGS_03,
         class ARGS_04>
inline
optional<TYPE, USES_BSLMA_ALLOC>::optional(
                              bsl::allocator_arg_t,
                              allocator_type                             alloc,
                              bsl::in_place_t,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_01) args_01,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_02) args_02,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_03) args_03,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_04) args_04)
: d_allocator(alloc)
{
    emplace(BSLS_COMPILERFEATURES_FORWARD(ARGS_01, args_01),
            BSLS_COMPILERFEATURES_FORWARD(ARGS_02, args_02),
            BSLS_COMPILERFEATURES_FORWARD(ARGS_03, args_03),
            BSLS_COMPILERFEATURES_FORWARD(ARGS_04, args_04));
}
#endif  // BSLSTL_OPTIONAL_VARIADIC_LIMIT_K >= 4

#if BSLSTL_OPTIONAL_VARIADIC_LIMIT_K >= 5
template <class TYPE, bool USES_BSLMA_ALLOC>
template<class ARGS_01,
         class ARGS_02,
         class ARGS_03,
         class ARGS_04,
         class ARGS_05>
inline
optional<TYPE, USES_BSLMA_ALLOC>::optional(
                              bsl::allocator_arg_t,
                              allocator_type                             alloc,
                              bsl::in_place_t,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_01) args_01,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_02) args_02,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_03) args_03,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_04) args_04,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_05) args_05)
: d_allocator(alloc)
{
    emplace(BSLS_COMPILERFEATURES_FORWARD(ARGS_01, args_01),
            BSLS_COMPILERFEATURES_FORWARD(ARGS_02, args_02),
            BSLS_COMPILERFEATURES_FORWARD(ARGS_03, args_03),
            BSLS_COMPILERFEATURES_FORWARD(ARGS_04, args_04),
            BSLS_COMPILERFEATURES_FORWARD(ARGS_05, args_05));
}
#endif  // BSLSTL_OPTIONAL_VARIADIC_LIMIT_K >= 5

#if BSLSTL_OPTIONAL_VARIADIC_LIMIT_K >= 6
template <class TYPE, bool USES_BSLMA_ALLOC>
template<class ARGS_01,
         class ARGS_02,
         class ARGS_03,
         class ARGS_04,
         class ARGS_05,
         class ARGS_06>
inline
optional<TYPE, USES_BSLMA_ALLOC>::optional(
                              bsl::allocator_arg_t,
                              allocator_type                             alloc,
                              bsl::in_place_t,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_01) args_01,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_02) args_02,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_03) args_03,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_04) args_04,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_05) args_05,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_06) args_06)
: d_allocator(alloc)
{
    emplace(BSLS_COMPILERFEATURES_FORWARD(ARGS_01, args_01),
            BSLS_COMPILERFEATURES_FORWARD(ARGS_02, args_02),
            BSLS_COMPILERFEATURES_FORWARD(ARGS_03, args_03),
            BSLS_COMPILERFEATURES_FORWARD(ARGS_04, args_04),
            BSLS_COMPILERFEATURES_FORWARD(ARGS_05, args_05),
            BSLS_COMPILERFEATURES_FORWARD(ARGS_06, args_06));
}
#endif  // BSLSTL_OPTIONAL_VARIADIC_LIMIT_K >= 6

#if BSLSTL_OPTIONAL_VARIADIC_LIMIT_K >= 7
template <class TYPE, bool USES_BSLMA_ALLOC>
template<class ARGS_01,
         class ARGS_02,
         class ARGS_03,
         class ARGS_04,
         class ARGS_05,
         class ARGS_06,
         class ARGS_07>
inline
optional<TYPE, USES_BSLMA_ALLOC>::optional(
                              bsl::allocator_arg_t,
                              allocator_type                             alloc,
                              bsl::in_place_t,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_01) args_01,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_02) args_02,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_03) args_03,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_04) args_04,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_05) args_05,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_06) args_06,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_07) args_07)
: d_allocator(alloc)
{
    emplace(BSLS_COMPILERFEATURES_FORWARD(ARGS_01, args_01),
            BSLS_COMPILERFEATURES_FORWARD(ARGS_02, args_02),
            BSLS_COMPILERFEATURES_FORWARD(ARGS_03, args_03),
            BSLS_COMPILERFEATURES_FORWARD(ARGS_04, args_04),
            BSLS_COMPILERFEATURES_FORWARD(ARGS_05, args_05),
            BSLS_COMPILERFEATURES_FORWARD(ARGS_06, args_06),
            BSLS_COMPILERFEATURES_FORWARD(ARGS_07, args_07));
}
#endif  // BSLSTL_OPTIONAL_VARIADIC_LIMIT_K >= 7

#if BSLSTL_OPTIONAL_VARIADIC_LIMIT_K >= 8
template <class TYPE, bool USES_BSLMA_ALLOC>
template<class ARGS_01,
         class ARGS_02,
         class ARGS_03,
         class ARGS_04,
         class ARGS_05,
         class ARGS_06,
         class ARGS_07,
         class ARGS_08>
inline
optional<TYPE, USES_BSLMA_ALLOC>::optional(
                              bsl::allocator_arg_t,
                              allocator_type                             alloc,
                              bsl::in_place_t,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_01) args_01,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_02) args_02,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_03) args_03,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_04) args_04,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_05) args_05,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_06) args_06,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_07) args_07,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_08) args_08)
: d_allocator(alloc)
{
    emplace(BSLS_COMPILERFEATURES_FORWARD(ARGS_01, args_01),
            BSLS_COMPILERFEATURES_FORWARD(ARGS_02, args_02),
            BSLS_COMPILERFEATURES_FORWARD(ARGS_03, args_03),
            BSLS_COMPILERFEATURES_FORWARD(ARGS_04, args_04),
            BSLS_COMPILERFEATURES_FORWARD(ARGS_05, args_05),
            BSLS_COMPILERFEATURES_FORWARD(ARGS_06, args_06),
            BSLS_COMPILERFEATURES_FORWARD(ARGS_07, args_07),
            BSLS_COMPILERFEATURES_FORWARD(ARGS_08, args_08));
}
#endif  // BSLSTL_OPTIONAL_VARIADIC_LIMIT_K >= 8

#if BSLSTL_OPTIONAL_VARIADIC_LIMIT_K >= 9
template <class TYPE, bool USES_BSLMA_ALLOC>
template<class ARGS_01,
         class ARGS_02,
         class ARGS_03,
         class ARGS_04,
         class ARGS_05,
         class ARGS_06,
         class ARGS_07,
         class ARGS_08,
         class ARGS_09>
inline
optional<TYPE, USES_BSLMA_ALLOC>::optional(
                              bsl::allocator_arg_t,
                              allocator_type                             alloc,
                              bsl::in_place_t,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_01) args_01,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_02) args_02,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_03) args_03,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_04) args_04,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_05) args_05,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_06) args_06,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_07) args_07,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_08) args_08,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_09) args_09)
: d_allocator(alloc)
{
    emplace(BSLS_COMPILERFEATURES_FORWARD(ARGS_01, args_01),
            BSLS_COMPILERFEATURES_FORWARD(ARGS_02, args_02),
            BSLS_COMPILERFEATURES_FORWARD(ARGS_03, args_03),
            BSLS_COMPILERFEATURES_FORWARD(ARGS_04, args_04),
            BSLS_COMPILERFEATURES_FORWARD(ARGS_05, args_05),
            BSLS_COMPILERFEATURES_FORWARD(ARGS_06, args_06),
            BSLS_COMPILERFEATURES_FORWARD(ARGS_07, args_07),
            BSLS_COMPILERFEATURES_FORWARD(ARGS_08, args_08),
            BSLS_COMPILERFEATURES_FORWARD(ARGS_09, args_09));
}
#endif  // BSLSTL_OPTIONAL_VARIADIC_LIMIT_K >= 9

#if BSLSTL_OPTIONAL_VARIADIC_LIMIT_K >= 10
template <class TYPE, bool USES_BSLMA_ALLOC>
template<class ARGS_01,
         class ARGS_02,
         class ARGS_03,
         class ARGS_04,
         class ARGS_05,
         class ARGS_06,
         class ARGS_07,
         class ARGS_08,
         class ARGS_09,
         class ARGS_10>
inline
optional<TYPE, USES_BSLMA_ALLOC>::optional(
                              bsl::allocator_arg_t,
                              allocator_type                             alloc,
                              bsl::in_place_t,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_01) args_01,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_02) args_02,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_03) args_03,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_04) args_04,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_05) args_05,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_06) args_06,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_07) args_07,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_08) args_08,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_09) args_09,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_10) args_10)
: d_allocator(alloc)
{
    emplace(BSLS_COMPILERFEATURES_FORWARD(ARGS_01, args_01),
            BSLS_COMPILERFEATURES_FORWARD(ARGS_02, args_02),
            BSLS_COMPILERFEATURES_FORWARD(ARGS_03, args_03),
            BSLS_COMPILERFEATURES_FORWARD(ARGS_04, args_04),
            BSLS_COMPILERFEATURES_FORWARD(ARGS_05, args_05),
            BSLS_COMPILERFEATURES_FORWARD(ARGS_06, args_06),
            BSLS_COMPILERFEATURES_FORWARD(ARGS_07, args_07),
            BSLS_COMPILERFEATURES_FORWARD(ARGS_08, args_08),
            BSLS_COMPILERFEATURES_FORWARD(ARGS_09, args_09),
            BSLS_COMPILERFEATURES_FORWARD(ARGS_10, args_10));
}
#endif  // BSLSTL_OPTIONAL_VARIADIC_LIMIT_K >= 10


#if defined(BSLS_COMPILERFEATURES_SUPPORT_GENERALIZED_INITIALIZERS)
#if BSLSTL_OPTIONAL_VARIADIC_LIMIT_K >= 0
template <class TYPE, bool USES_BSLMA_ALLOC>
template<class IL_TYPE>
inline
optional<TYPE, USES_BSLMA_ALLOC>::optional(
                              bsl::allocator_arg_t,
                              allocator_type                             alloc,
                              bsl::in_place_t,
                              std::initializer_list<IL_TYPE>             il)
: d_allocator(alloc)
{
    emplace(il);
}
#endif  // BSLSTL_OPTIONAL_VARIADIC_LIMIT_K >= 0

#if BSLSTL_OPTIONAL_VARIADIC_LIMIT_K >= 1
template <class TYPE, bool USES_BSLMA_ALLOC>
template<class IL_TYPE, class ARGS_01>
inline
optional<TYPE, USES_BSLMA_ALLOC>::optional(
                              bsl::allocator_arg_t,
                              allocator_type                             alloc,
                              bsl::in_place_t,
                              std::initializer_list<IL_TYPE>             il,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_01) args_01)
: d_allocator(alloc)
{
    emplace(il, BSLS_COMPILERFEATURES_FORWARD(ARGS_01, args_01));
}
#endif  // BSLSTL_OPTIONAL_VARIADIC_LIMIT_K >= 1

#if BSLSTL_OPTIONAL_VARIADIC_LIMIT_K >= 2
template <class TYPE, bool USES_BSLMA_ALLOC>
template<class IL_TYPE, class ARGS_01,
                        class ARGS_02>
inline
optional<TYPE, USES_BSLMA_ALLOC>::optional(
                              bsl::allocator_arg_t,
                              allocator_type                             alloc,
                              bsl::in_place_t,
                              std::initializer_list<IL_TYPE>             il,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_01) args_01,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_02) args_02)
: d_allocator(alloc)
{
    emplace(il, BSLS_COMPILERFEATURES_FORWARD(ARGS_01, args_01),
                BSLS_COMPILERFEATURES_FORWARD(ARGS_02, args_02));
}
#endif  // BSLSTL_OPTIONAL_VARIADIC_LIMIT_K >= 2

#if BSLSTL_OPTIONAL_VARIADIC_LIMIT_K >= 3
template <class TYPE, bool USES_BSLMA_ALLOC>
template<class IL_TYPE, class ARGS_01,
                        class ARGS_02,
                        class ARGS_03>
inline
optional<TYPE, USES_BSLMA_ALLOC>::optional(
                              bsl::allocator_arg_t,
                              allocator_type                             alloc,
                              bsl::in_place_t,
                              std::initializer_list<IL_TYPE>             il,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_01) args_01,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_02) args_02,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_03) args_03)
: d_allocator(alloc)
{
    emplace(il, BSLS_COMPILERFEATURES_FORWARD(ARGS_01, args_01),
                BSLS_COMPILERFEATURES_FORWARD(ARGS_02, args_02),
                BSLS_COMPILERFEATURES_FORWARD(ARGS_03, args_03));
}
#endif  // BSLSTL_OPTIONAL_VARIADIC_LIMIT_K >= 3

#if BSLSTL_OPTIONAL_VARIADIC_LIMIT_K >= 4
template <class TYPE, bool USES_BSLMA_ALLOC>
template<class IL_TYPE, class ARGS_01,
                        class ARGS_02,
                        class ARGS_03,
                        class ARGS_04>
inline
optional<TYPE, USES_BSLMA_ALLOC>::optional(
                              bsl::allocator_arg_t,
                              allocator_type                             alloc,
                              bsl::in_place_t,
                              std::initializer_list<IL_TYPE>             il,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_01) args_01,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_02) args_02,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_03) args_03,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_04) args_04)
: d_allocator(alloc)
{
    emplace(il, BSLS_COMPILERFEATURES_FORWARD(ARGS_01, args_01),
                BSLS_COMPILERFEATURES_FORWARD(ARGS_02, args_02),
                BSLS_COMPILERFEATURES_FORWARD(ARGS_03, args_03),
                BSLS_COMPILERFEATURES_FORWARD(ARGS_04, args_04));
}
#endif  // BSLSTL_OPTIONAL_VARIADIC_LIMIT_K >= 4

#if BSLSTL_OPTIONAL_VARIADIC_LIMIT_K >= 5
template <class TYPE, bool USES_BSLMA_ALLOC>
template<class IL_TYPE, class ARGS_01,
                        class ARGS_02,
                        class ARGS_03,
                        class ARGS_04,
                        class ARGS_05>
inline
optional<TYPE, USES_BSLMA_ALLOC>::optional(
                              bsl::allocator_arg_t,
                              allocator_type                             alloc,
                              bsl::in_place_t,
                              std::initializer_list<IL_TYPE>             il,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_01) args_01,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_02) args_02,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_03) args_03,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_04) args_04,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_05) args_05)
: d_allocator(alloc)
{
    emplace(il, BSLS_COMPILERFEATURES_FORWARD(ARGS_01, args_01),
                BSLS_COMPILERFEATURES_FORWARD(ARGS_02, args_02),
                BSLS_COMPILERFEATURES_FORWARD(ARGS_03, args_03),
                BSLS_COMPILERFEATURES_FORWARD(ARGS_04, args_04),
                BSLS_COMPILERFEATURES_FORWARD(ARGS_05, args_05));
}
#endif  // BSLSTL_OPTIONAL_VARIADIC_LIMIT_K >= 5

#if BSLSTL_OPTIONAL_VARIADIC_LIMIT_K >= 6
template <class TYPE, bool USES_BSLMA_ALLOC>
template<class IL_TYPE, class ARGS_01,
                        class ARGS_02,
                        class ARGS_03,
                        class ARGS_04,
                        class ARGS_05,
                        class ARGS_06>
inline
optional<TYPE, USES_BSLMA_ALLOC>::optional(
                              bsl::allocator_arg_t,
                              allocator_type                             alloc,
                              bsl::in_place_t,
                              std::initializer_list<IL_TYPE>             il,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_01) args_01,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_02) args_02,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_03) args_03,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_04) args_04,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_05) args_05,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_06) args_06)
: d_allocator(alloc)
{
    emplace(il, BSLS_COMPILERFEATURES_FORWARD(ARGS_01, args_01),
                BSLS_COMPILERFEATURES_FORWARD(ARGS_02, args_02),
                BSLS_COMPILERFEATURES_FORWARD(ARGS_03, args_03),
                BSLS_COMPILERFEATURES_FORWARD(ARGS_04, args_04),
                BSLS_COMPILERFEATURES_FORWARD(ARGS_05, args_05),
                BSLS_COMPILERFEATURES_FORWARD(ARGS_06, args_06));
}
#endif  // BSLSTL_OPTIONAL_VARIADIC_LIMIT_K >= 6

#if BSLSTL_OPTIONAL_VARIADIC_LIMIT_K >= 7
template <class TYPE, bool USES_BSLMA_ALLOC>
template<class IL_TYPE, class ARGS_01,
                        class ARGS_02,
                        class ARGS_03,
                        class ARGS_04,
                        class ARGS_05,
                        class ARGS_06,
                        class ARGS_07>
inline
optional<TYPE, USES_BSLMA_ALLOC>::optional(
                              bsl::allocator_arg_t,
                              allocator_type                             alloc,
                              bsl::in_place_t,
                              std::initializer_list<IL_TYPE>             il,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_01) args_01,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_02) args_02,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_03) args_03,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_04) args_04,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_05) args_05,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_06) args_06,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_07) args_07)
: d_allocator(alloc)
{
    emplace(il, BSLS_COMPILERFEATURES_FORWARD(ARGS_01, args_01),
                BSLS_COMPILERFEATURES_FORWARD(ARGS_02, args_02),
                BSLS_COMPILERFEATURES_FORWARD(ARGS_03, args_03),
                BSLS_COMPILERFEATURES_FORWARD(ARGS_04, args_04),
                BSLS_COMPILERFEATURES_FORWARD(ARGS_05, args_05),
                BSLS_COMPILERFEATURES_FORWARD(ARGS_06, args_06),
                BSLS_COMPILERFEATURES_FORWARD(ARGS_07, args_07));
}
#endif  // BSLSTL_OPTIONAL_VARIADIC_LIMIT_K >= 7

#if BSLSTL_OPTIONAL_VARIADIC_LIMIT_K >= 8
template <class TYPE, bool USES_BSLMA_ALLOC>
template<class IL_TYPE, class ARGS_01,
                        class ARGS_02,
                        class ARGS_03,
                        class ARGS_04,
                        class ARGS_05,
                        class ARGS_06,
                        class ARGS_07,
                        class ARGS_08>
inline
optional<TYPE, USES_BSLMA_ALLOC>::optional(
                              bsl::allocator_arg_t,
                              allocator_type                             alloc,
                              bsl::in_place_t,
                              std::initializer_list<IL_TYPE>             il,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_01) args_01,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_02) args_02,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_03) args_03,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_04) args_04,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_05) args_05,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_06) args_06,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_07) args_07,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_08) args_08)
: d_allocator(alloc)
{
    emplace(il, BSLS_COMPILERFEATURES_FORWARD(ARGS_01, args_01),
                BSLS_COMPILERFEATURES_FORWARD(ARGS_02, args_02),
                BSLS_COMPILERFEATURES_FORWARD(ARGS_03, args_03),
                BSLS_COMPILERFEATURES_FORWARD(ARGS_04, args_04),
                BSLS_COMPILERFEATURES_FORWARD(ARGS_05, args_05),
                BSLS_COMPILERFEATURES_FORWARD(ARGS_06, args_06),
                BSLS_COMPILERFEATURES_FORWARD(ARGS_07, args_07),
                BSLS_COMPILERFEATURES_FORWARD(ARGS_08, args_08));
}
#endif  // BSLSTL_OPTIONAL_VARIADIC_LIMIT_K >= 8

#if BSLSTL_OPTIONAL_VARIADIC_LIMIT_K >= 9
template <class TYPE, bool USES_BSLMA_ALLOC>
template<class IL_TYPE, class ARGS_01,
                        class ARGS_02,
                        class ARGS_03,
                        class ARGS_04,
                        class ARGS_05,
                        class ARGS_06,
                        class ARGS_07,
                        class ARGS_08,
                        class ARGS_09>
inline
optional<TYPE, USES_BSLMA_ALLOC>::optional(
                              bsl::allocator_arg_t,
                              allocator_type                             alloc,
                              bsl::in_place_t,
                              std::initializer_list<IL_TYPE>             il,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_01) args_01,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_02) args_02,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_03) args_03,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_04) args_04,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_05) args_05,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_06) args_06,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_07) args_07,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_08) args_08,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_09) args_09)
: d_allocator(alloc)
{
    emplace(il, BSLS_COMPILERFEATURES_FORWARD(ARGS_01, args_01),
                BSLS_COMPILERFEATURES_FORWARD(ARGS_02, args_02),
                BSLS_COMPILERFEATURES_FORWARD(ARGS_03, args_03),
                BSLS_COMPILERFEATURES_FORWARD(ARGS_04, args_04),
                BSLS_COMPILERFEATURES_FORWARD(ARGS_05, args_05),
                BSLS_COMPILERFEATURES_FORWARD(ARGS_06, args_06),
                BSLS_COMPILERFEATURES_FORWARD(ARGS_07, args_07),
                BSLS_COMPILERFEATURES_FORWARD(ARGS_08, args_08),
                BSLS_COMPILERFEATURES_FORWARD(ARGS_09, args_09));
}
#endif  // BSLSTL_OPTIONAL_VARIADIC_LIMIT_K >= 9

#if BSLSTL_OPTIONAL_VARIADIC_LIMIT_K >= 10
template <class TYPE, bool USES_BSLMA_ALLOC>
template<class IL_TYPE, class ARGS_01,
                        class ARGS_02,
                        class ARGS_03,
                        class ARGS_04,
                        class ARGS_05,
                        class ARGS_06,
                        class ARGS_07,
                        class ARGS_08,
                        class ARGS_09,
                        class ARGS_10>
inline
optional<TYPE, USES_BSLMA_ALLOC>::optional(
                              bsl::allocator_arg_t,
                              allocator_type                             alloc,
                              bsl::in_place_t,
                              std::initializer_list<IL_TYPE>             il,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_01) args_01,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_02) args_02,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_03) args_03,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_04) args_04,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_05) args_05,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_06) args_06,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_07) args_07,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_08) args_08,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_09) args_09,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_10) args_10)
: d_allocator(alloc)
{
    emplace(il, BSLS_COMPILERFEATURES_FORWARD(ARGS_01, args_01),
                BSLS_COMPILERFEATURES_FORWARD(ARGS_02, args_02),
                BSLS_COMPILERFEATURES_FORWARD(ARGS_03, args_03),
                BSLS_COMPILERFEATURES_FORWARD(ARGS_04, args_04),
                BSLS_COMPILERFEATURES_FORWARD(ARGS_05, args_05),
                BSLS_COMPILERFEATURES_FORWARD(ARGS_06, args_06),
                BSLS_COMPILERFEATURES_FORWARD(ARGS_07, args_07),
                BSLS_COMPILERFEATURES_FORWARD(ARGS_08, args_08),
                BSLS_COMPILERFEATURES_FORWARD(ARGS_09, args_09),
                BSLS_COMPILERFEATURES_FORWARD(ARGS_10, args_10));
}
#endif  // BSLSTL_OPTIONAL_VARIADIC_LIMIT_K >= 10

#endif
#else
// The generated code below is a workaround for the absence of perfect
// forwarding in some compilers.
template <class TYPE, bool USES_BSLMA_ALLOC>
template<class... ARGS>
inline
optional<TYPE, USES_BSLMA_ALLOC>::optional(
                              bsl::allocator_arg_t,
                              allocator_type                             alloc,
                              bsl::in_place_t,
                              BSLS_COMPILERFEATURES_FORWARD_REF(ARGS)... args)
: d_allocator(alloc)
{
    emplace(BSLS_COMPILERFEATURES_FORWARD(ARGS, args)...);
}

#if defined(BSLS_COMPILERFEATURES_SUPPORT_GENERALIZED_INITIALIZERS)
template <class TYPE, bool USES_BSLMA_ALLOC>
template<class IL_TYPE, class... ARGS>
inline
optional<TYPE, USES_BSLMA_ALLOC>::optional(
                              bsl::allocator_arg_t,
                              allocator_type                             alloc,
                              bsl::in_place_t,
                              std::initializer_list<IL_TYPE>             il,
                              BSLS_COMPILERFEATURES_FORWARD_REF(ARGS)... args)
: d_allocator(alloc)
{
    emplace(il, BSLS_COMPILERFEATURES_FORWARD(ARGS, args)...);
}
#endif
// }}} END GENERATED CODE
#endif

//MANIPULATORS
#if !BSLS_COMPILERFEATURES_SIMULATE_CPP11_FEATURES
template <class TYPE, bool USES_BSLMA_ALLOC>
template <class... ARGS>
inline
void optional<TYPE, USES_BSLMA_ALLOC>::emplace(ARGS&&... args)
{
    d_value.emplace(d_allocator.mechanism(),
                    BSLS_COMPILERFEATURES_FORWARD(ARGS, args)...);
}

#if defined(BSLS_COMPILERFEATURES_SUPPORT_GENERALIZED_INITIALIZERS)
template <class TYPE, bool USES_BSLMA_ALLOC>
template<class IL_TYPE, class... ARGS>
void optional<TYPE, USES_BSLMA_ALLOC>::emplace(
                                           std::initializer_list<IL_TYPE> il,
                                           ARGS&&...                      args)
{
    d_value.emplace(d_allocator.mechanism(),
                    il,
                    BSLS_COMPILERFEATURES_FORWARD(ARGS, args)...);
}
#endif//BSLS_COMPILERFEATURES_SUPPORT_GENERALIZED_INITIALIZERS
#elif BSLS_COMPILERFEATURES_SIMULATE_VARIADIC_TEMPLATES
// {{{ BEGIN GENERATED CODE
// The following section is automatically generated.  **DO NOT EDIT**
// Generator command line: sim_cpp11_features.pl bslstl_optional.h
#ifndef BSLSTL_OPTIONAL_VARIADIC_LIMIT
#define BSLSTL_OPTIONAL_VARIADIC_LIMIT 10
#endif
#ifndef BSLSTL_OPTIONAL_VARIADIC_LIMIT_L
#define BSLSTL_OPTIONAL_VARIADIC_LIMIT_L BSLSTL_OPTIONAL_VARIADIC_LIMIT
#endif
#if BSLSTL_OPTIONAL_VARIADIC_LIMIT_L >= 0
template <class TYPE, bool USES_BSLMA_ALLOC>
inline
void optional<TYPE, USES_BSLMA_ALLOC>::emplace(
                               )
{
    d_value.emplace(d_allocator.mechanism());
}
#endif  // BSLSTL_OPTIONAL_VARIADIC_LIMIT_L >= 0

#if BSLSTL_OPTIONAL_VARIADIC_LIMIT_L >= 1
template <class TYPE, bool USES_BSLMA_ALLOC>
template <class ARGS_01>
inline
void optional<TYPE, USES_BSLMA_ALLOC>::emplace(
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_01) args_01)
{
    d_value.emplace(d_allocator.mechanism(),
                    BSLS_COMPILERFEATURES_FORWARD(ARGS_01, args_01));
}
#endif  // BSLSTL_OPTIONAL_VARIADIC_LIMIT_L >= 1

#if BSLSTL_OPTIONAL_VARIADIC_LIMIT_L >= 2
template <class TYPE, bool USES_BSLMA_ALLOC>
template <class ARGS_01,
          class ARGS_02>
inline
void optional<TYPE, USES_BSLMA_ALLOC>::emplace(
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_01) args_01,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_02) args_02)
{
    d_value.emplace(d_allocator.mechanism(),
                    BSLS_COMPILERFEATURES_FORWARD(ARGS_01, args_01),
                    BSLS_COMPILERFEATURES_FORWARD(ARGS_02, args_02));
}
#endif  // BSLSTL_OPTIONAL_VARIADIC_LIMIT_L >= 2

#if BSLSTL_OPTIONAL_VARIADIC_LIMIT_L >= 3
template <class TYPE, bool USES_BSLMA_ALLOC>
template <class ARGS_01,
          class ARGS_02,
          class ARGS_03>
inline
void optional<TYPE, USES_BSLMA_ALLOC>::emplace(
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_01) args_01,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_02) args_02,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_03) args_03)
{
    d_value.emplace(d_allocator.mechanism(),
                    BSLS_COMPILERFEATURES_FORWARD(ARGS_01, args_01),
                    BSLS_COMPILERFEATURES_FORWARD(ARGS_02, args_02),
                    BSLS_COMPILERFEATURES_FORWARD(ARGS_03, args_03));
}
#endif  // BSLSTL_OPTIONAL_VARIADIC_LIMIT_L >= 3

#if BSLSTL_OPTIONAL_VARIADIC_LIMIT_L >= 4
template <class TYPE, bool USES_BSLMA_ALLOC>
template <class ARGS_01,
          class ARGS_02,
          class ARGS_03,
          class ARGS_04>
inline
void optional<TYPE, USES_BSLMA_ALLOC>::emplace(
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_01) args_01,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_02) args_02,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_03) args_03,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_04) args_04)
{
    d_value.emplace(d_allocator.mechanism(),
                    BSLS_COMPILERFEATURES_FORWARD(ARGS_01, args_01),
                    BSLS_COMPILERFEATURES_FORWARD(ARGS_02, args_02),
                    BSLS_COMPILERFEATURES_FORWARD(ARGS_03, args_03),
                    BSLS_COMPILERFEATURES_FORWARD(ARGS_04, args_04));
}
#endif  // BSLSTL_OPTIONAL_VARIADIC_LIMIT_L >= 4

#if BSLSTL_OPTIONAL_VARIADIC_LIMIT_L >= 5
template <class TYPE, bool USES_BSLMA_ALLOC>
template <class ARGS_01,
          class ARGS_02,
          class ARGS_03,
          class ARGS_04,
          class ARGS_05>
inline
void optional<TYPE, USES_BSLMA_ALLOC>::emplace(
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_01) args_01,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_02) args_02,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_03) args_03,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_04) args_04,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_05) args_05)
{
    d_value.emplace(d_allocator.mechanism(),
                    BSLS_COMPILERFEATURES_FORWARD(ARGS_01, args_01),
                    BSLS_COMPILERFEATURES_FORWARD(ARGS_02, args_02),
                    BSLS_COMPILERFEATURES_FORWARD(ARGS_03, args_03),
                    BSLS_COMPILERFEATURES_FORWARD(ARGS_04, args_04),
                    BSLS_COMPILERFEATURES_FORWARD(ARGS_05, args_05));
}
#endif  // BSLSTL_OPTIONAL_VARIADIC_LIMIT_L >= 5

#if BSLSTL_OPTIONAL_VARIADIC_LIMIT_L >= 6
template <class TYPE, bool USES_BSLMA_ALLOC>
template <class ARGS_01,
          class ARGS_02,
          class ARGS_03,
          class ARGS_04,
          class ARGS_05,
          class ARGS_06>
inline
void optional<TYPE, USES_BSLMA_ALLOC>::emplace(
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_01) args_01,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_02) args_02,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_03) args_03,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_04) args_04,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_05) args_05,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_06) args_06)
{
    d_value.emplace(d_allocator.mechanism(),
                    BSLS_COMPILERFEATURES_FORWARD(ARGS_01, args_01),
                    BSLS_COMPILERFEATURES_FORWARD(ARGS_02, args_02),
                    BSLS_COMPILERFEATURES_FORWARD(ARGS_03, args_03),
                    BSLS_COMPILERFEATURES_FORWARD(ARGS_04, args_04),
                    BSLS_COMPILERFEATURES_FORWARD(ARGS_05, args_05),
                    BSLS_COMPILERFEATURES_FORWARD(ARGS_06, args_06));
}
#endif  // BSLSTL_OPTIONAL_VARIADIC_LIMIT_L >= 6

#if BSLSTL_OPTIONAL_VARIADIC_LIMIT_L >= 7
template <class TYPE, bool USES_BSLMA_ALLOC>
template <class ARGS_01,
          class ARGS_02,
          class ARGS_03,
          class ARGS_04,
          class ARGS_05,
          class ARGS_06,
          class ARGS_07>
inline
void optional<TYPE, USES_BSLMA_ALLOC>::emplace(
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_01) args_01,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_02) args_02,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_03) args_03,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_04) args_04,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_05) args_05,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_06) args_06,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_07) args_07)
{
    d_value.emplace(d_allocator.mechanism(),
                    BSLS_COMPILERFEATURES_FORWARD(ARGS_01, args_01),
                    BSLS_COMPILERFEATURES_FORWARD(ARGS_02, args_02),
                    BSLS_COMPILERFEATURES_FORWARD(ARGS_03, args_03),
                    BSLS_COMPILERFEATURES_FORWARD(ARGS_04, args_04),
                    BSLS_COMPILERFEATURES_FORWARD(ARGS_05, args_05),
                    BSLS_COMPILERFEATURES_FORWARD(ARGS_06, args_06),
                    BSLS_COMPILERFEATURES_FORWARD(ARGS_07, args_07));
}
#endif  // BSLSTL_OPTIONAL_VARIADIC_LIMIT_L >= 7

#if BSLSTL_OPTIONAL_VARIADIC_LIMIT_L >= 8
template <class TYPE, bool USES_BSLMA_ALLOC>
template <class ARGS_01,
          class ARGS_02,
          class ARGS_03,
          class ARGS_04,
          class ARGS_05,
          class ARGS_06,
          class ARGS_07,
          class ARGS_08>
inline
void optional<TYPE, USES_BSLMA_ALLOC>::emplace(
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_01) args_01,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_02) args_02,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_03) args_03,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_04) args_04,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_05) args_05,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_06) args_06,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_07) args_07,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_08) args_08)
{
    d_value.emplace(d_allocator.mechanism(),
                    BSLS_COMPILERFEATURES_FORWARD(ARGS_01, args_01),
                    BSLS_COMPILERFEATURES_FORWARD(ARGS_02, args_02),
                    BSLS_COMPILERFEATURES_FORWARD(ARGS_03, args_03),
                    BSLS_COMPILERFEATURES_FORWARD(ARGS_04, args_04),
                    BSLS_COMPILERFEATURES_FORWARD(ARGS_05, args_05),
                    BSLS_COMPILERFEATURES_FORWARD(ARGS_06, args_06),
                    BSLS_COMPILERFEATURES_FORWARD(ARGS_07, args_07),
                    BSLS_COMPILERFEATURES_FORWARD(ARGS_08, args_08));
}
#endif  // BSLSTL_OPTIONAL_VARIADIC_LIMIT_L >= 8

#if BSLSTL_OPTIONAL_VARIADIC_LIMIT_L >= 9
template <class TYPE, bool USES_BSLMA_ALLOC>
template <class ARGS_01,
          class ARGS_02,
          class ARGS_03,
          class ARGS_04,
          class ARGS_05,
          class ARGS_06,
          class ARGS_07,
          class ARGS_08,
          class ARGS_09>
inline
void optional<TYPE, USES_BSLMA_ALLOC>::emplace(
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_01) args_01,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_02) args_02,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_03) args_03,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_04) args_04,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_05) args_05,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_06) args_06,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_07) args_07,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_08) args_08,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_09) args_09)
{
    d_value.emplace(d_allocator.mechanism(),
                    BSLS_COMPILERFEATURES_FORWARD(ARGS_01, args_01),
                    BSLS_COMPILERFEATURES_FORWARD(ARGS_02, args_02),
                    BSLS_COMPILERFEATURES_FORWARD(ARGS_03, args_03),
                    BSLS_COMPILERFEATURES_FORWARD(ARGS_04, args_04),
                    BSLS_COMPILERFEATURES_FORWARD(ARGS_05, args_05),
                    BSLS_COMPILERFEATURES_FORWARD(ARGS_06, args_06),
                    BSLS_COMPILERFEATURES_FORWARD(ARGS_07, args_07),
                    BSLS_COMPILERFEATURES_FORWARD(ARGS_08, args_08),
                    BSLS_COMPILERFEATURES_FORWARD(ARGS_09, args_09));
}
#endif  // BSLSTL_OPTIONAL_VARIADIC_LIMIT_L >= 9

#if BSLSTL_OPTIONAL_VARIADIC_LIMIT_L >= 10
template <class TYPE, bool USES_BSLMA_ALLOC>
template <class ARGS_01,
          class ARGS_02,
          class ARGS_03,
          class ARGS_04,
          class ARGS_05,
          class ARGS_06,
          class ARGS_07,
          class ARGS_08,
          class ARGS_09,
          class ARGS_10>
inline
void optional<TYPE, USES_BSLMA_ALLOC>::emplace(
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_01) args_01,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_02) args_02,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_03) args_03,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_04) args_04,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_05) args_05,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_06) args_06,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_07) args_07,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_08) args_08,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_09) args_09,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_10) args_10)
{
    d_value.emplace(d_allocator.mechanism(),
                    BSLS_COMPILERFEATURES_FORWARD(ARGS_01, args_01),
                    BSLS_COMPILERFEATURES_FORWARD(ARGS_02, args_02),
                    BSLS_COMPILERFEATURES_FORWARD(ARGS_03, args_03),
                    BSLS_COMPILERFEATURES_FORWARD(ARGS_04, args_04),
                    BSLS_COMPILERFEATURES_FORWARD(ARGS_05, args_05),
                    BSLS_COMPILERFEATURES_FORWARD(ARGS_06, args_06),
                    BSLS_COMPILERFEATURES_FORWARD(ARGS_07, args_07),
                    BSLS_COMPILERFEATURES_FORWARD(ARGS_08, args_08),
                    BSLS_COMPILERFEATURES_FORWARD(ARGS_09, args_09),
                    BSLS_COMPILERFEATURES_FORWARD(ARGS_10, args_10));
}
#endif  // BSLSTL_OPTIONAL_VARIADIC_LIMIT_L >= 10


#if defined(BSLS_COMPILERFEATURES_SUPPORT_GENERALIZED_INITIALIZERS)
#if BSLSTL_OPTIONAL_VARIADIC_LIMIT_L >= 0
template <class TYPE, bool USES_BSLMA_ALLOC>
template<class IL_TYPE>
void optional<TYPE, USES_BSLMA_ALLOC>::emplace(
                                           std::initializer_list<IL_TYPE> il)
{
    d_value.emplace(d_allocator.mechanism(),
                    il);
}
#endif  // BSLSTL_OPTIONAL_VARIADIC_LIMIT_L >= 0

#if BSLSTL_OPTIONAL_VARIADIC_LIMIT_L >= 1
template <class TYPE, bool USES_BSLMA_ALLOC>
template<class IL_TYPE, class ARGS_01>
void optional<TYPE, USES_BSLMA_ALLOC>::emplace(
                                           std::initializer_list<IL_TYPE> il,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_01) args_01)
{
    d_value.emplace(d_allocator.mechanism(),
                    il,
                    BSLS_COMPILERFEATURES_FORWARD(ARGS_01, args_01));
}
#endif  // BSLSTL_OPTIONAL_VARIADIC_LIMIT_L >= 1

#if BSLSTL_OPTIONAL_VARIADIC_LIMIT_L >= 2
template <class TYPE, bool USES_BSLMA_ALLOC>
template<class IL_TYPE, class ARGS_01,
                        class ARGS_02>
void optional<TYPE, USES_BSLMA_ALLOC>::emplace(
                                           std::initializer_list<IL_TYPE> il,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_01) args_01,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_02) args_02)
{
    d_value.emplace(d_allocator.mechanism(),
                    il,
                    BSLS_COMPILERFEATURES_FORWARD(ARGS_01, args_01),
                    BSLS_COMPILERFEATURES_FORWARD(ARGS_02, args_02));
}
#endif  // BSLSTL_OPTIONAL_VARIADIC_LIMIT_L >= 2

#if BSLSTL_OPTIONAL_VARIADIC_LIMIT_L >= 3
template <class TYPE, bool USES_BSLMA_ALLOC>
template<class IL_TYPE, class ARGS_01,
                        class ARGS_02,
                        class ARGS_03>
void optional<TYPE, USES_BSLMA_ALLOC>::emplace(
                                           std::initializer_list<IL_TYPE> il,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_01) args_01,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_02) args_02,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_03) args_03)
{
    d_value.emplace(d_allocator.mechanism(),
                    il,
                    BSLS_COMPILERFEATURES_FORWARD(ARGS_01, args_01),
                    BSLS_COMPILERFEATURES_FORWARD(ARGS_02, args_02),
                    BSLS_COMPILERFEATURES_FORWARD(ARGS_03, args_03));
}
#endif  // BSLSTL_OPTIONAL_VARIADIC_LIMIT_L >= 3

#if BSLSTL_OPTIONAL_VARIADIC_LIMIT_L >= 4
template <class TYPE, bool USES_BSLMA_ALLOC>
template<class IL_TYPE, class ARGS_01,
                        class ARGS_02,
                        class ARGS_03,
                        class ARGS_04>
void optional<TYPE, USES_BSLMA_ALLOC>::emplace(
                                           std::initializer_list<IL_TYPE> il,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_01) args_01,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_02) args_02,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_03) args_03,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_04) args_04)
{
    d_value.emplace(d_allocator.mechanism(),
                    il,
                    BSLS_COMPILERFEATURES_FORWARD(ARGS_01, args_01),
                    BSLS_COMPILERFEATURES_FORWARD(ARGS_02, args_02),
                    BSLS_COMPILERFEATURES_FORWARD(ARGS_03, args_03),
                    BSLS_COMPILERFEATURES_FORWARD(ARGS_04, args_04));
}
#endif  // BSLSTL_OPTIONAL_VARIADIC_LIMIT_L >= 4

#if BSLSTL_OPTIONAL_VARIADIC_LIMIT_L >= 5
template <class TYPE, bool USES_BSLMA_ALLOC>
template<class IL_TYPE, class ARGS_01,
                        class ARGS_02,
                        class ARGS_03,
                        class ARGS_04,
                        class ARGS_05>
void optional<TYPE, USES_BSLMA_ALLOC>::emplace(
                                           std::initializer_list<IL_TYPE> il,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_01) args_01,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_02) args_02,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_03) args_03,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_04) args_04,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_05) args_05)
{
    d_value.emplace(d_allocator.mechanism(),
                    il,
                    BSLS_COMPILERFEATURES_FORWARD(ARGS_01, args_01),
                    BSLS_COMPILERFEATURES_FORWARD(ARGS_02, args_02),
                    BSLS_COMPILERFEATURES_FORWARD(ARGS_03, args_03),
                    BSLS_COMPILERFEATURES_FORWARD(ARGS_04, args_04),
                    BSLS_COMPILERFEATURES_FORWARD(ARGS_05, args_05));
}
#endif  // BSLSTL_OPTIONAL_VARIADIC_LIMIT_L >= 5

#if BSLSTL_OPTIONAL_VARIADIC_LIMIT_L >= 6
template <class TYPE, bool USES_BSLMA_ALLOC>
template<class IL_TYPE, class ARGS_01,
                        class ARGS_02,
                        class ARGS_03,
                        class ARGS_04,
                        class ARGS_05,
                        class ARGS_06>
void optional<TYPE, USES_BSLMA_ALLOC>::emplace(
                                           std::initializer_list<IL_TYPE> il,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_01) args_01,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_02) args_02,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_03) args_03,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_04) args_04,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_05) args_05,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_06) args_06)
{
    d_value.emplace(d_allocator.mechanism(),
                    il,
                    BSLS_COMPILERFEATURES_FORWARD(ARGS_01, args_01),
                    BSLS_COMPILERFEATURES_FORWARD(ARGS_02, args_02),
                    BSLS_COMPILERFEATURES_FORWARD(ARGS_03, args_03),
                    BSLS_COMPILERFEATURES_FORWARD(ARGS_04, args_04),
                    BSLS_COMPILERFEATURES_FORWARD(ARGS_05, args_05),
                    BSLS_COMPILERFEATURES_FORWARD(ARGS_06, args_06));
}
#endif  // BSLSTL_OPTIONAL_VARIADIC_LIMIT_L >= 6

#if BSLSTL_OPTIONAL_VARIADIC_LIMIT_L >= 7
template <class TYPE, bool USES_BSLMA_ALLOC>
template<class IL_TYPE, class ARGS_01,
                        class ARGS_02,
                        class ARGS_03,
                        class ARGS_04,
                        class ARGS_05,
                        class ARGS_06,
                        class ARGS_07>
void optional<TYPE, USES_BSLMA_ALLOC>::emplace(
                                           std::initializer_list<IL_TYPE> il,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_01) args_01,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_02) args_02,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_03) args_03,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_04) args_04,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_05) args_05,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_06) args_06,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_07) args_07)
{
    d_value.emplace(d_allocator.mechanism(),
                    il,
                    BSLS_COMPILERFEATURES_FORWARD(ARGS_01, args_01),
                    BSLS_COMPILERFEATURES_FORWARD(ARGS_02, args_02),
                    BSLS_COMPILERFEATURES_FORWARD(ARGS_03, args_03),
                    BSLS_COMPILERFEATURES_FORWARD(ARGS_04, args_04),
                    BSLS_COMPILERFEATURES_FORWARD(ARGS_05, args_05),
                    BSLS_COMPILERFEATURES_FORWARD(ARGS_06, args_06),
                    BSLS_COMPILERFEATURES_FORWARD(ARGS_07, args_07));
}
#endif  // BSLSTL_OPTIONAL_VARIADIC_LIMIT_L >= 7

#if BSLSTL_OPTIONAL_VARIADIC_LIMIT_L >= 8
template <class TYPE, bool USES_BSLMA_ALLOC>
template<class IL_TYPE, class ARGS_01,
                        class ARGS_02,
                        class ARGS_03,
                        class ARGS_04,
                        class ARGS_05,
                        class ARGS_06,
                        class ARGS_07,
                        class ARGS_08>
void optional<TYPE, USES_BSLMA_ALLOC>::emplace(
                                           std::initializer_list<IL_TYPE> il,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_01) args_01,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_02) args_02,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_03) args_03,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_04) args_04,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_05) args_05,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_06) args_06,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_07) args_07,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_08) args_08)
{
    d_value.emplace(d_allocator.mechanism(),
                    il,
                    BSLS_COMPILERFEATURES_FORWARD(ARGS_01, args_01),
                    BSLS_COMPILERFEATURES_FORWARD(ARGS_02, args_02),
                    BSLS_COMPILERFEATURES_FORWARD(ARGS_03, args_03),
                    BSLS_COMPILERFEATURES_FORWARD(ARGS_04, args_04),
                    BSLS_COMPILERFEATURES_FORWARD(ARGS_05, args_05),
                    BSLS_COMPILERFEATURES_FORWARD(ARGS_06, args_06),
                    BSLS_COMPILERFEATURES_FORWARD(ARGS_07, args_07),
                    BSLS_COMPILERFEATURES_FORWARD(ARGS_08, args_08));
}
#endif  // BSLSTL_OPTIONAL_VARIADIC_LIMIT_L >= 8

#if BSLSTL_OPTIONAL_VARIADIC_LIMIT_L >= 9
template <class TYPE, bool USES_BSLMA_ALLOC>
template<class IL_TYPE, class ARGS_01,
                        class ARGS_02,
                        class ARGS_03,
                        class ARGS_04,
                        class ARGS_05,
                        class ARGS_06,
                        class ARGS_07,
                        class ARGS_08,
                        class ARGS_09>
void optional<TYPE, USES_BSLMA_ALLOC>::emplace(
                                           std::initializer_list<IL_TYPE> il,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_01) args_01,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_02) args_02,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_03) args_03,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_04) args_04,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_05) args_05,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_06) args_06,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_07) args_07,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_08) args_08,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_09) args_09)
{
    d_value.emplace(d_allocator.mechanism(),
                    il,
                    BSLS_COMPILERFEATURES_FORWARD(ARGS_01, args_01),
                    BSLS_COMPILERFEATURES_FORWARD(ARGS_02, args_02),
                    BSLS_COMPILERFEATURES_FORWARD(ARGS_03, args_03),
                    BSLS_COMPILERFEATURES_FORWARD(ARGS_04, args_04),
                    BSLS_COMPILERFEATURES_FORWARD(ARGS_05, args_05),
                    BSLS_COMPILERFEATURES_FORWARD(ARGS_06, args_06),
                    BSLS_COMPILERFEATURES_FORWARD(ARGS_07, args_07),
                    BSLS_COMPILERFEATURES_FORWARD(ARGS_08, args_08),
                    BSLS_COMPILERFEATURES_FORWARD(ARGS_09, args_09));
}
#endif  // BSLSTL_OPTIONAL_VARIADIC_LIMIT_L >= 9

#if BSLSTL_OPTIONAL_VARIADIC_LIMIT_L >= 10
template <class TYPE, bool USES_BSLMA_ALLOC>
template<class IL_TYPE, class ARGS_01,
                        class ARGS_02,
                        class ARGS_03,
                        class ARGS_04,
                        class ARGS_05,
                        class ARGS_06,
                        class ARGS_07,
                        class ARGS_08,
                        class ARGS_09,
                        class ARGS_10>
void optional<TYPE, USES_BSLMA_ALLOC>::emplace(
                                           std::initializer_list<IL_TYPE> il,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_01) args_01,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_02) args_02,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_03) args_03,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_04) args_04,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_05) args_05,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_06) args_06,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_07) args_07,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_08) args_08,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_09) args_09,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_10) args_10)
{
    d_value.emplace(d_allocator.mechanism(),
                    il,
                    BSLS_COMPILERFEATURES_FORWARD(ARGS_01, args_01),
                    BSLS_COMPILERFEATURES_FORWARD(ARGS_02, args_02),
                    BSLS_COMPILERFEATURES_FORWARD(ARGS_03, args_03),
                    BSLS_COMPILERFEATURES_FORWARD(ARGS_04, args_04),
                    BSLS_COMPILERFEATURES_FORWARD(ARGS_05, args_05),
                    BSLS_COMPILERFEATURES_FORWARD(ARGS_06, args_06),
                    BSLS_COMPILERFEATURES_FORWARD(ARGS_07, args_07),
                    BSLS_COMPILERFEATURES_FORWARD(ARGS_08, args_08),
                    BSLS_COMPILERFEATURES_FORWARD(ARGS_09, args_09),
                    BSLS_COMPILERFEATURES_FORWARD(ARGS_10, args_10));
}
#endif  // BSLSTL_OPTIONAL_VARIADIC_LIMIT_L >= 10

#endif
#else
// The generated code below is a workaround for the absence of perfect
// forwarding in some compilers.
template <class TYPE, bool USES_BSLMA_ALLOC>
template <class... ARGS>
inline
void optional<TYPE, USES_BSLMA_ALLOC>::emplace(
                               BSLS_COMPILERFEATURES_FORWARD_REF(ARGS)... args)
{
    d_value.emplace(d_allocator.mechanism(),
                    BSLS_COMPILERFEATURES_FORWARD(ARGS, args)...);
}

#if defined(BSLS_COMPILERFEATURES_SUPPORT_GENERALIZED_INITIALIZERS)
template <class TYPE, bool USES_BSLMA_ALLOC>
template<class IL_TYPE, class... ARGS>
void optional<TYPE, USES_BSLMA_ALLOC>::emplace(
                                           std::initializer_list<IL_TYPE> il,
                               BSLS_COMPILERFEATURES_FORWARD_REF(ARGS)... args)
{
    d_value.emplace(d_allocator.mechanism(),
                    il,
                    BSLS_COMPILERFEATURES_FORWARD(ARGS, args)...);
}
#endif
// }}} END GENERATED CODE
#endif

template <class TYPE, bool USES_BSLMA_ALLOC>
inline
void optional<TYPE, USES_BSLMA_ALLOC>::reset() BSLS_KEYWORD_NOEXCEPT
{
    d_value.reset();
}

//OBSERVERS
template <class TYPE, bool USES_BSLMA_ALLOC>
inline
TYPE& optional<TYPE, USES_BSLMA_ALLOC>::value() BSLS_KEYWORD_LVREF_QUAL
{
    if (!has_value()) BSLS_THROW(bsl::bad_optional_access());

    return d_value.value();
}

#if defined(BSLS_COMPILERFEATURES_SUPPORT_REF_QUALIFIERS)
template <class TYPE, bool USES_BSLMA_ALLOC>
inline
TYPE&& optional<TYPE, USES_BSLMA_ALLOC>::value() BSLS_KEYWORD_RVREF_QUAL
{
    if (!has_value()) BSLS_THROW(bsl::bad_optional_access());

    return std::move(d_value.value());
}

template <class TYPE, bool USES_BSLMA_ALLOC>
inline
const TYPE&&
optional<TYPE, USES_BSLMA_ALLOC>::value() const BSLS_KEYWORD_RVREF_QUAL
{
    if (!has_value()) BSLS_THROW(bsl::bad_optional_access());

    return std::move(d_value.value());
}

#endif
template <class TYPE, bool USES_BSLMA_ALLOC>
inline
const TYPE&
optional<TYPE, USES_BSLMA_ALLOC>::value() const BSLS_KEYWORD_LVREF_QUAL
{
    if (!has_value()) BSLS_THROW(bsl::bad_optional_access());

    return d_value.value();
}

template <class TYPE, bool USES_BSLMA_ALLOC>
template <class ANY_TYPE>
inline
TYPE
optional<TYPE, USES_BSLMA_ALLOC>::value_or(
                               BSLS_COMPILERFEATURES_FORWARD_REF(ANY_TYPE) rhs)
    const BSLS_KEYWORD_LVREF_QUAL
{
    if (this->has_value()) {
        return TYPE(this->value());                                   // RETURN
    }
    else {
        return TYPE(BSLS_COMPILERFEATURES_FORWARD(ANY_TYPE, rhs));    // RETURN
    }
}

#ifdef BSL_COMPILERFEATURES_GUARANTEED_COPY_ELISION
template <class TYPE, bool USES_BSLMA_ALLOC>
template <class ANY_TYPE>
inline
TYPE
optional<TYPE, USES_BSLMA_ALLOC>::value_or(
                        bsl::allocator_arg_t,
                        allocator_type allocator,
                        BSLS_COMPILERFEATURES_FORWARD_REF(ANY_TYPE) rhs)
    const BSLS_KEYWORD_LVREF_QUAL
{
    if (has_value()) {
        return BloombergLP::bslma::ConstructionUtil::make<TYPE>(
                                                         allocator.mechanism(),
                                                         this->value());
    }
    else {
        return BloombergLP::bslma::ConstructionUtil::make<TYPE>(
                                 allocator.mechanism(),
                                 BSLS_COMPILERFEATURES_FORWARD(ANY_TYPE, rhs));
    }
}
#endif //BSL_COMPILERFEATURES_GUARANTEED_COPY_ELISION

#ifdef BSLS_COMPILERFEATURES_SUPPORT_REF_QUALIFIERS
template <class TYPE, bool USES_BSLMA_ALLOC>
template <class ANY_TYPE>
inline
TYPE
optional<TYPE, USES_BSLMA_ALLOC>::value_or(ANY_TYPE&& rhs)
    BSLS_KEYWORD_RVREF_QUAL
{
    if (has_value()) {
        return TYPE(std::move(this->value()));                        // RETURN
    }
    else {
        return TYPE(std::forward<ANY_TYPE>(rhs));                     // RETURN
    }
}

#ifdef BSL_COMPILERFEATURES_GUARANTEED_COPY_ELISION
template <class TYPE, bool USES_BSLMA_ALLOC>
template <class ANY_TYPE>
inline
TYPE
optional<TYPE, USES_BSLMA_ALLOC>::value_or(bsl::allocator_arg_t,
                                           allocator_type allocator,
                                           ANY_TYPE&&     rhs)
    BSLS_KEYWORD_RVREF_QUAL
{
    if (has_value()) {
        return BloombergLP::bslma::ConstructionUtil::make<TYPE>(
                                                     allocator.mechanism(),
                                                     std::move(this->value()));
    }
    else {
        return BloombergLP::bslma::ConstructionUtil::make<TYPE>(
                                                  allocator.mechanism(),
                                                  std::forward<ANY_TYPE>(rhs));
    }
}
#endif //BSL_COMPILERFEATURES_GUARANTEED_COPY_ELISION
#endif //BSLS_COMPILERFEATURES_SUPPORT_REF_QUALIFIERS

template <class TYPE, bool USES_BSLMA_ALLOC>
inline
optional<TYPE, USES_BSLMA_ALLOC>&
optional<TYPE, USES_BSLMA_ALLOC>::operator=(bsl::nullopt_t)
    BSLS_KEYWORD_NOEXCEPT
{
    this->reset();
    return *this;
}

template <class TYPE, bool USES_BSLMA_ALLOC>
inline
optional<TYPE, USES_BSLMA_ALLOC>&
optional<TYPE, USES_BSLMA_ALLOC>::operator=(const optional &rhs)
{
    if (rhs.has_value()) {
        if (this->has_value()) {
            this->value() = rhs.value();
        }
        else {
            this->emplace(rhs.value());
        }
    }
    else {
        this->reset();
    }
    return *this;
}

template <class TYPE, bool USES_BSLMA_ALLOC>
inline
optional<TYPE, USES_BSLMA_ALLOC>&
optional<TYPE, USES_BSLMA_ALLOC>::operator=(
                                  BloombergLP::bslmf::MovableRef<optional> rhs)
{
    optional& lvalue = rhs;

    if (lvalue.has_value()) {
        if (this->has_value()) {
            this->value() = MoveUtil::move(lvalue.value());
        }
        else {
            this->emplace(MoveUtil::move(lvalue.value()));
        }
    }
    else {
        this->reset();
    }
    return *this;
}

#ifndef BSLS_COMPILERFEATURES_SUPPORT_RVALUE_REFERENCES
template <class TYPE, bool USES_BSLMA_ALLOC>
inline
optional<TYPE, USES_BSLMA_ALLOC>&
optional<TYPE, USES_BSLMA_ALLOC>::operator=(const TYPE& rhs)
{
    if (this->has_value()) {
        this->value() = rhs;
    }
    else {
        this->emplace(rhs);
    }
    return *this;
}

template <class TYPE, bool USES_BSLMA_ALLOC>
inline
optional<TYPE, USES_BSLMA_ALLOC>&
optional<TYPE, USES_BSLMA_ALLOC>::operator=(
                                      BloombergLP::bslmf::MovableRef<TYPE> rhs)
{
    TYPE& lvalue = rhs;

    if (this->has_value()) {
        this->value() = MoveUtil::move(lvalue);
    }
    else {
        this->emplace(MoveUtil::move(lvalue));
    }
    return *this;
}

template <class TYPE, bool USES_BSLMA_ALLOC>
template <class ANY_TYPE>
inline
optional<TYPE, USES_BSLMA_ALLOC>&
optional<TYPE, USES_BSLMA_ALLOC>::operator=(const ANY_TYPE& rhs)
{
    // Must be in-place inline because the use of 'enable_if' will
    // otherwise break the MSVC 2010 compiler.
    if (this->has_value()) {
        this->value() = rhs;
    }
    else {
        this->emplace(rhs);
    }
    return *this;
}

template <class TYPE, bool USES_BSLMA_ALLOC>
template <class ANY_TYPE>
inline
optional<TYPE, USES_BSLMA_ALLOC>&
optional<TYPE, USES_BSLMA_ALLOC>::operator=(
                                  BloombergLP::bslmf::MovableRef<ANY_TYPE> rhs)
{
    // Must be in-place inline because the use of 'enable_if' will
    // otherwise break the MSVC 2010 compiler.
    ANY_TYPE& lvalue = rhs;
    if (this->has_value()) {
        this->value() = MoveUtil::move(lvalue);
    }
    else {
        this->emplace(MoveUtil::move(lvalue));
    }
    return *this;
}
#endif //BSLS_COMPILERFEATURES_SUPPORT_RVALUE_REFERENCES

template <class TYPE, bool USES_BSLMA_ALLOC>
inline
TYPE& optional<TYPE, USES_BSLMA_ALLOC>::operator*() BSLS_KEYWORD_LVREF_QUAL
{
    return this->value();
}

template <class TYPE, bool USES_BSLMA_ALLOC>
inline
const TYPE&
optional<TYPE, USES_BSLMA_ALLOC>::operator*() const BSLS_KEYWORD_LVREF_QUAL
{
    return this->value();
}

template <class TYPE, bool USES_BSLMA_ALLOC>
inline
TYPE* optional<TYPE, USES_BSLMA_ALLOC>::operator->()
{
    return BSLS_UTIL_ADDRESSOF(this->value());
}

template <class TYPE, bool USES_BSLMA_ALLOC>
inline
const TYPE* optional<TYPE, USES_BSLMA_ALLOC>::operator->() const
{
    return BSLS_UTIL_ADDRESSOF(this->value());
}

template <class TYPE, bool USES_BSLMA_ALLOC>
inline
bool optional<TYPE, USES_BSLMA_ALLOC>::has_value() const BSLS_KEYWORD_NOEXCEPT
{
    return d_value.hasValue();
}

#ifdef BSLS_COMPILERFEATURES_SUPPORT_OPERATOR_EXPLICIT
template <class TYPE, bool USES_BSLMA_ALLOC>
optional<TYPE, USES_BSLMA_ALLOC>::operator bool() const BSLS_KEYWORD_NOEXCEPT
{
    return this->has_value();
}
#endif //#ifdef BSLS_COMPILERFEATURES_SUPPORT_OPERATOR_EXPLICIT

template <class TYPE, bool USES_BSLMA_ALLOC>
inline
typename optional<TYPE, USES_BSLMA_ALLOC>::allocator_type
optional<TYPE, USES_BSLMA_ALLOC>::get_allocator() const BSLS_KEYWORD_NOEXCEPT
{
    return d_allocator;
}

template <class TYPE, bool USES_BSLMA_ALLOC>
void optional<TYPE, USES_BSLMA_ALLOC>::swap(optional &other)
{
    if (this->has_value() && other.has_value()) {
        BloombergLP::bslalg::SwapUtil::swap(&(this->value()),
                                            &(other.value()));
    }
    else if (this->has_value()) {
        other.emplace(MoveUtil::move(this->value()));
        this->reset();
    }
    else if (other.has_value()) {
        this->emplace(MoveUtil::move(other.value()));
        other.reset();
    }
}

                      // ---------------------------
                      // class optional<TYPE, false>
                      // ---------------------------
#ifdef __cpp_lib_optional

template <class TYPE>
inline
optional<TYPE, false>::optional(const optional& rhs)
: optionalBase(static_cast<const optionalBase &>(rhs))
{
}

template <class TYPE>
inline
optional<TYPE, false>::optional(optional&& rhs)
: optionalBase(static_cast<optionalBase&&>(rhs))
{
}


template <class TYPE>
inline
optional<TYPE, false>&
optional<TYPE, false>::operator=(bsl::nullopt_t) BSLS_KEYWORD_NOEXCEPT
{
    this->reset();
    return *this;
}

template <class TYPE>
inline
optional<TYPE, false>&
optional<TYPE, false>::operator=(const optional &rhs)
{
    if (rhs.has_value()) {
        if (this->has_value()) {
            this->value() = rhs.value();
        }
        else {
            this->emplace(rhs.value());
        }
    }
    else {
        this->reset();
    }
    return *this;
}

template <class TYPE>
inline
optional<TYPE, false>&
optional<TYPE, false>::operator=(optional&& rhs)
{
    optional& lvalue = rhs;
    if (lvalue.has_value()) {
        if (this->has_value()) {
            this->value() = std::move(lvalue.value());
        }
        else {
            this->emplace(std::move(lvalue.value()));
        }
    }
    else {
        this->reset();
    }
    return *this;
}


#else

// CREATORS
template <class TYPE>
inline
optional<TYPE, false>::optional()
{
}

template <class TYPE>
inline
optional<TYPE, false>::optional(bsl::nullopt_t)
{
}

template <class TYPE>
inline
optional<TYPE, false>::optional(const optional& rhs)
{
    if (rhs.has_value()) {
         emplace(rhs.value());
    }
}

template <class TYPE>
inline
optional<TYPE, false>::optional(BloombergLP::bslmf::MovableRef<optional> rhs)
{
    optional& lvalue = rhs;

    if (lvalue.has_value()) {
         emplace(MoveUtil::move(lvalue.value()));
    }
}

#if !BSLS_COMPILERFEATURES_SIMULATE_CPP11_FEATURES
template <class TYPE>
template<class... ARGS>
inline
optional<TYPE, false>::optional(
                               bsl::in_place_t,
                               BSLS_COMPILERFEATURES_FORWARD_REF(ARGS)... args)
{
    emplace(BSLS_COMPILERFEATURES_FORWARD(ARGS, args)...);
}

#if defined(BSLS_COMPILERFEATURES_SUPPORT_GENERALIZED_INITIALIZERS)
template <class TYPE>
template<class IL_TYPE, class... ARGS>
inline
optional<TYPE, false>::optional(
                               bsl::in_place_t,
                               std::initializer_list<IL_TYPE>             it,
                               BSLS_COMPILERFEATURES_FORWARD_REF(ARGS)... args)
{
    emplace(it, BSLS_COMPILERFEATURES_FORWARD(ARGS, args)...);
}
#endif//BSLS_COMPILERFEATURES_SUPPORT_GENERALIZED_INITIALIZERS
#elif BSLS_COMPILERFEATURES_SIMULATE_VARIADIC_TEMPLATES
// {{{ BEGIN GENERATED CODE
// The following section is automatically generated.  **DO NOT EDIT**
// Generator command line: sim_cpp11_features.pl bslstl_optional.h
#ifndef BSLSTL_OPTIONAL_VARIADIC_LIMIT
#define BSLSTL_OPTIONAL_VARIADIC_LIMIT 10
#endif
#ifndef BSLSTL_OPTIONAL_VARIADIC_LIMIT_M
#define BSLSTL_OPTIONAL_VARIADIC_LIMIT_M BSLSTL_OPTIONAL_VARIADIC_LIMIT
#endif
#if BSLSTL_OPTIONAL_VARIADIC_LIMIT_M >= 0
template <class TYPE>
inline
optional<TYPE, false>::optional(
                               bsl::in_place_t)
{
    emplace();
}
#endif  // BSLSTL_OPTIONAL_VARIADIC_LIMIT_M >= 0

#if BSLSTL_OPTIONAL_VARIADIC_LIMIT_M >= 1
template <class TYPE>
template<class ARGS_01>
inline
optional<TYPE, false>::optional(
                               bsl::in_place_t,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_01) args_01)
{
    emplace(BSLS_COMPILERFEATURES_FORWARD(ARGS_01, args_01));
}
#endif  // BSLSTL_OPTIONAL_VARIADIC_LIMIT_M >= 1

#if BSLSTL_OPTIONAL_VARIADIC_LIMIT_M >= 2
template <class TYPE>
template<class ARGS_01,
         class ARGS_02>
inline
optional<TYPE, false>::optional(
                               bsl::in_place_t,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_01) args_01,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_02) args_02)
{
    emplace(BSLS_COMPILERFEATURES_FORWARD(ARGS_01, args_01),
            BSLS_COMPILERFEATURES_FORWARD(ARGS_02, args_02));
}
#endif  // BSLSTL_OPTIONAL_VARIADIC_LIMIT_M >= 2

#if BSLSTL_OPTIONAL_VARIADIC_LIMIT_M >= 3
template <class TYPE>
template<class ARGS_01,
         class ARGS_02,
         class ARGS_03>
inline
optional<TYPE, false>::optional(
                               bsl::in_place_t,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_01) args_01,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_02) args_02,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_03) args_03)
{
    emplace(BSLS_COMPILERFEATURES_FORWARD(ARGS_01, args_01),
            BSLS_COMPILERFEATURES_FORWARD(ARGS_02, args_02),
            BSLS_COMPILERFEATURES_FORWARD(ARGS_03, args_03));
}
#endif  // BSLSTL_OPTIONAL_VARIADIC_LIMIT_M >= 3

#if BSLSTL_OPTIONAL_VARIADIC_LIMIT_M >= 4
template <class TYPE>
template<class ARGS_01,
         class ARGS_02,
         class ARGS_03,
         class ARGS_04>
inline
optional<TYPE, false>::optional(
                               bsl::in_place_t,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_01) args_01,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_02) args_02,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_03) args_03,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_04) args_04)
{
    emplace(BSLS_COMPILERFEATURES_FORWARD(ARGS_01, args_01),
            BSLS_COMPILERFEATURES_FORWARD(ARGS_02, args_02),
            BSLS_COMPILERFEATURES_FORWARD(ARGS_03, args_03),
            BSLS_COMPILERFEATURES_FORWARD(ARGS_04, args_04));
}
#endif  // BSLSTL_OPTIONAL_VARIADIC_LIMIT_M >= 4

#if BSLSTL_OPTIONAL_VARIADIC_LIMIT_M >= 5
template <class TYPE>
template<class ARGS_01,
         class ARGS_02,
         class ARGS_03,
         class ARGS_04,
         class ARGS_05>
inline
optional<TYPE, false>::optional(
                               bsl::in_place_t,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_01) args_01,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_02) args_02,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_03) args_03,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_04) args_04,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_05) args_05)
{
    emplace(BSLS_COMPILERFEATURES_FORWARD(ARGS_01, args_01),
            BSLS_COMPILERFEATURES_FORWARD(ARGS_02, args_02),
            BSLS_COMPILERFEATURES_FORWARD(ARGS_03, args_03),
            BSLS_COMPILERFEATURES_FORWARD(ARGS_04, args_04),
            BSLS_COMPILERFEATURES_FORWARD(ARGS_05, args_05));
}
#endif  // BSLSTL_OPTIONAL_VARIADIC_LIMIT_M >= 5

#if BSLSTL_OPTIONAL_VARIADIC_LIMIT_M >= 6
template <class TYPE>
template<class ARGS_01,
         class ARGS_02,
         class ARGS_03,
         class ARGS_04,
         class ARGS_05,
         class ARGS_06>
inline
optional<TYPE, false>::optional(
                               bsl::in_place_t,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_01) args_01,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_02) args_02,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_03) args_03,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_04) args_04,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_05) args_05,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_06) args_06)
{
    emplace(BSLS_COMPILERFEATURES_FORWARD(ARGS_01, args_01),
            BSLS_COMPILERFEATURES_FORWARD(ARGS_02, args_02),
            BSLS_COMPILERFEATURES_FORWARD(ARGS_03, args_03),
            BSLS_COMPILERFEATURES_FORWARD(ARGS_04, args_04),
            BSLS_COMPILERFEATURES_FORWARD(ARGS_05, args_05),
            BSLS_COMPILERFEATURES_FORWARD(ARGS_06, args_06));
}
#endif  // BSLSTL_OPTIONAL_VARIADIC_LIMIT_M >= 6

#if BSLSTL_OPTIONAL_VARIADIC_LIMIT_M >= 7
template <class TYPE>
template<class ARGS_01,
         class ARGS_02,
         class ARGS_03,
         class ARGS_04,
         class ARGS_05,
         class ARGS_06,
         class ARGS_07>
inline
optional<TYPE, false>::optional(
                               bsl::in_place_t,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_01) args_01,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_02) args_02,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_03) args_03,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_04) args_04,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_05) args_05,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_06) args_06,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_07) args_07)
{
    emplace(BSLS_COMPILERFEATURES_FORWARD(ARGS_01, args_01),
            BSLS_COMPILERFEATURES_FORWARD(ARGS_02, args_02),
            BSLS_COMPILERFEATURES_FORWARD(ARGS_03, args_03),
            BSLS_COMPILERFEATURES_FORWARD(ARGS_04, args_04),
            BSLS_COMPILERFEATURES_FORWARD(ARGS_05, args_05),
            BSLS_COMPILERFEATURES_FORWARD(ARGS_06, args_06),
            BSLS_COMPILERFEATURES_FORWARD(ARGS_07, args_07));
}
#endif  // BSLSTL_OPTIONAL_VARIADIC_LIMIT_M >= 7

#if BSLSTL_OPTIONAL_VARIADIC_LIMIT_M >= 8
template <class TYPE>
template<class ARGS_01,
         class ARGS_02,
         class ARGS_03,
         class ARGS_04,
         class ARGS_05,
         class ARGS_06,
         class ARGS_07,
         class ARGS_08>
inline
optional<TYPE, false>::optional(
                               bsl::in_place_t,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_01) args_01,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_02) args_02,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_03) args_03,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_04) args_04,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_05) args_05,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_06) args_06,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_07) args_07,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_08) args_08)
{
    emplace(BSLS_COMPILERFEATURES_FORWARD(ARGS_01, args_01),
            BSLS_COMPILERFEATURES_FORWARD(ARGS_02, args_02),
            BSLS_COMPILERFEATURES_FORWARD(ARGS_03, args_03),
            BSLS_COMPILERFEATURES_FORWARD(ARGS_04, args_04),
            BSLS_COMPILERFEATURES_FORWARD(ARGS_05, args_05),
            BSLS_COMPILERFEATURES_FORWARD(ARGS_06, args_06),
            BSLS_COMPILERFEATURES_FORWARD(ARGS_07, args_07),
            BSLS_COMPILERFEATURES_FORWARD(ARGS_08, args_08));
}
#endif  // BSLSTL_OPTIONAL_VARIADIC_LIMIT_M >= 8

#if BSLSTL_OPTIONAL_VARIADIC_LIMIT_M >= 9
template <class TYPE>
template<class ARGS_01,
         class ARGS_02,
         class ARGS_03,
         class ARGS_04,
         class ARGS_05,
         class ARGS_06,
         class ARGS_07,
         class ARGS_08,
         class ARGS_09>
inline
optional<TYPE, false>::optional(
                               bsl::in_place_t,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_01) args_01,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_02) args_02,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_03) args_03,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_04) args_04,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_05) args_05,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_06) args_06,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_07) args_07,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_08) args_08,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_09) args_09)
{
    emplace(BSLS_COMPILERFEATURES_FORWARD(ARGS_01, args_01),
            BSLS_COMPILERFEATURES_FORWARD(ARGS_02, args_02),
            BSLS_COMPILERFEATURES_FORWARD(ARGS_03, args_03),
            BSLS_COMPILERFEATURES_FORWARD(ARGS_04, args_04),
            BSLS_COMPILERFEATURES_FORWARD(ARGS_05, args_05),
            BSLS_COMPILERFEATURES_FORWARD(ARGS_06, args_06),
            BSLS_COMPILERFEATURES_FORWARD(ARGS_07, args_07),
            BSLS_COMPILERFEATURES_FORWARD(ARGS_08, args_08),
            BSLS_COMPILERFEATURES_FORWARD(ARGS_09, args_09));
}
#endif  // BSLSTL_OPTIONAL_VARIADIC_LIMIT_M >= 9

#if BSLSTL_OPTIONAL_VARIADIC_LIMIT_M >= 10
template <class TYPE>
template<class ARGS_01,
         class ARGS_02,
         class ARGS_03,
         class ARGS_04,
         class ARGS_05,
         class ARGS_06,
         class ARGS_07,
         class ARGS_08,
         class ARGS_09,
         class ARGS_10>
inline
optional<TYPE, false>::optional(
                               bsl::in_place_t,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_01) args_01,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_02) args_02,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_03) args_03,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_04) args_04,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_05) args_05,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_06) args_06,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_07) args_07,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_08) args_08,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_09) args_09,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_10) args_10)
{
    emplace(BSLS_COMPILERFEATURES_FORWARD(ARGS_01, args_01),
            BSLS_COMPILERFEATURES_FORWARD(ARGS_02, args_02),
            BSLS_COMPILERFEATURES_FORWARD(ARGS_03, args_03),
            BSLS_COMPILERFEATURES_FORWARD(ARGS_04, args_04),
            BSLS_COMPILERFEATURES_FORWARD(ARGS_05, args_05),
            BSLS_COMPILERFEATURES_FORWARD(ARGS_06, args_06),
            BSLS_COMPILERFEATURES_FORWARD(ARGS_07, args_07),
            BSLS_COMPILERFEATURES_FORWARD(ARGS_08, args_08),
            BSLS_COMPILERFEATURES_FORWARD(ARGS_09, args_09),
            BSLS_COMPILERFEATURES_FORWARD(ARGS_10, args_10));
}
#endif  // BSLSTL_OPTIONAL_VARIADIC_LIMIT_M >= 10


#if defined(BSLS_COMPILERFEATURES_SUPPORT_GENERALIZED_INITIALIZERS)
#if BSLSTL_OPTIONAL_VARIADIC_LIMIT_M >= 0
template <class TYPE>
template<class IL_TYPE>
inline
optional<TYPE, false>::optional(
                               bsl::in_place_t,
                               std::initializer_list<IL_TYPE>             it)
{
    emplace(it);
}
#endif  // BSLSTL_OPTIONAL_VARIADIC_LIMIT_M >= 0

#if BSLSTL_OPTIONAL_VARIADIC_LIMIT_M >= 1
template <class TYPE>
template<class IL_TYPE, class ARGS_01>
inline
optional<TYPE, false>::optional(
                               bsl::in_place_t,
                               std::initializer_list<IL_TYPE>             it,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_01) args_01)
{
    emplace(it, BSLS_COMPILERFEATURES_FORWARD(ARGS_01, args_01));
}
#endif  // BSLSTL_OPTIONAL_VARIADIC_LIMIT_M >= 1

#if BSLSTL_OPTIONAL_VARIADIC_LIMIT_M >= 2
template <class TYPE>
template<class IL_TYPE, class ARGS_01,
                        class ARGS_02>
inline
optional<TYPE, false>::optional(
                               bsl::in_place_t,
                               std::initializer_list<IL_TYPE>             it,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_01) args_01,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_02) args_02)
{
    emplace(it, BSLS_COMPILERFEATURES_FORWARD(ARGS_01, args_01),
                BSLS_COMPILERFEATURES_FORWARD(ARGS_02, args_02));
}
#endif  // BSLSTL_OPTIONAL_VARIADIC_LIMIT_M >= 2

#if BSLSTL_OPTIONAL_VARIADIC_LIMIT_M >= 3
template <class TYPE>
template<class IL_TYPE, class ARGS_01,
                        class ARGS_02,
                        class ARGS_03>
inline
optional<TYPE, false>::optional(
                               bsl::in_place_t,
                               std::initializer_list<IL_TYPE>             it,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_01) args_01,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_02) args_02,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_03) args_03)
{
    emplace(it, BSLS_COMPILERFEATURES_FORWARD(ARGS_01, args_01),
                BSLS_COMPILERFEATURES_FORWARD(ARGS_02, args_02),
                BSLS_COMPILERFEATURES_FORWARD(ARGS_03, args_03));
}
#endif  // BSLSTL_OPTIONAL_VARIADIC_LIMIT_M >= 3

#if BSLSTL_OPTIONAL_VARIADIC_LIMIT_M >= 4
template <class TYPE>
template<class IL_TYPE, class ARGS_01,
                        class ARGS_02,
                        class ARGS_03,
                        class ARGS_04>
inline
optional<TYPE, false>::optional(
                               bsl::in_place_t,
                               std::initializer_list<IL_TYPE>             it,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_01) args_01,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_02) args_02,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_03) args_03,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_04) args_04)
{
    emplace(it, BSLS_COMPILERFEATURES_FORWARD(ARGS_01, args_01),
                BSLS_COMPILERFEATURES_FORWARD(ARGS_02, args_02),
                BSLS_COMPILERFEATURES_FORWARD(ARGS_03, args_03),
                BSLS_COMPILERFEATURES_FORWARD(ARGS_04, args_04));
}
#endif  // BSLSTL_OPTIONAL_VARIADIC_LIMIT_M >= 4

#if BSLSTL_OPTIONAL_VARIADIC_LIMIT_M >= 5
template <class TYPE>
template<class IL_TYPE, class ARGS_01,
                        class ARGS_02,
                        class ARGS_03,
                        class ARGS_04,
                        class ARGS_05>
inline
optional<TYPE, false>::optional(
                               bsl::in_place_t,
                               std::initializer_list<IL_TYPE>             it,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_01) args_01,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_02) args_02,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_03) args_03,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_04) args_04,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_05) args_05)
{
    emplace(it, BSLS_COMPILERFEATURES_FORWARD(ARGS_01, args_01),
                BSLS_COMPILERFEATURES_FORWARD(ARGS_02, args_02),
                BSLS_COMPILERFEATURES_FORWARD(ARGS_03, args_03),
                BSLS_COMPILERFEATURES_FORWARD(ARGS_04, args_04),
                BSLS_COMPILERFEATURES_FORWARD(ARGS_05, args_05));
}
#endif  // BSLSTL_OPTIONAL_VARIADIC_LIMIT_M >= 5

#if BSLSTL_OPTIONAL_VARIADIC_LIMIT_M >= 6
template <class TYPE>
template<class IL_TYPE, class ARGS_01,
                        class ARGS_02,
                        class ARGS_03,
                        class ARGS_04,
                        class ARGS_05,
                        class ARGS_06>
inline
optional<TYPE, false>::optional(
                               bsl::in_place_t,
                               std::initializer_list<IL_TYPE>             it,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_01) args_01,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_02) args_02,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_03) args_03,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_04) args_04,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_05) args_05,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_06) args_06)
{
    emplace(it, BSLS_COMPILERFEATURES_FORWARD(ARGS_01, args_01),
                BSLS_COMPILERFEATURES_FORWARD(ARGS_02, args_02),
                BSLS_COMPILERFEATURES_FORWARD(ARGS_03, args_03),
                BSLS_COMPILERFEATURES_FORWARD(ARGS_04, args_04),
                BSLS_COMPILERFEATURES_FORWARD(ARGS_05, args_05),
                BSLS_COMPILERFEATURES_FORWARD(ARGS_06, args_06));
}
#endif  // BSLSTL_OPTIONAL_VARIADIC_LIMIT_M >= 6

#if BSLSTL_OPTIONAL_VARIADIC_LIMIT_M >= 7
template <class TYPE>
template<class IL_TYPE, class ARGS_01,
                        class ARGS_02,
                        class ARGS_03,
                        class ARGS_04,
                        class ARGS_05,
                        class ARGS_06,
                        class ARGS_07>
inline
optional<TYPE, false>::optional(
                               bsl::in_place_t,
                               std::initializer_list<IL_TYPE>             it,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_01) args_01,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_02) args_02,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_03) args_03,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_04) args_04,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_05) args_05,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_06) args_06,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_07) args_07)
{
    emplace(it, BSLS_COMPILERFEATURES_FORWARD(ARGS_01, args_01),
                BSLS_COMPILERFEATURES_FORWARD(ARGS_02, args_02),
                BSLS_COMPILERFEATURES_FORWARD(ARGS_03, args_03),
                BSLS_COMPILERFEATURES_FORWARD(ARGS_04, args_04),
                BSLS_COMPILERFEATURES_FORWARD(ARGS_05, args_05),
                BSLS_COMPILERFEATURES_FORWARD(ARGS_06, args_06),
                BSLS_COMPILERFEATURES_FORWARD(ARGS_07, args_07));
}
#endif  // BSLSTL_OPTIONAL_VARIADIC_LIMIT_M >= 7

#if BSLSTL_OPTIONAL_VARIADIC_LIMIT_M >= 8
template <class TYPE>
template<class IL_TYPE, class ARGS_01,
                        class ARGS_02,
                        class ARGS_03,
                        class ARGS_04,
                        class ARGS_05,
                        class ARGS_06,
                        class ARGS_07,
                        class ARGS_08>
inline
optional<TYPE, false>::optional(
                               bsl::in_place_t,
                               std::initializer_list<IL_TYPE>             it,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_01) args_01,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_02) args_02,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_03) args_03,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_04) args_04,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_05) args_05,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_06) args_06,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_07) args_07,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_08) args_08)
{
    emplace(it, BSLS_COMPILERFEATURES_FORWARD(ARGS_01, args_01),
                BSLS_COMPILERFEATURES_FORWARD(ARGS_02, args_02),
                BSLS_COMPILERFEATURES_FORWARD(ARGS_03, args_03),
                BSLS_COMPILERFEATURES_FORWARD(ARGS_04, args_04),
                BSLS_COMPILERFEATURES_FORWARD(ARGS_05, args_05),
                BSLS_COMPILERFEATURES_FORWARD(ARGS_06, args_06),
                BSLS_COMPILERFEATURES_FORWARD(ARGS_07, args_07),
                BSLS_COMPILERFEATURES_FORWARD(ARGS_08, args_08));
}
#endif  // BSLSTL_OPTIONAL_VARIADIC_LIMIT_M >= 8

#if BSLSTL_OPTIONAL_VARIADIC_LIMIT_M >= 9
template <class TYPE>
template<class IL_TYPE, class ARGS_01,
                        class ARGS_02,
                        class ARGS_03,
                        class ARGS_04,
                        class ARGS_05,
                        class ARGS_06,
                        class ARGS_07,
                        class ARGS_08,
                        class ARGS_09>
inline
optional<TYPE, false>::optional(
                               bsl::in_place_t,
                               std::initializer_list<IL_TYPE>             it,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_01) args_01,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_02) args_02,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_03) args_03,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_04) args_04,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_05) args_05,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_06) args_06,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_07) args_07,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_08) args_08,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_09) args_09)
{
    emplace(it, BSLS_COMPILERFEATURES_FORWARD(ARGS_01, args_01),
                BSLS_COMPILERFEATURES_FORWARD(ARGS_02, args_02),
                BSLS_COMPILERFEATURES_FORWARD(ARGS_03, args_03),
                BSLS_COMPILERFEATURES_FORWARD(ARGS_04, args_04),
                BSLS_COMPILERFEATURES_FORWARD(ARGS_05, args_05),
                BSLS_COMPILERFEATURES_FORWARD(ARGS_06, args_06),
                BSLS_COMPILERFEATURES_FORWARD(ARGS_07, args_07),
                BSLS_COMPILERFEATURES_FORWARD(ARGS_08, args_08),
                BSLS_COMPILERFEATURES_FORWARD(ARGS_09, args_09));
}
#endif  // BSLSTL_OPTIONAL_VARIADIC_LIMIT_M >= 9

#if BSLSTL_OPTIONAL_VARIADIC_LIMIT_M >= 10
template <class TYPE>
template<class IL_TYPE, class ARGS_01,
                        class ARGS_02,
                        class ARGS_03,
                        class ARGS_04,
                        class ARGS_05,
                        class ARGS_06,
                        class ARGS_07,
                        class ARGS_08,
                        class ARGS_09,
                        class ARGS_10>
inline
optional<TYPE, false>::optional(
                               bsl::in_place_t,
                               std::initializer_list<IL_TYPE>             it,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_01) args_01,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_02) args_02,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_03) args_03,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_04) args_04,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_05) args_05,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_06) args_06,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_07) args_07,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_08) args_08,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_09) args_09,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_10) args_10)
{
    emplace(it, BSLS_COMPILERFEATURES_FORWARD(ARGS_01, args_01),
                BSLS_COMPILERFEATURES_FORWARD(ARGS_02, args_02),
                BSLS_COMPILERFEATURES_FORWARD(ARGS_03, args_03),
                BSLS_COMPILERFEATURES_FORWARD(ARGS_04, args_04),
                BSLS_COMPILERFEATURES_FORWARD(ARGS_05, args_05),
                BSLS_COMPILERFEATURES_FORWARD(ARGS_06, args_06),
                BSLS_COMPILERFEATURES_FORWARD(ARGS_07, args_07),
                BSLS_COMPILERFEATURES_FORWARD(ARGS_08, args_08),
                BSLS_COMPILERFEATURES_FORWARD(ARGS_09, args_09),
                BSLS_COMPILERFEATURES_FORWARD(ARGS_10, args_10));
}
#endif  // BSLSTL_OPTIONAL_VARIADIC_LIMIT_M >= 10

#endif
#else
// The generated code below is a workaround for the absence of perfect
// forwarding in some compilers.
template <class TYPE>
template<class... ARGS>
inline
optional<TYPE, false>::optional(
                               bsl::in_place_t,
                               BSLS_COMPILERFEATURES_FORWARD_REF(ARGS)... args)
{
    emplace(BSLS_COMPILERFEATURES_FORWARD(ARGS, args)...);
}

#if defined(BSLS_COMPILERFEATURES_SUPPORT_GENERALIZED_INITIALIZERS)
template <class TYPE>
template<class IL_TYPE, class... ARGS>
inline
optional<TYPE, false>::optional(
                               bsl::in_place_t,
                               std::initializer_list<IL_TYPE>             it,
                               BSLS_COMPILERFEATURES_FORWARD_REF(ARGS)... args)
{
    emplace(it, BSLS_COMPILERFEATURES_FORWARD(ARGS, args)...);
}
#endif
// }}} END GENERATED CODE
#endif

//MANIPULATORS
#if !BSLS_COMPILERFEATURES_SIMULATE_CPP11_FEATURES
template <class TYPE>
template <class... ARGS>
inline
void optional<TYPE, false>::emplace(
                               BSLS_COMPILERFEATURES_FORWARD_REF(ARGS)... args)
{
    d_value.emplace(NULL, BSLS_COMPILERFEATURES_FORWARD(ARGS, args)...);
}

#if defined(BSLS_COMPILERFEATURES_SUPPORT_GENERALIZED_INITIALIZERS)
template <class TYPE>
template<class IL_TYPE, class... ARGS>
void optional<TYPE, false>::emplace(
                               std::initializer_list<IL_TYPE>             il,
                               BSLS_COMPILERFEATURES_FORWARD_REF(ARGS)... args)
{
    d_value.emplace(NULL,
                    il,
                    BSLS_COMPILERFEATURES_FORWARD(ARGS, args)...);
}
#endif//BSLS_COMPILERFEATURES_SUPPORT_GENERALIZED_INITIALIZERS
#elif BSLS_COMPILERFEATURES_SIMULATE_VARIADIC_TEMPLATES
// {{{ BEGIN GENERATED CODE
// The following section is automatically generated.  **DO NOT EDIT**
// Generator command line: sim_cpp11_features.pl bslstl_optional.h
#ifndef BSLSTL_OPTIONAL_VARIADIC_LIMIT
#define BSLSTL_OPTIONAL_VARIADIC_LIMIT 10
#endif
#ifndef BSLSTL_OPTIONAL_VARIADIC_LIMIT_N
#define BSLSTL_OPTIONAL_VARIADIC_LIMIT_N BSLSTL_OPTIONAL_VARIADIC_LIMIT
#endif
#if BSLSTL_OPTIONAL_VARIADIC_LIMIT_N >= 0
template <class TYPE>
inline
void optional<TYPE, false>::emplace(
                               )
{
    d_value.emplace(NULL);
}
#endif  // BSLSTL_OPTIONAL_VARIADIC_LIMIT_N >= 0

#if BSLSTL_OPTIONAL_VARIADIC_LIMIT_N >= 1
template <class TYPE>
template <class ARGS_01>
inline
void optional<TYPE, false>::emplace(
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_01) args_01)
{
    d_value.emplace(NULL, BSLS_COMPILERFEATURES_FORWARD(ARGS_01, args_01));
}
#endif  // BSLSTL_OPTIONAL_VARIADIC_LIMIT_N >= 1

#if BSLSTL_OPTIONAL_VARIADIC_LIMIT_N >= 2
template <class TYPE>
template <class ARGS_01,
          class ARGS_02>
inline
void optional<TYPE, false>::emplace(
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_01) args_01,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_02) args_02)
{
    d_value.emplace(NULL, BSLS_COMPILERFEATURES_FORWARD(ARGS_01, args_01),
                          BSLS_COMPILERFEATURES_FORWARD(ARGS_02, args_02));
}
#endif  // BSLSTL_OPTIONAL_VARIADIC_LIMIT_N >= 2

#if BSLSTL_OPTIONAL_VARIADIC_LIMIT_N >= 3
template <class TYPE>
template <class ARGS_01,
          class ARGS_02,
          class ARGS_03>
inline
void optional<TYPE, false>::emplace(
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_01) args_01,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_02) args_02,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_03) args_03)
{
    d_value.emplace(NULL, BSLS_COMPILERFEATURES_FORWARD(ARGS_01, args_01),
                          BSLS_COMPILERFEATURES_FORWARD(ARGS_02, args_02),
                          BSLS_COMPILERFEATURES_FORWARD(ARGS_03, args_03));
}
#endif  // BSLSTL_OPTIONAL_VARIADIC_LIMIT_N >= 3

#if BSLSTL_OPTIONAL_VARIADIC_LIMIT_N >= 4
template <class TYPE>
template <class ARGS_01,
          class ARGS_02,
          class ARGS_03,
          class ARGS_04>
inline
void optional<TYPE, false>::emplace(
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_01) args_01,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_02) args_02,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_03) args_03,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_04) args_04)
{
    d_value.emplace(NULL, BSLS_COMPILERFEATURES_FORWARD(ARGS_01, args_01),
                          BSLS_COMPILERFEATURES_FORWARD(ARGS_02, args_02),
                          BSLS_COMPILERFEATURES_FORWARD(ARGS_03, args_03),
                          BSLS_COMPILERFEATURES_FORWARD(ARGS_04, args_04));
}
#endif  // BSLSTL_OPTIONAL_VARIADIC_LIMIT_N >= 4

#if BSLSTL_OPTIONAL_VARIADIC_LIMIT_N >= 5
template <class TYPE>
template <class ARGS_01,
          class ARGS_02,
          class ARGS_03,
          class ARGS_04,
          class ARGS_05>
inline
void optional<TYPE, false>::emplace(
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_01) args_01,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_02) args_02,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_03) args_03,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_04) args_04,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_05) args_05)
{
    d_value.emplace(NULL, BSLS_COMPILERFEATURES_FORWARD(ARGS_01, args_01),
                          BSLS_COMPILERFEATURES_FORWARD(ARGS_02, args_02),
                          BSLS_COMPILERFEATURES_FORWARD(ARGS_03, args_03),
                          BSLS_COMPILERFEATURES_FORWARD(ARGS_04, args_04),
                          BSLS_COMPILERFEATURES_FORWARD(ARGS_05, args_05));
}
#endif  // BSLSTL_OPTIONAL_VARIADIC_LIMIT_N >= 5

#if BSLSTL_OPTIONAL_VARIADIC_LIMIT_N >= 6
template <class TYPE>
template <class ARGS_01,
          class ARGS_02,
          class ARGS_03,
          class ARGS_04,
          class ARGS_05,
          class ARGS_06>
inline
void optional<TYPE, false>::emplace(
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_01) args_01,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_02) args_02,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_03) args_03,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_04) args_04,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_05) args_05,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_06) args_06)
{
    d_value.emplace(NULL, BSLS_COMPILERFEATURES_FORWARD(ARGS_01, args_01),
                          BSLS_COMPILERFEATURES_FORWARD(ARGS_02, args_02),
                          BSLS_COMPILERFEATURES_FORWARD(ARGS_03, args_03),
                          BSLS_COMPILERFEATURES_FORWARD(ARGS_04, args_04),
                          BSLS_COMPILERFEATURES_FORWARD(ARGS_05, args_05),
                          BSLS_COMPILERFEATURES_FORWARD(ARGS_06, args_06));
}
#endif  // BSLSTL_OPTIONAL_VARIADIC_LIMIT_N >= 6

#if BSLSTL_OPTIONAL_VARIADIC_LIMIT_N >= 7
template <class TYPE>
template <class ARGS_01,
          class ARGS_02,
          class ARGS_03,
          class ARGS_04,
          class ARGS_05,
          class ARGS_06,
          class ARGS_07>
inline
void optional<TYPE, false>::emplace(
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_01) args_01,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_02) args_02,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_03) args_03,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_04) args_04,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_05) args_05,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_06) args_06,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_07) args_07)
{
    d_value.emplace(NULL, BSLS_COMPILERFEATURES_FORWARD(ARGS_01, args_01),
                          BSLS_COMPILERFEATURES_FORWARD(ARGS_02, args_02),
                          BSLS_COMPILERFEATURES_FORWARD(ARGS_03, args_03),
                          BSLS_COMPILERFEATURES_FORWARD(ARGS_04, args_04),
                          BSLS_COMPILERFEATURES_FORWARD(ARGS_05, args_05),
                          BSLS_COMPILERFEATURES_FORWARD(ARGS_06, args_06),
                          BSLS_COMPILERFEATURES_FORWARD(ARGS_07, args_07));
}
#endif  // BSLSTL_OPTIONAL_VARIADIC_LIMIT_N >= 7

#if BSLSTL_OPTIONAL_VARIADIC_LIMIT_N >= 8
template <class TYPE>
template <class ARGS_01,
          class ARGS_02,
          class ARGS_03,
          class ARGS_04,
          class ARGS_05,
          class ARGS_06,
          class ARGS_07,
          class ARGS_08>
inline
void optional<TYPE, false>::emplace(
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_01) args_01,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_02) args_02,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_03) args_03,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_04) args_04,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_05) args_05,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_06) args_06,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_07) args_07,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_08) args_08)
{
    d_value.emplace(NULL, BSLS_COMPILERFEATURES_FORWARD(ARGS_01, args_01),
                          BSLS_COMPILERFEATURES_FORWARD(ARGS_02, args_02),
                          BSLS_COMPILERFEATURES_FORWARD(ARGS_03, args_03),
                          BSLS_COMPILERFEATURES_FORWARD(ARGS_04, args_04),
                          BSLS_COMPILERFEATURES_FORWARD(ARGS_05, args_05),
                          BSLS_COMPILERFEATURES_FORWARD(ARGS_06, args_06),
                          BSLS_COMPILERFEATURES_FORWARD(ARGS_07, args_07),
                          BSLS_COMPILERFEATURES_FORWARD(ARGS_08, args_08));
}
#endif  // BSLSTL_OPTIONAL_VARIADIC_LIMIT_N >= 8

#if BSLSTL_OPTIONAL_VARIADIC_LIMIT_N >= 9
template <class TYPE>
template <class ARGS_01,
          class ARGS_02,
          class ARGS_03,
          class ARGS_04,
          class ARGS_05,
          class ARGS_06,
          class ARGS_07,
          class ARGS_08,
          class ARGS_09>
inline
void optional<TYPE, false>::emplace(
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_01) args_01,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_02) args_02,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_03) args_03,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_04) args_04,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_05) args_05,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_06) args_06,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_07) args_07,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_08) args_08,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_09) args_09)
{
    d_value.emplace(NULL, BSLS_COMPILERFEATURES_FORWARD(ARGS_01, args_01),
                          BSLS_COMPILERFEATURES_FORWARD(ARGS_02, args_02),
                          BSLS_COMPILERFEATURES_FORWARD(ARGS_03, args_03),
                          BSLS_COMPILERFEATURES_FORWARD(ARGS_04, args_04),
                          BSLS_COMPILERFEATURES_FORWARD(ARGS_05, args_05),
                          BSLS_COMPILERFEATURES_FORWARD(ARGS_06, args_06),
                          BSLS_COMPILERFEATURES_FORWARD(ARGS_07, args_07),
                          BSLS_COMPILERFEATURES_FORWARD(ARGS_08, args_08),
                          BSLS_COMPILERFEATURES_FORWARD(ARGS_09, args_09));
}
#endif  // BSLSTL_OPTIONAL_VARIADIC_LIMIT_N >= 9

#if BSLSTL_OPTIONAL_VARIADIC_LIMIT_N >= 10
template <class TYPE>
template <class ARGS_01,
          class ARGS_02,
          class ARGS_03,
          class ARGS_04,
          class ARGS_05,
          class ARGS_06,
          class ARGS_07,
          class ARGS_08,
          class ARGS_09,
          class ARGS_10>
inline
void optional<TYPE, false>::emplace(
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_01) args_01,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_02) args_02,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_03) args_03,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_04) args_04,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_05) args_05,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_06) args_06,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_07) args_07,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_08) args_08,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_09) args_09,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_10) args_10)
{
    d_value.emplace(NULL, BSLS_COMPILERFEATURES_FORWARD(ARGS_01, args_01),
                          BSLS_COMPILERFEATURES_FORWARD(ARGS_02, args_02),
                          BSLS_COMPILERFEATURES_FORWARD(ARGS_03, args_03),
                          BSLS_COMPILERFEATURES_FORWARD(ARGS_04, args_04),
                          BSLS_COMPILERFEATURES_FORWARD(ARGS_05, args_05),
                          BSLS_COMPILERFEATURES_FORWARD(ARGS_06, args_06),
                          BSLS_COMPILERFEATURES_FORWARD(ARGS_07, args_07),
                          BSLS_COMPILERFEATURES_FORWARD(ARGS_08, args_08),
                          BSLS_COMPILERFEATURES_FORWARD(ARGS_09, args_09),
                          BSLS_COMPILERFEATURES_FORWARD(ARGS_10, args_10));
}
#endif  // BSLSTL_OPTIONAL_VARIADIC_LIMIT_N >= 10


#if defined(BSLS_COMPILERFEATURES_SUPPORT_GENERALIZED_INITIALIZERS)
#if BSLSTL_OPTIONAL_VARIADIC_LIMIT_N >= 0
template <class TYPE>
template<class IL_TYPE>
void optional<TYPE, false>::emplace(
                               std::initializer_list<IL_TYPE>             il)
{
    d_value.emplace(NULL,
                    il);
}
#endif  // BSLSTL_OPTIONAL_VARIADIC_LIMIT_N >= 0

#if BSLSTL_OPTIONAL_VARIADIC_LIMIT_N >= 1
template <class TYPE>
template<class IL_TYPE, class ARGS_01>
void optional<TYPE, false>::emplace(
                               std::initializer_list<IL_TYPE>             il,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_01) args_01)
{
    d_value.emplace(NULL,
                    il,
                    BSLS_COMPILERFEATURES_FORWARD(ARGS_01, args_01));
}
#endif  // BSLSTL_OPTIONAL_VARIADIC_LIMIT_N >= 1

#if BSLSTL_OPTIONAL_VARIADIC_LIMIT_N >= 2
template <class TYPE>
template<class IL_TYPE, class ARGS_01,
                        class ARGS_02>
void optional<TYPE, false>::emplace(
                               std::initializer_list<IL_TYPE>             il,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_01) args_01,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_02) args_02)
{
    d_value.emplace(NULL,
                    il,
                    BSLS_COMPILERFEATURES_FORWARD(ARGS_01, args_01),
                    BSLS_COMPILERFEATURES_FORWARD(ARGS_02, args_02));
}
#endif  // BSLSTL_OPTIONAL_VARIADIC_LIMIT_N >= 2

#if BSLSTL_OPTIONAL_VARIADIC_LIMIT_N >= 3
template <class TYPE>
template<class IL_TYPE, class ARGS_01,
                        class ARGS_02,
                        class ARGS_03>
void optional<TYPE, false>::emplace(
                               std::initializer_list<IL_TYPE>             il,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_01) args_01,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_02) args_02,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_03) args_03)
{
    d_value.emplace(NULL,
                    il,
                    BSLS_COMPILERFEATURES_FORWARD(ARGS_01, args_01),
                    BSLS_COMPILERFEATURES_FORWARD(ARGS_02, args_02),
                    BSLS_COMPILERFEATURES_FORWARD(ARGS_03, args_03));
}
#endif  // BSLSTL_OPTIONAL_VARIADIC_LIMIT_N >= 3

#if BSLSTL_OPTIONAL_VARIADIC_LIMIT_N >= 4
template <class TYPE>
template<class IL_TYPE, class ARGS_01,
                        class ARGS_02,
                        class ARGS_03,
                        class ARGS_04>
void optional<TYPE, false>::emplace(
                               std::initializer_list<IL_TYPE>             il,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_01) args_01,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_02) args_02,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_03) args_03,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_04) args_04)
{
    d_value.emplace(NULL,
                    il,
                    BSLS_COMPILERFEATURES_FORWARD(ARGS_01, args_01),
                    BSLS_COMPILERFEATURES_FORWARD(ARGS_02, args_02),
                    BSLS_COMPILERFEATURES_FORWARD(ARGS_03, args_03),
                    BSLS_COMPILERFEATURES_FORWARD(ARGS_04, args_04));
}
#endif  // BSLSTL_OPTIONAL_VARIADIC_LIMIT_N >= 4

#if BSLSTL_OPTIONAL_VARIADIC_LIMIT_N >= 5
template <class TYPE>
template<class IL_TYPE, class ARGS_01,
                        class ARGS_02,
                        class ARGS_03,
                        class ARGS_04,
                        class ARGS_05>
void optional<TYPE, false>::emplace(
                               std::initializer_list<IL_TYPE>             il,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_01) args_01,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_02) args_02,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_03) args_03,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_04) args_04,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_05) args_05)
{
    d_value.emplace(NULL,
                    il,
                    BSLS_COMPILERFEATURES_FORWARD(ARGS_01, args_01),
                    BSLS_COMPILERFEATURES_FORWARD(ARGS_02, args_02),
                    BSLS_COMPILERFEATURES_FORWARD(ARGS_03, args_03),
                    BSLS_COMPILERFEATURES_FORWARD(ARGS_04, args_04),
                    BSLS_COMPILERFEATURES_FORWARD(ARGS_05, args_05));
}
#endif  // BSLSTL_OPTIONAL_VARIADIC_LIMIT_N >= 5

#if BSLSTL_OPTIONAL_VARIADIC_LIMIT_N >= 6
template <class TYPE>
template<class IL_TYPE, class ARGS_01,
                        class ARGS_02,
                        class ARGS_03,
                        class ARGS_04,
                        class ARGS_05,
                        class ARGS_06>
void optional<TYPE, false>::emplace(
                               std::initializer_list<IL_TYPE>             il,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_01) args_01,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_02) args_02,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_03) args_03,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_04) args_04,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_05) args_05,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_06) args_06)
{
    d_value.emplace(NULL,
                    il,
                    BSLS_COMPILERFEATURES_FORWARD(ARGS_01, args_01),
                    BSLS_COMPILERFEATURES_FORWARD(ARGS_02, args_02),
                    BSLS_COMPILERFEATURES_FORWARD(ARGS_03, args_03),
                    BSLS_COMPILERFEATURES_FORWARD(ARGS_04, args_04),
                    BSLS_COMPILERFEATURES_FORWARD(ARGS_05, args_05),
                    BSLS_COMPILERFEATURES_FORWARD(ARGS_06, args_06));
}
#endif  // BSLSTL_OPTIONAL_VARIADIC_LIMIT_N >= 6

#if BSLSTL_OPTIONAL_VARIADIC_LIMIT_N >= 7
template <class TYPE>
template<class IL_TYPE, class ARGS_01,
                        class ARGS_02,
                        class ARGS_03,
                        class ARGS_04,
                        class ARGS_05,
                        class ARGS_06,
                        class ARGS_07>
void optional<TYPE, false>::emplace(
                               std::initializer_list<IL_TYPE>             il,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_01) args_01,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_02) args_02,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_03) args_03,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_04) args_04,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_05) args_05,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_06) args_06,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_07) args_07)
{
    d_value.emplace(NULL,
                    il,
                    BSLS_COMPILERFEATURES_FORWARD(ARGS_01, args_01),
                    BSLS_COMPILERFEATURES_FORWARD(ARGS_02, args_02),
                    BSLS_COMPILERFEATURES_FORWARD(ARGS_03, args_03),
                    BSLS_COMPILERFEATURES_FORWARD(ARGS_04, args_04),
                    BSLS_COMPILERFEATURES_FORWARD(ARGS_05, args_05),
                    BSLS_COMPILERFEATURES_FORWARD(ARGS_06, args_06),
                    BSLS_COMPILERFEATURES_FORWARD(ARGS_07, args_07));
}
#endif  // BSLSTL_OPTIONAL_VARIADIC_LIMIT_N >= 7

#if BSLSTL_OPTIONAL_VARIADIC_LIMIT_N >= 8
template <class TYPE>
template<class IL_TYPE, class ARGS_01,
                        class ARGS_02,
                        class ARGS_03,
                        class ARGS_04,
                        class ARGS_05,
                        class ARGS_06,
                        class ARGS_07,
                        class ARGS_08>
void optional<TYPE, false>::emplace(
                               std::initializer_list<IL_TYPE>             il,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_01) args_01,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_02) args_02,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_03) args_03,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_04) args_04,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_05) args_05,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_06) args_06,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_07) args_07,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_08) args_08)
{
    d_value.emplace(NULL,
                    il,
                    BSLS_COMPILERFEATURES_FORWARD(ARGS_01, args_01),
                    BSLS_COMPILERFEATURES_FORWARD(ARGS_02, args_02),
                    BSLS_COMPILERFEATURES_FORWARD(ARGS_03, args_03),
                    BSLS_COMPILERFEATURES_FORWARD(ARGS_04, args_04),
                    BSLS_COMPILERFEATURES_FORWARD(ARGS_05, args_05),
                    BSLS_COMPILERFEATURES_FORWARD(ARGS_06, args_06),
                    BSLS_COMPILERFEATURES_FORWARD(ARGS_07, args_07),
                    BSLS_COMPILERFEATURES_FORWARD(ARGS_08, args_08));
}
#endif  // BSLSTL_OPTIONAL_VARIADIC_LIMIT_N >= 8

#if BSLSTL_OPTIONAL_VARIADIC_LIMIT_N >= 9
template <class TYPE>
template<class IL_TYPE, class ARGS_01,
                        class ARGS_02,
                        class ARGS_03,
                        class ARGS_04,
                        class ARGS_05,
                        class ARGS_06,
                        class ARGS_07,
                        class ARGS_08,
                        class ARGS_09>
void optional<TYPE, false>::emplace(
                               std::initializer_list<IL_TYPE>             il,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_01) args_01,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_02) args_02,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_03) args_03,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_04) args_04,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_05) args_05,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_06) args_06,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_07) args_07,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_08) args_08,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_09) args_09)
{
    d_value.emplace(NULL,
                    il,
                    BSLS_COMPILERFEATURES_FORWARD(ARGS_01, args_01),
                    BSLS_COMPILERFEATURES_FORWARD(ARGS_02, args_02),
                    BSLS_COMPILERFEATURES_FORWARD(ARGS_03, args_03),
                    BSLS_COMPILERFEATURES_FORWARD(ARGS_04, args_04),
                    BSLS_COMPILERFEATURES_FORWARD(ARGS_05, args_05),
                    BSLS_COMPILERFEATURES_FORWARD(ARGS_06, args_06),
                    BSLS_COMPILERFEATURES_FORWARD(ARGS_07, args_07),
                    BSLS_COMPILERFEATURES_FORWARD(ARGS_08, args_08),
                    BSLS_COMPILERFEATURES_FORWARD(ARGS_09, args_09));
}
#endif  // BSLSTL_OPTIONAL_VARIADIC_LIMIT_N >= 9

#if BSLSTL_OPTIONAL_VARIADIC_LIMIT_N >= 10
template <class TYPE>
template<class IL_TYPE, class ARGS_01,
                        class ARGS_02,
                        class ARGS_03,
                        class ARGS_04,
                        class ARGS_05,
                        class ARGS_06,
                        class ARGS_07,
                        class ARGS_08,
                        class ARGS_09,
                        class ARGS_10>
void optional<TYPE, false>::emplace(
                               std::initializer_list<IL_TYPE>             il,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_01) args_01,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_02) args_02,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_03) args_03,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_04) args_04,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_05) args_05,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_06) args_06,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_07) args_07,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_08) args_08,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_09) args_09,
                            BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_10) args_10)
{
    d_value.emplace(NULL,
                    il,
                    BSLS_COMPILERFEATURES_FORWARD(ARGS_01, args_01),
                    BSLS_COMPILERFEATURES_FORWARD(ARGS_02, args_02),
                    BSLS_COMPILERFEATURES_FORWARD(ARGS_03, args_03),
                    BSLS_COMPILERFEATURES_FORWARD(ARGS_04, args_04),
                    BSLS_COMPILERFEATURES_FORWARD(ARGS_05, args_05),
                    BSLS_COMPILERFEATURES_FORWARD(ARGS_06, args_06),
                    BSLS_COMPILERFEATURES_FORWARD(ARGS_07, args_07),
                    BSLS_COMPILERFEATURES_FORWARD(ARGS_08, args_08),
                    BSLS_COMPILERFEATURES_FORWARD(ARGS_09, args_09),
                    BSLS_COMPILERFEATURES_FORWARD(ARGS_10, args_10));
}
#endif  // BSLSTL_OPTIONAL_VARIADIC_LIMIT_N >= 10

#endif
#else
// The generated code below is a workaround for the absence of perfect
// forwarding in some compilers.
template <class TYPE>
template <class... ARGS>
inline
void optional<TYPE, false>::emplace(
                               BSLS_COMPILERFEATURES_FORWARD_REF(ARGS)... args)
{
    d_value.emplace(NULL, BSLS_COMPILERFEATURES_FORWARD(ARGS, args)...);
}

#if defined(BSLS_COMPILERFEATURES_SUPPORT_GENERALIZED_INITIALIZERS)
template <class TYPE>
template<class IL_TYPE, class... ARGS>
void optional<TYPE, false>::emplace(
                               std::initializer_list<IL_TYPE>             il,
                               BSLS_COMPILERFEATURES_FORWARD_REF(ARGS)... args)
{
    d_value.emplace(NULL,
                    il,
                    BSLS_COMPILERFEATURES_FORWARD(ARGS, args)...);
}
#endif
// }}} END GENERATED CODE
#endif

template <class TYPE>
inline
void optional<TYPE, false>::reset() BSLS_KEYWORD_NOEXCEPT
{
    d_value.reset();
}

//OBSERVERS
template <class TYPE>
inline
TYPE& optional<TYPE, false>::value() BSLS_KEYWORD_LVREF_QUAL
{
    if (!has_value()) BSLS_THROW(bsl::bad_optional_access());

    return d_value.value();
}

template <class TYPE>
inline
const TYPE& optional<TYPE, false>::value() const BSLS_KEYWORD_LVREF_QUAL
{
    if (!has_value()) BSLS_THROW(bsl::bad_optional_access());

    return d_value.value();
}

#if defined(BSLS_COMPILERFEATURES_SUPPORT_REF_QUALIFIERS)
template <class TYPE>
inline
TYPE&& optional<TYPE, false>::value() BSLS_KEYWORD_RVREF_QUAL
{
    if (!has_value()) BSLS_THROW(bsl::bad_optional_access());

    return std::move(d_value.value());
}

template <class TYPE>
inline
const TYPE&& optional<TYPE, false>::value() const BSLS_KEYWORD_RVREF_QUAL
{
    if (!has_value()) BSLS_THROW(bsl::bad_optional_access());

    return std::move(d_value.value());
}
#endif

template <class TYPE>
template <class ANY_TYPE>
inline
TYPE
optional<TYPE, false>::value_or(
                               BSLS_COMPILERFEATURES_FORWARD_REF(ANY_TYPE) rhs)
const BSLS_KEYWORD_LVREF_QUAL
{
    if (this->has_value()) {
        return TYPE(this->value());                                   // RETURN
    }
    else {
        return TYPE(BSLS_COMPILERFEATURES_FORWARD(ANY_TYPE, rhs));    // RETURN
    }
}

#if defined(BSLS_COMPILERFEATURES_SUPPORT_REF_QUALIFIERS)
template <class TYPE>
template <class ANY_TYPE>
inline
TYPE
optional<TYPE, false>::value_or(ANY_TYPE&& rhs) BSLS_KEYWORD_RVREF_QUAL
{
    if (has_value()) {
        return TYPE(std::move(this->value()));                        // RETURN
    }
    else {
        return TYPE(std::forward<ANY_TYPE>(rhs));                     // RETURN
    }
}
#endif

template <class TYPE>
inline
optional<TYPE, false>&
optional<TYPE, false>::operator=(bsl::nullopt_t) BSLS_KEYWORD_NOEXCEPT
{
    this->reset();
    return *this;
}

template <class TYPE>
inline
optional<TYPE, false>&
optional<TYPE, false>::operator=(const optional &rhs)
{
    if (rhs.has_value()) {
        if (this->has_value()) {
            this->value() = rhs.value();
        }
        else {
            this->emplace(rhs.value());
        }
    }
    else {
        this->reset();
    }
    return *this;
}

template <class TYPE>
inline
optional<TYPE, false>&
optional<TYPE, false>::operator=(BloombergLP::bslmf::MovableRef<optional> rhs)
{
    optional& lvalue = rhs;
    if (lvalue.has_value()) {
        if (this->has_value()) {
            this->value() = MoveUtil::move(lvalue.value());
        }
        else {
            this->emplace(MoveUtil::move(lvalue.value()));
        }
    }
    else {
        this->reset();
    }
    return *this;
}

#ifndef BSLS_COMPILERFEATURES_SUPPORT_RVALUE_REFERENCES
template <class TYPE>
inline
optional<TYPE, false>&
optional<TYPE, false>::operator=(const TYPE& rhs)
{
    if (this->has_value()) {
        this->value() = rhs;
    }
    else {
        this->emplace(rhs);
    }
    return *this;
}

template <class TYPE>
inline
optional<TYPE, false>&
optional<TYPE, false>::operator=(BloombergLP::bslmf::MovableRef<TYPE> rhs)
{
    TYPE& lvalue = rhs;

    if (this->has_value()) {
        this->value() = MoveUtil::move(lvalue);
    }
    else {
        this->emplace(MoveUtil::move(lvalue));
    }
    return *this;
}

template <class TYPE>
template <class ANY_TYPE>
inline
optional<TYPE, false>&
optional<TYPE, false>::operator=(const ANY_TYPE& rhs)
{
    // Must be in-place inline because the use of 'enable_if' will
    // otherwise break the MSVC 2010 compiler.
    if (this->has_value()) {
        this->value() = rhs;
    }
    else {
        this->emplace(rhs);
    }
    return *this;
}

template <class TYPE>
template <class ANY_TYPE>
inline
optional<TYPE, false>&
optional<TYPE, false>::operator=(BloombergLP::bslmf::MovableRef<ANY_TYPE> rhs)
{
    // Must be in-place inline because the use of 'enable_if' will
    // otherwise break the MSVC 2010 compiler.
    ANY_TYPE& lvalue = rhs;
    if (this->has_value()) {
        this->value() = MoveUtil::move(lvalue);
    }
    else {
        this->emplace(MoveUtil::move(lvalue));
    }
    return *this;
}
#endif //BSLS_COMPILERFEATURES_SUPPORT_RVALUE_REFERENCES

template <class TYPE>
inline
TYPE& optional<TYPE, false>::operator*() BSLS_KEYWORD_LVREF_QUAL
{
    BSLS_ASSERT(has_value());

    return d_value.value();
}

template <class TYPE>
inline
const TYPE& optional<TYPE, false>::operator*() const BSLS_KEYWORD_LVREF_QUAL
{
    BSLS_ASSERT(has_value());

    return d_value.value();
}

template <class TYPE>
inline
TYPE* optional<TYPE, false>::operator->()
{
    BSLS_ASSERT(has_value());

    return BSLS_UTIL_ADDRESSOF(d_value.value());
}

template <class TYPE>
inline
const TYPE* optional<TYPE, false>::operator->() const
{
    BSLS_ASSERT(has_value());

    return BSLS_UTIL_ADDRESSOF(d_value.value());
}

template <class TYPE>
inline
bool optional<TYPE, false>::has_value() const BSLS_KEYWORD_NOEXCEPT
{
    return d_value.hasValue();
}

#ifdef BSLS_COMPILERFEATURES_SUPPORT_OPERATOR_EXPLICIT
template <class TYPE>
optional<TYPE, false>::operator bool() const BSLS_KEYWORD_NOEXCEPT
{
    return this->has_value();
}
#endif //#ifdef BSLS_COMPILERFEATURES_SUPPORT_OPERATOR_EXPLICIT

template <class TYPE>
void optional<TYPE, false>::swap(optional &other)
{
    if (this->has_value() && other.has_value()) {
        BloombergLP::bslalg::SwapUtil::swap(&(this->value()),
                                            &(other.value()));
    }
    else if (this->has_value()) {
        other.emplace(MoveUtil::move(this->value()));
        this->reset();
    }
    else if (other.has_value()) {
        this->emplace(MoveUtil::move(other.value()));
        other.reset();
    }
}
#endif // __cpp_lib_optional
// FREE FUNCTIONS

template <class HASHALG, class TYPE>
void hashAppend(HASHALG& hashAlg, const optional<TYPE>& input)
{
    if (input.has_value()) {
        hashAppend(hashAlg, true);
        hashAppend(hashAlg, input.value());
    }
    else {
        hashAppend(hashAlg, false);
    }
}

template<class TYPE>
inline
void swap(bsl::optional<TYPE>& lhs, optional<TYPE>& rhs)
{
    lhs.swap(rhs);
}

template <class LHS_TYPE, class RHS_TYPE>
inline
bool operator==(const bsl::optional<LHS_TYPE>& lhs,
                const bsl::optional<RHS_TYPE>& rhs)
{
    if (lhs.has_value() && rhs.has_value()) {
        return lhs.value() == rhs.value();                            // RETURN
    }
    return lhs.has_value() == rhs.has_value();
}

template <class LHS_TYPE, class RHS_TYPE>
inline
bool operator!=(const bsl::optional<LHS_TYPE>& lhs,
                const bsl::optional<RHS_TYPE>& rhs)
{
    if (lhs.has_value() && rhs.has_value()) {
        return lhs.value() != rhs.value();                            // RETURN
    }

    return lhs.has_value() != rhs.has_value();
}

template <class LHS_TYPE, class RHS_TYPE>
inline
bool operator==(const bsl::optional<LHS_TYPE>& lhs,
                const RHS_TYPE&                rhs)
{
    return lhs.has_value() && lhs.value() == rhs;
}

template <class LHS_TYPE, class RHS_TYPE>
inline
bool operator==(const LHS_TYPE&                lhs,
                const bsl::optional<RHS_TYPE>& rhs)
{
    return rhs.has_value() && rhs.value() == lhs;
}

template <class LHS_TYPE, class RHS_TYPE>
inline
bool operator!=(const bsl::optional<LHS_TYPE>& lhs,
                const RHS_TYPE&                rhs)
{
    return !lhs.has_value() || lhs.value() != rhs;
}

template <class LHS_TYPE, class RHS_TYPE>
inline
bool operator!=(const LHS_TYPE&                lhs,
                const bsl::optional<RHS_TYPE>& rhs)
{
    return !rhs.has_value() || rhs.value() != lhs;
}

template <class LHS_TYPE, class RHS_TYPE>
inline
bool operator<(const bsl::optional<LHS_TYPE>& lhs,
               const bsl::optional<RHS_TYPE>& rhs)
{
    if (!rhs.has_value()) {
        return false;                                                 // RETURN
    }

    return !lhs.has_value() || lhs.value() < rhs.value();
}

template <class LHS_TYPE, class RHS_TYPE>
inline
bool operator<(const bsl::optional<LHS_TYPE>& lhs,
               const RHS_TYPE&                rhs)
{
    return !lhs.has_value() || lhs.value() < rhs;
}

template <class LHS_TYPE, class RHS_TYPE>
inline
bool operator<(const LHS_TYPE&                lhs,
               const bsl::optional<RHS_TYPE>& rhs)
{
    return rhs.has_value() && lhs < rhs.value();
}

template <class LHS_TYPE, class RHS_TYPE>
inline
bool operator>(const bsl::optional<LHS_TYPE>& lhs,
               const bsl::optional<RHS_TYPE>& rhs)
{
    return rhs < lhs;
}

template <class LHS_TYPE, class RHS_TYPE>
inline
bool operator>(const bsl::optional<LHS_TYPE>& lhs,
               const RHS_TYPE&                rhs)
{
    return rhs < lhs;
}

template <class LHS_TYPE, class RHS_TYPE>
inline
bool operator>(const LHS_TYPE&                lhs,
               const bsl::optional<RHS_TYPE>& rhs)
{
    return rhs < lhs;
}

template <class LHS_TYPE, class RHS_TYPE>
inline
bool operator<=(const bsl::optional<LHS_TYPE>& lhs,
                const bsl::optional<RHS_TYPE>& rhs)
{
    return !(rhs < lhs);
}

template <class LHS_TYPE, class RHS_TYPE>
inline
bool operator<=(const bsl::optional<LHS_TYPE>& lhs,
                const RHS_TYPE&                rhs)
{
    return !(rhs < lhs);
}

template <class LHS_TYPE, class RHS_TYPE>
inline
bool operator<=(const LHS_TYPE&                lhs,
                const bsl::optional<RHS_TYPE>& rhs)
{
    return !(rhs < lhs);
}

template <class LHS_TYPE, class RHS_TYPE>
inline
bool operator>=(const bsl::optional<LHS_TYPE>& lhs,
                const bsl::optional<RHS_TYPE>& rhs)
{
    return !(lhs < rhs);
}

template <class LHS_TYPE, class RHS_TYPE>
inline
bool operator>=(const bsl::optional<LHS_TYPE>& lhs,
                const RHS_TYPE&                rhs)
{
    return !(lhs < rhs);
}

template <class LHS_TYPE, class RHS_TYPE>
inline
bool operator>=(const LHS_TYPE&                lhs,
                const bsl::optional<RHS_TYPE>& rhs)
{
    return !(lhs < rhs);
}

template <class TYPE>
inline BSLS_KEYWORD_CONSTEXPR
bool operator==(const bsl::optional<TYPE>& value,
                const bsl::nullopt_t&) BSLS_KEYWORD_NOEXCEPT
{
    return !value.has_value();
}

template <class TYPE>
inline BSLS_KEYWORD_CONSTEXPR
bool operator==(const bsl::nullopt_t&,
                const bsl::optional<TYPE>& value) BSLS_KEYWORD_NOEXCEPT
{
    return !value.has_value();
}

template <class TYPE>
inline BSLS_KEYWORD_CONSTEXPR
bool operator!=(const bsl::optional<TYPE>& value,
                const bsl::nullopt_t& ) BSLS_KEYWORD_NOEXCEPT
{
    return value.has_value();
}

template <class TYPE>
inline BSLS_KEYWORD_CONSTEXPR
bool operator!=(const bsl::nullopt_t&,
                const bsl::optional<TYPE>& value) BSLS_KEYWORD_NOEXCEPT
{
    return value.has_value();
}

template <class TYPE>
inline BSLS_KEYWORD_CONSTEXPR
bool operator<(const bsl::optional<TYPE>&,
               const bsl::nullopt_t&) BSLS_KEYWORD_NOEXCEPT
{
    return false;
}

template <class TYPE>
inline BSLS_KEYWORD_CONSTEXPR
bool operator<(const bsl::nullopt_t&,
               const bsl::optional<TYPE>& value) BSLS_KEYWORD_NOEXCEPT
{
    return value.has_value();
}

template <class TYPE>
inline BSLS_KEYWORD_CONSTEXPR
bool operator>(const bsl::optional<TYPE>& value,
               const bsl::nullopt_t& ) BSLS_KEYWORD_NOEXCEPT
{
    return value.has_value();
}

template <class TYPE>
inline BSLS_KEYWORD_CONSTEXPR
bool operator>(const bsl::nullopt_t&,
               const bsl::optional<TYPE>& ) BSLS_KEYWORD_NOEXCEPT
{
    return false;
}

template <class TYPE>
inline BSLS_KEYWORD_CONSTEXPR
bool operator<=(const bsl::optional<TYPE>& value,
                const bsl::nullopt_t& ) BSLS_KEYWORD_NOEXCEPT
{
    return !value.has_value();
}

template <class TYPE>
inline BSLS_KEYWORD_CONSTEXPR
bool operator<=(const bsl::nullopt_t&,
                const bsl::optional<TYPE>& ) BSLS_KEYWORD_NOEXCEPT
{
    return true;
}

template <class TYPE>
inline BSLS_KEYWORD_CONSTEXPR
bool operator>=(const bsl::optional<TYPE>& ,
                const bsl::nullopt_t& ) BSLS_KEYWORD_NOEXCEPT
{
    return true;
}

template <class TYPE>
inline BSLS_KEYWORD_CONSTEXPR
bool operator>=(const bsl::nullopt_t&,
                const bsl::optional<TYPE>& value) BSLS_KEYWORD_NOEXCEPT
{
    return !value.has_value();
}

template<class TYPE>
BSLS_KEYWORD_CONSTEXPR
bsl::optional<typename bsl::decay<TYPE>::type >
alloc_optional(typename bsl::optional<typename bsl::decay<TYPE>::type
                                >::allocator_type const& alloc,
               BSLS_COMPILERFEATURES_FORWARD_REF(TYPE)   rhs)
{
    return bsl::optional<typename bsl::decay<TYPE>::type >(
                                     bsl::allocator_arg,
                                     alloc,
                                     bsl::in_place,
                                     BSLS_COMPILERFEATURES_FORWARD(TYPE, rhs));
}

#if !BSLS_COMPILERFEATURES_SIMULATE_CPP11_FEATURES
template<class TYPE, class... ARGS>
BSLS_KEYWORD_CONSTEXPR
bsl::optional<TYPE>
alloc_optional(typename bsl::optional<TYPE>::allocator_type const& alloc,
               BSLS_COMPILERFEATURES_FORWARD_REF(ARGS)...          args)
{
    return bsl::optional<TYPE>(bsl::allocator_arg,
                               alloc,
                               bsl::in_place,
                               BSLS_COMPILERFEATURES_FORWARD(ARGS, args)...);
}
#elif BSLS_COMPILERFEATURES_SIMULATE_VARIADIC_TEMPLATES
// {{{ BEGIN GENERATED CODE
// The following section is automatically generated.  **DO NOT EDIT**
// Generator command line: sim_cpp11_features.pl bslstl_optional.h
#ifndef BSLSTL_OPTIONAL_VARIADIC_LIMIT
#define BSLSTL_OPTIONAL_VARIADIC_LIMIT 10
#endif
#ifndef BSLSTL_OPTIONAL_VARIADIC_LIMIT_O
#define BSLSTL_OPTIONAL_VARIADIC_LIMIT_O BSLSTL_OPTIONAL_VARIADIC_LIMIT
#endif
#if BSLSTL_OPTIONAL_VARIADIC_LIMIT_O >= 0
template<class TYPE>
BSLS_KEYWORD_CONSTEXPR
bsl::optional<TYPE>
alloc_optional(typename bsl::optional<TYPE>::allocator_type const& alloc)
{
    return bsl::optional<TYPE>(bsl::allocator_arg,
                               alloc,
                               bsl::in_place);
}
#endif  // BSLSTL_OPTIONAL_VARIADIC_LIMIT_O >= 0

#if BSLSTL_OPTIONAL_VARIADIC_LIMIT_O >= 1
template<class TYPE, class ARGS_01>
BSLS_KEYWORD_CONSTEXPR
bsl::optional<TYPE>
alloc_optional(typename bsl::optional<TYPE>::allocator_type const& alloc,
               BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_01) args_01)
{
    return bsl::optional<TYPE>(bsl::allocator_arg,
                               alloc,
                               bsl::in_place,
                              BSLS_COMPILERFEATURES_FORWARD(ARGS_01, args_01));
}
#endif  // BSLSTL_OPTIONAL_VARIADIC_LIMIT_O >= 1

#if BSLSTL_OPTIONAL_VARIADIC_LIMIT_O >= 2
template<class TYPE, class ARGS_01,
                     class ARGS_02>
BSLS_KEYWORD_CONSTEXPR
bsl::optional<TYPE>
alloc_optional(typename bsl::optional<TYPE>::allocator_type const& alloc,
               BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_01) args_01,
               BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_02) args_02)
{
    return bsl::optional<TYPE>(bsl::allocator_arg,
                               alloc,
                               bsl::in_place,
                              BSLS_COMPILERFEATURES_FORWARD(ARGS_01, args_01),
                              BSLS_COMPILERFEATURES_FORWARD(ARGS_02, args_02));
}
#endif  // BSLSTL_OPTIONAL_VARIADIC_LIMIT_O >= 2

#if BSLSTL_OPTIONAL_VARIADIC_LIMIT_O >= 3
template<class TYPE, class ARGS_01,
                     class ARGS_02,
                     class ARGS_03>
BSLS_KEYWORD_CONSTEXPR
bsl::optional<TYPE>
alloc_optional(typename bsl::optional<TYPE>::allocator_type const& alloc,
               BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_01) args_01,
               BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_02) args_02,
               BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_03) args_03)
{
    return bsl::optional<TYPE>(bsl::allocator_arg,
                               alloc,
                               bsl::in_place,
                              BSLS_COMPILERFEATURES_FORWARD(ARGS_01, args_01),
                              BSLS_COMPILERFEATURES_FORWARD(ARGS_02, args_02),
                              BSLS_COMPILERFEATURES_FORWARD(ARGS_03, args_03));
}
#endif  // BSLSTL_OPTIONAL_VARIADIC_LIMIT_O >= 3

#if BSLSTL_OPTIONAL_VARIADIC_LIMIT_O >= 4
template<class TYPE, class ARGS_01,
                     class ARGS_02,
                     class ARGS_03,
                     class ARGS_04>
BSLS_KEYWORD_CONSTEXPR
bsl::optional<TYPE>
alloc_optional(typename bsl::optional<TYPE>::allocator_type const& alloc,
               BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_01) args_01,
               BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_02) args_02,
               BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_03) args_03,
               BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_04) args_04)
{
    return bsl::optional<TYPE>(bsl::allocator_arg,
                               alloc,
                               bsl::in_place,
                              BSLS_COMPILERFEATURES_FORWARD(ARGS_01, args_01),
                              BSLS_COMPILERFEATURES_FORWARD(ARGS_02, args_02),
                              BSLS_COMPILERFEATURES_FORWARD(ARGS_03, args_03),
                              BSLS_COMPILERFEATURES_FORWARD(ARGS_04, args_04));
}
#endif  // BSLSTL_OPTIONAL_VARIADIC_LIMIT_O >= 4

#if BSLSTL_OPTIONAL_VARIADIC_LIMIT_O >= 5
template<class TYPE, class ARGS_01,
                     class ARGS_02,
                     class ARGS_03,
                     class ARGS_04,
                     class ARGS_05>
BSLS_KEYWORD_CONSTEXPR
bsl::optional<TYPE>
alloc_optional(typename bsl::optional<TYPE>::allocator_type const& alloc,
               BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_01) args_01,
               BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_02) args_02,
               BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_03) args_03,
               BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_04) args_04,
               BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_05) args_05)
{
    return bsl::optional<TYPE>(bsl::allocator_arg,
                               alloc,
                               bsl::in_place,
                              BSLS_COMPILERFEATURES_FORWARD(ARGS_01, args_01),
                              BSLS_COMPILERFEATURES_FORWARD(ARGS_02, args_02),
                              BSLS_COMPILERFEATURES_FORWARD(ARGS_03, args_03),
                              BSLS_COMPILERFEATURES_FORWARD(ARGS_04, args_04),
                              BSLS_COMPILERFEATURES_FORWARD(ARGS_05, args_05));
}
#endif  // BSLSTL_OPTIONAL_VARIADIC_LIMIT_O >= 5

#if BSLSTL_OPTIONAL_VARIADIC_LIMIT_O >= 6
template<class TYPE, class ARGS_01,
                     class ARGS_02,
                     class ARGS_03,
                     class ARGS_04,
                     class ARGS_05,
                     class ARGS_06>
BSLS_KEYWORD_CONSTEXPR
bsl::optional<TYPE>
alloc_optional(typename bsl::optional<TYPE>::allocator_type const& alloc,
               BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_01) args_01,
               BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_02) args_02,
               BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_03) args_03,
               BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_04) args_04,
               BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_05) args_05,
               BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_06) args_06)
{
    return bsl::optional<TYPE>(bsl::allocator_arg,
                               alloc,
                               bsl::in_place,
                              BSLS_COMPILERFEATURES_FORWARD(ARGS_01, args_01),
                              BSLS_COMPILERFEATURES_FORWARD(ARGS_02, args_02),
                              BSLS_COMPILERFEATURES_FORWARD(ARGS_03, args_03),
                              BSLS_COMPILERFEATURES_FORWARD(ARGS_04, args_04),
                              BSLS_COMPILERFEATURES_FORWARD(ARGS_05, args_05),
                              BSLS_COMPILERFEATURES_FORWARD(ARGS_06, args_06));
}
#endif  // BSLSTL_OPTIONAL_VARIADIC_LIMIT_O >= 6

#if BSLSTL_OPTIONAL_VARIADIC_LIMIT_O >= 7
template<class TYPE, class ARGS_01,
                     class ARGS_02,
                     class ARGS_03,
                     class ARGS_04,
                     class ARGS_05,
                     class ARGS_06,
                     class ARGS_07>
BSLS_KEYWORD_CONSTEXPR
bsl::optional<TYPE>
alloc_optional(typename bsl::optional<TYPE>::allocator_type const& alloc,
               BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_01) args_01,
               BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_02) args_02,
               BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_03) args_03,
               BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_04) args_04,
               BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_05) args_05,
               BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_06) args_06,
               BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_07) args_07)
{
    return bsl::optional<TYPE>(bsl::allocator_arg,
                               alloc,
                               bsl::in_place,
                              BSLS_COMPILERFEATURES_FORWARD(ARGS_01, args_01),
                              BSLS_COMPILERFEATURES_FORWARD(ARGS_02, args_02),
                              BSLS_COMPILERFEATURES_FORWARD(ARGS_03, args_03),
                              BSLS_COMPILERFEATURES_FORWARD(ARGS_04, args_04),
                              BSLS_COMPILERFEATURES_FORWARD(ARGS_05, args_05),
                              BSLS_COMPILERFEATURES_FORWARD(ARGS_06, args_06),
                              BSLS_COMPILERFEATURES_FORWARD(ARGS_07, args_07));
}
#endif  // BSLSTL_OPTIONAL_VARIADIC_LIMIT_O >= 7

#if BSLSTL_OPTIONAL_VARIADIC_LIMIT_O >= 8
template<class TYPE, class ARGS_01,
                     class ARGS_02,
                     class ARGS_03,
                     class ARGS_04,
                     class ARGS_05,
                     class ARGS_06,
                     class ARGS_07,
                     class ARGS_08>
BSLS_KEYWORD_CONSTEXPR
bsl::optional<TYPE>
alloc_optional(typename bsl::optional<TYPE>::allocator_type const& alloc,
               BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_01) args_01,
               BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_02) args_02,
               BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_03) args_03,
               BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_04) args_04,
               BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_05) args_05,
               BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_06) args_06,
               BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_07) args_07,
               BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_08) args_08)
{
    return bsl::optional<TYPE>(bsl::allocator_arg,
                               alloc,
                               bsl::in_place,
                              BSLS_COMPILERFEATURES_FORWARD(ARGS_01, args_01),
                              BSLS_COMPILERFEATURES_FORWARD(ARGS_02, args_02),
                              BSLS_COMPILERFEATURES_FORWARD(ARGS_03, args_03),
                              BSLS_COMPILERFEATURES_FORWARD(ARGS_04, args_04),
                              BSLS_COMPILERFEATURES_FORWARD(ARGS_05, args_05),
                              BSLS_COMPILERFEATURES_FORWARD(ARGS_06, args_06),
                              BSLS_COMPILERFEATURES_FORWARD(ARGS_07, args_07),
                              BSLS_COMPILERFEATURES_FORWARD(ARGS_08, args_08));
}
#endif  // BSLSTL_OPTIONAL_VARIADIC_LIMIT_O >= 8

#if BSLSTL_OPTIONAL_VARIADIC_LIMIT_O >= 9
template<class TYPE, class ARGS_01,
                     class ARGS_02,
                     class ARGS_03,
                     class ARGS_04,
                     class ARGS_05,
                     class ARGS_06,
                     class ARGS_07,
                     class ARGS_08,
                     class ARGS_09>
BSLS_KEYWORD_CONSTEXPR
bsl::optional<TYPE>
alloc_optional(typename bsl::optional<TYPE>::allocator_type const& alloc,
               BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_01) args_01,
               BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_02) args_02,
               BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_03) args_03,
               BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_04) args_04,
               BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_05) args_05,
               BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_06) args_06,
               BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_07) args_07,
               BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_08) args_08,
               BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_09) args_09)
{
    return bsl::optional<TYPE>(bsl::allocator_arg,
                               alloc,
                               bsl::in_place,
                              BSLS_COMPILERFEATURES_FORWARD(ARGS_01, args_01),
                              BSLS_COMPILERFEATURES_FORWARD(ARGS_02, args_02),
                              BSLS_COMPILERFEATURES_FORWARD(ARGS_03, args_03),
                              BSLS_COMPILERFEATURES_FORWARD(ARGS_04, args_04),
                              BSLS_COMPILERFEATURES_FORWARD(ARGS_05, args_05),
                              BSLS_COMPILERFEATURES_FORWARD(ARGS_06, args_06),
                              BSLS_COMPILERFEATURES_FORWARD(ARGS_07, args_07),
                              BSLS_COMPILERFEATURES_FORWARD(ARGS_08, args_08),
                              BSLS_COMPILERFEATURES_FORWARD(ARGS_09, args_09));
}
#endif  // BSLSTL_OPTIONAL_VARIADIC_LIMIT_O >= 9

#if BSLSTL_OPTIONAL_VARIADIC_LIMIT_O >= 10
template<class TYPE, class ARGS_01,
                     class ARGS_02,
                     class ARGS_03,
                     class ARGS_04,
                     class ARGS_05,
                     class ARGS_06,
                     class ARGS_07,
                     class ARGS_08,
                     class ARGS_09,
                     class ARGS_10>
BSLS_KEYWORD_CONSTEXPR
bsl::optional<TYPE>
alloc_optional(typename bsl::optional<TYPE>::allocator_type const& alloc,
               BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_01) args_01,
               BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_02) args_02,
               BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_03) args_03,
               BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_04) args_04,
               BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_05) args_05,
               BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_06) args_06,
               BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_07) args_07,
               BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_08) args_08,
               BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_09) args_09,
               BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_10) args_10)
{
    return bsl::optional<TYPE>(bsl::allocator_arg,
                               alloc,
                               bsl::in_place,
                              BSLS_COMPILERFEATURES_FORWARD(ARGS_01, args_01),
                              BSLS_COMPILERFEATURES_FORWARD(ARGS_02, args_02),
                              BSLS_COMPILERFEATURES_FORWARD(ARGS_03, args_03),
                              BSLS_COMPILERFEATURES_FORWARD(ARGS_04, args_04),
                              BSLS_COMPILERFEATURES_FORWARD(ARGS_05, args_05),
                              BSLS_COMPILERFEATURES_FORWARD(ARGS_06, args_06),
                              BSLS_COMPILERFEATURES_FORWARD(ARGS_07, args_07),
                              BSLS_COMPILERFEATURES_FORWARD(ARGS_08, args_08),
                              BSLS_COMPILERFEATURES_FORWARD(ARGS_09, args_09),
                              BSLS_COMPILERFEATURES_FORWARD(ARGS_10, args_10));
}
#endif  // BSLSTL_OPTIONAL_VARIADIC_LIMIT_O >= 10

#else
// The generated code below is a workaround for the absence of perfect
// forwarding in some compilers.
template<class TYPE, class... ARGS>
BSLS_KEYWORD_CONSTEXPR
bsl::optional<TYPE>
alloc_optional(typename bsl::optional<TYPE>::allocator_type const& alloc,
               BSLS_COMPILERFEATURES_FORWARD_REF(ARGS)...          args)
{
    return bsl::optional<TYPE>(bsl::allocator_arg,
                               alloc,
                               bsl::in_place,
                               BSLS_COMPILERFEATURES_FORWARD(ARGS, args)...);
}
// }}} END GENERATED CODE
#endif

#if defined(BSLS_COMPILERFEATURES_SUPPORT_GENERALIZED_INITIALIZERS)
template<class TYPE, class IL_TYPE, class... ARGS>
BSLS_KEYWORD_CONSTEXPR
bsl::optional<TYPE>
alloc_optional(typename bsl::optional<TYPE>::allocator_type const& alloc,
               std::initializer_list<IL_TYPE>                      il,
               BSLS_COMPILERFEATURES_FORWARD_REF(ARGS)...          args)
{
    return bsl::optional<TYPE>(bsl::allocator_arg,
                          alloc,
                          bsl::in_place,
                          il,
                          BSLS_COMPILERFEATURES_FORWARD(ARGS, args)...);
}
#endif // defined(BSLS_COMPILERFEATURES_SUPPORT_GENERALIZED_INITIALIZERS)

template<class TYPE>
BSLS_KEYWORD_CONSTEXPR
bsl::optional<typename bsl::decay<TYPE>::type >
make_optional(BSLS_COMPILERFEATURES_FORWARD_REF(TYPE) rhs)
{
    return bsl::optional<typename bsl::decay<TYPE>::type >(
                                     BSLS_COMPILERFEATURES_FORWARD(TYPE, rhs));
}

#if !BSLS_COMPILERFEATURES_SIMULATE_CPP11_FEATURES
template<class TYPE, class... ARGS>
BSLS_KEYWORD_CONSTEXPR
bsl::optional<TYPE>
make_optional(BSLS_COMPILERFEATURES_FORWARD_REF(ARGS)... args)
{
    return bsl::optional<TYPE>(bsl::in_place,
                          BSLS_COMPILERFEATURES_FORWARD(ARGS, args)...);
}

#if defined(BSLS_COMPILERFEATURES_SUPPORT_GENERALIZED_INITIALIZERS)
template<class TYPE, class IL_TYPE, class... ARGS>
BSLS_KEYWORD_CONSTEXPR
bsl::optional<TYPE>
make_optional(std::initializer_list<IL_TYPE>             il,
              BSLS_COMPILERFEATURES_FORWARD_REF(ARGS)... args)
{
    return bsl::optional<TYPE>(bsl::in_place, il,
                          BSLS_COMPILERFEATURES_FORWARD(ARGS, args)...);
}
#endif // defined(BSLS_COMPILERFEATURES_SUPPORT_GENERALIZED_INITIALIZERS)
#elif BSLS_COMPILERFEATURES_SIMULATE_VARIADIC_TEMPLATES
// {{{ BEGIN GENERATED CODE
// The following section is automatically generated.  **DO NOT EDIT**
// Generator command line: sim_cpp11_features.pl bslstl_optional.h
#ifndef BSLSTL_OPTIONAL_VARIADIC_LIMIT
#define BSLSTL_OPTIONAL_VARIADIC_LIMIT 10
#endif
#ifndef BSLSTL_OPTIONAL_VARIADIC_LIMIT_P
#define BSLSTL_OPTIONAL_VARIADIC_LIMIT_P BSLSTL_OPTIONAL_VARIADIC_LIMIT
#endif
#if BSLSTL_OPTIONAL_VARIADIC_LIMIT_P >= 0
template<class TYPE>
BSLS_KEYWORD_CONSTEXPR
bsl::optional<TYPE>
make_optional()
{
    return bsl::optional<TYPE>(bsl::in_place);
}
#endif  // BSLSTL_OPTIONAL_VARIADIC_LIMIT_P >= 0

#if BSLSTL_OPTIONAL_VARIADIC_LIMIT_P >= 1
template<class TYPE, class ARGS_01>
BSLS_KEYWORD_CONSTEXPR
bsl::optional<TYPE>
make_optional(BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_01) args_01)
{
    return bsl::optional<TYPE>(bsl::in_place,
                          BSLS_COMPILERFEATURES_FORWARD(ARGS_01, args_01));
}
#endif  // BSLSTL_OPTIONAL_VARIADIC_LIMIT_P >= 1

#if BSLSTL_OPTIONAL_VARIADIC_LIMIT_P >= 2
template<class TYPE, class ARGS_01,
                     class ARGS_02>
BSLS_KEYWORD_CONSTEXPR
bsl::optional<TYPE>
make_optional(BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_01) args_01,
              BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_02) args_02)
{
    return bsl::optional<TYPE>(bsl::in_place,
                          BSLS_COMPILERFEATURES_FORWARD(ARGS_01, args_01),
                          BSLS_COMPILERFEATURES_FORWARD(ARGS_02, args_02));
}
#endif  // BSLSTL_OPTIONAL_VARIADIC_LIMIT_P >= 2

#if BSLSTL_OPTIONAL_VARIADIC_LIMIT_P >= 3
template<class TYPE, class ARGS_01,
                     class ARGS_02,
                     class ARGS_03>
BSLS_KEYWORD_CONSTEXPR
bsl::optional<TYPE>
make_optional(BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_01) args_01,
              BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_02) args_02,
              BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_03) args_03)
{
    return bsl::optional<TYPE>(bsl::in_place,
                          BSLS_COMPILERFEATURES_FORWARD(ARGS_01, args_01),
                          BSLS_COMPILERFEATURES_FORWARD(ARGS_02, args_02),
                          BSLS_COMPILERFEATURES_FORWARD(ARGS_03, args_03));
}
#endif  // BSLSTL_OPTIONAL_VARIADIC_LIMIT_P >= 3

#if BSLSTL_OPTIONAL_VARIADIC_LIMIT_P >= 4
template<class TYPE, class ARGS_01,
                     class ARGS_02,
                     class ARGS_03,
                     class ARGS_04>
BSLS_KEYWORD_CONSTEXPR
bsl::optional<TYPE>
make_optional(BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_01) args_01,
              BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_02) args_02,
              BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_03) args_03,
              BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_04) args_04)
{
    return bsl::optional<TYPE>(bsl::in_place,
                          BSLS_COMPILERFEATURES_FORWARD(ARGS_01, args_01),
                          BSLS_COMPILERFEATURES_FORWARD(ARGS_02, args_02),
                          BSLS_COMPILERFEATURES_FORWARD(ARGS_03, args_03),
                          BSLS_COMPILERFEATURES_FORWARD(ARGS_04, args_04));
}
#endif  // BSLSTL_OPTIONAL_VARIADIC_LIMIT_P >= 4

#if BSLSTL_OPTIONAL_VARIADIC_LIMIT_P >= 5
template<class TYPE, class ARGS_01,
                     class ARGS_02,
                     class ARGS_03,
                     class ARGS_04,
                     class ARGS_05>
BSLS_KEYWORD_CONSTEXPR
bsl::optional<TYPE>
make_optional(BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_01) args_01,
              BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_02) args_02,
              BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_03) args_03,
              BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_04) args_04,
              BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_05) args_05)
{
    return bsl::optional<TYPE>(bsl::in_place,
                          BSLS_COMPILERFEATURES_FORWARD(ARGS_01, args_01),
                          BSLS_COMPILERFEATURES_FORWARD(ARGS_02, args_02),
                          BSLS_COMPILERFEATURES_FORWARD(ARGS_03, args_03),
                          BSLS_COMPILERFEATURES_FORWARD(ARGS_04, args_04),
                          BSLS_COMPILERFEATURES_FORWARD(ARGS_05, args_05));
}
#endif  // BSLSTL_OPTIONAL_VARIADIC_LIMIT_P >= 5

#if BSLSTL_OPTIONAL_VARIADIC_LIMIT_P >= 6
template<class TYPE, class ARGS_01,
                     class ARGS_02,
                     class ARGS_03,
                     class ARGS_04,
                     class ARGS_05,
                     class ARGS_06>
BSLS_KEYWORD_CONSTEXPR
bsl::optional<TYPE>
make_optional(BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_01) args_01,
              BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_02) args_02,
              BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_03) args_03,
              BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_04) args_04,
              BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_05) args_05,
              BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_06) args_06)
{
    return bsl::optional<TYPE>(bsl::in_place,
                          BSLS_COMPILERFEATURES_FORWARD(ARGS_01, args_01),
                          BSLS_COMPILERFEATURES_FORWARD(ARGS_02, args_02),
                          BSLS_COMPILERFEATURES_FORWARD(ARGS_03, args_03),
                          BSLS_COMPILERFEATURES_FORWARD(ARGS_04, args_04),
                          BSLS_COMPILERFEATURES_FORWARD(ARGS_05, args_05),
                          BSLS_COMPILERFEATURES_FORWARD(ARGS_06, args_06));
}
#endif  // BSLSTL_OPTIONAL_VARIADIC_LIMIT_P >= 6

#if BSLSTL_OPTIONAL_VARIADIC_LIMIT_P >= 7
template<class TYPE, class ARGS_01,
                     class ARGS_02,
                     class ARGS_03,
                     class ARGS_04,
                     class ARGS_05,
                     class ARGS_06,
                     class ARGS_07>
BSLS_KEYWORD_CONSTEXPR
bsl::optional<TYPE>
make_optional(BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_01) args_01,
              BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_02) args_02,
              BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_03) args_03,
              BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_04) args_04,
              BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_05) args_05,
              BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_06) args_06,
              BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_07) args_07)
{
    return bsl::optional<TYPE>(bsl::in_place,
                          BSLS_COMPILERFEATURES_FORWARD(ARGS_01, args_01),
                          BSLS_COMPILERFEATURES_FORWARD(ARGS_02, args_02),
                          BSLS_COMPILERFEATURES_FORWARD(ARGS_03, args_03),
                          BSLS_COMPILERFEATURES_FORWARD(ARGS_04, args_04),
                          BSLS_COMPILERFEATURES_FORWARD(ARGS_05, args_05),
                          BSLS_COMPILERFEATURES_FORWARD(ARGS_06, args_06),
                          BSLS_COMPILERFEATURES_FORWARD(ARGS_07, args_07));
}
#endif  // BSLSTL_OPTIONAL_VARIADIC_LIMIT_P >= 7

#if BSLSTL_OPTIONAL_VARIADIC_LIMIT_P >= 8
template<class TYPE, class ARGS_01,
                     class ARGS_02,
                     class ARGS_03,
                     class ARGS_04,
                     class ARGS_05,
                     class ARGS_06,
                     class ARGS_07,
                     class ARGS_08>
BSLS_KEYWORD_CONSTEXPR
bsl::optional<TYPE>
make_optional(BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_01) args_01,
              BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_02) args_02,
              BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_03) args_03,
              BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_04) args_04,
              BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_05) args_05,
              BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_06) args_06,
              BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_07) args_07,
              BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_08) args_08)
{
    return bsl::optional<TYPE>(bsl::in_place,
                          BSLS_COMPILERFEATURES_FORWARD(ARGS_01, args_01),
                          BSLS_COMPILERFEATURES_FORWARD(ARGS_02, args_02),
                          BSLS_COMPILERFEATURES_FORWARD(ARGS_03, args_03),
                          BSLS_COMPILERFEATURES_FORWARD(ARGS_04, args_04),
                          BSLS_COMPILERFEATURES_FORWARD(ARGS_05, args_05),
                          BSLS_COMPILERFEATURES_FORWARD(ARGS_06, args_06),
                          BSLS_COMPILERFEATURES_FORWARD(ARGS_07, args_07),
                          BSLS_COMPILERFEATURES_FORWARD(ARGS_08, args_08));
}
#endif  // BSLSTL_OPTIONAL_VARIADIC_LIMIT_P >= 8

#if BSLSTL_OPTIONAL_VARIADIC_LIMIT_P >= 9
template<class TYPE, class ARGS_01,
                     class ARGS_02,
                     class ARGS_03,
                     class ARGS_04,
                     class ARGS_05,
                     class ARGS_06,
                     class ARGS_07,
                     class ARGS_08,
                     class ARGS_09>
BSLS_KEYWORD_CONSTEXPR
bsl::optional<TYPE>
make_optional(BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_01) args_01,
              BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_02) args_02,
              BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_03) args_03,
              BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_04) args_04,
              BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_05) args_05,
              BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_06) args_06,
              BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_07) args_07,
              BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_08) args_08,
              BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_09) args_09)
{
    return bsl::optional<TYPE>(bsl::in_place,
                          BSLS_COMPILERFEATURES_FORWARD(ARGS_01, args_01),
                          BSLS_COMPILERFEATURES_FORWARD(ARGS_02, args_02),
                          BSLS_COMPILERFEATURES_FORWARD(ARGS_03, args_03),
                          BSLS_COMPILERFEATURES_FORWARD(ARGS_04, args_04),
                          BSLS_COMPILERFEATURES_FORWARD(ARGS_05, args_05),
                          BSLS_COMPILERFEATURES_FORWARD(ARGS_06, args_06),
                          BSLS_COMPILERFEATURES_FORWARD(ARGS_07, args_07),
                          BSLS_COMPILERFEATURES_FORWARD(ARGS_08, args_08),
                          BSLS_COMPILERFEATURES_FORWARD(ARGS_09, args_09));
}
#endif  // BSLSTL_OPTIONAL_VARIADIC_LIMIT_P >= 9

#if BSLSTL_OPTIONAL_VARIADIC_LIMIT_P >= 10
template<class TYPE, class ARGS_01,
                     class ARGS_02,
                     class ARGS_03,
                     class ARGS_04,
                     class ARGS_05,
                     class ARGS_06,
                     class ARGS_07,
                     class ARGS_08,
                     class ARGS_09,
                     class ARGS_10>
BSLS_KEYWORD_CONSTEXPR
bsl::optional<TYPE>
make_optional(BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_01) args_01,
              BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_02) args_02,
              BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_03) args_03,
              BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_04) args_04,
              BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_05) args_05,
              BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_06) args_06,
              BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_07) args_07,
              BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_08) args_08,
              BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_09) args_09,
              BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_10) args_10)
{
    return bsl::optional<TYPE>(bsl::in_place,
                          BSLS_COMPILERFEATURES_FORWARD(ARGS_01, args_01),
                          BSLS_COMPILERFEATURES_FORWARD(ARGS_02, args_02),
                          BSLS_COMPILERFEATURES_FORWARD(ARGS_03, args_03),
                          BSLS_COMPILERFEATURES_FORWARD(ARGS_04, args_04),
                          BSLS_COMPILERFEATURES_FORWARD(ARGS_05, args_05),
                          BSLS_COMPILERFEATURES_FORWARD(ARGS_06, args_06),
                          BSLS_COMPILERFEATURES_FORWARD(ARGS_07, args_07),
                          BSLS_COMPILERFEATURES_FORWARD(ARGS_08, args_08),
                          BSLS_COMPILERFEATURES_FORWARD(ARGS_09, args_09),
                          BSLS_COMPILERFEATURES_FORWARD(ARGS_10, args_10));
}
#endif  // BSLSTL_OPTIONAL_VARIADIC_LIMIT_P >= 10


#if defined(BSLS_COMPILERFEATURES_SUPPORT_GENERALIZED_INITIALIZERS)
#if BSLSTL_OPTIONAL_VARIADIC_LIMIT_P >= 0
template<class TYPE, class IL_TYPE>
BSLS_KEYWORD_CONSTEXPR
bsl::optional<TYPE>
make_optional(std::initializer_list<IL_TYPE>             il)
{
    return bsl::optional<TYPE>(bsl::in_place, il);
}
#endif  // BSLSTL_OPTIONAL_VARIADIC_LIMIT_P >= 0

#if BSLSTL_OPTIONAL_VARIADIC_LIMIT_P >= 1
template<class TYPE, class IL_TYPE, class ARGS_01>
BSLS_KEYWORD_CONSTEXPR
bsl::optional<TYPE>
make_optional(std::initializer_list<IL_TYPE>             il,
              BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_01) args_01)
{
    return bsl::optional<TYPE>(bsl::in_place, il,
                          BSLS_COMPILERFEATURES_FORWARD(ARGS_01, args_01));
}
#endif  // BSLSTL_OPTIONAL_VARIADIC_LIMIT_P >= 1

#if BSLSTL_OPTIONAL_VARIADIC_LIMIT_P >= 2
template<class TYPE, class IL_TYPE, class ARGS_01,
                                    class ARGS_02>
BSLS_KEYWORD_CONSTEXPR
bsl::optional<TYPE>
make_optional(std::initializer_list<IL_TYPE>             il,
              BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_01) args_01,
              BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_02) args_02)
{
    return bsl::optional<TYPE>(bsl::in_place, il,
                          BSLS_COMPILERFEATURES_FORWARD(ARGS_01, args_01),
                          BSLS_COMPILERFEATURES_FORWARD(ARGS_02, args_02));
}
#endif  // BSLSTL_OPTIONAL_VARIADIC_LIMIT_P >= 2

#if BSLSTL_OPTIONAL_VARIADIC_LIMIT_P >= 3
template<class TYPE, class IL_TYPE, class ARGS_01,
                                    class ARGS_02,
                                    class ARGS_03>
BSLS_KEYWORD_CONSTEXPR
bsl::optional<TYPE>
make_optional(std::initializer_list<IL_TYPE>             il,
              BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_01) args_01,
              BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_02) args_02,
              BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_03) args_03)
{
    return bsl::optional<TYPE>(bsl::in_place, il,
                          BSLS_COMPILERFEATURES_FORWARD(ARGS_01, args_01),
                          BSLS_COMPILERFEATURES_FORWARD(ARGS_02, args_02),
                          BSLS_COMPILERFEATURES_FORWARD(ARGS_03, args_03));
}
#endif  // BSLSTL_OPTIONAL_VARIADIC_LIMIT_P >= 3

#if BSLSTL_OPTIONAL_VARIADIC_LIMIT_P >= 4
template<class TYPE, class IL_TYPE, class ARGS_01,
                                    class ARGS_02,
                                    class ARGS_03,
                                    class ARGS_04>
BSLS_KEYWORD_CONSTEXPR
bsl::optional<TYPE>
make_optional(std::initializer_list<IL_TYPE>             il,
              BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_01) args_01,
              BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_02) args_02,
              BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_03) args_03,
              BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_04) args_04)
{
    return bsl::optional<TYPE>(bsl::in_place, il,
                          BSLS_COMPILERFEATURES_FORWARD(ARGS_01, args_01),
                          BSLS_COMPILERFEATURES_FORWARD(ARGS_02, args_02),
                          BSLS_COMPILERFEATURES_FORWARD(ARGS_03, args_03),
                          BSLS_COMPILERFEATURES_FORWARD(ARGS_04, args_04));
}
#endif  // BSLSTL_OPTIONAL_VARIADIC_LIMIT_P >= 4

#if BSLSTL_OPTIONAL_VARIADIC_LIMIT_P >= 5
template<class TYPE, class IL_TYPE, class ARGS_01,
                                    class ARGS_02,
                                    class ARGS_03,
                                    class ARGS_04,
                                    class ARGS_05>
BSLS_KEYWORD_CONSTEXPR
bsl::optional<TYPE>
make_optional(std::initializer_list<IL_TYPE>             il,
              BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_01) args_01,
              BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_02) args_02,
              BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_03) args_03,
              BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_04) args_04,
              BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_05) args_05)
{
    return bsl::optional<TYPE>(bsl::in_place, il,
                          BSLS_COMPILERFEATURES_FORWARD(ARGS_01, args_01),
                          BSLS_COMPILERFEATURES_FORWARD(ARGS_02, args_02),
                          BSLS_COMPILERFEATURES_FORWARD(ARGS_03, args_03),
                          BSLS_COMPILERFEATURES_FORWARD(ARGS_04, args_04),
                          BSLS_COMPILERFEATURES_FORWARD(ARGS_05, args_05));
}
#endif  // BSLSTL_OPTIONAL_VARIADIC_LIMIT_P >= 5

#if BSLSTL_OPTIONAL_VARIADIC_LIMIT_P >= 6
template<class TYPE, class IL_TYPE, class ARGS_01,
                                    class ARGS_02,
                                    class ARGS_03,
                                    class ARGS_04,
                                    class ARGS_05,
                                    class ARGS_06>
BSLS_KEYWORD_CONSTEXPR
bsl::optional<TYPE>
make_optional(std::initializer_list<IL_TYPE>             il,
              BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_01) args_01,
              BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_02) args_02,
              BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_03) args_03,
              BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_04) args_04,
              BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_05) args_05,
              BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_06) args_06)
{
    return bsl::optional<TYPE>(bsl::in_place, il,
                          BSLS_COMPILERFEATURES_FORWARD(ARGS_01, args_01),
                          BSLS_COMPILERFEATURES_FORWARD(ARGS_02, args_02),
                          BSLS_COMPILERFEATURES_FORWARD(ARGS_03, args_03),
                          BSLS_COMPILERFEATURES_FORWARD(ARGS_04, args_04),
                          BSLS_COMPILERFEATURES_FORWARD(ARGS_05, args_05),
                          BSLS_COMPILERFEATURES_FORWARD(ARGS_06, args_06));
}
#endif  // BSLSTL_OPTIONAL_VARIADIC_LIMIT_P >= 6

#if BSLSTL_OPTIONAL_VARIADIC_LIMIT_P >= 7
template<class TYPE, class IL_TYPE, class ARGS_01,
                                    class ARGS_02,
                                    class ARGS_03,
                                    class ARGS_04,
                                    class ARGS_05,
                                    class ARGS_06,
                                    class ARGS_07>
BSLS_KEYWORD_CONSTEXPR
bsl::optional<TYPE>
make_optional(std::initializer_list<IL_TYPE>             il,
              BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_01) args_01,
              BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_02) args_02,
              BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_03) args_03,
              BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_04) args_04,
              BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_05) args_05,
              BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_06) args_06,
              BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_07) args_07)
{
    return bsl::optional<TYPE>(bsl::in_place, il,
                          BSLS_COMPILERFEATURES_FORWARD(ARGS_01, args_01),
                          BSLS_COMPILERFEATURES_FORWARD(ARGS_02, args_02),
                          BSLS_COMPILERFEATURES_FORWARD(ARGS_03, args_03),
                          BSLS_COMPILERFEATURES_FORWARD(ARGS_04, args_04),
                          BSLS_COMPILERFEATURES_FORWARD(ARGS_05, args_05),
                          BSLS_COMPILERFEATURES_FORWARD(ARGS_06, args_06),
                          BSLS_COMPILERFEATURES_FORWARD(ARGS_07, args_07));
}
#endif  // BSLSTL_OPTIONAL_VARIADIC_LIMIT_P >= 7

#if BSLSTL_OPTIONAL_VARIADIC_LIMIT_P >= 8
template<class TYPE, class IL_TYPE, class ARGS_01,
                                    class ARGS_02,
                                    class ARGS_03,
                                    class ARGS_04,
                                    class ARGS_05,
                                    class ARGS_06,
                                    class ARGS_07,
                                    class ARGS_08>
BSLS_KEYWORD_CONSTEXPR
bsl::optional<TYPE>
make_optional(std::initializer_list<IL_TYPE>             il,
              BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_01) args_01,
              BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_02) args_02,
              BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_03) args_03,
              BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_04) args_04,
              BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_05) args_05,
              BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_06) args_06,
              BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_07) args_07,
              BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_08) args_08)
{
    return bsl::optional<TYPE>(bsl::in_place, il,
                          BSLS_COMPILERFEATURES_FORWARD(ARGS_01, args_01),
                          BSLS_COMPILERFEATURES_FORWARD(ARGS_02, args_02),
                          BSLS_COMPILERFEATURES_FORWARD(ARGS_03, args_03),
                          BSLS_COMPILERFEATURES_FORWARD(ARGS_04, args_04),
                          BSLS_COMPILERFEATURES_FORWARD(ARGS_05, args_05),
                          BSLS_COMPILERFEATURES_FORWARD(ARGS_06, args_06),
                          BSLS_COMPILERFEATURES_FORWARD(ARGS_07, args_07),
                          BSLS_COMPILERFEATURES_FORWARD(ARGS_08, args_08));
}
#endif  // BSLSTL_OPTIONAL_VARIADIC_LIMIT_P >= 8

#if BSLSTL_OPTIONAL_VARIADIC_LIMIT_P >= 9
template<class TYPE, class IL_TYPE, class ARGS_01,
                                    class ARGS_02,
                                    class ARGS_03,
                                    class ARGS_04,
                                    class ARGS_05,
                                    class ARGS_06,
                                    class ARGS_07,
                                    class ARGS_08,
                                    class ARGS_09>
BSLS_KEYWORD_CONSTEXPR
bsl::optional<TYPE>
make_optional(std::initializer_list<IL_TYPE>             il,
              BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_01) args_01,
              BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_02) args_02,
              BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_03) args_03,
              BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_04) args_04,
              BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_05) args_05,
              BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_06) args_06,
              BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_07) args_07,
              BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_08) args_08,
              BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_09) args_09)
{
    return bsl::optional<TYPE>(bsl::in_place, il,
                          BSLS_COMPILERFEATURES_FORWARD(ARGS_01, args_01),
                          BSLS_COMPILERFEATURES_FORWARD(ARGS_02, args_02),
                          BSLS_COMPILERFEATURES_FORWARD(ARGS_03, args_03),
                          BSLS_COMPILERFEATURES_FORWARD(ARGS_04, args_04),
                          BSLS_COMPILERFEATURES_FORWARD(ARGS_05, args_05),
                          BSLS_COMPILERFEATURES_FORWARD(ARGS_06, args_06),
                          BSLS_COMPILERFEATURES_FORWARD(ARGS_07, args_07),
                          BSLS_COMPILERFEATURES_FORWARD(ARGS_08, args_08),
                          BSLS_COMPILERFEATURES_FORWARD(ARGS_09, args_09));
}
#endif  // BSLSTL_OPTIONAL_VARIADIC_LIMIT_P >= 9

#if BSLSTL_OPTIONAL_VARIADIC_LIMIT_P >= 10
template<class TYPE, class IL_TYPE, class ARGS_01,
                                    class ARGS_02,
                                    class ARGS_03,
                                    class ARGS_04,
                                    class ARGS_05,
                                    class ARGS_06,
                                    class ARGS_07,
                                    class ARGS_08,
                                    class ARGS_09,
                                    class ARGS_10>
BSLS_KEYWORD_CONSTEXPR
bsl::optional<TYPE>
make_optional(std::initializer_list<IL_TYPE>             il,
              BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_01) args_01,
              BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_02) args_02,
              BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_03) args_03,
              BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_04) args_04,
              BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_05) args_05,
              BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_06) args_06,
              BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_07) args_07,
              BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_08) args_08,
              BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_09) args_09,
              BSLS_COMPILERFEATURES_FORWARD_REF(ARGS_10) args_10)
{
    return bsl::optional<TYPE>(bsl::in_place, il,
                          BSLS_COMPILERFEATURES_FORWARD(ARGS_01, args_01),
                          BSLS_COMPILERFEATURES_FORWARD(ARGS_02, args_02),
                          BSLS_COMPILERFEATURES_FORWARD(ARGS_03, args_03),
                          BSLS_COMPILERFEATURES_FORWARD(ARGS_04, args_04),
                          BSLS_COMPILERFEATURES_FORWARD(ARGS_05, args_05),
                          BSLS_COMPILERFEATURES_FORWARD(ARGS_06, args_06),
                          BSLS_COMPILERFEATURES_FORWARD(ARGS_07, args_07),
                          BSLS_COMPILERFEATURES_FORWARD(ARGS_08, args_08),
                          BSLS_COMPILERFEATURES_FORWARD(ARGS_09, args_09),
                          BSLS_COMPILERFEATURES_FORWARD(ARGS_10, args_10));
}
#endif  // BSLSTL_OPTIONAL_VARIADIC_LIMIT_P >= 10

#endif
#else
// The generated code below is a workaround for the absence of perfect
// forwarding in some compilers.
template<class TYPE, class... ARGS>
BSLS_KEYWORD_CONSTEXPR
bsl::optional<TYPE>
make_optional(BSLS_COMPILERFEATURES_FORWARD_REF(ARGS)... args)
{
    return bsl::optional<TYPE>(bsl::in_place,
                          BSLS_COMPILERFEATURES_FORWARD(ARGS, args)...);
}

#if defined(BSLS_COMPILERFEATURES_SUPPORT_GENERALIZED_INITIALIZERS)
template<class TYPE, class IL_TYPE, class... ARGS>
BSLS_KEYWORD_CONSTEXPR
bsl::optional<TYPE>
make_optional(std::initializer_list<IL_TYPE>             il,
              BSLS_COMPILERFEATURES_FORWARD_REF(ARGS)... args)
{
    return bsl::optional<TYPE>(bsl::in_place, il,
                          BSLS_COMPILERFEATURES_FORWARD(ARGS, args)...);
}
#endif
// }}} END GENERATED CODE
#endif



#ifdef __cpp_lib_optional

// in presence of std::optional, we need to provide better match relation
// operators, or some comparisons may be ambiguous.
template<class TYPE>
inline
void swap(bsl::optional<TYPE>& lhs, std::optional<TYPE>& rhs)
{
    lhs.swap(rhs);
}

template<class TYPE>
inline
void swap(std::optional<TYPE>& lhs, bsl::optional<TYPE>& rhs)
{
    lhs.swap(rhs);
}

template <class LHS_TYPE, class RHS_TYPE>
inline
bool operator==(const bsl::optional<LHS_TYPE>& lhs,
                const std::optional<RHS_TYPE>& rhs)
{
    if (lhs && rhs) {
        return lhs.value() == rhs.value();
    }
    return lhs.has_value() == rhs.has_value();
}

template <class LHS_TYPE, class RHS_TYPE>
inline
bool operator==(const std::optional<LHS_TYPE>& lhs,
                const bsl::optional<RHS_TYPE>& rhs)
{
    if (lhs && rhs) {
        return lhs.value() == rhs.value();
    }
    return lhs.has_value() == rhs.has_value();
}

template <class LHS_TYPE, class RHS_TYPE>
inline
bool operator!=(const bsl::optional<LHS_TYPE>& lhs,
                const std::optional<RHS_TYPE>& rhs)
{
    if (lhs && rhs) {
        return lhs.value() != rhs.value();
    }

    return lhs.has_value() != rhs.has_value();
}

template <class LHS_TYPE, class RHS_TYPE>
inline
bool operator!=(const std::optional<LHS_TYPE>& lhs,
                const bsl::optional<RHS_TYPE>& rhs)
{
    if (lhs && rhs) {
        return lhs.value() != rhs.value();
    }

    return lhs.has_value() != rhs.has_value();
}

template <class LHS_TYPE, class RHS_TYPE>
inline
bool operator<(const bsl::optional<LHS_TYPE>& lhs,
               const std::optional<RHS_TYPE>& rhs)
{
    if (!rhs) {
        return false;
    }

    return !lhs || lhs.value() < rhs.value();
}

template <class LHS_TYPE, class RHS_TYPE>
inline
bool operator<(const std::optional<LHS_TYPE>& lhs,
               const bsl::optional<RHS_TYPE>& rhs)
{
    if (!rhs) {
        return false;
    }

    return !lhs || lhs.value() < rhs.value();
}

template <class LHS_TYPE, class RHS_TYPE>
inline
bool operator>(const bsl::optional<LHS_TYPE>& lhs,
               const std::optional<RHS_TYPE>& rhs)
{
    return rhs < lhs;
}

template <class LHS_TYPE, class RHS_TYPE>
inline
bool operator>(const std::optional<LHS_TYPE>& lhs,
               const bsl::optional<RHS_TYPE>& rhs)
{
    return rhs < lhs;
}

template <class LHS_TYPE, class RHS_TYPE>
inline
bool operator<=(const bsl::optional<LHS_TYPE>& lhs,
                const std::optional<RHS_TYPE>& rhs)
{
    return !(rhs < lhs);
}

template <class LHS_TYPE, class RHS_TYPE>
inline
bool operator<=(const std::optional<LHS_TYPE>& lhs,
                const bsl::optional<RHS_TYPE>& rhs)
{
    return !(rhs < lhs);
}

template <class LHS_TYPE, class RHS_TYPE>
inline
bool operator>=(const bsl::optional<LHS_TYPE>& lhs,
                const std::optional<RHS_TYPE>& rhs)
{
    return !(lhs < rhs);
}

template <class LHS_TYPE, class RHS_TYPE>
inline
bool operator>=(const std::optional<LHS_TYPE>& lhs,
                const bsl::optional<RHS_TYPE>& rhs)
{
    return !(lhs < rhs);
}

#endif
}  // close namespace bsl
#undef BSLS_KEYWORD_LVREF_QUAL
#undef BSLS_KEYWORD_RVREF_QUAL

#endif // INCLUDED_BSLSTL_OPTIONAL
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
