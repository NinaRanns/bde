// bdlcfxxxi_vfunc1.h               -*-C++-*-
#ifndef INCLUDED_BDLCFXXXI_VFUNC1
#define INCLUDED_BDLCFXXXI_VFUNC1

#ifndef INCLUDED_BSLS_IDENT
#include <bsls_ident.h>
#endif
BSLS_IDENT("$Id: $")


//@PURPOSE: Provide concrete representations for 1-arg void function objects.
//
//@CLASSES:
// bdlcfxxxi::Vfunc1Null: a 1-arg void no-op functor
//
// bdlcfxxxi::Vfunc1F1: a 1-arg void functor created from a 1-arg function
// bdlcfxxxi::Vfunc1F2: a 1-arg void functor created from a 2-arg function
// bdlcfxxxi::Vfunc1F3: a 1-arg void functor created from a 3-arg function
// bdlcfxxxi::Vfunc1F4: a 1-arg void functor created from a 4-arg function
// bdlcfxxxi::Vfunc1F5: a 1-arg void functor created from a 5-arg function
// bdlcfxxxi::Vfunc1F6: a 1-arg void functor created from a 6-arg function
// bdlcfxxxi::Vfunc1F7: a 1-arg void functor created from a 7-arg function
// bdlcfxxxi::Vfunc1F8: a 1-arg void functor created from a 8-arg function
// bdlcfxxxi::Vfunc1F9: a 1-arg void functor created from a 9-arg function
//
// bdlcfxxxi::Vfunc1C1: a 1-arg void functor created from obj & 1-arg method
// bdlcfxxxi::Vfunc1C2: a 1-arg void functor created from obj & 2-arg method
// bdlcfxxxi::Vfunc1C3: a 1-arg void functor created from obj & 3-arg method
// bdlcfxxxi::Vfunc1C4: a 1-arg void functor created from obj & 4-arg method
// bdlcfxxxi::Vfunc1C5: a 1-arg void functor created from obj & 5-arg method
// bdlcfxxxi::Vfunc1C6: a 1-arg void functor created from obj & 6-arg method
// bdlcfxxxi::Vfunc1C7: a 1-arg void functor created from obj & 7-arg method
// bdlcfxxxi::Vfunc1C8: a 1-arg void functor created from obj & 8-arg method
// bdlcfxxxi::Vfunc1C9: a 1-arg void functor created from obj & 9-arg method
//
// bdlcfxxxi::Vfunc1M1: a 1-arg void functor created from obj ptr & 1-arg method
// bdlcfxxxi::Vfunc1M2: a 1-arg void functor created from obj ptr & 2-arg method
// bdlcfxxxi::Vfunc1M3: a 1-arg void functor created from obj ptr & 3-arg method
// bdlcfxxxi::Vfunc1M4: a 1-arg void functor created from obj ptr & 4-arg method
// bdlcfxxxi::Vfunc1M5: a 1-arg void functor created from obj ptr & 5-arg method
// bdlcfxxxi::Vfunc1M6: a 1-arg void functor created from obj ptr & 6-arg method
// bdlcfxxxi::Vfunc1M7: a 1-arg void functor created from obj ptr & 7-arg method
// bdlcfxxxi::Vfunc1M8: a 1-arg void functor created from obj ptr & 8-arg method
// bdlcfxxxi::Vfunc1M9: a 1-arg void functor created from obj ptr & 9-arg method
//
//@AUTHOR: John Lakos (jlakos)
//
//@DEPRECATED: This component should not be used in new code, and will be
// deleted in the near future.  Please see 'bdlf_function', 'bdlf_bind', etc.
// for alternatives that should be used for all future development.
//
//@DESCRIPTION: This component contains a suite of classes to implement the
// polymorphic representation of a function object (functor) whose
// function-call operator ('execute') takes one argument and returns 'void'
// (hence the root name 'vfunc1' -- "v" for 'void' and "1" for the number of
// arguments).  Direct clients of this component choose a concrete
// implementation of the base 'bdlcfxxxr::Vfunc1' representation based on the
// number of additional arguments required by the underlying procedure, and on
// the specific kind or "flavor" of function that the functor is to invoke.
// If the user does not wish to invoke any function on the functor invocation,
// then the "null functor" 'bdlcfxxxi::Vfunc1Null' may be used.
//
///Language-Imposed Details
///------------------------
// For practical reasons, this component must distinguish among four types of
// functors.  The first type is a "null functor" 'bdlcfxxxi::Vfunc1Null', which
// differs from the other three types in that it does not invoke any function.
//
// The other functors are distinguished by three flavors 'X' of functions they
// invoke, and also by the total number 'N' of arguments (N >= 1) that the
// function takes.  This component supplies a separate concrete implementation
// for each of the specified types.  These 27 non-null concrete
// implementations are named 'bcefi::Vfunc1XN' where 'X' is one of 'F', 'C', or
// 'M', and 'N' is in the range [1..9].
//
// The possible function flavors (and labels) are classified as:
//..
//     Label   Classification
//     -----   --------------
//       F     free function, static member function, or "free" functor object
//       C     'const' (accessor) member function
//       M     non-'const' (manipulator) member function
//..
// Note that a 'const' member function may be wrapped by a 'bcefi::Vfunc1MN'
// functor, but wrapping a non-'const' method by a 'bcefi::Vfunc1CN' will
// result in compile time error in most compiler implementations.
//
// Note also that, to accommodate the preservation of 'const', the
// 'bcefi::Vfunc1CN' functors hold an object 'Obj', supplied by a 'const'
// 'Obj&' in the constructor, while 'bcefi::Vfunc1MN' functors hold a pointer
// to 'Obj' supplied by a modifiable 'Obj*' argument in the constructor.
//
// The (N - 1) additional arguments specified at the construction of the
// functor are held by the functor, and appended, in order, to the one
// required (invocation-specific) argument specified in each call to the
// virtual 'execute' method when the underlying function is invoked by
// 'execute'.  Any value returned from the underlying procedure is ignored.
//
// Note that the respective types of the parameters of the underlying procedure
// must correspond to the overall parameterization of the particular template
// representation class.  The virtual 'execute' method in each representation
// class defined in this component takes one argument.  The respective types
// of the leading parameter of the underlying procedure must match (or be
// automatically convertible from) those of the parameterized template
// representation class's virtual 'execute' method.  The respective types of
// any additional trailing parameters to the underlying procedure must match
// (or be convertible from) those specified in the particular representation
// class's constructor.
//
///USAGE
///-----
// Given the following free function, which takes four arguments (and happens
// to return an 'int' value)
//..
//   int myFunction(int a, double b, const char *c, int d);
//..
// we can create a (dynamically-allocated) one-argument functor object that, at
// construction, wraps up the function along with the three additional
// arguments, and subsequently refer to this 'bdlcfxxxi::Vfunc1F3' functor only
// through its base class, 'bdlcfxxxr::Vfunc1':
//..
//   typedef int (*MyFunctionPtr)(int, double, const char *, int);
//   typedef bdlcfxxxi::Vfunc1F4<MyFunctionPtr,
//                        int, double, const char *, int> MyFunctorRep;
//
//   bslma::Allocator *ba = bslma::Default::allocator(0);
//
//   bdlcfxxxr::Vfunc1<int> *rep =
//                           new(ba) MyFunctorRep(myFunction, 1, "3", 4, ba);
//..
// The virtual 'execute' method (taking one argument) can now be called through
// the 'bdlcfxxxr::Vfunc1' base class pointer 'rep'.  This method will, in turn,
// invoke the underlying function with one argument supplied by the caller of
// the 'execute' method, concatenated with the sequence of three additional
// (trailing) arguments supplied at construction.
//..
//   rep->execute(10);
//..
// The derived object is destroyed through the 'deleteObject' class method of
// the base class, which allows us to declare its destructor 'private'.
//..
//   bdlcfxxxr::Vfunc1<int>::deleteObject(rep);
//..
// This mechanism allows us to hide the details of destruction and memory
// deallocation from the user of this class.  By declaring the destructor
// private we also prevent creation of (automatic) instances on the program
// stack, which precludes accidental use of 'delete' and also prevents further
// derivation.
//
// Finally, note that the integer returned to the functor by 'myFunction' is
// ignored.

#ifndef INCLUDED_BDLSCM_VERSION
#include <bdlscm_version.h>
#endif

#ifndef INCLUDED_BSLALG_CONSTRUCTORPROXY
#include <bslalg_constructorproxy.h>
#endif

#ifndef INCLUDED_BDLCFXXXR_VFUNC1
#include <bdlcfxxxr_vfunc1.h>
#endif


namespace BloombergLP {

namespace bdlcfxxxi {
                        // ======================
                        // class Vfunc1Null
                        // ======================

template <class A1>
class Vfunc1Null : public bdlcfxxxr::Vfunc1<A1> {
    // This class defines the representation for a function object (functor),
    // characterized by a function-call operator taking one argument,
    // returning 'void', and performing no operations.

    // NOT IMPLEMENTED
    Vfunc1Null(const Vfunc1Null<A1>&);
    Vfunc1Null<A1>& operator=(const Vfunc1Null<A1>&);

  private:
    // PRIVATE CREATORS
    ~Vfunc1Null();
        // Destroy this functor.  Note that this destructor can be invoked only
        // indirectly, via the static 'deleteObject' method of the base class.

    // PRIVATE ACCESSORS
    void execute(const A1& argument1) const;
        // Invoke the no-op function with the specified 'argument1'.

  public:
    // CREATORS
    Vfunc1Null(bslma::Allocator *basicAllocator);
        // Create a representation for a function object (functor) taking one
        // argument, returning 'void', and performing no operation.
};

                        // ====================
                        // class Vfunc1F1
                        // ====================

template <class F, class A1>
class Vfunc1F1 : public bdlcfxxxr::Vfunc1<A1> {
    // This class defines the representation for a function object (functor),
    // characterized by a function-call operator taking one argument and
    // returning 'void', that holds a pure procedure (i.e., free function,
    // static member function, or functor) taking no additional arguments.

    // DATA
    bslalg::ConstructorProxy<F> d_f;  // function pointer (or functor)

    // NOT IMPLEMENTED
    Vfunc1F1(const Vfunc1F1<F, A1>&);
    Vfunc1F1<F, A1>& operator=(const Vfunc1F1<F, A1>&);

  private:
    // PRIVATE CREATORS
    ~Vfunc1F1();
        // Destroy this functor.  Note that this destructor can be invoked only
        // indirectly, via the static 'deleteObject' method of the base class.

    // PRIVATE ACCESSORS
    void execute(const A1& argument1) const;
        // Invoke the underlying procedure (free function, static member
        // function, or functor) with the specified 'argument1' (and no
        // additional arguments).

  public:
    // CREATORS
    Vfunc1F1(F procedure, bslma::Allocator *basicAllocator);
        // Create a representation for a function object (functor) taking one
        // argument and returning 'void', using the specified 'procedure'
        // (i.e., free function, static member function, or functor) taking no
        // additional arguments.  Use the specified 'basicAllocator' to supply
        // memory.  Note that any value returned from 'procedure' will be
        // ignored.
};

                        // ====================
                        // class Vfunc1F2
                        // ====================

template <class F, class A1, class D1>
class Vfunc1F2 : public bdlcfxxxr::Vfunc1<A1> {
    // This class defines the representation for a function object (functor),
    // characterized by a function-call operator taking one argument and
    // returning 'void', that holds a pure procedure (i.e., free function,
    // static member function, or functor) taking one additional trailing
    // argument, and that argument's corresponding value.

    // DATA
    bslalg::ConstructorProxy<F>  d_f;   // function pointer (or functor)
    bslalg::ConstructorProxy<D1> d_d1;  // first embedded argument

    // NOT IMPLEMENTED
    Vfunc1F2(const Vfunc1F2<F, A1, D1>&);
    Vfunc1F2<F, A1, D1>& operator=(const Vfunc1F2<F, A1, D1>&);

  private:
    // PRIVATE CREATORS
    ~Vfunc1F2();
        // Destroy this functor.  Note that this destructor can be only
        // indirectly, via the static 'deleteObject' method of the base class.

    // PRIVATE ACCESSORS
    void execute(const A1& argument1) const;
        // Invoke the underlying procedure (free function, static member
        // function, or functor) with the specified 'argument1' along with the
        // trailing argument value specified at construction.

  public:
    // CREATORS
    Vfunc1F2(F                procedure,
                   const D1&        embeddedArg1,
                   bslma::Allocator*basicAllocator);
        // Create a representation for a function object (functor) taking one
        // argument and returning 'void', using the specified 'procedure'
        // (i.e., free function, static member function, or functor) taking
        // one additional trailing argument, and this corresponding argument's
        // specified 'embeddedArg1' value.  Use the specified 'basicAllocator'
        // to supply memory.  Note that any value returned from 'procedure'
        // will be ignored.
};

                        // ====================
                        // class Vfunc1F3
                        // ====================

template <class F, class A1, class D1, class D2>
class Vfunc1F3 : public bdlcfxxxr::Vfunc1<A1> {
    // This class defines the representation for a function object (functor),
    // characterized by a function-call operator taking one argument and
    // returning 'void', that holds a pure procedure (i.e., free function,
    // static member function, or functor) taking two additional trailing
    // arguments, and those arguments' corresponding values.

    // DATA
    bslalg::ConstructorProxy<F>  d_f;   // function pointer or functor
    bslalg::ConstructorProxy<D1> d_d1;  // first embedded argument
    bslalg::ConstructorProxy<D2> d_d2;  // second embedded argument

    // NOT IMPLEMENTED
    Vfunc1F3(const Vfunc1F3<F, A1, D1, D2>&);
    Vfunc1F3<F, A1, D1, D2>& operator=(
                   const Vfunc1F3<F, A1, D1, D2>&);

  private:
    // PRIVATE CREATORS
    ~Vfunc1F3();
        // Destroy this functor.  Note that this destructor can be invoked only
        // indirectly, via the static 'deleteObject' method of the base class.

    // PRIVATE ACCESSORS
    void execute(const A1& argument1) const;
        // Invoke the underlying procedure (free function, static member
        // function, or functor) with the specified 'argument1' along with the
        // sequence of two argument values specified at construction.

  public:
    // CREATORS
    Vfunc1F3(F                procedure,
                   const D1&        embeddedArg1,
                   const D2&        embeddedArg2,
                   bslma::Allocator*basicAllocator);
        // Create a representation for a function object (functor) taking one
        // argument and returning 'void', using the specified 'procedure'
        // (i.e., free function, static member function, or functor) taking
        // two additional trailing arguments, and the corresponding arguments'
        // specified 'embeddedArg1' and 'embeddedArg2' values.  Use the
        // specified 'basicAllocator' to supply memory.  Note that any value
        // returned from 'procedure' will be ignored.
};

                        // ====================
                        // class Vfunc1F4
                        // ====================

template <class F, class A1, class D1, class D2, class D3>
class Vfunc1F4 : public bdlcfxxxr::Vfunc1<A1> {
    // This class defines the representation for a function object (functor),
    // characterized by a function-call operator taking one argument and
    // returning 'void', that holds a pure procedure (i.e., free function,
    // static member function, or functor) taking three additional trailing
    // arguments, and those arguments' corresponding values.

    // DATA
    bslalg::ConstructorProxy<F>  d_f;   // function pointer (or functor)
    bslalg::ConstructorProxy<D1> d_d1;  // first embedded argument
    bslalg::ConstructorProxy<D2> d_d2;  // second embedded argument
    bslalg::ConstructorProxy<D3> d_d3;  // third embedded argument

    // NOT IMPLEMENTED
    Vfunc1F4(const Vfunc1F4<F, A1, D1, D2, D3>&);
    Vfunc1F4<F, A1, D1, D2, D3>& operator=(
                   const Vfunc1F4<F, A1, D1, D2, D3>&);

  private:
    // PRIVATE CREATORS
    ~Vfunc1F4();
        // Destroy this functor.  Note that this destructor can be invoked only
        // indirectly, via the static 'deleteObject' method of the base class.

    // PRIVATE ACCESSORS
    void execute(const A1& argument1) const;
        // Invoke the underlying procedure (free function, static member
        // function, or functor) with the specified 'argument1' along with the
        // sequence of three argument values specified at construction.

  public:
    // CREATORS
    Vfunc1F4(F                procedure,
                   const D1&        embeddedArg1,
                   const D2&        embeddedArg2,
                   const D3&        embeddedArg3,
                   bslma::Allocator*basicAllocator);
        // Create a representation for a function object (functor) taking one
        // argument and returning 'void', using the specified 'procedure'
        // (i.e., free function, static member function, or functor) taking
        // three additional trailing arguments and the corresponding
        // arguments' specified 'embeddedArg1', 'embeddedArg2, and
        // 'embeddedArg3' values.  Use the specified 'basicAllocator' to
        // supply memory.  Note that any value returned from 'procedure' will
        // be ignored.
};

                        // ====================
                        // class Vfunc1F5
                        // ====================

template <class F, class A1, class D1, class D2, class D3, class D4>
class Vfunc1F5 : public bdlcfxxxr::Vfunc1<A1> {
    // This class defines the representation for a function object (functor),
    // characterized by a function-call operator taking one argument and
    // returning 'void', that holds a pure procedure (i.e., free function,
    // static member function, or functor) taking four additional trailing
    // arguments, and those arguments' corresponding values.

    // DATA
    bslalg::ConstructorProxy<F>  d_f;   // function pointer (or functor)
    bslalg::ConstructorProxy<D1> d_d1;  // first embedded argument
    bslalg::ConstructorProxy<D2> d_d2;  // second embedded argument
    bslalg::ConstructorProxy<D3> d_d3;  // third embedded argument
    bslalg::ConstructorProxy<D4> d_d4;  // fourth embedded argument

    // NOT IMPLEMENTED
    Vfunc1F5(const Vfunc1F5<F, A1, D1, D2, D3, D4>&);
    Vfunc1F5<F, A1, D1, D2, D3, D4>& operator=(
                   const Vfunc1F5<F, A1, D1, D2, D3, D4>&);

  private:
    // PRIVATE CREATORS
    ~Vfunc1F5();
        // Destroy this functor.  Note that this destructor can be invoked only
        // indirectly, via the static 'deleteObject' method of the base class.

    // PRIVATE ACCESSORS
    void execute(const A1& argument1) const;
        // Invoke the underlying procedure (free function, static member
        // function, or functor) with the specified 'argument1' along with the
        // sequence of four argument values specified at construction.

  public:
    // CREATORS
    Vfunc1F5(F                procedure,
                   const D1&        embeddedArg1,
                   const D2&        embeddedArg2,
                   const D3&        embeddedArg3,
                   const D4&        embeddedArg4,
                   bslma::Allocator*basicAllocator);
        // Create a representation for a function object (functor) taking one
        // argument and returning 'void', using the specified 'procedure'
        // (i.e., free function, static member function, or functor) taking
        // four additional trailing arguments and the corresponding arguments'
        // specified 'embeddedArg1', 'embeddedArg2', 'embeddedArg3', and
        // 'embeddedArg4' values.  Use the specified 'basicAllocator' to
        // supply memory.  Note that any value returned from 'procedure' will
        // be ignored.
};

                        // ====================
                        // class Vfunc1F6
                        // ====================

template <class F, class A1, class D1, class D2, class D3, class D4,
          class D5>
class Vfunc1F6 : public bdlcfxxxr::Vfunc1<A1> {
    // This class defines the representation for a function object (functor),
    // characterized by a function-call operator taking one argument and
    // returning 'void', that holds a pure procedure (i.e., free function,
    // static member function, or functor) taking five additional trailing
    // arguments, and those arguments' corresponding values.

    // DATA
    bslalg::ConstructorProxy<F>  d_f;   // function pointer (or functor)
    bslalg::ConstructorProxy<D1> d_d1;  // first embedded argument
    bslalg::ConstructorProxy<D2> d_d2;  // second embedded argument
    bslalg::ConstructorProxy<D3> d_d3;  // third embedded argument
    bslalg::ConstructorProxy<D4> d_d4;  // fourth embedded argument
    bslalg::ConstructorProxy<D5> d_d5;  // fifth embedded argument

    // NOT IMPLEMENTED
    Vfunc1F6(const Vfunc1F6<F, A1, D1, D2, D3, D4, D5>&);
    Vfunc1F6<F, A1, D1, D2, D3, D4, D5>& operator=(
                   const Vfunc1F6<F, A1, D1, D2, D3, D4, D5>&);

  private:
    // PRIVATE CREATORS
    ~Vfunc1F6();
        // Destroy this functor.  Note that this destructor can be invoked only
        // indirectly, via the static 'deleteObject' method of the base class.

    // PRIVATE ACCESSORS
    void execute(const A1& argument1) const;
        // Invoke the underlying procedure (free function, static member
        // function, or functor) with the specified 'argument1' along with the
        // sequence of five argument values specified at construction.

  public:
    // CREATORS
    Vfunc1F6(F                procedure,
                   const D1&        embeddedArg1,
                   const D2&        embeddedArg2,
                   const D3&        embeddedArg3,
                   const D4&        embeddedArg4,
                   const D5&        embeddedArg5,
                   bslma::Allocator*basicAllocator);
        // Create a representation for a function object (functor) taking one
        // argument and returning 'void', using the specified 'procedure'
        // (i.e., free function, static member function, or functor) taking
        // five additional trailing arguments and the corresponding arguments'
        // specified 'embeddedArg1', 'embeddedArg2', 'embeddedArg3',
        // 'embeddedArg4', and 'embeddedArg5' values.  Use the specified
        // 'basicAllocator' to supply memory.  Note that any value returned
        // from 'procedure' will be ignored.
};

                        // ====================
                        // class Vfunc1F7
                        // ====================

template <class  F, class A1, class D1, class D2, class D3, class D4,
          class D5, class D6>
class Vfunc1F7 : public bdlcfxxxr::Vfunc1<A1> {
    // This class defines the representation for a function object (functor),
    // characterized by a function-call operator taking one argument and
    // returning 'void', that holds a pure procedure (i.e., free function,
    // static member function, or functor) taking six additional trailing
    // arguments, and those arguments' corresponding values.

    // DATA
    bslalg::ConstructorProxy<F>  d_f;   // function pointer (or functor)
    bslalg::ConstructorProxy<D1> d_d1;  // first embedded argument
    bslalg::ConstructorProxy<D2> d_d2;  // second embedded argument
    bslalg::ConstructorProxy<D3> d_d3;  // third embedded argument
    bslalg::ConstructorProxy<D4> d_d4;  // fourth embedded argument
    bslalg::ConstructorProxy<D5> d_d5;  // fifth embedded argument
    bslalg::ConstructorProxy<D6> d_d6;  // sixth embedded argument

    // NOT IMPLEMENTED
    Vfunc1F7(const Vfunc1F7<F, A1, D1, D2, D3, D4, D5, D6>&);
    Vfunc1F7<F, A1, D1, D2, D3, D4, D5, D6>& operator=(
                   const Vfunc1F7<F, A1, D1, D2, D3, D4, D5, D6>&);

  private:
    // PRIVATE CREATORS
    ~Vfunc1F7();
        // Destroy this functor.  Note that this destructor can be invoked only
        // indirectly, via the static 'deleteObject' method of the base class.

    // PRIVATE ACCESSORS
    void execute(const A1& argument1) const;
        // Invoke the underlying procedure (free function, static member
        // function, or functor) with the specified 'argument1' along with the
        // sequence of six argument values specified at construction.

  public:
    // CREATORS
    Vfunc1F7(F                procedure,
                   const D1&        embeddedArg1,
                   const D2&        embeddedArg2,
                   const D3&        embeddedArg3,
                   const D4&        embeddedArg4,
                   const D5&        embeddedArg5,
                   const D6&        embeddedArg6,
                   bslma::Allocator*basicAllocator);
        // Create a representation for a function object (functor) taking one
        // argument and returning 'void', using the specified 'procedure'
        // (i.e., free function, static member function, or functor) taking
        // six additional trailing arguments and the corresponding arguments'
        // specified 'embeddedArg1', 'embeddedArg2', 'embeddedArg3',
        // 'embeddedArg4', 'embeddedArg5', and 'embeddedArg6' values.  Use the
        // specified 'basicAllocator' to supply memory.  Note that any value
        // returned from 'procedure' will be ignored.
};

                        // ====================
                        // class Vfunc1F8
                        // ====================

template <class  F, class A1, class D1, class D2, class D3, class D4,
          class D5, class D6, class D7>
class Vfunc1F8 : public bdlcfxxxr::Vfunc1<A1> {
    // This class defines the representation for a function object (functor),
    // characterized by a function-call operator taking one argument and
    // returning 'void', that holds a pure procedure (i.e., free function,
    // static member function, or functor) taking seven additional trailing
    // arguments, and those arguments' corresponding values.

    // DATA
    bslalg::ConstructorProxy<F>  d_f;   // function pointer (or functor)
    bslalg::ConstructorProxy<D1> d_d1;  // first embedded argument
    bslalg::ConstructorProxy<D2> d_d2;  // second embedded argument
    bslalg::ConstructorProxy<D3> d_d3;  // third embedded argument
    bslalg::ConstructorProxy<D4> d_d4;  // fourth embedded argument
    bslalg::ConstructorProxy<D5> d_d5;  // fifth embedded argument
    bslalg::ConstructorProxy<D6> d_d6;  // sixth embedded argument
    bslalg::ConstructorProxy<D7> d_d7;  // seventh embedded argument

    // NOT IMPLEMENTED
    Vfunc1F8(const Vfunc1F8<F, A1, D1, D2, D3, D4, D5, D6, D7>&);
    Vfunc1F8<F, A1, D1, D2, D3, D4, D5, D6, D7>& operator=(
                   const Vfunc1F8<F, A1, D1, D2, D3, D4, D5, D6, D7>&);

  private:
    // PRIVATE CREATORS
    ~Vfunc1F8();
        // Destroy this functor.  Note that this destructor can be invoked only
        // indirectly, via the static 'deleteObject' method of the base class.

    // PRIVATE ACCESSORS
    void execute(const A1& argument1) const;
        // Invoke the underlying procedure (free function, static member
        // function, or functor) with the specified 'argument1' along with the
        // sequence of seven argument values specified at construction.

  public:
    // CREATORS
    Vfunc1F8(F                procedure,
                   const D1&        embeddedArg1,
                   const D2&        embeddedArg2,
                   const D3&        embeddedArg3,
                   const D4&        embeddedArg4,
                   const D5&        embeddedArg5,
                   const D6&        embeddedArg6,
                   const D7&        embeddedArg7,
                   bslma::Allocator*basicAllocator);
        // Create a representation for a function object (functor) taking one
        // argument and returning 'void', using the specified 'procedure'
        // (i.e., free function, static member function, or functor) taking
        // seven additional trailing arguments and the corresponding
        // arguments' specified 'embeddedArg1', 'embeddedArg2, 'embeddedArg3',
        // 'embeddedArg4', 'embeddedArg5', 'embeddedArg6', and 'embeddedArg7'
        // values.  Use the specified 'basicAllocator' to supply memory.  Note
        // that any value returned from 'procedure' will be ignored.
};

                        // ====================
                        // class Vfunc1F9
                        // ====================

template <class  F, class A1, class D1, class D2, class D3, class D4,
          class D5, class D6, class D7, class D8>
class Vfunc1F9 : public bdlcfxxxr::Vfunc1<A1> {
    // This class defines the representation for a function object (functor),
    // characterized by a function-call operator taking one argument and
    // returning 'void', that holds a pure procedure (i.e., free function,
    // static member function, or functor) taking eight additional trailing
    // arguments, and those arguments' corresponding values.

    // DATA
    bslalg::ConstructorProxy<F>  d_f;   // function pointer (or functor)
    bslalg::ConstructorProxy<D1> d_d1;  // first embedded argument
    bslalg::ConstructorProxy<D2> d_d2;  // second embedded argument
    bslalg::ConstructorProxy<D3> d_d3;  // third embedded argument
    bslalg::ConstructorProxy<D4> d_d4;  // fourth embedded argument
    bslalg::ConstructorProxy<D5> d_d5;  // fifth embedded argument
    bslalg::ConstructorProxy<D6> d_d6;  // sixth embedded argument
    bslalg::ConstructorProxy<D7> d_d7;  // seventh embedded argument
    bslalg::ConstructorProxy<D8> d_d8;  // eighth embedded argument

    // NOT IMPLEMENTED
    Vfunc1F9(const Vfunc1F9<F, A1, D1, D2, D3, D4, D5, D6,
                                        D7, D8>&);
    Vfunc1F9<F, A1, D1, D2, D3, D4, D5, D6, D7, D8>& operator=(
                   const Vfunc1F9<F, A1, D1, D2, D3, D4, D5, D6,
                                        D7, D8>&);

  private:
    // PRIVATE CREATORS
    ~Vfunc1F9();
        // Destroy this functor.  Note that this destructor can be invoked only
        // indirectly, via the static 'deleteObject' method of the base class.

    // PRIVATE ACCESSORS
    void execute(const A1& argument1) const;
        // Invoke the underlying procedure (free function, static member
        // function, or functor) with the specified 'argument1' along with the
        // sequence of eight argument values specified at construction.

  public:
    // CREATORS
    Vfunc1F9(F                procedure,
                   const D1&        embeddedArg1,
                   const D2&        embeddedArg2,
                   const D3&        embeddedArg3,
                   const D4&        embeddedArg4,
                   const D5&        embeddedArg5,
                   const D6&        embeddedArg6,
                   const D7&        embeddedArg7,
                   const D8&        embeddedArg8,
                   bslma::Allocator*basicAllocator);
        // Create a representation for a function object (functor) taking one
        // argument and returning 'void', using the specified 'procedure'
        // (i.e., free function, static member function, or functor) taking
        // eight additional trailing arguments and the corresponding
        // arguments' specified 'embeddedArg1', 'embeddedArg2, 'embeddedArg3',
        // 'embeddedArg4', 'embeddedArg5', 'embeddedArg6', 'embeddedArg7', and
        // 'embeddedArg8' values.  Use the specified 'basicAllocator' to
        // supply memory.  Note that any value returned from 'procedure' will
        // be ignored.
};

                        // ====================
                        // class Vfunc1C1
                        // ====================

template <class OBJ, class C, class A1>
class Vfunc1C1 : public bdlcfxxxr::Vfunc1<A1> {
    // This class defines the representation for a function object (functor),
    // characterized by a function-call operator taking one argument and
    // returning void, that holds an object and one of that object's 'const'
    // member functions taking no additional arguments.

    // DATA
    bslalg::ConstructorProxy<OBJ> d_obj;  // object
    C                             d_m;    // const member function pointer

    // NOT IMPLEMENTED
    Vfunc1C1(const Vfunc1C1<OBJ, C, A1>&);
    Vfunc1C1<OBJ, C, A1>& operator=(const Vfunc1C1<OBJ, C, A1>&);

  private:
    // PRIVATE CREATORS
    ~Vfunc1C1();
        // Destroy this functor.  Note that this destructor can be invoked only
        // indirectly, via the static 'deleteObject' method of the base class.

    // PRIVATE ACCESSORS
    void execute(const A1& argument1) const;
        // Invoke the underlying 'const' member function on the embedded object
        // with the specified 'argument1' (and no additional arguments).

  public:
    // CREATORS
    Vfunc1C1(const OBJ&       object,
                   C                method,
                   bslma::Allocator*basicAllocator);
        // Create a representation for a function object (functor) taking one
        // argument and returning 'void', using the specified 'object' and the
        // specified 'const' 'method' of 'object' taking no additional
        // arguments.  Use the specified 'basicAllocator' to supply memory.
        // Note that any value returned from 'method' will be ignored.
};

                        // ====================
                        // class Vfunc1C2
                        // ====================

template <class OBJ, class C, class A1, class D1>
class Vfunc1C2 : public bdlcfxxxr::Vfunc1<A1> {
    // This class defines the representation for a function object (functor),
    // characterized by a function-call operator taking one argument and
    // returning 'void', that holds an object and one of that object's 'const'
    // member functions taking one additional trailing argument, and the
    // argument's corresponding value.

    // DATA
    bslalg::ConstructorProxy<OBJ> d_obj;  // object
    C                             d_m;    // const member function pointer
    bslalg::ConstructorProxy<D1>  d_d1;   // first embedded argument

    // NOT IMPLEMENTED
    Vfunc1C2(const Vfunc1C2<OBJ, C, A1, D1>&);
    Vfunc1C2<OBJ, C, A1, D1>& operator=(
                   const Vfunc1C2<OBJ, C, A1, D1>&);

  private:
    // PRIVATE CREATORS
    ~Vfunc1C2();
        // Destroy this functor.  Note that this destructor can be invoked only
        // indirectly, via the static 'deleteObject' method of the base class.

    // PRIVATE ACCESSORS
    void execute(const A1& argument1) const;
        // Invoke the underlying 'const' member function on the embedded object
        // with the specified 'argument1' along with the trailing argument
        // value specified at construction.

  public:
    // CREATORS
    Vfunc1C2(const OBJ&       object,
                   C                method,
                   const D1&        embeddedArg1,
                   bslma::Allocator*basicAllocator);
        // Create a representation for a function object (functor) taking one
        // argument and returning 'void', using the specified 'object' and the
        // specified 'const' 'method' of 'object' taking one additional
        // trailing argument, and the corresponding argument's specified
        // 'embeddedArg1' value.  Use the specified 'basicAllocator' to supply
        // memory.  Note that any value returned from 'method' will be ignored.
};

                        // ====================
                        // class Vfunc1C3
                        // ====================

template <class OBJ, class C, class A1, class D1, class D2>
class Vfunc1C3 : public bdlcfxxxr::Vfunc1<A1> {
    // This class defines the representation for a function object (functor),
    // characterized by a function-call operator taking one argument and
    // returning 'void', that holds an object and one of that object's 'const'
    // member functions taking two additional trailing arguments, and the
    // arguments' corresponding values.

    // DATA
    bslalg::ConstructorProxy<OBJ> d_obj;  // object
    C                             d_m;    // const member function pointer
    bslalg::ConstructorProxy<D1>  d_d1;   // first embedded argument
    bslalg::ConstructorProxy<D2>  d_d2;   // second embedded argument

    // NOT IMPLEMENTED
    Vfunc1C3(const Vfunc1C3<OBJ, C, A1, D1, D2>&);
    Vfunc1C3<OBJ, C, A1, D1, D2>& operator=(
                   const Vfunc1C3<OBJ, C, A1, D1, D2>&);

  private:
    // PRIVATE CREATORS
    ~Vfunc1C3();
        // Destroy this functor.  Note that this destructor can be invoked only
        // indirectly, via the static 'deleteObject' method of the base class.

    // PRIVATE ACCESSORS
    void execute(const A1& argument1) const;
        // Invoke the underlying 'const' member function on the embedded object
        // with the specified 'argument1' along with the sequence of two
        // trailing argument values specified at construction.

  public:
    // CREATORS
    Vfunc1C3(const OBJ&       object,
                   C                method,
                   const D1&        embeddedArg1,
                   const D2&        embeddedArg2,
                   bslma::Allocator*basicAllocator);
        // Create a representation for a function object (functor) taking one
        // argument and returning 'void', using the specified 'object' and the
        // specified 'const' 'method' of 'object' taking two additional
        // trailing arguments, and the corresponding arguments' specified
        // 'embeddedArg1' and 'embeddedArg2' values.  Use the specified
        // 'basicAllocator' to supply memory.  Note that any value returned
        // from 'method' will be ignored.
};

                        // ====================
                        // class Vfunc1C4
                        // ====================

template <class OBJ, class C, class A1, class D1, class D2, class D3>
class Vfunc1C4 : public bdlcfxxxr::Vfunc1<A1> {
    // This class defines the representation for a function object (functor),
    // characterized by a function-call operator taking one argument and
    // returning 'void', that holds an object and one of that object's 'const'
    // member functions taking three additional trailing arguments, and the
    // arguments' corresponding values.

    // DATA
    bslalg::ConstructorProxy<OBJ> d_obj;  // object
    C                             d_m;    // const member function pointer
    bslalg::ConstructorProxy<D1>  d_d1;   // first embedded argument
    bslalg::ConstructorProxy<D2>  d_d2;   // second embedded argument
    bslalg::ConstructorProxy<D3>  d_d3;   // third embedded argument

    // NOT IMPLEMENTED
    Vfunc1C4(const Vfunc1C4<OBJ, C, A1, D1, D2, D3>&);
    Vfunc1C4<OBJ, C, A1, D1, D2, D3>& operator=(
                   const Vfunc1C4<OBJ, C, A1, D1, D2, D3>&);

  private:
    // PRIVATE CREATORS
    ~Vfunc1C4();
        // Destroy this functor.  Note that this destructor can be invoked only
        // indirectly, via the static 'deleteObject' method of the base class.

    // PRIVATE ACCESSORS
    void execute(const A1& argument1) const;
        // Invoke the underlying 'const' member function on the embedded object
        // with the specified 'argument1' along with the sequence of three
        // trailing argument values specified at construction.

  public:
    // CREATORS
    Vfunc1C4(const OBJ&       object,
                   C                method,
                   const D1&        embeddedArg1,
                   const D2&        embeddedArg2,
                   const D3&        embeddedArg3,
                   bslma::Allocator*basicAllocator);
        // Create a representation for a function object (functor) taking one
        // argument and returning 'void', using the specified 'object' and the
        // specified 'const' 'method' of 'object' taking three additional
        // trailing arguments, and the corresponding arguments' specified
        // 'embeddedArg1', 'embeddedArg2', and 'embeddedArg3' values.  Use the
        // specified 'basicAllocator' to supply memory.  Note that any value
        // returned from 'method' will be ignored.
};

                        // ====================
                        // class Vfunc1C5
                        // ====================

template <class OBJ, class C, class A1, class D1, class D2, class D3,
          class D4>
class Vfunc1C5 : public bdlcfxxxr::Vfunc1<A1> {
    // This class defines the representation for a function object (functor),
    // characterized by a function-call operator taking one argument and
    // returning 'void', that holds an object and one of that object's 'const'
    // member functions taking four additional trailing arguments, and the
    // arguments' corresponding values.

    // DATA
    bslalg::ConstructorProxy<OBJ> d_obj;  // object
    C                             d_m;    // const member function pointer
    bslalg::ConstructorProxy<D1>  d_d1;   // first embedded argument
    bslalg::ConstructorProxy<D2>  d_d2;   // second embedded argument
    bslalg::ConstructorProxy<D3>  d_d3;   // third embedded argument
    bslalg::ConstructorProxy<D4>  d_d4;   // fourth embedded argument

    // NOT IMPLEMENTED
    Vfunc1C5(const Vfunc1C5<OBJ, C, A1, D1, D2, D3, D4>&);
    Vfunc1C5<OBJ, C, A1, D1, D2, D3, D4>& operator=(
                   const Vfunc1C5<OBJ, C, A1, D1, D2, D3, D4>&);

  private:
    // PRIVATE CREATORS
    ~Vfunc1C5();
        // Destroy this functor.  Note that this destructor can be invoked only
        // indirectly, via the static 'deleteObject' method of the base class.

    // PRIVATE ACCESSORS
    void execute(const A1& argument1) const;
        // Invoke the underlying 'const' member function on the embedded object
        // with the specified 'argument1' along with the sequence of four
        // trailing argument values specified at construction.

  public:
    // CREATORS
    Vfunc1C5(const OBJ&       object,
                   C                method,
                   const D1&        embeddedArg1,
                   const D2&        embeddedArg2,
                   const D3&        embeddedArg3,
                   const D4&        embeddedArg4,
                   bslma::Allocator*basicAllocator);
        // Create a representation for a function object (functor) taking one
        // argument and returning 'void', using the specified 'object' and the
        // specified 'const' 'method' of 'object' taking four additional
        // trailing arguments, and the corresponding arguments' specified
        // 'embeddedArg1', 'embeddedArg2', 'embeddedArg3', and 'embeddedArg4'
        // values.  Use the specified 'basicAllocator' to supply memory.  Note
        // that any value returned from 'method' will be ignored.
};

                        // ====================
                        // class Vfunc1C6
                        // ====================

template <class OBJ, class C, class A1, class D1, class D2, class D3,
          class D4, class D5>
class Vfunc1C6 : public bdlcfxxxr::Vfunc1<A1> {
    // This class defines the representation for a function object (functor),
    // characterized by a function-call operator taking one argument and
    // returning 'void', that holds an object and one of that object's 'const'
    // member functions taking five additional trailing arguments, and the
    // arguments' corresponding values.

    // DATA
    bslalg::ConstructorProxy<OBJ> d_obj;  // object
    C                             d_m;    // const member function pointer
    bslalg::ConstructorProxy<D1>  d_d1;   // first embedded argument
    bslalg::ConstructorProxy<D2>  d_d2;   // second embedded argument
    bslalg::ConstructorProxy<D3>  d_d3;   // third embedded argument
    bslalg::ConstructorProxy<D4>  d_d4;   // fourth embedded argument
    bslalg::ConstructorProxy<D5>  d_d5;   // fifth embedded argument

    // NOT IMPLEMENTED
    Vfunc1C6(const Vfunc1C6<OBJ, C, A1, D1, D2, D3, D4, D5>&);
    Vfunc1C6<OBJ, C, A1, D1, D2, D3, D4, D5>& operator=(
                   const Vfunc1C6<OBJ, C, A1, D1, D2, D3, D4, D5>&);

  private:
    // PRIVATE CREATORS
    ~Vfunc1C6();
        // Destroy this functor.  Note that this destructor can be invoked only
        // indirectly, via the static 'deleteObject' method of the base class.

    // PRIVATE ACCESSORS
    void execute(const A1& argument1) const;
        // Invoke the underlying 'const' member function on the embedded object
        // with the specified 'argument1' along with the sequence of five
        // trailing argument values specified at construction.

  public:
    // CREATORS
    Vfunc1C6(const OBJ&       object,
                   C                method,
                   const D1&        embeddedArg1,
                   const D2&        embeddedArg2,
                   const D3&        embeddedArg3,
                   const D4&        embeddedArg4,
                   const D5&        embeddedArg5,
                   bslma::Allocator*basicAllocator);
        // Create a representation for a function object (functor) taking one
        // argument and returning 'void', using the specified 'object' and the
        // specified 'const' 'method' of 'object' taking five additional
        // trailing arguments, and the corresponding arguments' specified
        // 'embeddedArg1', 'embeddedArg2', 'embeddedArg3', 'embeddedArg4', and
        // 'embeddedArg5' values.  Use the specified 'basicAllocator' to
        // supply memory.  Note that any value returned from 'method' will be
        // ignored.
};

                        // ====================
                        // class Vfunc1C7
                        // ====================

template <class OBJ, class C, class A1, class D1, class D2, class D3,
          class D4, class D5, class D6>
class Vfunc1C7 : public bdlcfxxxr::Vfunc1<A1> {
    // This class defines the representation for a function object (functor),
    // characterized by a function-call operator taking one argument and
    // returning 'void', that holds an object and one of that object's 'const'
    // member functions taking six additional trailing arguments, and the
    // arguments' corresponding values.

    // DATA
    bslalg::ConstructorProxy<OBJ> d_obj;  // object
    C                             d_m;    // const member function pointer
    bslalg::ConstructorProxy<D1>  d_d1;   // first embedded argument
    bslalg::ConstructorProxy<D2>  d_d2;   // second embedded argument
    bslalg::ConstructorProxy<D3>  d_d3;   // third embedded argument
    bslalg::ConstructorProxy<D4>  d_d4;   // fourth embedded argument
    bslalg::ConstructorProxy<D5>  d_d5;   // fifth embedded argument
    bslalg::ConstructorProxy<D6>  d_d6;   // sixth embedded argument

    // NOT IMPLEMENTED
    Vfunc1C7(const Vfunc1C7<OBJ, C, A1, D1, D2, D3, D4, D5, D6>&);
    Vfunc1C7<OBJ, C, A1, D1, D2, D3, D4, D5, D6>& operator=(
                   const Vfunc1C7<OBJ, C, A1, D1, D2, D3, D4, D5, D6>&);

  private:
    // PRIVATE CREATORS
    ~Vfunc1C7();
        // Destroy this functor.  Note that this destructor can be invoked only
        // indirectly, via the static 'deleteObject' method of the base class.

    // PRIVATE ACCESSORS
    void execute(const A1& argument1) const;
        // Invoke the underlying 'const' member function on the embedded object
        // with the specified 'argument1' along with the sequence of six
        // trailing argument values specified at construction.

  public:
    // CREATORS
    Vfunc1C7(const OBJ&       object,
                   C                method,
                   const D1&        embeddedArg1,
                   const D2&        embeddedArg2,
                   const D3&        embeddedArg3,
                   const D4&        embeddedArg4,
                   const D5&        embeddedArg5,
                   const D6&        embeddedArg6,
                   bslma::Allocator*basicAllocator);
        // Create a representation for a function object (functor) taking one
        // argument and returning 'void', using the specified 'object' and the
        // specified 'const' 'method' of 'object' taking six additional
        // trailing arguments, and the corresponding arguments' specified
        // 'embeddedArg1', 'embeddedArg2', 'embeddedArg3', 'embeddedArg4',
        // 'embeddedArg5', and 'embeddedArg6' values.  Use the specified
        // 'basicAllocator' to supply memory.  Note that any value returned
        // from 'method' will be ignored.
};

                        // ====================
                        // class Vfunc1C8
                        // ====================

template <class OBJ, class C, class A1, class D1, class D2, class D3,
          class D4, class D5, class D6, class D7>
class Vfunc1C8 : public bdlcfxxxr::Vfunc1<A1> {
    // This class defines the representation for a function object (functor),
    // characterized by a function-call operator taking one argument and
    // returning 'void', that holds an object and one of that object's 'const'
    // member functions taking seven additional trailing arguments, and the
    // arguments' corresponding values.

    // DATA
    bslalg::ConstructorProxy<OBJ> d_obj;  // object
    C                             d_m;    // const member function pointer
    bslalg::ConstructorProxy<D1>  d_d1;   // first embedded argument
    bslalg::ConstructorProxy<D2>  d_d2;   // second embedded argument
    bslalg::ConstructorProxy<D3>  d_d3;   // third embedded argument
    bslalg::ConstructorProxy<D4>  d_d4;   // fourth embedded argument
    bslalg::ConstructorProxy<D5>  d_d5;   // fifth embedded argument
    bslalg::ConstructorProxy<D6>  d_d6;   // sixth embedded argument
    bslalg::ConstructorProxy<D7>  d_d7;   // seventh embedded argument

    // NOT IMPLEMENTED
    Vfunc1C8(const Vfunc1C8<OBJ, C, A1, D1, D2, D3, D4, D5,
                                        D6, D7>&);
    Vfunc1C8<OBJ, C, A1, D1, D2, D3, D4, D5, D6, D7>& operator=(
                   const Vfunc1C8<OBJ, C, A1, D1, D2, D3, D4, D5,
                                        D6, D7>&);

  private:
    // PRIVATE CREATORS
    ~Vfunc1C8();
        // Destroy this functor.  Note that this destructor can be invoked only
        // indirectly, via the static 'deleteObject' method of the base class.

    // PRIVATE ACCESSORS
    void execute(const A1& argument1) const;
        // Invoke the underlying 'const' member function on the embedded object
        // with the specified 'argument1' along with the sequence of seven
        // trailing argument values specified at construction.

  public:
    // CREATORS
    Vfunc1C8(const OBJ&       object,
                   C                method,
                   const D1&        embeddedArg1,
                   const D2&        embeddedArg2,
                   const D3&        embeddedArg3,
                   const D4&        embeddedArg4,
                   const D5&        embeddedArg5,
                   const D6&        embeddedArg6,
                   const D7&        embeddedArg7,
                   bslma::Allocator*basicAllocator);
        // Create a representation for a function object (functor) taking one
        // argument and returning 'void', using the specified 'object' and the
        // specified 'const' 'method' of 'object' taking seven additional
        // trailing arguments, and the corresponding arguments' specified
        // 'embeddedArg1', 'embeddedArg2', 'embeddedArg3', 'embeddedArg4',
        // 'embeddedArg5', 'embeddedArg6', and 'embeddedArg7' values.  Use the
        // specified 'basicAllocator' to supply memory.  Note that any value
        // returned from 'method' will be ignored.
};

                        // ====================
                        // class Vfunc1C9
                        // ====================

template <class OBJ, class C, class A1, class D1, class D2, class D3,
          class D4, class D5, class D6, class D7, class D8>
class Vfunc1C9 : public bdlcfxxxr::Vfunc1<A1> {
    // This class defines the representation for a function object (functor),
    // characterized by a function-call operator taking one argument and
    // returning 'void', that holds an object and one of that object's 'const'
    // member functions taking eight additional trailing arguments, and the
    // arguments' corresponding values.

    // DATA
    bslalg::ConstructorProxy<OBJ> d_obj;  // object
    C                             d_m;    // const member function pointer
    bslalg::ConstructorProxy<D1>  d_d1;   // first embedded argument
    bslalg::ConstructorProxy<D2>  d_d2;   // second embedded argument
    bslalg::ConstructorProxy<D3>  d_d3;   // third embedded argument
    bslalg::ConstructorProxy<D4>  d_d4;   // fourth embedded argument
    bslalg::ConstructorProxy<D5>  d_d5;   // fifth embedded argument
    bslalg::ConstructorProxy<D6>  d_d6;   // sixth embedded argument
    bslalg::ConstructorProxy<D7>  d_d7;   // seventh embedded argument
    bslalg::ConstructorProxy<D8>  d_d8;   // eighth embedded argument

    // NOT IMPLEMENTED
    Vfunc1C9(const Vfunc1C9<OBJ, C, A1, D1, D2, D3, D4, D5,
                                        D6, D7, D8>&);
    Vfunc1C9<OBJ, C, A1, D1, D2, D3, D4, D5, D6, D7, D8>& operator=(
                   const Vfunc1C9<OBJ, C, A1, D1, D2, D3, D4, D5,
                                        D6, D7, D8>&);

  private:
    // PRIVATE CREATORS
    ~Vfunc1C9();
        // Destroy this functor.  Note that this destructor can be invoked only
        // indirectly, via the static 'deleteObject' method of the base class.

    // PRIVATE ACCESSORS
    void execute(const A1& argument1) const;
        // Invoke the underlying 'const' member function on the embedded object
        // with the specified 'argument1' along with the sequence of eight
        // trailing argument values specified at construction.

  public:
    // CREATORS
    Vfunc1C9(const OBJ&       object,
                   C                method,
                   const D1&        embeddedArg1,
                   const D2&        embeddedArg2,
                   const D3&        embeddedArg3,
                   const D4&        embeddedArg4,
                   const D5&        embeddedArg5,
                   const D6&        embeddedArg6,
                   const D7&        embeddedArg7,
                   const D8&        embeddedArg8,
                   bslma::Allocator*basicAllocator);
        // Create a representation for a function object (functor) taking one
        // argument and returning 'void', using the specified 'object' and the
        // specified 'const' 'method' of 'object' taking eight additional
        // trailing arguments, and the corresponding arguments' specified
        // 'embeddedArg1', 'embeddedArg2', 'embeddedArg3', 'embeddedArg4',
        // 'embeddedArg5', 'embeddedArg6', 'embeddedArg7' and 'embeddedArg8'
        // values.  Use the specified 'basicAllocator' to supply memory.  Note
        // that any value returned from 'method' will be ignored.
};

                        // ====================
                        // class Vfunc1M1
                        // ====================

template <class OBJ, class M, class A1>
class Vfunc1M1 : public bdlcfxxxr::Vfunc1<A1> {
    // This class defines the representation for a function object (functor),
    // characterized by a function-call operator taking one argument and
    // returning 'void', that holds the address of an object and one of that
    // object's ('const' or non-'const') member functions taking no additional
    // arguments.

    // DATA
    OBJ *d_obj_p;  // object
    M    d_m;      // member function pointer

    // NOT IMPLEMENTED
    Vfunc1M1(const Vfunc1M1<OBJ, M, A1>&);
    Vfunc1M1<OBJ, M, A1>& operator=(const Vfunc1M1<OBJ, M, A1>&);

  private:
    // PRIVATE CREATORS
    ~Vfunc1M1();
        // Destroy this functor.  Note that this destructor can be invoked only
        // indirectly, via the static 'deleteObject' method of the base class.

    // PRIVATE ACCESSORS
    void execute(const A1& argument1) const;
        // Invoke the underlying member function on the embedded object pointer
        // with the specified 'argument1' (and no additional arguments).

  public:
    // CREATORS
    Vfunc1M1(OBJ             *object,
                   M                method,
                   bslma::Allocator*basicAllocator);
        // Create a representation for a function object (functor) taking one
        // argument and returning 'void', using the specified 'object' address
        // and the specified ('const' or non-'const') 'method' of 'object'
        // taking no additional arguments.  Use the specified 'basicAllocator'
        // to supply memory.  Note that any value returned from 'method' will
        // be ignored.
};

                        // ====================
                        // class Vfunc1M2
                        // ====================

template <class OBJ, class M, class A1, class D1>
class Vfunc1M2 : public bdlcfxxxr::Vfunc1<A1> {
    // This class defines the representation for a function object (functor),
    // characterized by a function-call operator taking one argument and
    // returning 'void', that holds the address of an object and one of that
    // object's ('const' or non-'const') member functions taking one
    // additional trailing argument, and that argument's corresponding value.

    // DATA
    OBJ                          *d_obj_p;  // object
    M                             d_m;      // member function pointer
    bslalg::ConstructorProxy<D1>  d_d1;     // first embedded argument

    // NOT IMPLEMENTED
    Vfunc1M2(const Vfunc1M2<OBJ, M, A1, D1>&);
    Vfunc1M2<OBJ, M, A1, D1>& operator=(
                   const Vfunc1M2<OBJ, M, A1, D1>&);

  private:
    // PRIVATE CREATORS
    ~Vfunc1M2();
        // Destroy this functor.  Note that this destructor can be invoked only
        // indirectly, via the static 'deleteObject' method of the base class.

    // PRIVATE ACCESSORS
    void execute(const A1& argument1) const;
        // Invoke the underlying member function on the embedded object pointer
        // with the specified 'argument1' along with the additional trailing
        // argument value specified at construction.

  public:
    // CREATORS
    Vfunc1M2(OBJ             *object,
                   M                method,
                   const D1&        embeddedArg1,
                   bslma::Allocator*basicAllocator);
        // Create a representation for a function object (functor) taking one
        // argument and returning 'void', using the specified 'object' address
        // and the specified ('const' or non-'const') 'method' of 'object'
        // taking one additional trailing argument, and the corresponding
        // argument's specified 'embeddedArg1' value.  Use the specified
        // 'basicAllocator' to supply memory.  Note that any value returned
        // from 'method' will be ignored.
};

                        // ====================
                        // class Vfunc1M3
                        // ====================

template <class OBJ, class M, class A1, class D1, class D2>
class Vfunc1M3 : public bdlcfxxxr::Vfunc1<A1> {
    // This class defines the representation for a function object (functor),
    // characterized by a function-call operator taking one argument and
    // returning 'void', that holds the address of an object and one of that
    // object's ('const' or non-'const') member functions taking two
    // additional trailing arguments, and those arguments' corresponding
    // values.

    // DATA
    OBJ                          *d_obj_p;  // object
    M                             d_m;      // member function pointer
    bslalg::ConstructorProxy<D1>  d_d1;     // first embedded argument
    bslalg::ConstructorProxy<D2>  d_d2;     // second embedded argument

    // NOT IMPLEMENTED
    Vfunc1M3(const Vfunc1M3<OBJ, M, A1, D1, D2>&);
    Vfunc1M3<OBJ, M, A1, D1, D2>& operator=(
                   const Vfunc1M3<OBJ, M, A1, D1, D2>&);

  private:
    // PRIVATE CREATORS
    ~Vfunc1M3();
        // Destroy this functor.  Note that this destructor can be invoked only
        // indirectly, via the static 'deleteObject' method of the base class.

    // PRIVATE ACCESSORS
    void execute(const A1& argument1) const;
        // Invoke the underlying member function on the embedded object pointer
        // with the specified 'argument1' along with the sequence of two
        // argument values specified at construction.

  public:
    // CREATORS
    Vfunc1M3(OBJ             *object,
                   M                method,
                   const D1&        embeddedArg1,
                   const D2&        embeddedArg2,
                   bslma::Allocator*basicAllocator);
        // Create a representation for a function object (functor) taking one
        // argument and returning 'void', using the specified 'object' address
        // and the specified ('const' or non-'const') 'method' of 'object'
        // taking two additional trailing arguments, and the corresponding
        // arguments' specified 'embeddedArg1' and 'embeddedArg2' values.  Use
        // the specified 'basicAllocator' to supply memory.  Note that any
        // value returned from 'method' will be ignored.
};
                        // ====================
                        // class Vfunc1M4
                        // ====================

template <class OBJ, class M, class A1, class D1, class D2, class D3>
class Vfunc1M4 : public bdlcfxxxr::Vfunc1<A1> {
    // This class defines the representation for a function object (functor),
    // characterized by a function-call operator taking one argument and
    // returning 'void', that holds the address of an object and one of that
    // object's ('const' or non-'const') member functions taking three
    // additional trailing arguments, and those arguments' corresponding
    // values.

    // DATA
    OBJ                          *d_obj_p;  // object
    M                             d_m;      // member function pointer
    bslalg::ConstructorProxy<D1>  d_d1;     // first embedded argument
    bslalg::ConstructorProxy<D2>  d_d2;     // second embedded argument
    bslalg::ConstructorProxy<D3>  d_d3;     // third embedded argument

    // NOT IMPLEMENTED
    Vfunc1M4(const Vfunc1M4<OBJ, M, A1, D1, D2, D3>&);
    Vfunc1M4<OBJ, M, A1, D1, D2, D3>& operator=(
                   const Vfunc1M4<OBJ, M, A1, D1, D2, D3>&);

  private:
    // PRIVATE CREATORS
    ~Vfunc1M4();
        // Destroy this functor.  Note that this destructor can be invoked only
        // indirectly, via the static 'deleteObject' method of the base class.

    // PRIVATE ACCESSORS
    void execute(const A1& argument) const;
        // Invoke the underlying member function on the embedded object pointer
        // with the specified 'argument1' along with the sequence of three
        // argument values specified at construction.

  public:
    // CREATORS
    Vfunc1M4(OBJ             *object,
                   M                method,
                   const D1&        embeddedArg1,
                   const D2&        embeddedArg2,
                   const D3&        embeddedArg3,
                   bslma::Allocator*basicAllocator);
        // Create a representation for a function object (functor) taking one
        // argument and returning void, using the specified object address and
        // the specified ('const' or non-'const') 'method' of 'object' taking
        // three additional trailing arguments, and the corresponding
        // arguments' specified 'embeddedArg1', 'embeddedArg2', and
        // 'embeddedArg3' values.  Use the specified 'basicAllocator' to
        // supply memory.  Note that any value returned from 'method' will be
        // ignored.
};

                        // ====================
                        // class Vfunc1M5
                        // ====================

template <class OBJ, class M, class A1, class D1, class D2, class D3,
          class D4>
class Vfunc1M5 : public bdlcfxxxr::Vfunc1<A1> {
    // This class defines the representation for a function object (functor),
    // characterized by a function-call operator taking one argument and
    // returning 'void', that holds the address of an object and one of that
    // object's ('const' or non-'const') member functions taking four
    // additional trailing arguments, and those arguments' corresponding
    // values.

    // DATA
    OBJ                          *d_obj_p;  // object
    M                             d_m;      // member function pointer
    bslalg::ConstructorProxy<D1>  d_d1;     // first embedded argument
    bslalg::ConstructorProxy<D2>  d_d2;     // second embedded argument
    bslalg::ConstructorProxy<D3>  d_d3;     // third embedded argument
    bslalg::ConstructorProxy<D4>  d_d4;     // fourth embedded argument

    // NOT IMPLEMENTED
    Vfunc1M5(const Vfunc1M5<OBJ, M, A1, D1, D2, D3, D4>&);
    Vfunc1M5<OBJ, M, A1, D1, D2, D3, D4>& operator=(
                   const Vfunc1M5<OBJ, M, A1, D1, D2, D3, D4>&);

  private:
    // PRIVATE CREATORS
    ~Vfunc1M5();
        // Destroy this functor.  Note that this destructor can be invoked only
        // indirectly, via the static 'deleteObject' method of the base class.

    // PRIVATE ACCESSORS
    void execute(const A1& argument1) const;
        // Invoke the underlying 'const' member function on the embedded object
        // with the specified 'argument1' along with the sequence of four
        // trailing argument values specified at construction.

  public:
    // CREATORS
    Vfunc1M5(OBJ             *object,
                   M                method,
                   const D1&        embeddedArg1,
                   const D2&        embeddedArg2,
                   const D3&        embeddedArg3,
                   const D4&        embeddedArg4,
                   bslma::Allocator*basicAllocator);
        // Create a representation for a function object (functor) taking one
        // argument and returning 'void', using the specified 'object' address
        // and the specified ('const' or non-'const') 'method' of 'object'
        // 'method' taking four additional trailing arguments, and the
        // corresponding arguments' specified 'embeddedArg1', 'embeddedArg2',
        // 'embeddedArg3', and 'embeddedArg4' values.  Use the specified
        // 'basicAllocator' to supply memory.  Note that any value returned
        // from 'method' will be ignored.
};

                        // ====================
                        // class Vfunc1M6
                        // ====================

template <class OBJ, class M, class A1, class D1, class D2, class D3,
          class D4, class D5>
class Vfunc1M6 : public bdlcfxxxr::Vfunc1<A1> {
    // This class defines the representation for a function object (functor),
    // characterized by a function-call operator taking one argument and
    // returning 'void', that holds the address of an object and one of that
    // object's ('const' or non-'const') member functions taking five
    // additional trailing arguments, and those arguments' corresponding
    // values.

    // DATA
    OBJ                          *d_obj_p;  // object
    M                             d_m;      // member function pointer
    bslalg::ConstructorProxy<D1>  d_d1;     // first embedded argument
    bslalg::ConstructorProxy<D2>  d_d2;     // second embedded argument
    bslalg::ConstructorProxy<D3>  d_d3;     // third embedded argument
    bslalg::ConstructorProxy<D4>  d_d4;     // fourth embedded argument
    bslalg::ConstructorProxy<D5>  d_d5;     // fifth embedded argument

    // NOT IMPLEMENTED
    Vfunc1M6(const Vfunc1M6<OBJ, M, A1, D1, D2, D3, D4, D5>&);
    Vfunc1M6<OBJ, M, A1, D1, D2, D3, D4, D5>& operator=(
                   const Vfunc1M6<OBJ, M, A1, D1, D2, D3, D4, D5>&);

  private:
    // PRIVATE CREATORS
    ~Vfunc1M6();
        // Destroy this functor.  Note that this destructor can be invoked only
        // indirectly, via the static 'deleteObject' method of the base class.

    // PRIVATE ACCESSORS
    void execute(const A1& argument1) const;
        // Invoke the underlying 'const' member function on the embedded object
        // with the specified 'argument1' along with the sequence of five
        // trailing argument values specified at construction.

  public:
    // CREATORS
    Vfunc1M6(OBJ             *object,
                   M                method,
                   const D1&        embeddedArg1,
                   const D2&        embeddedArg2,
                   const D3&        embeddedArg3,
                   const D4&        embeddedArg4,
                   const D5&        embeddedArg5,
                   bslma::Allocator*basicAllocator);
        // Create a representation for a function object (functor) taking one
        // argument and returning 'void', using the specified 'object' address
        // and the specified ('const' or non-'const') 'method' of 'object'
        // taking five additional trailing arguments, and the corresponding
        // arguments' specified 'embeddedArg1', 'embeddedArg2',
        // 'embeddedArg3', 'embeddedArg4', and 'embeddedArg5' values.  Use the
        // specified 'basicAllocator' to supply memory.  Note that any value
        // returned from 'method' will be ignored.
};

                        // ====================
                        // class Vfunc1M7
                        // ====================

template <class OBJ, class M, class A1, class D1, class D2, class D3,
          class D4, class D5, class D6>
class Vfunc1M7 : public bdlcfxxxr::Vfunc1<A1> {
    // This class defines the representation for a function object (functor),
    // characterized by a function-call operator taking one argument and
    // returning 'void', that holds the address of an object and one of that
    // object's ('const' or non-'const') member functions taking six
    // additional trailing arguments, and those arguments' corresponding
    // values.

    // DATA
    OBJ                          *d_obj_p;  // object
    M                             d_m;      // member function pointer
    bslalg::ConstructorProxy<D1>  d_d1;     // first embedded argument
    bslalg::ConstructorProxy<D2>  d_d2;     // second embedded argument
    bslalg::ConstructorProxy<D3>  d_d3;     // third embedded argument
    bslalg::ConstructorProxy<D4>  d_d4;     // fourth embedded argument
    bslalg::ConstructorProxy<D5>  d_d5;     // fifth embedded argument
    bslalg::ConstructorProxy<D6>  d_d6;     // sixth embedded argument

    // NOT IMPLEMENTED
    Vfunc1M7(const Vfunc1M7<OBJ, M, A1, D1, D2, D3, D4, D5, D6>&);
    Vfunc1M7<OBJ, M, A1, D1, D2, D3, D4, D5, D6>& operator=(
                   const Vfunc1M7<OBJ, M, A1, D1, D2, D3, D4, D5, D6>&);

  private:
    // PRIVATE CREATORS
    ~Vfunc1M7();
        // Destroy this functor.  Note that this destructor can be invoked only
        // indirectly, via the static 'deleteObject' method of the base class.

    // PRIVATE ACCESSORS
    void execute(const A1& argument1) const;
        // Invoke the underlying 'const' member function on the embedded object
        // with the specified 'argument1' along with the sequence of six
        // trailing argument values specified at construction.

  public:
    // CREATORS
    Vfunc1M7(OBJ             *object,
                   M                method,
                   const D1&        embeddedArg1,
                   const D2&        embeddedArg2,
                   const D3&        embeddedArg3,
                   const D4&        embeddedArg4,
                   const D5&        embeddedArg5,
                   const D6&        embeddedArg6,
                   bslma::Allocator*basicAllocator);
        // Create a representation for a function object (functor) taking one
        // argument and returning 'void', using the specified 'object' address
        // and the specified ('const' or non-'const') 'method' of 'object'
        // taking six additional trailing arguments, and the corresponding
        // arguments' specified 'embeddedArg1', 'embeddedArg2',
        // 'embeddedArg3', 'embeddedArg4', 'embeddedArg5', and 'embeddedArg6'
        // values.  Use the specified 'basicAllocator' to supply memory.  Note
        // that any value returned from 'method' will be ignored.
};

                        // ====================
                        // class Vfunc1M8
                        // ====================

template <class OBJ, class M, class A1, class D1, class D2, class D3,
          class D4, class D5, class D6, class D7>
class Vfunc1M8 : public bdlcfxxxr::Vfunc1<A1> {
    // This class defines the representation for a function object (functor),
    // characterized by a function-call operator taking one argument and
    // returning 'void', that holds the address of an object and one of that
    // object's ('const' or non-'const') member functions taking seven
    // additional trailing arguments, and those arguments' corresponding
    // values.

    // DATA
    OBJ                          *d_obj_p;  // object
    M                             d_m;      // member function pointer
    bslalg::ConstructorProxy<D1>  d_d1;     // first embedded argument
    bslalg::ConstructorProxy<D2>  d_d2;     // second embedded argument
    bslalg::ConstructorProxy<D3>  d_d3;     // third embedded argument
    bslalg::ConstructorProxy<D4>  d_d4;     // fourth embedded argument
    bslalg::ConstructorProxy<D5>  d_d5;     // fifth embedded argument
    bslalg::ConstructorProxy<D6>  d_d6;     // sixth embedded argument
    bslalg::ConstructorProxy<D7>  d_d7;     // seventh embedded argument

    // NOT IMPLEMENTED
    Vfunc1M8(const Vfunc1M8<OBJ, M, A1, D1, D2, D3, D4, D5,
                                        D6, D7>&);
    Vfunc1M8<OBJ, M, A1, D1, D2, D3, D4, D5, D6, D7>& operator=(
                   const Vfunc1M8<OBJ, M, A1, D1, D2, D3, D4, D5,
                                        D6, D7>&);

  private:
    // PRIVATE CREATORS
    ~Vfunc1M8();
        // Destroy this functor.  Note that this destructor can be invoked only
        // indirectly, via the static 'deleteObject' method of the base class.

    // PRIVATE ACCESSORS
    void execute(const A1& argument1) const;
        // Invoke the underlying 'const' member function on the embedded object
        // with the specified 'argument1' along with the sequence of seven
        // trailing argument values specified at construction.

  public:
    // CREATORS
    Vfunc1M8(OBJ             *object,
                   M                method,
                   const D1&        embeddedArg1,
                   const D2&        embeddedArg2,
                   const D3&        embeddedArg3,
                   const D4&        embeddedArg4,
                   const D5&        embeddedArg5,
                   const D6&        embeddedArg6,
                   const D7&        embeddedArg7,
                   bslma::Allocator*basicAllocator);
        // Create a representation for a function object (functor) taking one
        // argument and returning 'void', using the specified 'object' address
        // and the specified ('const' or non-'const') 'method' of 'object'
        // 'method' taking seven additional trailing arguments, and the
        // corresponding arguments' specified 'embeddedArg1', 'embeddedArg2',
        // 'embeddedArg3', 'embeddedArg4', 'embeddedArg5', 'embeddedArg6', and
        // 'embeddedArg7' values.  Use the specified 'basicAllocator' to
        // supply memory.  Note that any value returned from 'method' will be
        // ignored.
};

                        // ====================
                        // class Vfunc1M9
                        // ====================

template <class OBJ, class M, class A1, class D1, class D2, class D3,
          class D4, class D5, class D6, class D7, class D8>
class Vfunc1M9 : public bdlcfxxxr::Vfunc1<A1> {
    // This class defines the representation for a function object (functor),
    // characterized by a function-call operator taking one argument and
    // returning 'void', that holds the address of an object and one of that
    // object's ('const' or non-'const') member functions taking eight
    // additional trailing arguments, and those arguments' corresponding
    // values.

    // DATA
    OBJ                          *d_obj_p;  // object
    M                             d_m;      // member function pointer
    bslalg::ConstructorProxy<D1>  d_d1;     // first embedded argument
    bslalg::ConstructorProxy<D2>  d_d2;     // second embedded argument
    bslalg::ConstructorProxy<D3>  d_d3;     // third embedded argument
    bslalg::ConstructorProxy<D4>  d_d4;     // fourth embedded argument
    bslalg::ConstructorProxy<D5>  d_d5;     // fifth embedded argument
    bslalg::ConstructorProxy<D6>  d_d6;     // sixth embedded argument
    bslalg::ConstructorProxy<D7>  d_d7;     // seventh embedded argument
    bslalg::ConstructorProxy<D8>  d_d8;     // eighth embedded argument

    // NOT IMPLEMENTED
    Vfunc1M9(const Vfunc1M9<OBJ, M, A1, D1, D2, D3, D4, D5,
                                        D6, D7, D8>&);
    Vfunc1M9<OBJ, M, A1, D1, D2, D3, D4, D5, D6, D7, D8>& operator=(
                   const Vfunc1M9<OBJ, M, A1, D1, D2, D3, D4, D5,
                                        D6, D7, D8>&);

  private:
    // PRIVATE CREATORS
    ~Vfunc1M9();
        // Destroy this functor.  Note that this destructor can be invoked only
        // indirectly, via the static 'deleteObject' method of the base class.

    // PRIVATE ACCESSORS
    void execute(const A1& argument1) const;
        // Invoke the underlying 'const' member function on the embedded object
        // with the specified 'argument1' along with the sequence of eight
        // trailing argument values specified at construction.

  public:
    // CREATORS
    Vfunc1M9(OBJ             *object,
                   M                method,
                   const D1&        embeddedArg1,
                   const D2&        embeddedArg2,
                   const D3&        embeddedArg3,
                   const D4&        embeddedArg4,
                   const D5&        embeddedArg5,
                   const D6&        embeddedArg6,
                   const D7&        embeddedArg7,
                   const D8&        embeddedArg8,
                   bslma::Allocator*basicAllocator);
        // Create a representation for a function object (functor) taking one
        // argument and returning 'void', using the specified 'object' address
        // and the specified ('const' or non-'const') 'method' of 'object'
        // 'method' taking eight additional trailing arguments, and the
        // corresponding arguments' specified 'embeddedArg1', 'embeddedArg2',
        // 'embeddedArg3', 'embeddedArg4', 'embeddedArg5', 'embeddedArg6',
        // 'embeddedArg7', and 'embeddedArg8' values.  Use the specified
        // 'basicAllocator' to supply memory.  Note that any value returned
        // from 'method' will be ignored.
};

// ============================================================================
//                      INLINE FUNCTION DEFINITIONS
// ============================================================================

                        // ----------------------
                        // class Vfunc1Null
                        // ----------------------

template <class A1>
inline
Vfunc1Null<A1>::Vfunc1Null(bslma::Allocator*basicAllocator)
: bdlcfxxxr::Vfunc1<A1>(basicAllocator)
{
}

template <class A1>
inline
Vfunc1Null<A1>::~Vfunc1Null()
{
}

template <class A1>
inline
void Vfunc1Null<A1>::execute(const A1&) const
{
}

                        // --------------------
                        // class Vfunc1F1
                        // --------------------

template <class F, class A1>
inline
Vfunc1F1<F, A1>::Vfunc1F1(F                procedure,
                                      bslma::Allocator*basicAllocator)
: bdlcfxxxr::Vfunc1<A1>(basicAllocator)
, d_f(procedure, basicAllocator)
{
}

template <class F, class A1>
inline
Vfunc1F1<F, A1>::~Vfunc1F1()
{
}

template <class F, class A1>
inline
void Vfunc1F1<F, A1>::execute(const A1& argument1) const
{
    const F& f = d_f.object();  // for readability
    f(argument1);
}

                        // --------------------
                        // class Vfunc1F2
                        // --------------------

template <class F, class A1, class D1>
inline
Vfunc1F2<F, A1, D1>::Vfunc1F2(F                procedure,
                                          const D1&        embeddedArg1,
                                          bslma::Allocator*basicAllocator)
: bdlcfxxxr::Vfunc1<A1>(basicAllocator)
, d_f(procedure, basicAllocator)
, d_d1(embeddedArg1, basicAllocator)
{
}

template <class F, class A1, class D1>
inline
Vfunc1F2<F, A1, D1>::~Vfunc1F2()
{
}

template <class F, class A1, class D1>
inline
void Vfunc1F2<F, A1, D1>::execute(const A1& argument1) const
{
    const F& f = d_f.object();  // for readability
    f(argument1, d_d1.object());
}

                        // --------------------
                        // class Vfunc1F3
                        // --------------------

template <class F, class A1, class D1, class D2>
inline
Vfunc1F3<F, A1, D1, D2>::Vfunc1F3(F                procedure,
                                              const D1&        embeddedArg1,
                                              const D2&        embeddedArg2,
                                              bslma::Allocator*basicAllocator)
: bdlcfxxxr::Vfunc1<A1>(basicAllocator)
, d_f(procedure, basicAllocator)
, d_d1(embeddedArg1, basicAllocator)
, d_d2(embeddedArg2, basicAllocator)
{
}

template <class F, class A1, class D1, class D2>
inline
Vfunc1F3<F, A1, D1, D2>::~Vfunc1F3()
{
}

template <class F, class A1, class D1, class D2>
inline
void Vfunc1F3<F, A1, D1, D2>::execute(const A1& argument1) const
{
    const F& f = d_f.object();  // for readability
    f(argument1, d_d1.object(), d_d2.object());
}

                        // --------------------
                        // class Vfunc1F4
                        // --------------------

template <class F, class A1, class D1, class D2, class D3>
inline
Vfunc1F4<F, A1, D1, D2, D3>::Vfunc1F4(
                                             F                procedure,
                                             const D1&        embeddedArg1,
                                             const D2&        embeddedArg2,
                                             const D3&        embeddedArg3,
                                             bslma::Allocator*basicAllocator)
: bdlcfxxxr::Vfunc1<A1>(basicAllocator)
, d_f(procedure, basicAllocator)
, d_d1(embeddedArg1, basicAllocator)
, d_d2(embeddedArg2, basicAllocator)
, d_d3(embeddedArg3, basicAllocator)
{
}

template <class F, class A1, class D1, class D2, class D3>
inline
Vfunc1F4<F, A1, D1, D2, D3>::~Vfunc1F4()
{
}

template <class F, class A1, class D1, class D2, class D3>
inline
void Vfunc1F4<F, A1, D1, D2, D3>::execute(const A1& argument1) const
{
    const F& f = d_f.object();  // for readability
    f(argument1, d_d1.object(), d_d2.object(), d_d3.object());
}

                        // --------------------
                        // class Vfunc1F5
                        // --------------------

template <class F, class A1, class D1, class D2, class D3, class D4>
inline
Vfunc1F5<F, A1, D1, D2, D3, D4>::Vfunc1F5(
                                             F                procedure,
                                             const D1&        embeddedArg1,
                                             const D2&        embeddedArg2,
                                             const D3&        embeddedArg3,
                                             const D4&        embeddedArg4,
                                             bslma::Allocator*basicAllocator)
: bdlcfxxxr::Vfunc1<A1>(basicAllocator)
, d_f(procedure, basicAllocator)
, d_d1(embeddedArg1, basicAllocator)
, d_d2(embeddedArg2, basicAllocator)
, d_d3(embeddedArg3, basicAllocator)
, d_d4(embeddedArg4, basicAllocator)
{
}

template <class F, class A1, class D1, class D2, class D3, class D4>
inline
Vfunc1F5<F, A1, D1, D2, D3, D4>::~Vfunc1F5()
{
}

template <class F, class A1, class D1, class D2, class D3, class D4>
inline
void Vfunc1F5<F, A1, D1, D2, D3, D4>::execute(const A1& argument1) const
{
    const F& f = d_f.object();  // for readability
    f(argument1, d_d1.object(), d_d2.object(), d_d3.object(), d_d4.object());
}

                        // --------------------
                        // class Vfunc1F6
                        // --------------------

template <class F, class A1, class D1, class D2, class D3, class D4,
          class D5>
inline
Vfunc1F6<F, A1, D1, D2, D3, D4, D5>::Vfunc1F6(
                                             F                procedure,
                                             const D1&        embeddedArg1,
                                             const D2&        embeddedArg2,
                                             const D3&        embeddedArg3,
                                             const D4&        embeddedArg4,
                                             const D5&        embeddedArg5,
                                             bslma::Allocator*basicAllocator)
: bdlcfxxxr::Vfunc1<A1>(basicAllocator)
, d_f(procedure, basicAllocator)
, d_d1(embeddedArg1, basicAllocator)
, d_d2(embeddedArg2, basicAllocator)
, d_d3(embeddedArg3, basicAllocator)
, d_d4(embeddedArg4, basicAllocator)
, d_d5(embeddedArg5, basicAllocator)
{
}

template <class F, class A1, class D1, class D2, class D3, class D4,
          class D5>
inline
Vfunc1F6<F, A1, D1, D2, D3, D4, D5>::~Vfunc1F6()
{
}

template <class F, class A1, class D1, class D2, class D3, class D4, class D5>
inline
void Vfunc1F6<F, A1, D1, D2, D3, D4, D5>::execute(
                                                   const A1& argument1) const
{
    const F& f = d_f.object();  // for readability
    f(argument1, d_d1.object(), d_d2.object(), d_d3.object(), d_d4.object(),
                 d_d5.object());
}

                        // --------------------
                        // class Vfunc1F7
                        // --------------------

template <class F, class A1, class D1, class D2, class D3, class D4,
          class D5, class D6>
inline
Vfunc1F7<F, A1, D1, D2, D3, D4, D5, D6>::Vfunc1F7(
                                             F                procedure,
                                             const D1&        embeddedArg1,
                                             const D2&        embeddedArg2,
                                             const D3&        embeddedArg3,
                                             const D4&        embeddedArg4,
                                             const D5&        embeddedArg5,
                                             const D6&        embeddedArg6,
                                             bslma::Allocator*basicAllocator)
: bdlcfxxxr::Vfunc1<A1>(basicAllocator)
, d_f(procedure, basicAllocator)
, d_d1(embeddedArg1, basicAllocator)
, d_d2(embeddedArg2, basicAllocator)
, d_d3(embeddedArg3, basicAllocator)
, d_d4(embeddedArg4, basicAllocator)
, d_d5(embeddedArg5, basicAllocator)
, d_d6(embeddedArg6, basicAllocator)
{
}

template <class F, class A1, class D1, class D2, class D3, class D4,
          class D5, class D6>
inline
Vfunc1F7<F, A1, D1, D2, D3, D4, D5, D6>::~Vfunc1F7()
{
}

template <class F, class A1, class D1, class D2, class D3,
          class D4, class D5, class D6>
inline
void Vfunc1F7<F, A1, D1, D2, D3, D4, D5, D6>::execute(
                                                   const A1& argument1) const
{
    const F& f = d_f.object();  // for readability
    f(argument1, d_d1.object(), d_d2.object(), d_d3.object(), d_d4.object(),
                 d_d5.object(), d_d6.object());
}

                        // --------------------
                        // class Vfunc1F8
                        // --------------------

template <class F, class A1, class D1, class D2, class D3, class D4,
          class D5, class D6, class D7>
inline
Vfunc1F8<F, A1, D1, D2, D3, D4, D5, D6, D7>::Vfunc1F8(
                                             F                procedure,
                                             const D1&        embeddedArg1,
                                             const D2&        embeddedArg2,
                                             const D3&        embeddedArg3,
                                             const D4&        embeddedArg4,
                                             const D5&        embeddedArg5,
                                             const D6&        embeddedArg6,
                                             const D7&        embeddedArg7,
                                             bslma::Allocator*basicAllocator)
: bdlcfxxxr::Vfunc1<A1>(basicAllocator)
, d_f(procedure, basicAllocator)
, d_d1(embeddedArg1, basicAllocator)
, d_d2(embeddedArg2, basicAllocator)
, d_d3(embeddedArg3, basicAllocator)
, d_d4(embeddedArg4, basicAllocator)
, d_d5(embeddedArg5, basicAllocator)
, d_d6(embeddedArg6, basicAllocator)
, d_d7(embeddedArg7, basicAllocator)
{
}

template <class F, class A1, class D1, class D2, class D3, class D4,
          class D5, class D6, class D7>
inline
Vfunc1F8<F, A1, D1, D2, D3, D4, D5, D6, D7>::~Vfunc1F8()
{
}

template <class F, class A1, class D1, class D2, class D3,
          class D4, class D5, class D6, class D7>
inline
void Vfunc1F8<F, A1, D1, D2, D3, D4, D5, D6, D7>::execute(
                                                   const A1& argument1) const
{
    const F& f = d_f.object();  // for readability
    f(argument1, d_d1.object(), d_d2.object(), d_d3.object(), d_d4.object(),
                 d_d5.object(), d_d6.object(), d_d7.object());
}

                        // --------------------
                        // class Vfunc1F9
                        // --------------------

template <class F, class A1, class D1, class D2, class D3, class D4,
          class D5, class D6, class D7, class D8>
inline
Vfunc1F9<F, A1, D1, D2, D3, D4, D5, D6, D7, D8>::Vfunc1F9(
                                             F                procedure,
                                             const D1&        embeddedArg1,
                                             const D2&        embeddedArg2,
                                             const D3&        embeddedArg3,
                                             const D4&        embeddedArg4,
                                             const D5&        embeddedArg5,
                                             const D6&        embeddedArg6,
                                             const D7&        embeddedArg7,
                                             const D8&        embeddedArg8,
                                             bslma::Allocator*basicAllocator)
: bdlcfxxxr::Vfunc1<A1>(basicAllocator)
, d_f(procedure, basicAllocator)
, d_d1(embeddedArg1, basicAllocator)
, d_d2(embeddedArg2, basicAllocator)
, d_d3(embeddedArg3, basicAllocator)
, d_d4(embeddedArg4, basicAllocator)
, d_d5(embeddedArg5, basicAllocator)
, d_d6(embeddedArg6, basicAllocator)
, d_d7(embeddedArg7, basicAllocator)
, d_d8(embeddedArg8, basicAllocator)
{
}

template <class F, class A1, class D1, class D2, class D3, class D4,
          class D5, class D6, class D7, class D8>
inline
Vfunc1F9<F, A1, D1, D2, D3, D4, D5, D6, D7, D8>::~Vfunc1F9()
{
}

template <class F, class A1, class D1, class D2, class D3,
          class D4, class D5, class D6, class D7, class D8>
inline
void Vfunc1F9<F, A1, D1, D2, D3, D4, D5, D6, D7, D8>::execute(
                                                   const A1& argument1) const
{
    const F& f = d_f.object();  // for readability
    f(argument1, d_d1.object(), d_d2.object(), d_d3.object(), d_d4.object(),
                 d_d5.object(), d_d6.object(), d_d7.object(), d_d8.object());
}

                        // --------------------
                        // class Vfunc1C1
                        // --------------------

template <class OBJ, class C, class A1>
inline
Vfunc1C1<OBJ, C, A1>::Vfunc1C1(const OBJ&       object,
                                           C                method,
                                           bslma::Allocator*basicAllocator)
: bdlcfxxxr::Vfunc1<A1>(basicAllocator)
, d_obj(object, basicAllocator)
, d_m(method)
{
}

template <class OBJ, class C, class A1>
inline
Vfunc1C1<OBJ, C, A1>::~Vfunc1C1()
{
}

template <class OBJ, class C, class A1>
inline
void Vfunc1C1<OBJ, C, A1>::execute(const A1& argument1) const
{
    const OBJ& obj = d_obj.object();  // for readability
    (obj.*d_m)(argument1);
}

                        // --------------------
                        // class Vfunc1C2
                        // --------------------

template <class OBJ, class C, class A1, class D1>
inline
Vfunc1C2<OBJ, C, A1, D1>::Vfunc1C2(const OBJ&       object,
                                               C                method,
                                               const D1&        embeddedArg1,
                                               bslma::Allocator*basicAllocator)
: bdlcfxxxr::Vfunc1<A1>(basicAllocator)
, d_obj(object, basicAllocator)
, d_m(method)
, d_d1(embeddedArg1, basicAllocator)
{
}

template <class OBJ, class C, class A1, class D1>
inline
Vfunc1C2<OBJ, C, A1, D1>::~Vfunc1C2()
{
}

template <class OBJ, class C, class A1, class D1>
inline
void Vfunc1C2<OBJ, C, A1, D1>::execute(const A1& argument1) const
{
    const OBJ& obj = d_obj.object();  // for readability
    (obj.*d_m)(argument1, d_d1.object());
}

                        // --------------------
                        // class Vfunc1C3
                        // --------------------

template <class OBJ, class C, class A1, class D1, class D2>
inline
Vfunc1C3<OBJ, C, A1, D1, D2>::Vfunc1C3(
                                             const OBJ&       object,
                                             C                method,
                                             const D1&        embeddedArg1,
                                             const D2&        embeddedArg2,
                                             bslma::Allocator*basicAllocator)
: bdlcfxxxr::Vfunc1<A1>(basicAllocator)
, d_obj(object, basicAllocator)
, d_m(method)
, d_d1(embeddedArg1, basicAllocator)
, d_d2(embeddedArg2, basicAllocator)
{
}

template <class OBJ, class C, class A1, class D1, class D2>
inline
Vfunc1C3<OBJ, C, A1, D1, D2>::~Vfunc1C3()
{
}

template <class OBJ, class C, class A1, class D1, class D2>
inline
void Vfunc1C3<OBJ, C, A1, D1, D2>::execute(const A1& argument1) const
{
    const OBJ& obj = d_obj.object();  // for readability
    (obj.*d_m)(argument1, d_d1.object(), d_d2.object());
}

                        // --------------------
                        // class Vfunc1C4
                        // --------------------

template <class OBJ, class C, class A1, class D1, class D2, class D3>
inline
Vfunc1C4<OBJ, C, A1, D1, D2, D3>::Vfunc1C4(
                                             const OBJ&       object,
                                             C                method,
                                             const D1&        embeddedArg1,
                                             const D2&        embeddedArg2,
                                             const D3&        embeddedArg3,
                                             bslma::Allocator*basicAllocator)
: bdlcfxxxr::Vfunc1<A1>(basicAllocator)
, d_obj(object, basicAllocator)
, d_m(method)
, d_d1(embeddedArg1, basicAllocator)
, d_d2(embeddedArg2, basicAllocator)
, d_d3(embeddedArg3, basicAllocator)
{
}

template <class OBJ, class C, class A1, class D1, class D2, class D3>
inline
Vfunc1C4<OBJ, C, A1, D1, D2, D3>::~Vfunc1C4()
{
}

template <class OBJ, class C, class A1, class D1, class D2, class D3>
inline
void Vfunc1C4<OBJ, C, A1, D1, D2, D3>::execute(
                                                   const A1& argument1) const
{
    const OBJ& obj = d_obj.object();  // for readability
    (obj.*d_m)(argument1, d_d1.object(), d_d2.object(), d_d3.object());
}

                        // --------------------
                        // class Vfunc1C5
                        // --------------------

template <class OBJ, class C, class A1, class D1, class D2, class D3,
          class D4>
inline
Vfunc1C5<OBJ, C, A1, D1, D2, D3, D4>::Vfunc1C5(
                                             const OBJ&       object,
                                             C                method,
                                             const D1&        embeddedArg1,
                                             const D2&        embeddedArg2,
                                             const D3&        embeddedArg3,
                                             const D4&        embeddedArg4,
                                             bslma::Allocator*basicAllocator)
: bdlcfxxxr::Vfunc1<A1>(basicAllocator)
, d_obj(object, basicAllocator)
, d_m(method)
, d_d1(embeddedArg1, basicAllocator)
, d_d2(embeddedArg2, basicAllocator)
, d_d3(embeddedArg3, basicAllocator)
, d_d4(embeddedArg4, basicAllocator)
{
}

template <class OBJ, class C, class A1, class D1, class D2, class D3,
          class D4>
inline
Vfunc1C5<OBJ, C, A1, D1, D2, D3, D4>::~Vfunc1C5()
{
}

template <class OBJ, class C, class A1, class D1, class D2, class D3, class D4>
inline
void Vfunc1C5<OBJ, C, A1, D1, D2, D3, D4>::execute(
                                                   const A1& argument1) const
{
    const OBJ& obj = d_obj.object();  // for readability
    (obj.*d_m)(argument1, d_d1.object(), d_d2.object(), d_d3.object(),
                          d_d4.object());
}

                        // --------------------
                        // class Vfunc1C6
                        // --------------------

template <class OBJ, class C, class A1, class D1, class D2, class D3,
          class D4, class D5>
inline
Vfunc1C6<OBJ, C, A1, D1, D2, D3, D4, D5>::Vfunc1C6(
                                             const OBJ&       object,
                                             C                method,
                                             const D1&        embeddedArg1,
                                             const D2&        embeddedArg2,
                                             const D3&        embeddedArg3,
                                             const D4&        embeddedArg4,
                                             const D5&        embeddedArg5,
                                             bslma::Allocator*basicAllocator)
: bdlcfxxxr::Vfunc1<A1>(basicAllocator)
, d_obj(object, basicAllocator)
, d_m(method)
, d_d1(embeddedArg1, basicAllocator)
, d_d2(embeddedArg2, basicAllocator)
, d_d3(embeddedArg3, basicAllocator)
, d_d4(embeddedArg4, basicAllocator)
, d_d5(embeddedArg5, basicAllocator)
{
}

template <class OBJ, class C, class A1, class D1, class D2, class D3,
          class D4, class D5>
inline
Vfunc1C6<OBJ, C, A1, D1, D2, D3, D4, D5>::~Vfunc1C6()
{
}

template <class OBJ, class C, class A1, class D1, class D2, class D3,
          class D4, class D5>
inline
void Vfunc1C6<OBJ, C, A1, D1, D2, D3, D4, D5>::execute(
                                                   const A1& argument1) const
{
    const OBJ& obj = d_obj.object();  // for readability
    (obj.*d_m)(argument1, d_d1.object(), d_d2.object(), d_d3.object(),
                          d_d4.object(), d_d5.object());
}

                        // --------------------
                        // class Vfunc1C7
                        // --------------------

template <class OBJ, class C, class A1, class D1, class D2, class D3,
          class D4, class D5, class D6>
inline
Vfunc1C7<OBJ, C, A1, D1, D2, D3, D4, D5, D6>::Vfunc1C7(
                                             const OBJ&       object,
                                             C                method,
                                             const D1&        embeddedArg1,
                                             const D2&        embeddedArg2,
                                             const D3&        embeddedArg3,
                                             const D4&        embeddedArg4,
                                             const D5&        embeddedArg5,
                                             const D6&        embeddedArg6,
                                             bslma::Allocator*basicAllocator)
: bdlcfxxxr::Vfunc1<A1>(basicAllocator)
, d_obj(object, basicAllocator)
, d_m(method)
, d_d1(embeddedArg1, basicAllocator)
, d_d2(embeddedArg2, basicAllocator)
, d_d3(embeddedArg3, basicAllocator)
, d_d4(embeddedArg4, basicAllocator)
, d_d5(embeddedArg5, basicAllocator)
, d_d6(embeddedArg6, basicAllocator)
{
}

template <class OBJ, class C, class A1, class D1, class D2, class D3,
          class D4, class D5, class D6>
inline
Vfunc1C7<OBJ, C, A1, D1, D2, D3, D4, D5, D6>::~Vfunc1C7()
{
}

template <class OBJ, class C, class A1, class D1, class D2, class D3,
          class D4, class D5, class D6>
inline
void Vfunc1C7<OBJ, C, A1, D1, D2, D3, D4, D5, D6>::execute(
                                                   const A1& argument1) const
{
    const OBJ& obj = d_obj.object();  // for readability
    (obj.*d_m)(argument1, d_d1.object(), d_d2.object(), d_d3.object(),
                          d_d4.object(), d_d5.object(), d_d6.object());
}

                        // --------------------
                        // class Vfunc1C8
                        // --------------------

template <class OBJ, class C, class A1, class D1, class D2, class D3,
          class D4, class D5, class D6, class D7>
inline
Vfunc1C8<OBJ, C, A1, D1, D2, D3, D4, D5, D6, D7>::Vfunc1C8(
                                             const OBJ&       object,
                                             C                method,
                                             const D1&        embeddedArg1,
                                             const D2&        embeddedArg2,
                                             const D3&        embeddedArg3,
                                             const D4&        embeddedArg4,
                                             const D5&        embeddedArg5,
                                             const D6&        embeddedArg6,
                                             const D7&        embeddedArg7,
                                             bslma::Allocator*basicAllocator)
: bdlcfxxxr::Vfunc1<A1>(basicAllocator)
, d_obj(object, basicAllocator)
, d_m(method)
, d_d1(embeddedArg1, basicAllocator)
, d_d2(embeddedArg2, basicAllocator)
, d_d3(embeddedArg3, basicAllocator)
, d_d4(embeddedArg4, basicAllocator)
, d_d5(embeddedArg5, basicAllocator)
, d_d6(embeddedArg6, basicAllocator)
, d_d7(embeddedArg7, basicAllocator)
{
}

template <class OBJ, class C, class A1, class D1, class D2, class D3,
          class D4, class D5, class D6, class D7>
inline
Vfunc1C8<OBJ, C, A1, D1, D2, D3, D4, D5, D6, D7>::~Vfunc1C8()
{
}

template <class OBJ, class C, class A1, class D1, class D2, class D3,
          class D4, class D5, class D6, class D7>
inline
void Vfunc1C8<OBJ, C, A1, D1, D2, D3, D4, D5, D6, D7>::execute(
                                                   const A1& argument1) const
{
    const OBJ& obj = d_obj.object();  // for readability
    (obj.*d_m)(argument1, d_d1.object(), d_d2.object(), d_d3.object(),
                          d_d4.object(), d_d5.object(), d_d6.object(),
                          d_d7.object());
}

                        // --------------------
                        // class Vfunc1C9
                        // --------------------

template <class OBJ, class C, class A1, class D1, class D2, class D3,
          class D4, class D5, class D6, class D7, class D8>
inline
Vfunc1C9<OBJ, C, A1, D1, D2, D3, D4, D5, D6, D7, D8>::Vfunc1C9(
                                             const OBJ&       object,
                                             C                method,
                                             const D1&        embeddedArg1,
                                             const D2&        embeddedArg2,
                                             const D3&        embeddedArg3,
                                             const D4&        embeddedArg4,
                                             const D5&        embeddedArg5,
                                             const D6&        embeddedArg6,
                                             const D7&        embeddedArg7,
                                             const D8&        embeddedArg8,
                                             bslma::Allocator*basicAllocator)
: bdlcfxxxr::Vfunc1<A1>(basicAllocator)
, d_obj(object, basicAllocator)
, d_m(method)
, d_d1(embeddedArg1, basicAllocator)
, d_d2(embeddedArg2, basicAllocator)
, d_d3(embeddedArg3, basicAllocator)
, d_d4(embeddedArg4, basicAllocator)
, d_d5(embeddedArg5, basicAllocator)
, d_d6(embeddedArg6, basicAllocator)
, d_d7(embeddedArg7, basicAllocator)
, d_d8(embeddedArg8, basicAllocator)
{
}

template <class OBJ, class C, class A1, class D1, class D2, class D3,
          class D4, class D5, class D6, class D7, class D8>
inline
Vfunc1C9<OBJ, C, A1, D1, D2, D3, D4, D5, D6, D7, D8>::~Vfunc1C9()
{
}

template <class OBJ, class C, class A1, class D1, class D2, class D3,
          class D4, class D5, class D6, class D7, class D8>
inline
void Vfunc1C9<OBJ, C, A1, D1, D2, D3, D4, D5, D6, D7, D8>::execute(
                                                   const A1& argument1) const
{
    const OBJ& obj = d_obj.object();  // for readability
    (obj.*d_m)(argument1, d_d1.object(), d_d2.object(), d_d3.object(),
                          d_d4.object(), d_d5.object(), d_d6.object(),
                          d_d7.object(), d_d8.object());
}

                        // --------------------
                        // class Vfunc1M1
                        // --------------------

template <class OBJ, class M, class A1>
inline
Vfunc1M1<OBJ, M, A1>::Vfunc1M1(OBJ             *object,
                                           M                method,
                                           bslma::Allocator*basicAllocator)
: bdlcfxxxr::Vfunc1<A1>(basicAllocator)
, d_obj_p(object)
, d_m(method)
{
}

template <class OBJ, class M, class A1>
inline
Vfunc1M1<OBJ, M, A1>::~Vfunc1M1()
{
}

template <class OBJ, class M, class A1>
inline
void Vfunc1M1<OBJ, M, A1>::execute(const A1& argument1) const
{
    (d_obj_p->*d_m)(argument1);
}

                        // --------------------
                        // class Vfunc1M2
                        // --------------------

template <class OBJ, class M, class A1, class D1>
inline
Vfunc1M2<OBJ, M, A1, D1>::Vfunc1M2(OBJ             *object,
                                               M                method,
                                               const D1&        embeddedArg1,
                                               bslma::Allocator*basicAllocator)
: bdlcfxxxr::Vfunc1<A1>(basicAllocator)
, d_obj_p(object)
, d_m(method)
, d_d1(embeddedArg1, basicAllocator)
{
}

template <class OBJ, class M, class A1, class D1>
inline
Vfunc1M2<OBJ, M, A1, D1>::~Vfunc1M2()
{
}

template <class OBJ, class M, class A1, class D1>
inline
void Vfunc1M2<OBJ, M, A1, D1>::execute(const A1& argument1) const
{
    (d_obj_p->*d_m)(argument1, d_d1.object());
}

                        // --------------------
                        // class Vfunc1M3
                        // --------------------

template <class OBJ, class M, class A1, class D1, class D2>
inline
Vfunc1M3<OBJ, M, A1, D1, D2>::Vfunc1M3(
                                             OBJ             *object,
                                             M                method,
                                             const D1&        embeddedArg1,
                                             const D2&        embeddedArg2,
                                             bslma::Allocator*basicAllocator)
: bdlcfxxxr::Vfunc1<A1>(basicAllocator)
, d_obj_p(object)
, d_m(method)
, d_d1(embeddedArg1, basicAllocator)
, d_d2(embeddedArg2, basicAllocator)
{
}

template <class OBJ, class M, class A1, class D1, class D2>
inline
Vfunc1M3<OBJ, M, A1, D1, D2>::~Vfunc1M3()
{
}

template <class OBJ, class M, class A1, class D1, class D2>
inline
void Vfunc1M3<OBJ, M, A1, D1, D2>::execute(const A1& argument1) const
{
    (d_obj_p->*d_m)(argument1, d_d1.object(), d_d2.object());
}

                        // --------------------
                        // class Vfunc1M4
                        // --------------------

template <class OBJ, class M, class A1, class D1, class D2, class D3>
inline
Vfunc1M4<OBJ, M, A1, D1, D2, D3>::Vfunc1M4(
                                             OBJ             *object,
                                             M                method,
                                             const D1&        embeddedArg1,
                                             const D2&        embeddedArg2,
                                             const D3&        embeddedArg3,
                                             bslma::Allocator*basicAllocator)
: bdlcfxxxr::Vfunc1<A1>(basicAllocator)
, d_obj_p(object)
, d_m(method)
, d_d1(embeddedArg1, basicAllocator)
, d_d2(embeddedArg2, basicAllocator)
, d_d3(embeddedArg3, basicAllocator)
{
}

template <class OBJ, class M, class A1, class D1, class D2, class D3>
inline
Vfunc1M4<OBJ, M, A1, D1, D2, D3>::~Vfunc1M4()
{
}

template <class OBJ, class M, class A1, class D1, class D2, class D3>
inline
void Vfunc1M4<OBJ, M, A1, D1, D2, D3>::execute(const A1& argument1) const
{
    (d_obj_p->*d_m)(argument1, d_d1.object(), d_d2.object(), d_d3.object());
}

                        // --------------------
                        // class Vfunc1M5
                        // --------------------

template <class OBJ, class M, class A1, class D1, class D2, class D3,
          class D4>
inline
Vfunc1M5<OBJ, M, A1, D1, D2, D3, D4>::Vfunc1M5(
                                             OBJ             *object,
                                             M                method,
                                             const D1&        embeddedArg1,
                                             const D2&        embeddedArg2,
                                             const D3&        embeddedArg3,
                                             const D4&        embeddedArg4,
                                             bslma::Allocator*basicAllocator)
: bdlcfxxxr::Vfunc1<A1>(basicAllocator)
, d_obj_p(object)
, d_m(method)
, d_d1(embeddedArg1, basicAllocator)
, d_d2(embeddedArg2, basicAllocator)
, d_d3(embeddedArg3, basicAllocator)
, d_d4(embeddedArg4, basicAllocator)
{
}

template <class OBJ, class M, class A1, class D1, class D2, class D3,
          class D4>
inline
Vfunc1M5<OBJ, M, A1, D1, D2, D3, D4>::~Vfunc1M5()
{
}

template <class OBJ, class M, class A1, class D1, class D2, class D3, class D4>
inline
void Vfunc1M5<OBJ, M, A1, D1, D2, D3, D4>::execute(
                                                   const A1& argument1) const
{
    (d_obj_p->*d_m)(argument1, d_d1.object(), d_d2.object(), d_d3.object(),
                               d_d4.object());
}

                        // --------------------
                        // class Vfunc1M6
                        // --------------------

template <class OBJ, class M, class A1, class D1, class D2, class D3,
          class D4, class D5>
inline
Vfunc1M6<OBJ, M, A1, D1, D2, D3, D4, D5>::Vfunc1M6(
                                             OBJ             *object,
                                             M                method,
                                             const D1&        embeddedArg1,
                                             const D2&        embeddedArg2,
                                             const D3&        embeddedArg3,
                                             const D4&        embeddedArg4,
                                             const D5&        embeddedArg5,
                                             bslma::Allocator*basicAllocator)
: bdlcfxxxr::Vfunc1<A1>(basicAllocator)
, d_obj_p(object)
, d_m(method)
, d_d1(embeddedArg1, basicAllocator)
, d_d2(embeddedArg2, basicAllocator)
, d_d3(embeddedArg3, basicAllocator)
, d_d4(embeddedArg4, basicAllocator)
, d_d5(embeddedArg5, basicAllocator)
{
}

template <class OBJ, class M, class A1, class D1, class D2, class D3,
          class D4, class D5>
inline
Vfunc1M6<OBJ, M, A1, D1, D2, D3, D4, D5>::~Vfunc1M6()
{
}

template <class OBJ, class M, class A1, class D1, class D2, class D3,
          class D4, class D5>
inline
void Vfunc1M6<OBJ, M, A1, D1, D2, D3, D4, D5>::execute(
                                                   const A1& argument1) const
{
    (d_obj_p->*d_m)(argument1, d_d1.object(), d_d2.object(), d_d3.object(),
                               d_d4.object(), d_d5.object());
}

                        // --------------------
                        // class Vfunc1M7
                        // --------------------

template <class OBJ, class M, class A1, class D1, class D2, class D3,
          class D4, class D5, class D6>
inline
Vfunc1M7<OBJ, M, A1, D1, D2, D3, D4, D5, D6>::Vfunc1M7(
                                             OBJ             *object,
                                             M                method,
                                             const D1&        embeddedArg1,
                                             const D2&        embeddedArg2,
                                             const D3&        embeddedArg3,
                                             const D4&        embeddedArg4,
                                             const D5&        embeddedArg5,
                                             const D6&        embeddedArg6,
                                             bslma::Allocator*basicAllocator)
: bdlcfxxxr::Vfunc1<A1>(basicAllocator)
, d_obj_p(object)
, d_m(method)
, d_d1(embeddedArg1, basicAllocator)
, d_d2(embeddedArg2, basicAllocator)
, d_d3(embeddedArg3, basicAllocator)
, d_d4(embeddedArg4, basicAllocator)
, d_d5(embeddedArg5, basicAllocator)
, d_d6(embeddedArg6, basicAllocator)
{
}

template <class OBJ, class M, class A1, class D1, class D2, class D3,
          class D4, class D5, class D6>
inline
Vfunc1M7<OBJ, M, A1, D1, D2, D3, D4, D5, D6>::~Vfunc1M7()
{
}

template <class OBJ, class M, class A1, class D1, class D2, class D3,
          class D4, class D5, class D6>
inline
void Vfunc1M7<OBJ, M, A1, D1, D2, D3, D4, D5, D6>::execute(
                                                   const A1& argument1) const
{
    (d_obj_p->*d_m)(argument1, d_d1.object(), d_d2.object(), d_d3.object(),
                               d_d4.object(), d_d5.object(), d_d6.object());
}

                        // --------------------
                        // class Vfunc1M8
                        // --------------------

template <class OBJ, class M, class A1, class D1, class D2, class D3,
          class D4, class D5, class D6, class D7>
inline
Vfunc1M8<OBJ, M, A1, D1, D2, D3, D4, D5, D6, D7>::Vfunc1M8(
                                             OBJ             *object,
                                             M                method,
                                             const D1&        embeddedArg1,
                                             const D2&        embeddedArg2,
                                             const D3&        embeddedArg3,
                                             const D4&        embeddedArg4,
                                             const D5&        embeddedArg5,
                                             const D6&        embeddedArg6,
                                             const D7&        embeddedArg7,
                                             bslma::Allocator*basicAllocator)
: bdlcfxxxr::Vfunc1<A1>(basicAllocator)
, d_obj_p(object)
, d_m(method)
, d_d1(embeddedArg1, basicAllocator)
, d_d2(embeddedArg2, basicAllocator)
, d_d3(embeddedArg3, basicAllocator)
, d_d4(embeddedArg4, basicAllocator)
, d_d5(embeddedArg5, basicAllocator)
, d_d6(embeddedArg6, basicAllocator)
, d_d7(embeddedArg7, basicAllocator)
{
}

template <class OBJ, class M, class A1, class D1, class D2, class D3,
          class D4, class D5, class D6, class D7>
inline
Vfunc1M8<OBJ, M, A1, D1, D2, D3, D4, D5, D6, D7>::~Vfunc1M8()
{
}

template <class OBJ, class M, class A1, class D1, class D2, class D3,
          class D4, class D5, class D6, class D7>
inline
void Vfunc1M8<OBJ, M, A1, D1, D2, D3, D4, D5, D6, D7>::execute(
                                                   const A1& argument1) const
{
    (d_obj_p->*d_m)(argument1, d_d1.object(), d_d2.object(), d_d3.object(),
                               d_d4.object(), d_d5.object(), d_d6.object(),
                               d_d7.object());
}

                        // --------------------
                        // class Vfunc1M9
                        // --------------------

template <class OBJ, class M, class A1, class D1, class D2, class D3,
          class D4, class D5, class D6, class D7, class D8>
inline
Vfunc1M9<OBJ, M, A1, D1, D2, D3, D4, D5, D6, D7, D8>::Vfunc1M9(
                                             OBJ             *object,
                                             M                method,
                                             const D1&        embeddedArg1,
                                             const D2&        embeddedArg2,
                                             const D3&        embeddedArg3,
                                             const D4&        embeddedArg4,
                                             const D5&        embeddedArg5,
                                             const D6&        embeddedArg6,
                                             const D7&        embeddedArg7,
                                             const D8&        embeddedArg8,
                                             bslma::Allocator*basicAllocator)
: bdlcfxxxr::Vfunc1<A1>(basicAllocator)
, d_obj_p(object)
, d_m(method)
, d_d1(embeddedArg1, basicAllocator)
, d_d2(embeddedArg2, basicAllocator)
, d_d3(embeddedArg3, basicAllocator)
, d_d4(embeddedArg4, basicAllocator)
, d_d5(embeddedArg5, basicAllocator)
, d_d6(embeddedArg6, basicAllocator)
, d_d7(embeddedArg7, basicAllocator)
, d_d8(embeddedArg8, basicAllocator)
{
}

template <class OBJ, class M, class A1, class D1, class D2, class D3,
          class D4, class D5, class D6, class D7, class D8>
inline
Vfunc1M9<OBJ, M, A1, D1, D2, D3, D4, D5, D6, D7, D8>::~Vfunc1M9()
{
}

template <class OBJ, class M, class A1, class D1, class D2, class D3,
          class D4, class D5, class D6, class D7, class D8>
inline
void Vfunc1M9<OBJ, M, A1, D1, D2, D3, D4, D5, D6, D7, D8>::execute(
                                                   const A1& argument1) const
{
    (d_obj_p->*d_m)(argument1, d_d1.object(), d_d2.object(), d_d3.object(),
                               d_d4.object(), d_d5.object(), d_d6.object(),
                               d_d7.object(), d_d8.object());
}
}  // close package namespace

}  // close namespace BloombergLP

#endif

// ---------------------------------------------------------------------------
// NOTICE:
//      Copyright (C) Bloomberg L.P., 2002
//      All Rights Reserved.
//      Property of Bloomberg L.P. (BLP)
//      This software is made available solely pursuant to the
//      terms of a BLP license agreement which governs its use.
// ----------------------------- END-OF-FILE ---------------------------------
