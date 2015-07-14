// bdlmca_defaultdeleter.h             -*-C++-*-
#ifndef INCLUDED_BDLMCA_DEFAULTDELETER
#define INCLUDED_BDLMCA_DEFAULTDELETER

#ifndef INCLUDED_BSLS_IDENT
#include <bsls_ident.h>
#endif
BSLS_IDENT("$Id: $")

//@PURPOSE: Provide a concrete default deleter w/optionally-supplied allocator.
//
//@CLASSES:
//   bdlmca::DefaultDeleter: concrete default 'bdema'-based deleter
//
//@SEE_ALSO: bslma_allocator, bdlmca_deleter
//
//@AUTHOR: Andrei Basov (abasov)
//
//@DESCRIPTION: This component provides a default concrete implementation of
// the 'bdlmca::Deleter' protocol:
//..
//                      ( bdlmca::DefaultDeleter )
//                                 |       ctor
//                                 v
//                         ( bdlmca::Deleter )
//                                         dtor
//                                         deleteObject
//..
// Upon construction, a 'bdlmca::DefaultDeleter' is optionally supplied with a
// 'bdema'-style allocator.  If an allocator is not specified, the currently
// installed default allocator is used instead.  The memory footprint of
// objects that are subsequently deleted via calls to the 'deleteObject' method
// of the deleter will be returned to the allocator that was established at
// construction.  Note that the allocator used to create the footprint of
// objects passed to 'deleteObject' *must* be the same allocator that is used
// by the deleter.
//
///Usage
///-----
// Suppose that we would like to transfer ownership of an object between
// threads using 'bsl::shared_ptr'.  For the sake of discussion, the type
// of this object is 'my_Obj' and we will suppose that it is created using a
// given 'basicAllocator'.  Note that we assume that 'my_Obj' does not require
// an allocator for any of its members:
//..
//  void f(bslma::Allocator *basicAllocator)
//  {
//      my_Obj *object = new(*basicAllocator) my_Obj;
//..
// Next, create a concrete deleter for 'object' using the same allocator as was
// used to allocate its footprint:
//..
//      bdlmca::DefaultDeleter<my_Obj> deleter(basicAllocator);
//..
// Finally, create a shared pointer passing to it 'object' and the address of
// 'deleter':
//..
//      bsl::shared_ptr<my_Obj> handle(object, &deleter, basicAllocator);
//..
// Now the 'handle' can be passed to another thread or enqueued efficiently.
// When the reference count of 'handle' goes to 0, 'object' is automatically
// deleted via the 'deleteObject' method of 'deleter', which in turn will
// invoke the destructor of 'object'.  Note that since the type of the deleter
// used to instantiate 'handle' is 'bdlmca::Deleter<my_Obj>', any kind of deleter
// that implements this protocol can be passed.  Also note, on the downside,
// that the lifetime of 'deleter' must be longer than the lifetime of all
// associated instances.
//..
//  }
//..

#ifndef INCLUDED_BDLSCM_VERSION
#include <bdlscm_version.h>
#endif

#ifndef INCLUDED_BDLMCA_DELETER
#include <bdlmca_deleter.h>
#endif

#ifndef INCLUDED_BSLMA_ALLOCATOR
#include <bslma_allocator.h>
#endif

#ifndef INCLUDED_BSLMA_DEFAULT
#include <bslma_default.h>
#endif

namespace BloombergLP {

namespace bdlmca {
                        // ==========================
                        // class DefaultDeleter
                        // ==========================

template <class TYPE>
class DefaultDeleter : public Deleter<TYPE> {
    // This 'class' provides a default concrete implementation of the
    // 'Deleter' protocol.  Instances of 'DefaultDeleter<TYPE>'
    // either use an allocator optionally supplied at construction, or the
    // currently installed default allocator if an allocator is not provided.

    // DATA
    bslma::Allocator *d_allocator_p;  // memory allocator (held, *not* owned)

    // NOT IMPLEMENTED
    DefaultDeleter(const DefaultDeleter<TYPE>&);
    DefaultDeleter<TYPE>& operator=(const DefaultDeleter<TYPE>&);

  public:
    // CREATORS
    DefaultDeleter(bslma::Allocator *basicAllocator = 0);
        // Create a default deleter.  Optionally specify a 'basicAllocator'
        // used to manage the memory footprint of objects passed to the
        // 'deleteObject' method.  If 'basicAllocator' is 0, the currently
        // installed default allocator is used.

    virtual ~DefaultDeleter();
        // Destroy this default deleter.

    // MANIPULATORS
    virtual void deleteObject(TYPE *instance);
        // Destroy the specified 'instance' based on its static type and return
        // its memory footprint to the appropriate memory manager.  The
        // behavior is undefined unless the memory for the footprint of
        // 'instance' was supplied by the same allocator as is used by this
        // default deleter.  Note that this method does not destroy the deleter
        // itself.
};

// ===========================================================================
//                        INLINE FUNCTION DEFINITIONS
// ===========================================================================

// CREATORS
template <class TYPE>
inline
DefaultDeleter<TYPE>::DefaultDeleter(
                                              bslma::Allocator *basicAllocator)
: d_allocator_p(bslma::Default::allocator(basicAllocator))
{
}

template <class TYPE>
DefaultDeleter<TYPE>::~DefaultDeleter()
{
}

// MANIPULATORS
template <class TYPE>
inline
void DefaultDeleter<TYPE>::deleteObject(TYPE *instance)
{
    d_allocator_p->deleteObject(instance);
}
}  // close package namespace

}  // close namespace BloombergLP

#endif

// ---------------------------------------------------------------------------
// NOTICE:
//      Copyright (C) Bloomberg L.P., 2004
//      All Rights Reserved.
//      Property of Bloomberg L.P. (BLP)
//      This software is made available solely pursuant to the
//      terms of a BLP license agreement which governs its use.
// ----------------------------- END-OF-FILE ---------------------------------
