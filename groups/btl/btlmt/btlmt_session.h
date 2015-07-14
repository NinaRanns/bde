// btlmt_session.h                                                    -*-C++-*-
#ifndef INCLUDED_BTLMT_SESSION
#define INCLUDED_BTLMT_SESSION

#ifndef INCLUDED_BSLS_IDENT
#include <bsls_ident.h>
#endif
BSLS_IDENT("$Id: $")

//@PURPOSE: Provide a *pure* protocol class for creating sessions
//
//@CLASSES:
//        btlmt::Session: protocol for an abstract session.
// btlmt::SessionFactory: abstract factory for sessions.
//
//@SEE_ALSO: btlmt_sessionpool
//
//@AUTHOR: Ilougino Rocha (irocha), Olga Zykova (ozykova)
//
//@DESCRIPTION: The classes provided by this component are intended to be used
// with 'btlmt::SessionPool', which allocates/manages TCP sessions given a
// session factory.  See the 'btlmt_sessionpool' component for a more detailed
// documentation.

#ifndef INCLUDED_BTLSCM_VERSION
#include <btlscm_version.h>
#endif

#ifndef INCLUDED_BDLF_FUNCTION
#include <bdlf_function.h>
#endif

#ifndef INCLUDED_BSL_MEMORY
#include <bsl_memory.h>
#endif

#ifndef BDE_DONT_ALLOW_TRANSITIVE_INCLUDES

#ifndef INCLUDED_BSL_MEMORY
#include <bsl_memory.h>
#endif

#endif

namespace BloombergLP {


namespace btlmt {class AsyncChannel;

                            // ===================
                            // class Session
                            // ===================

class Session {
   // This class defines a *pure* protocol for the starting and stopping
   // of communication sessions.

  public:
    // CREATORS
    virtual ~Session();
        // Destroy this session object.

    // MANIPULATORS
    virtual int start() = 0;
        // Begin the asynchronous operation of the session.
        // Return 0 on success and a non-zero value otherwise.

    virtual int stop() = 0;
        // Stop the operation of this session.
        // Return 0 on success and a non-zero value otherwise.

    // ACCESSORS
    virtual AsyncChannel* channel() const = 0;
        // Return the channel associated with the session.
};

                            // ==========================
                            // class SessionFactory
                            // ==========================

class SessionFactory {
    // This class defines a *pure* protocol for the allocation & deallocation
    // of 'Session' objects.

  public:
    // PUBLIC TYPES
    typedef bdlf::Function<void (*)(int,Session*)> Callback;
       // The signature of a function to be called after a new session is
       // allocated.  The first argument is taken to indicate the status
       // of the allocation - 0 if successful, nonzero otherwise.  The second
       // argument is a pointer to the newly allocated session.

    // CREATORS
    virtual ~SessionFactory();
       // Destroy this factory

    // MANIPULATORS
    virtual void allocate(const bsl::shared_ptr<AsyncChannel>& channel,
                          const SessionFactory::Callback&      callback);
       // Asynchronously allocate a 'Session' object for the specified
       // 'channel', and invoke the specified 'callback' with this session.
       //
       // Note that the default implementation of this (non-pure virtual)
       // method is to call 'allocate' with the raw channel pointer.

    virtual void allocate(AsyncChannel                   *channel,
                          const SessionFactory::Callback& callback) = 0;
       // Asynchronously allocate a 'Session' object for the specified
       // 'channel', and invoke the specified 'callback' with this session.

    virtual void deallocate(Session *session) = 0;
       // Deallocate the specified 'session'.
};
}  // close package namespace

} // close namespace BloombergLP

#endif

// ---------------------------------------------------------------------------
// NOTICE:
//      Copyright (C) Bloomberg L.P., 2007
//      All Rights Reserved.
//      Property of Bloomberg L.P. (BLP)
//      This software is made available solely pursuant to the
//      terms of a BLP license agreement which governs its use.
// ----------------------------- END-OF-FILE ---------------------------------
