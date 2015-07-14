// bdlmtt_semaphoreimpl_pthread.h                -*-C++-*-
#ifndef INCLUDED_BDLMTT_SEMAPHOREIMPL_PTHREAD
#define INCLUDED_BDLMTT_SEMAPHOREIMPL_PTHREAD

#ifndef INCLUDED_BSLS_IDENT
#include <bsls_ident.h>
#endif
BSLS_IDENT("$Id: $")

//@PURPOSE: Provide a POSIX implementation of 'bdlmtt::Semaphore'.
//
//@CLASSES:
//  bdlmtt::SemaphoreImpl<PosixSemaphore>: POSIX specialization
//
//@SEE_ALSO: bdlmtt_semaphore
//
//@AUTHOR: Ilougino Rocha (irocha)
//
//@DESCRIPTION: This component provides an implementation of 'bdlmtt::Semaphore'
// for POSIX threads ("pthreads") via the template specialization:
//..
//  bdlmtt::SemaphoreImpl<bdlmtt::Platform::PosixThreads>
//..
// This template class should not be used (directly) by client code.  Clients
// should instead use 'bdlmtt::Semaphore'.
//
///Usage
///-----
// This component is an implementation detail of 'bcemt' and is *not* intended
// for direct client use.  It is subject to change without notice.  As such, a
// usage example is not provided.

#ifndef INCLUDED_BDLSCM_VERSION
#include <bdlscm_version.h>
#endif

#ifndef INCLUDED_BDLMTT_PLATFORM
#include <bdlmtt_platform.h>
#endif

#if defined(BDLMTT_PLATFORM_POSIX_SEMAPHORE)

// Platform-specific implementation starts here.

#ifndef INCLUDED_BSLS_ASSERT
#include <bsls_assert.h>
#endif

#ifndef INCLUDED_SEMAPHORE
#include <semaphore.h>
#define INCLUDED_SEMAPHORE
#endif

namespace BloombergLP {


namespace bdlmtt {template <typename SEMAPHORE_POLICY>
class SemaphoreImpl;

             // ========================================================
             // class SemaphoreImpl<bdlmtt::Platform::PosixSemaphore>
             // ========================================================

template <>
class SemaphoreImpl<bdlmtt::Platform::PosixSemaphore> {
    // This class provides a full specialization of 'SemaphoreImpl'
    // for pthreads.  The implementation provided here defines an efficient
    // proxy for the 'sem_t' pthread type, and related operations.

    // DATA
    sem_t d_sem;                // native semaphore handle

  private:
    // NOT IMPLEMENTED
    SemaphoreImpl(const SemaphoreImpl&);
    SemaphoreImpl& operator=(const SemaphoreImpl&);

  public:
    // CREATORS
    explicit
    SemaphoreImpl(int count);
        // Create a semaphore initialized to the specified 'count'.

    ~SemaphoreImpl();
        // Destroy a semaphore

    // MANIPULATORS
    void post();
        // Atomically increment the count of this semaphore.

    void post(int number);
        // Atomically increment the count of this semaphore by the specified
        // 'number'.  The behavior is undefined unless 'number > 0'.

    int tryWait();
        // Decrement the count of this semaphore if it is positive and return
        // 0.  Return a non-zero value otherwise.

    void wait();
        // Block until the count of this semaphore is a positive value and
        // atomically decrement it.

    // ACCESSORS
    int getValue() const;
        // Return the current value of this semaphore.
};

// ===========================================================================
//                        INLINE FUNCTION DEFINITIONS
// ===========================================================================

             // --------------------------------------------------------
             // class SemaphoreImpl<bdlmtt::Platform::PosixSemaphore>
             // --------------------------------------------------------

// CREATORS
inline
SemaphoreImpl<bdlmtt::Platform::PosixSemaphore>::~SemaphoreImpl()
{
    int result = ::sem_destroy(&d_sem);

    (void) result;
    BSLS_ASSERT(result == 0);
}

// MANIPULATORS
inline
void SemaphoreImpl<bdlmtt::Platform::PosixSemaphore>::post()
{
    int result = ::sem_post(&d_sem);

    (void) result;
    BSLS_ASSERT(result == 0);
}

inline
int SemaphoreImpl<bdlmtt::Platform::PosixSemaphore>::tryWait()
{
    return ::sem_trywait(&d_sem);
}

// ACCESSORS
inline
int SemaphoreImpl<bdlmtt::Platform::PosixSemaphore>::getValue() const
{
    int value = 0;
    int result = ::sem_getvalue(const_cast<sem_t *>(&d_sem), &value);

    (void) result;
    BSLS_ASSERT_SAFE(result == 0);

    return value;
}
}  // close package namespace

}  // close namespace BloombergLP

#endif  // defined(BDLMTT_PLATFORM_POSIX_SEMAPHORE)

#endif

// ---------------------------------------------------------------------------
// NOTICE:
//      Copyright (C) Bloomberg L.P., 2010
//      All Rights Reserved.
//      Property of Bloomberg L.P. (BLP)
//      This software is made available solely pursuant to the
//      terms of a BLP license agreement which governs its use.
// ----------------------------- END-OF-FILE ---------------------------------
