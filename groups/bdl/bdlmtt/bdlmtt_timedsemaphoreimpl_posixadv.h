// bdlmtt_timedsemaphoreimpl_posixadv.h                                -*-C++-*-
#ifndef INCLUDED_BDLMTT_TIMEDSEMAPHOREIMPL_POSIXADV
#define INCLUDED_BDLMTT_TIMEDSEMAPHOREIMPL_POSIXADV

#ifndef INCLUDED_BSLS_IDENT
#include <bsls_ident.h>
#endif
BSLS_IDENT("$Id: $")

//@PURPOSE: Provide "advanced" POSIX implementation of 'bdlmtt::TimedSemaphore'.
//
//@CLASSES:
//  bdlmtt::TimedSemaphoreImpl<PosixAdvTimedSemaphore>: POSIXa specialization
//
//@SEE_ALSO: bdlmtt_timedsemaphore
//
//@AUTHOR: David Schumann (dschumann1)
//
//@DESCRIPTION: This component provides an implementation of
// 'bdlmtt::TimedSemaphore' for conforming POSIX platforms via the template
// specialization:
//..
//  bdlmtt::TimedSemaphoreImpl<bdlmtt::Platform::PosixAdvTimedSemaphore>
//..
// This template class should not be used (directly) by client code.  Clients
// should instead use 'bdlmtt::TimedSemaphore'.
//
// This implementation of 'bdlmtt::TimedSemaphore' is preferred over that defined
// in 'bdlmtt_timedsemaphoreimpl_pthread' on platforms that support advanced
// realtime POSIX extensions (e.g., 'sem_timedwait').
//
///Supported Clock-Types
///-------------------------
// The component 'bsls::SystemClockType' supplies the enumeration indicating
// the system clock on which timeouts supplied to other methods should be
// based.  If the clock type indicated at construction is
// 'bsls::SystemClockType::e_REALTIME', the timeout should be expressed as an
// absolute offset since 00:00:00 UTC, January 1, 1970 (which matches the epoch
// used in 'bdlt::CurrentTime::now(bsls::SystemClockType::e_REALTIME)'.  If the
// clock type indicated at construction is
// 'bsls::SystemClockType::e_MONOTONIC', the timeout should be expressed as an
// absolute offset since the epoch of this clock (which matches the epoch used
// in 'bdlt::CurrentTime::now(bsls::SystemClockType::e_MONOTONIC)'.
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

#ifdef BDLMTT_PLATFORM_POSIXADV_TIMEDSEMAPHORE

// Platform-specific implementation starts here.

#ifndef INCLUDED_BSLS_SYSTEMCLOCKTYPE
#include <bsls_systemclocktype.h>
#endif

#ifndef INCLUDED_SEMAPHORE
#include <semaphore.h>
#define INCLUDED_SEMAPHORE
#endif

namespace BloombergLP {


namespace bdlmtt {template <class TIMED_SEMAPHORE_POLICY>
class TimedSemaphoreImpl;
}  // close package namespace



// Updated by 'bde-replace-bdet-forward-declares.py -m bdlt': 2015-02-03
// Updated declarations tagged with '// bdet -> bdlt'.

namespace bsls { class TimeInterval; }                          // bdet -> bdlt

namespace bdet {typedef ::BloombergLP::bsls::TimeInterval TimeInterval;    // bdet -> bdlt
}  // close package namespace

namespace bdlmtt {
           // ======================================================
           // class TimedSemaphoreImpl<PosixAdvTimedSemaphore>
           // ======================================================

template <>
class TimedSemaphoreImpl<bdlmtt::Platform::PosixAdvTimedSemaphore> {
    // This class implements a timed semaphore in terms of POSIX operations.
    // Note that only certain platforms provide 'sem_timedwait'; on those that
    // do not, 'TimedSemaphoreImpl<PthreadTimedSemaphore>' is used.

    // DATA
    sem_t                       d_sem;        // POSIX timed semaphore

    bsls::SystemClockType::Enum d_clockType;  // clock type used for timeout in
                                              // 'timedWait'

    // NOT IMPLEMENTED
    TimedSemaphoreImpl(const TimedSemaphoreImpl&);
    TimedSemaphoreImpl& operator=(const TimedSemaphoreImpl&);

  public:
    // CREATORS
    explicit
    TimedSemaphoreImpl(bsls::SystemClockType::Enum clockType
                                          = bsls::SystemClockType::e_REALTIME);
        // Create a timed semaphore initially having a count of 0.  Optionally
        // specify a 'clockType' indicating the type of the system clock
        // against which the 'bsls::TimeInterval' timeouts passed to the
        // 'timedWait' method are to be interpreted.  If 'clockType' is not
        // specified then the realtime system clock is used.

    explicit
    TimedSemaphoreImpl(int                         count,
                             bsls::SystemClockType::Enum clockType
                                          = bsls::SystemClockType::e_REALTIME);
        // Create a timed semaphore initially having the specified 'count'.
        // Optionally specify a 'clockType' indicating the type of the system
        // clock against which the 'bsls::TimeInterval' timeouts passed to the
        // 'timedWait' method are to be interpreted.  If 'clockType' is not
        // specified then the realtime system clock is used.

    ~TimedSemaphoreImpl();
        // Destroy this semaphore object.

    // MANIPULATORS
    void post();
        // Atomically increment the count of the semaphore.

    void post(int number);
        // Atomically increment the count by the specified 'number' of the
        // semaphore.  The behavior is undefined unless 'number' is a positive
        // value.

    int timedWait(const bsls::TimeInterval& timeout);
        // Block until the count of this semaphore is a positive value, or
        // until the specified 'timeout' expires.  The 'timeout' is an absolute
        // time represented as an interval from some epoch, which is detemined
        // by the clock indicated at construction (see
        // {'Supported Clock-Types'} in the component documentation).  If the
        // 'timeout' did not expire before the count attained a positive value,
        // atomically decrement the count and return 0; otherwise, return a
        // non-zero value with no effect on the count.

    int tryWait();
        // Decrement the count of this semaphore if it is positive and return
        // 0.  Return a non-zero value otherwise.

    void wait();
        // Block until the count is a positive value and atomically decrement
        // it.
};

// ============================================================================
//                        INLINE FUNCTION DEFINITIONS
// ============================================================================

           // ------------------------------------------------------
           // class TimedSemaphoreImpl<PosixAdvTimedSemaphore>
           // ------------------------------------------------------

// CREATORS
inline
TimedSemaphoreImpl<bdlmtt::Platform::PosixAdvTimedSemaphore>::
                TimedSemaphoreImpl(bsls::SystemClockType::Enum clockType)
: d_clockType(clockType)
{
    ::sem_init(&d_sem, 0, 0);
}

inline
TimedSemaphoreImpl<bdlmtt::Platform::PosixAdvTimedSemaphore>::
     TimedSemaphoreImpl(int count, bsls::SystemClockType::Enum clockType)
: d_clockType(clockType)
{
    ::sem_init(&d_sem, 0, count);
}

inline
TimedSemaphoreImpl<bdlmtt::Platform::PosixAdvTimedSemaphore>::
                                                    ~TimedSemaphoreImpl()
{
    ::sem_destroy(&d_sem);
}

// MANIPULATORS
inline
void TimedSemaphoreImpl<bdlmtt::Platform::PosixAdvTimedSemaphore>::post()
{
    ::sem_post(&d_sem);
}

inline
int TimedSemaphoreImpl<bdlmtt::Platform::PosixAdvTimedSemaphore>::tryWait()
{
    return ::sem_trywait(&d_sem);
}
}  // close package namespace

}  // close namespace BloombergLP

#endif  // BDLMTT_PLATFORM_POSIX_THREADS

#endif

// ----------------------------------------------------------------------------
// NOTICE:
//      Copyright (C) Bloomberg L.P., 2014
//      All Rights Reserved.
//      Property of Bloomberg L.P. (BLP)
//      This software is made available solely pursuant to the
//      terms of a BLP license agreement which governs its use.
// ----------------------------- END-OF-FILE ----------------------------------
