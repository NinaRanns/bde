// bdlmt_multiprioritythreadpool.cpp        -*-C++-*-
#include <bdlmt_multiprioritythreadpool.h>

#include <bsls_ident.h>
BSLS_IDENT_RCSID(bdlmt_multiprioritythreadpool_cpp,"$Id$ $CSID$")

///IMPLEMENTATION NOTES
///--------------------
// The states of 'd_threadStartState' and 'd_threadSuspendState' are defined by
// the enums 'StartType' and 'SuspendType', respectively, defined below under
// 'LOCAL TYPES'.
//
// All changes of 'd_threadStartState' and 'd_threadSuspendState' are guarded
// by 'd_mutex'.  Also note that ALL transitions of those two variables into
// transitional states (states ending with 'ING') are guarded by 'd_metaMutex',
// and that mutex is never released until neither of those variables are in a
// transitional state.
//
// Also note that if the thread pool is suspended and 'd_metaMutex' is not
// locked, then all threads are either stopped, or waiting on condition
// 'd_resumeCondition'.
//
// The third orthogonal property of the thread pool, the enabled / disabled
// state of the queue, is much simpler.  It is just turned on and off at will,
// and the functionality is fully encapsulated in the queue.  The enabled /
// disabled state only affects the 'enqueueJob' method, pops from the queue can
// happen regardless of that state.
//
// Things get a bit tricky when starting or stopping a thread pool that is
// suspended.  When starting a thread pool that is suspended, it is important
// that the threads all wait before processing any jobs, and similarly, when
// stopping, they must stop before processing any other jobs.

#include <bslma_default.h>
#include <bdlmtt_barrier.h>
#include <bdlmtt_lockguard.h>
#include <bdlf_bind.h>
#include <bslma_allocator.h>
#include <bsls_assert.h>

namespace BloombergLP {

// LOCAL TYPES
enum StartState {       // type of 'd_threadStartState'
    STARTING,
    STARTED,
    STOPPING,
    STOPPED
};

enum SuspendState {     // type of 'd_threadSuspendState'
    SUSPENDING,
    SUSPENDED,
//  RESUMING,   // not used - we go straight from suspended to resumed
    RESUMED
};

namespace bdlmt {
                       // ----------------------------------
                       // class MultipriorityThreadPool
                       // ----------------------------------

// PRIVATE MANIPULATORS
void MultipriorityThreadPool::worker()
{
    {
        bdlmtt::LockGuard<bdlmtt::Mutex> lock(&d_mutex);

        if (STOPPING == d_threadStartState) {
            return;
        }
        BSLS_ASSERT(STARTING == d_threadStartState);

        if (d_numThreads == ++d_numStartedThreads) {
            d_threadStartState = STARTED;
            d_allThreadsStartedCondition.broadcast();
        }
        else {
            do {
                d_allThreadsStartedCondition.wait(&d_mutex);
            } while (STARTING == d_threadStartState);
            // Note that state might have been changed to 'STOPPING' if
            // any threads failed to start.
        }
    }

    while (1) {
        if (STARTED != d_threadStartState || RESUMED != d_threadSuspendState) {
            bdlmtt::LockGuard<bdlmtt::Mutex> lock(&d_mutex);

            do {
                // Note 'd_metaMutex' may or may not be locked when we get
                // here.

                if (STOPPING == d_threadStartState) {
                    --d_numStartedThreads;
                    return;                                           // RETURN
                }
                BSLS_ASSERT(STARTED == d_threadStartState);

                BSLS_ASSERT(SUSPENDED != d_threadSuspendState);
                if (SUSPENDING == d_threadSuspendState) {
                    if (d_numThreads == ++d_numSuspendedThreads) {
                        d_threadSuspendState = SUSPENDED;
                        d_allThreadsSuspendedCondition.broadcast();
                    }
                    do {
                        d_resumeCondition.wait(&d_mutex);
                    } while (RESUMED != d_threadSuspendState
                          && STARTED == d_threadStartState);
                    --d_numSuspendedThreads;
                }

                // If we were suspended and then told to stop, we will pass
                // through this point within 'SUSPENDED' and 'STOPPING' states.
            } while (STARTED != d_threadStartState
                  || RESUMED != d_threadSuspendState);
        }  // release mutex

        {
            ThreadFunctor job;

            // Retrieve the next job, blocking until one is available.
            d_queue.popFront(&job);

            if (job) {
                // Run the job.
                ++d_numActiveThreads;
                job();                                                // INVOKE
                --d_numActiveThreads;
            }
        }
    }
}

// CREATORS
MultipriorityThreadPool::MultipriorityThreadPool(
                                              int               numThreads,
                                              int               numPriorities,
                                              bslma::Allocator *basicAllocator)
: d_queue(numPriorities, bslma::Default::allocator(basicAllocator))
, d_threadGroup(bslma::Default::allocator(basicAllocator))
, d_numThreads(numThreads)
, d_threadStartState(STOPPED)
, d_threadSuspendState(RESUMED)
, d_numStartedThreads(0)
, d_numSuspendedThreads(0)
, d_numActiveThreads(0)
{
    BSLS_ASSERT(BCEP_MAX_NUM_PRIORITIES >= numPriorities);
    BSLS_ASSERT(1 <= numPriorities);
    BSLS_ASSERT(1 <= numThreads);
}

MultipriorityThreadPool::MultipriorityThreadPool(
                                      int                     numThreads,
                                      int                     numPriorities,
                                      const bcemt_Attribute&  threadAttributes,
                                      bslma::Allocator       *basicAllocator)
: d_queue(numPriorities, bslma::Default::allocator(basicAllocator))
, d_threadAttributes(threadAttributes)
, d_threadGroup(bslma::Default::allocator(basicAllocator))
, d_numThreads(numThreads)
, d_threadStartState(STOPPED)
, d_threadSuspendState(RESUMED)
, d_numStartedThreads(0)
, d_numSuspendedThreads(0)
, d_numActiveThreads(0)
{
    BSLS_ASSERT(BCEP_MAX_NUM_PRIORITIES >= numPriorities);
    BSLS_ASSERT(1 <= numPriorities);
    BSLS_ASSERT(1 <= numThreads);

    // Force all threads to be joinable.
    d_threadAttributes.setDetachedState(
                                       bcemt_Attribute::BCEMT_CREATE_JOINABLE);
}

MultipriorityThreadPool::~MultipriorityThreadPool()
{
    BSLS_ASSERT(STOPPED == d_threadStartState);
}

// MANIPULATORS
int MultipriorityThreadPool::enqueueJob(const ThreadFunctor& job,
                                             int                  priority)
{
    BSLS_ASSERT((unsigned) priority < (unsigned) d_queue.numPriorities());
    // checks '0 <= priority < numPriorities()'

    return d_queue.pushBack(job, priority);
}

int MultipriorityThreadPool::enqueueJob(
                                          bcemt_ThreadFunction  jobFunction,
                                          void                 *jobData,
                                          int                   priority)
{
    return enqueueJob(bdlf::BindUtil::bind(jobFunction, jobData), priority);
}

void MultipriorityThreadPool::enableQueue()
{
    d_queue.enable();
}

void MultipriorityThreadPool::disableQueue()
{
    d_queue.disable();
}

int MultipriorityThreadPool::startThreads()
{
    bdlmtt::LockGuard<bdlmtt::Mutex> metaLock(&d_metaMutex);
    int rc = 0;

    if (STARTED == d_threadStartState) {
        return 0;                                                     // RETURN
    }
    BSLS_ASSERT(STOPPED == d_threadStartState);

#if defined(BSLS_PLATFORM_OS_UNIX)
    sigset_t oldBlockSet, newBlockSet;  // set of signals to be blocked in
                                        // managed threads
    sigfillset(&newBlockSet);

    static const int signals[] = {      // synchronous signals
         SIGBUS, SIGFPE, SIGILL, SIGSEGV, SIGSYS, SIGABRT, SIGTRAP
        #if !defined(BSLS_PLATFORM_OS_CYGWIN) || defined(SIGIOT)
        ,SIGIOT
        #endif
    };

    enum {
        NUM_SIGNALS = sizeof signals / sizeof *signals
    };

    for (int i = 0; i < NUM_SIGNALS; ++i) {
        sigdelset(&newBlockSet, signals[i]);
    }

    // Block all synchronous signals.
    pthread_sigmask(SIG_BLOCK, &newBlockSet, &oldBlockSet);
#endif

    // 'workerFunctor' calls the private 'worker' method.

    const ThreadFunctor workerFunctor =
            bdlf::MemFnUtil::memFn(&MultipriorityThreadPool::worker, this);

    {
        bdlmtt::LockGuard<bdlmtt::Mutex> lock(&d_mutex);

        d_threadStartState = STARTING;
        if (SUSPENDED == d_threadSuspendState) {
            // This is necessary because when we wait on
            // 'd_allThreadsStartedCondition', we need to be waiting for a
            // state transition to 'SUSPENDED' to confirm our wait is over.
            d_threadSuspendState = SUSPENDING;
        }

        int startedThreads = d_threadGroup.addThreads(workerFunctor,
                                                      d_numThreads,
                                                      d_threadAttributes);
        if (d_numThreads == startedThreads) {
            if (SUSPENDING == d_threadSuspendState) {
                do {
                    d_allThreadsSuspendedCondition.wait(&d_mutex);
                } while (SUSPENDED != d_threadSuspendState);
            }
            else {
                do {
                    d_allThreadsStartedCondition.wait(&d_mutex);
                } while (STARTING == d_threadStartState);
            }
            BSLS_ASSERT(STARTED      == d_threadStartState);
            BSLS_ASSERT(d_numThreads == d_numStartedThreads);
        }
        else {
            // start failed -- shut down all the threads

            d_threadStartState = STOPPING;
            d_allThreadsStartedCondition.broadcast();

            // Note that if we are in the 'SUSPENDING' state, the threads will
            // wait on this condition before they wait on the suspended
            // condition.  Once released from the started condition, they will
            // run straight into the check for 'STOPPED' before reaching the
            // wait on a suspended condition.

            {
                bdlmtt::LockGuardUnlock<bdlmtt::Mutex> unlock(&d_mutex);
                d_threadGroup.joinAll();
            }

            if (SUSPENDING == d_threadSuspendState) {
                d_threadSuspendState = SUSPENDED;
            }
            d_threadStartState = STOPPED;

            rc = -1;
        }
    }

#if defined(BSLS_PLATFORM_OS_UNIX)
    // Restore the mask.
    pthread_sigmask(SIG_SETMASK, &oldBlockSet, &newBlockSet);
#endif

    BSLS_ASSERT(STARTED   == d_threadStartState
                || STOPPED   == d_threadStartState);
    BSLS_ASSERT(RESUMED   == d_threadSuspendState
                || SUSPENDED == d_threadSuspendState);

    return rc;
}

void MultipriorityThreadPool::stopThreads()
{
    bdlmtt::LockGuard<bdlmtt::Mutex> metaLock(&d_metaMutex);
    bdlmtt::LockGuard<bdlmtt::Mutex> lock(&d_mutex);

    if (STOPPED == d_threadStartState) {
        return;                                                       // RETURN
    }

    BSLS_ASSERT(STARTED == d_threadStartState);
    d_threadStartState = STOPPING;

    // Give waiting threads a chance to stop.
    if (SUSPENDED == d_threadSuspendState) {
        BSLS_ASSERT(0 == numActiveThreads());
        d_resumeCondition.broadcast();
    }
    else {
        const ThreadFunctor nullJob;
        // Push high-priority null jobs into multi-priority queue, in case
        // threads are already blocking on pops of the queue for input.
        // 'worker' will do a no-op when it encounters these jobs.  There
        // might be fewer than 'numThreads()' threads to stop, but extra
        // null jobs do no harm.
        d_queue.pushFrontMultipleRaw(nullJob, 0, d_numThreads);
    }

    {
        bdlmtt::LockGuardUnlock<bdlmtt::Mutex> unlock(&d_mutex);
        d_threadGroup.joinAll();
    }

    d_threadStartState = STOPPED;

    BSLS_ASSERT(0 == d_numStartedThreads);
    BSLS_ASSERT(0 == d_numSuspendedThreads);
    BSLS_ASSERT(0 == d_numActiveThreads);
}

void MultipriorityThreadPool::suspendProcessing()
{
    bdlmtt::LockGuard<bdlmtt::Mutex> metaLock(&d_metaMutex);
    bdlmtt::LockGuard<bdlmtt::Mutex> lock(&d_mutex);

    if (SUSPENDED == d_threadSuspendState) {
        return;                                                       // RETURN
    }
    BSLS_ASSERT(RESUMED == d_threadSuspendState);

    if (STOPPED == d_threadStartState) {
        d_threadSuspendState = SUSPENDED;

        BSLS_ASSERT(0 == d_numStartedThreads);
        BSLS_ASSERT(0 == d_numSuspendedThreads);
        return;                                                       // RETURN
    }

    d_threadSuspendState = SUSPENDING;

    const ThreadFunctor nullJob;

    // Push high-priority null jobs into multi-priority queue, in case
    // threads are already blocking on pops of the queue for input.
    // 'worker' will do a no-op when it encounters these jobs.
    d_queue.pushFrontMultipleRaw(nullJob, 0, d_numThreads);

    do {
        d_allThreadsSuspendedCondition.wait(&d_mutex);
    } while (SUSPENDED != d_threadSuspendState);

    BSLS_ASSERT(d_numThreads == d_numStartedThreads);
    BSLS_ASSERT(d_numThreads == d_numSuspendedThreads);
}

void MultipriorityThreadPool::resumeProcessing()
{
    bdlmtt::LockGuard<bdlmtt::Mutex> metaLock(&d_metaMutex);

    if (RESUMED == d_threadSuspendState) {
        return;                                                       // RETURN
    }
    BSLS_ASSERT(SUSPENDED == d_threadSuspendState);

    bdlmtt::LockGuard<bdlmtt::Mutex> lock(&d_mutex);

    d_threadSuspendState = RESUMED;
    d_resumeCondition.broadcast();
}

void MultipriorityThreadPool::drainJobs()
{
    bdlmtt::LockGuard<bdlmtt::Mutex> metaLock(&d_metaMutex);

    // If these two conditions are not true, this method will hang.
    BSLS_ASSERT(STARTED == d_threadStartState);
    BSLS_ASSERT(RESUMED == d_threadSuspendState);

    bdlmtt::Barrier barrier(d_numThreads + 1);
    const ThreadFunctor barrierJob =
                         bdlf::MemFnUtil::memFn(&bdlmtt::Barrier::wait, &barrier);

    d_queue.pushBackMultipleRaw(barrierJob, d_queue.numPriorities() - 1,
                                                                 d_numThreads);

    barrier.wait();
}

void MultipriorityThreadPool::removeJobs()
{
    bdlmtt::LockGuard<bdlmtt::Mutex> metaLock(&d_metaMutex);

    d_queue.removeAll();
}

void MultipriorityThreadPool::shutdown()
{
    disableQueue();
    removeJobs();
    stopThreads();
}

// ACCESSORS
bool MultipriorityThreadPool::isEnabled() const
{
    return d_queue.isEnabled();
}

bool MultipriorityThreadPool::isStarted() const
{
    return STARTED == d_threadStartState;
}

bool MultipriorityThreadPool::isSuspended() const
{
    return SUSPENDED == d_threadSuspendState;
}

int MultipriorityThreadPool::numActiveThreads() const
{
    return d_numActiveThreads;
}

int MultipriorityThreadPool::numPriorities() const
{
    return d_queue.numPriorities();
}

int MultipriorityThreadPool::numPendingJobs() const
{
    return d_queue.length();
}

int MultipriorityThreadPool::numStartedThreads() const
{
    return d_numStartedThreads;
}

int MultipriorityThreadPool::numThreads() const
{
    return d_numThreads;
}
}  // close package namespace

}  // close namespace BloombergLP

// ----------------------------------------------------------------------------
// NOTICE:
//      Copyright (C) Bloomberg L.P., 2008
//      All Rights Reserved.
//      Property of Bloomberg L.P. (BLP)
//      This software is made available solely pursuant to the
//      terms of a BLP license agreement which governs its use.
// ------------------------------- END-OF-FILE --------------------------------
