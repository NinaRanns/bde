// btlmt_tcptimereventmanager.cpp                                     -*-C++-*-
#include <btlmt_tcptimereventmanager.h>

#include <bsls_ident.h>
BSLS_IDENT_RCSID(btlmt_tcptimereventmanager_cpp,"$Id$ $CSID$")

#include <btlso_defaulteventmanager.h>
#include <btlso_defaulteventmanager_devpoll.h>
#include <btlso_defaulteventmanager_epoll.h>
#include <btlso_defaulteventmanager_poll.h>
#include <btlso_defaulteventmanager_select.h>
#include <btlso_eventmanager.h>
#include <btlso_platform.h>
#include <btlso_socketimputil.h>

#include <bdlmtt_lockguard.h>
#include <bdlmtt_readlockguard.h>
#include <bdlmtt_writelockguard.h>

#include <bsls_timeinterval.h>
#include <bdlt_currenttime.h>

#include <bdlf_function.h>
#include <bdlf_bind.h>
#include <bdlf_memfn.h>

#include <bdlma_bufferedsequentialallocator.h>

#include <bslalg_typetraits.h>
#include <bslalg_typetraitusesbslmaallocator.h>
#include <bslma_autorawdeleter.h>
#include <bslma_default.h>
#include <bslmf_assert.h>

#include <bsls_assert.h>
#include <bsls_types.h>

#include <bsl_cstdio.h>                // printf
#include <bsl_ostream.h>

#include <bsl_c_errno.h>

#if defined(BSLS_PLATFORM_OS_UNIX)
#include <bsl_c_signal.h>              // sigfillset
#endif

using namespace bsl;  // automatically added by script

namespace BloombergLP {


// Confirm that we use the right size atomic variable for the socket handle in
// the control channel.

#if defined(BTLSO_PLATFORM_WIN_SOCKETS) && defined(BSLS_PLATFORM_CPU_64_BIT)

BSLMF_ASSERT(8 == sizeof(btlso::SocketHandle::Handle));

#else

BSLMF_ASSERT(4 == sizeof(btlso::SocketHandle::Handle));

#endif

enum {
   // This 'enum' specifies the constants used in this component.

    MAX_NUM_RETRIES = 3   // Maximum number of times the control channel can be
                          // reinitialized.
};

namespace btlmt {                // ===============================================
                // class TcpTimerEventManager_ControlChannel
                // ===============================================

// IMPLEMENTATION NOTES: This class manages a client-server pair of
// connected sockets.  It is used to communicate requests to the dispatcher
// thread while it is executing the dispatcher loop.  This communication is
// done via sockets because this ensures that the loop is unblocked if it
// is blocking in the socket polling step (when no timers are enqueued).
//
// Reinitialization of the ControlChannel:  In DRQS 44989721 it was found that
// on Windows machines one of the client-server sockets was occasionally
// killed by firewall or security software.  This made the ControlChannel
// useless and completely paralysed the functioning of
// 'TcpTimerEventManager'.  The fix to this problem was to identify this
// scenario and restart the ControlChannel.  In that scenario all data
// associated with the channel is lost and the channel opens a new
// client-server socket pair.  Subsequent communication is expected to work as
// before.  'TcpTimerEventManager' tries to recover from this error
// 'MAX_NUM_RETRIES' times after which it asserts on detecting this error.


                   // ========================================
                   // class TcpTimerEventManager_Request
                   // ========================================

class TcpTimerEventManager_Request {
    // This class represents a request to the dispatcher thread.
    // It contains all the parameters associated with the request and,
    // depending on the request type, can be used to report results.  It is
    // simple by design, and does not use dynamic memory.  It is used by the
    // timer event manager to synchronize with the dispatcher thread, and is
    // enqueued onto a ControlChannel (connect pair of sockets) in order to
    // trigger the polling mechanism in the dispatcher thread loop, even if no
    // other event is scheduled.

  public:
    // TRAITS
    BSLALG_DECLARE_NESTED_TRAITS(TcpTimerEventManager_Request,
                                 bslalg::TypeTraitUsesBslmaAllocator);

    enum OpCode {
        NO_OP,                         // no operation
        TERMINATE,                     // exit signal
        DEREGISTER_ALL_SOCKET_EVENTS,  // invoke 'deregisterAllSocketEvents'
        DEREGISTER_ALL_TIMERS,         // invoke 'deregisterAllTimers'
        DEREGISTER_SOCKET_EVENT,       // invoke 'deregisterSocketEvent'
        DEREGISTER_SOCKET,             // invoke 'deregisterSocket'
        DEREGISTER_TIMER,              // invoke 'deregisterTimer'
        REGISTER_SOCKET_EVENT,         // invoke 'registerSocketEvent'
        EXECUTE,                       // invoke 'execute'
        REGISTER_TIMER,                // invoke 'registerTimer'
        RESCHEDULE_TIMER,              // invoke 'rescheduleTimer'
        IS_REGISTERED,                 // invoke 'isRegistered'.
        NUM_SOCKET_EVENTS              // invoke 'numSocketEvents'.
    };

  private:
    // DATA
    OpCode                        d_opCode;       // request type
    bdlmtt::Mutex                  *d_mutex_p;      // result notification
    bdlmtt::Condition              *d_condition_p;  //

    // The following two fields are used in socket related requests.
    btlso::SocketHandle::Handle    d_handle;       // socket handle associated
                                                  // with this request (in)
    btlso::EventType::Type         d_eventType;    // event code (in)

    // The following two fields are used in timer related requests.
    bsls::TimeInterval             d_timeout;      // timeout interval (in)
    void                         *d_timerId;      // timer ID (in/out)

    // The following field is used in both socket and timer related
    // registration requests.
    btlso::EventManager::Callback  d_callback;     // callback to be registered
                                                  // (in)

    // The following object is used for responses.
    int                           d_result;       // (out)

  public:
    // CREATORS
    TcpTimerEventManager_Request(
                      const btlso::SocketHandle::Handle&    handle,
                      btlso::EventType::Type                event,
                      const btlso::EventManager::Callback&  callback,
                      bslma::Allocator                    *basicAllocator = 0);
        // Create a 'REGISTER_SOCKET_EVENT' request containing the specified
        // socket 'handle', the specified 'event' and the specified
        // 'callback'.  Optionally specify a 'basicAllocator' used to supply
        // memory.  If 'basicAllocator' is 0, the currently installed default
        // allocator is used.

    TcpTimerEventManager_Request(
                      const bsls::TimeInterval&             timeout,
                      const btlso::EventManager::Callback&  callback,
                      bslma::Allocator                    *basicAllocator = 0);
        // Create a 'REGISTER_TIMER' request containing the specified 'timeout'
        // and the specified 'callback'.  Optionally specify a
        // 'basicAllocator' used to supply memory.  If 'basicAllocator' is 0,
        // the currently installed default allocator is used.

    TcpTimerEventManager_Request(
                                 const void               *timerId,
                                 const bsls::TimeInterval&  timeout,
                                 bslma::Allocator         *basicAllocator = 0);
        // Create a 'RESCHEDULE_TIMER' request containing the specified
        // 'timerId' and the specified 'timeOut'.  Optionally specify a
        // 'basicAllocator' used to supply memory.  If 'basicAllocator' is 0,
        // the currently installed default allocator is used.

    TcpTimerEventManager_Request(void             *timerId,
                                       bslma::Allocator *basicAllocator = 0);
        // Create a 'DEREGISTER_TIMER' request containing the specified
        // 'timerId'.  Optionally specify a 'basicAllocator' used to supply
        // memory.  If 'basicAllocator' is 0, the currently installed default
        // allocator is used.

    TcpTimerEventManager_Request(
                        const btlso::SocketHandle::Handle&  handle,
                        bslma::Allocator                  *basicAllocator = 0);
        // Create a 'DEREGISTER_SOCKET' request containing the specified
        // 'handle'.  Optionally specify a 'basicAllocator' used to supply
        // memory.  If 'basicAllocator' is 0, the currently installed default
        // allocator is used.

    TcpTimerEventManager_Request(
                               btlso::SocketHandle::Handle  handle,
                               btlso::EventType::Type       event,
                               bslma::Allocator           *basicAllocator = 0);
        // Create a 'DEREGISTER_SOCKET_EVENT' request containing the specified
        // 'handle' and the specified 'event'.  Optionally specify a
        // 'basicAllocator' used to supply memory.  If 'basicAllocator' is 0,
        // the currently installed default allocator is used.

    TcpTimerEventManager_Request(
                               btlso::SocketHandle::Handle  handle,
                               btlso::EventType::Type       event,
                               bdlmtt::Condition            *condition,
                               bdlmtt::Mutex                *mutex,
                               bslma::Allocator           *basicAllocator = 0);
        // Create an 'IS_REGISTERED' request containing the specified 'handle'
        // and the specified 'event'; the specified 'condition' is signaled
        // when the request is processed and the specified 'mutex' is used to
        // synchronize access to the result (i.e., 'result').  Optionally
        // specify a 'basicAllocator' used to supply memory.  If
        // 'basicAllocator' is 0, the currently installed default allocator is
        // used.

    TcpTimerEventManager_Request(
                        const btlso::SocketHandle::Handle&  handle,
                        bdlmtt::Condition                   *condition,
                        bdlmtt::Mutex                       *mutex,
                        bslma::Allocator                  *basicAllocator = 0);
        // Create a 'NUM_SOCKET_EVENTS' request containing the specified
        // 'handle' the specified 'condition' is signaled when the request is
        // processed and the specified 'mutex' is used to synchronize access to
        // the result (i.e., 'result').  Optionally specify a 'basicAllocator'
        // used to supply memory.  If 'basicAllocator' is 0, the currently
        // installed default allocator is used.

    TcpTimerEventManager_Request(OpCode            code,
                                       bslma::Allocator *basicAllocator = 0);
        // Create a request having the specified 'code'.  The behavior is
        // undefined unless 'code' is 'DEREGISTER_ALL_SOCKET_EVENTS',
        // 'DEREGISTER_ALL_TIMERS' or 'NO_OP'.  Optionally specify a
        // 'basicAllocator' used to supply memory.  If 'basicAllocator' is 0,
        // the currently installed default allocator is used.

    TcpTimerEventManager_Request(OpCode            code,
                                       bdlmtt::Condition  *condition,
                                       bdlmtt::Mutex      *mutex,
                                       bslma::Allocator *basicAllocator = 0);
        // Create a request with the specified 'code'; the request will be
        // processed according to the value of 'code'.  The behavior is
        // undefined if 'code' has a value that requires some fields other
        // than 'condition' or 'mutex' to be defined.  The treatment of the
        // specified 'condition' and 'mutex' depends on the value of 'opCode'.
        // Optionally specify a 'basicAllocator' used to supply memory.  If
        // 'basicAllocator' is 0, the currently installed default allocator is
        // used.

    TcpTimerEventManager_Request(
                      const btlso::EventManager::Callback&  callback,
                      bslma::Allocator                    *basicAllocator = 0);
        // Create an 'EXECUTE' request for the specified 'functor.  Optionally
        // specify a 'basicAllocator' used to supply memory.  If
        // 'basicAllocator' is 0, the currently installed default allocator is
        // used.

    ~TcpTimerEventManager_Request();
        // Destroy this request

    // MANIPULATORS
    void setTimerId(void *value);
        // Set the timer id contained in this request to the specified
        // 'value'.  The behavior is undefined unless 'opCode' is
        // 'REGISTER_TIMER'.

    void setResult(int value);
        // Set the result contained in this request to the specified
        // 'value'.  The behavior is undefined unless 'opCode' is
        // 'IS_REGISTERED'.

    void signal();
        // Signal the completion of the processing of this request.
        // The behavior is undefined unless 'opCode' refers to one of the
        // blocking request types.

    void waitForResult();
        // Suspend the calling thread until this request is processed
        // and the result is available.  The behavior is undefined unless
        // 'opCode' for this request refers to one of the blocking request
        // types.

    // ACCESSORS
    const btlso::EventManager::Callback& callback() const;
        // Return the callback contained in this request.  The behavior is
        // undefined unless 'opCode' is either 'REGISTER_SOCKET_EVENT' or
        // 'REGISTER_TIMER';

    btlso::EventType::Type event() const;
        // Return the socket event type contained in this request.  The
        // behavior is undefined unless 'opCode' is one of the following:
        // 'REGISTER_SOCKET_EVENT', 'DEREGISTER_SOCKET_EVENT'.

    OpCode opCode() const;
        // Return the op code for this request;

    const btlso::SocketHandle::Handle& socketHandle() const;
        // Return the socked handle contained in this request.  The behavior
        // is undefined unless 'opCode' is one of the following:
        // 'REGISTER_SOCKET_EVENT', 'DEREGISTER_SOCKET_EVENT',
        // 'DEREGISTER_SOCKET', 'NUM_SOCKET_EVENTS'.

    int result() const;
        // Return the result contained in this request.

    const bsls::TimeInterval& timeout() const;
        // Return the timeout value contained with this request.  The
        // behavior is undefined unless 'opCode' is 'REGISTER_TIMER' or
        // 'RESCHEDULE_TIMER'.

    const void *timerId() const;
        // Return the timer 'id' contained in this object.  The behavior is
        // undefined unless 'opCode' is 'REGISTER_TIMER', 'RESCHEDULE_TIMER'
        // or 'DEREGISTER_TIMER'.
};

                   // ----------------------------------------
                   // class TcpTimerEventManager_Request
                   // ----------------------------------------

// CREATORS
inline
TcpTimerEventManager_Request::TcpTimerEventManager_Request(
                           const btlso::SocketHandle::Handle&    handle,
                           btlso::EventType::Type                event,
                           const btlso::EventManager::Callback&  callback,
                           bslma::Allocator                    *basicAllocator)
: d_opCode(REGISTER_SOCKET_EVENT)
, d_mutex_p(0)
, d_condition_p(0)
, d_handle(handle)
, d_eventType(event)
, d_timerId((void *) 0)
, d_callback(callback, basicAllocator)
, d_result(-1)
{
}

inline
TcpTimerEventManager_Request::TcpTimerEventManager_Request(
                           const bsls::TimeInterval&             timeout,
                           const btlso::EventManager::Callback&  callback,
                           bslma::Allocator                    *basicAllocator)
: d_opCode(REGISTER_TIMER)
, d_mutex_p(0)
, d_condition_p(0)
, d_timeout(timeout)
, d_timerId((void *) 0)
, d_callback(callback, basicAllocator)
, d_result(-1)
{
}

inline
TcpTimerEventManager_Request::TcpTimerEventManager_Request(
                                      const void               *timerId,
                                      const bsls::TimeInterval&  timeout,
                                      bslma::Allocator         *basicAllocator)
: d_opCode(RESCHEDULE_TIMER)
, d_mutex_p(0)
, d_condition_p(0)
, d_timeout(timeout)
, d_timerId(const_cast<void *>(timerId))
, d_callback(basicAllocator)
, d_result(-1)
{
}

inline
TcpTimerEventManager_Request::TcpTimerEventManager_Request(
                                              void             *timerId,
                                              bslma::Allocator *basicAllocator)
: d_opCode(DEREGISTER_TIMER)
, d_mutex_p(0)
, d_condition_p(0)
, d_timerId(timerId)
, d_callback(basicAllocator)
, d_result(-1)
{
}

inline
TcpTimerEventManager_Request::TcpTimerEventManager_Request(
                             const btlso::SocketHandle::Handle&  handle,
                             bslma::Allocator                  *basicAllocator)
: d_opCode(DEREGISTER_SOCKET)
, d_mutex_p(0)
, d_condition_p(0)
, d_handle(handle)
, d_timerId((void *) 0)
, d_callback(basicAllocator)
, d_result(-1)
{
}

inline
TcpTimerEventManager_Request::TcpTimerEventManager_Request(
                             const btlso::SocketHandle::Handle&  handle,
                             bdlmtt::Condition                   *condition,
                             bdlmtt::Mutex                       *mutex,
                             bslma::Allocator                  *basicAllocator)
: d_opCode(NUM_SOCKET_EVENTS)
, d_mutex_p(mutex)
, d_condition_p(condition)
, d_handle(handle)
, d_timerId((void *) 0)
, d_callback(basicAllocator)
, d_result(-1)
{
}

inline
TcpTimerEventManager_Request::TcpTimerEventManager_Request(
                                    btlso::SocketHandle::Handle  handle,
                                    btlso::EventType::Type       event,
                                    bslma::Allocator           *basicAllocator)
: d_opCode(DEREGISTER_SOCKET_EVENT)
, d_mutex_p(0)
, d_condition_p(0)
, d_handle(handle)
, d_eventType(event)
, d_timerId((void *) 0)
, d_callback(basicAllocator)
, d_result(-1)
{
}

inline
TcpTimerEventManager_Request::TcpTimerEventManager_Request(
                                    btlso::SocketHandle::Handle  handle,
                                    btlso::EventType::Type       event,
                                    bdlmtt::Condition            *condition,
                                    bdlmtt::Mutex                *mutex,
                                    bslma::Allocator           *basicAllocator)
: d_opCode(IS_REGISTERED)
, d_mutex_p(mutex)
, d_condition_p(condition)
, d_handle(handle)
, d_eventType(event)
, d_timerId((void *) 0)
, d_callback(basicAllocator)
, d_result(-1)
{
}

inline
TcpTimerEventManager_Request::TcpTimerEventManager_Request(
                                              OpCode            code,
                                              bslma::Allocator *basicAllocator)
: d_opCode(code)
, d_mutex_p(0)
, d_condition_p(0)
, d_timerId((void *) 0)
, d_callback(basicAllocator)
, d_result(-1)
{
    BSLS_ASSERT(NO_OP == code
                || TERMINATE == code
                || DEREGISTER_ALL_SOCKET_EVENTS == code
                || DEREGISTER_ALL_TIMERS == code);
}

inline
TcpTimerEventManager_Request::TcpTimerEventManager_Request(
                                              OpCode            code,
                                              bdlmtt::Condition  *condition,
                                              bdlmtt::Mutex      *mutex,
                                              bslma::Allocator *basicAllocator)
: d_opCode(code)
, d_mutex_p(mutex)
, d_condition_p(condition)
, d_timerId((void *) 0)
, d_callback(basicAllocator)
, d_result(-1)
{
    BSLS_ASSERT(NO_OP == code || TERMINATE == code);
}

inline
TcpTimerEventManager_Request::TcpTimerEventManager_Request(
                           const btlso::EventManager::Callback&  callback,
                           bslma::Allocator                    *basicAllocator)
: d_opCode(EXECUTE)
, d_mutex_p(0)
, d_condition_p(0)
, d_timerId((void *) 0)
, d_callback(callback, basicAllocator)
, d_result(-1)
{
}

inline
TcpTimerEventManager_Request::~TcpTimerEventManager_Request()
{
}

// MANIPULATORS
inline
void TcpTimerEventManager_Request::setTimerId(void *value)
{
    bdlmtt::LockGuard<bdlmtt::Mutex> lock(d_mutex_p);
    d_timerId = value;
}

inline
void TcpTimerEventManager_Request::setResult(int value)
{
    bdlmtt::LockGuard<bdlmtt::Mutex> lock(d_mutex_p);
    d_result = value;
}

inline
void TcpTimerEventManager_Request::signal()
{
    BSLS_ASSERT(d_condition_p);
    d_condition_p->signal();
}

inline
void TcpTimerEventManager_Request::waitForResult()
{
    switch (d_opCode) {
      case NO_OP:
      case IS_REGISTERED:
      case NUM_SOCKET_EVENTS: {
        while (-1 == d_result) {
            d_condition_p->wait(d_mutex_p);
        }
      } break;
      case REGISTER_TIMER: {
        while (!d_timerId) {
            d_condition_p->wait(d_mutex_p);
        }
      } break;
      default: {
        BSLS_ASSERT("MUST BE UNREACHABLE BY DESIGN." && 0);
      } break;
    }
}

// ACCESSORS
inline
const btlso::EventManager::Callback&
TcpTimerEventManager_Request::callback() const
{
    BSLS_ASSERT(d_opCode == REGISTER_SOCKET_EVENT
                || d_opCode == REGISTER_TIMER
                || d_opCode == EXECUTE);
    return d_callback;
}

inline
btlso::EventType::Type TcpTimerEventManager_Request::event() const
{
    BSLS_ASSERT(REGISTER_SOCKET_EVENT == d_opCode
                || DEREGISTER_SOCKET_EVENT == d_opCode
                || IS_REGISTERED == d_opCode);
    return d_eventType;
}

inline
TcpTimerEventManager_Request::OpCode
TcpTimerEventManager_Request::opCode() const
{
    return d_opCode;
}

inline
const btlso::SocketHandle::Handle&
TcpTimerEventManager_Request::socketHandle() const
{
    BSLS_ASSERT(REGISTER_SOCKET_EVENT == d_opCode
                || DEREGISTER_SOCKET_EVENT == d_opCode
                || DEREGISTER_SOCKET == d_opCode
                || IS_REGISTERED == d_opCode
                || NUM_SOCKET_EVENTS == d_opCode);
    return d_handle;
}

inline
int TcpTimerEventManager_Request::result() const
{
    return d_result;
}

inline
const bsls::TimeInterval& TcpTimerEventManager_Request::timeout() const
{
    return d_timeout;
}

inline
const void *TcpTimerEventManager_Request::timerId() const
{
    BSLS_ASSERT(EXECUTE == d_opCode
                || REGISTER_TIMER == d_opCode
                || RESCHEDULE_TIMER == d_opCode
                || DEREGISTER_TIMER == d_opCode);
    return d_timerId;
}

inline
const char *toAscii(btlmt::TcpTimerEventManager_Request::OpCode value)
{
#define CASE(X) case(X): return #X;

    switch (value) {
      CASE(btlmt::TcpTimerEventManager_Request::NO_OP)
      CASE(btlmt::TcpTimerEventManager_Request::EXECUTE)
      CASE(btlmt::TcpTimerEventManager_Request::TERMINATE)
      CASE(btlmt::TcpTimerEventManager_Request::DEREGISTER_ALL_SOCKET_EVENTS)
      CASE(btlmt::TcpTimerEventManager_Request::DEREGISTER_ALL_TIMERS)
      CASE(btlmt::TcpTimerEventManager_Request::DEREGISTER_SOCKET_EVENT)
      CASE(btlmt::TcpTimerEventManager_Request::DEREGISTER_SOCKET)
      CASE(btlmt::TcpTimerEventManager_Request::DEREGISTER_TIMER)
      CASE(btlmt::TcpTimerEventManager_Request::REGISTER_SOCKET_EVENT)
      CASE(btlmt::TcpTimerEventManager_Request::REGISTER_TIMER)
      CASE(btlmt::TcpTimerEventManager_Request::IS_REGISTERED)
      CASE(btlmt::TcpTimerEventManager_Request::NUM_SOCKET_EVENTS)
      default: return "(* UNKNOWN *)";
    }

#undef CASE
}

// FREE OPERATORS
bsl::ostream& operator<<(bsl::ostream& stream,
                         TcpTimerEventManager_Request::OpCode rhs)
{
    return stream << toAscii(rhs);
}

              // -----------------------------------------------
              // class TcpTimerEventManager_ControlChannel
              // -----------------------------------------------

// CREATORS
TcpTimerEventManager_ControlChannel::
                                    TcpTimerEventManager_ControlChannel()
: d_byte(0x53)
, d_numServerReads(0)
, d_numServerBytesRead(0)
{
    const int rc = open();
    BSLS_ASSERT(0 == rc);
    (void) rc;                                   // quash warning in opt builds
}

TcpTimerEventManager_ControlChannel::
                                   ~TcpTimerEventManager_ControlChannel()
{
    const int rc = close();
    BSLS_ASSERT(0 == rc);
    (void) rc;                                   // quash warning in opt builds
}

// MANIPULATORS
int TcpTimerEventManager_ControlChannel::clientWrite(bool forceWrite)
{
    if (1 == ++d_numPendingRequests || forceWrite) {
        int errorNumber = 0;
        int rc;
        do {
            rc = btlso::SocketImpUtil::write(clientFd(),
                                            &d_byte,
                                            sizeof(char),
                                            &errorNumber);

            if (rc < 0 && btlso::SocketHandle::BTESO_ERROR_INTERRUPTED != rc) {
                --d_numPendingRequests;
                return rc;                                            // RETURN
            }
        } while (btlso::SocketHandle::BTESO_ERROR_INTERRUPTED == rc);
        if (rc >= 0) {
            return rc;
        }
        bsl::printf("%s(%d): Failed to communicate request to control channel"
                    " (errno = %d, errorNumber = %d, rc = %d).\n",
                    __FILE__, __LINE__, errno, errorNumber, rc);
        BSLS_ASSERT(errorNumber > 0);
        return -errorNumber;
    }
    return 0;
}

int TcpTimerEventManager_ControlChannel::close()
{
    btlso::SocketImpUtil::close(clientFd());
    btlso::SocketImpUtil::close(serverFd());
    return 0;
}

int TcpTimerEventManager_ControlChannel::open()
{
    btlso::SocketHandle::Handle fds[2];

#ifdef BTLSO_PLATFORM_BSD_SOCKETS
    // Use UNIX domain sockets, if possible, rather than a standard socket
    // pair, to avoid using ephemeral ports for the control channel.  AIX and
    // Sun platforms have a more restrictive number of epheremal ports, and
    // several production machines have come close to that limit ({DRQS
    // 28135201<GO>}).  Note that the posix standard 'AF_LOCAL', is not
    // supported by a number of platforms -- use the legacy identifier,
    // 'AF_UNIX', instead.

    int rc = ::socketpair(AF_UNIX, SOCK_STREAM, 0, fds);
#else
    int rc = btlso::SocketImpUtil::socketPair<btlso::IPv4Address>(
                                     fds,
                                     btlso::SocketImpUtil::BTESO_SOCKET_STREAM);

#endif

    if (rc) {
        d_fds[0] = static_cast<int>(btlso::SocketHandle::INVALID_SOCKET_HANDLE);
        d_fds[1] = static_cast<int>(btlso::SocketHandle::INVALID_SOCKET_HANDLE);

        bsl::printf("%s(%d): Failed to create control channel"
                    " (errno = %d, rc = %d).\n",
                    __FILE__, __LINE__, errno, rc);
        return rc;                                                    // RETURN
    }

    btlso::IoUtil::setBlockingMode(fds[1],
                                  btlso::IoUtil::BTESO_NONBLOCKING,
                                  0);
    btlso::SocketOptUtil::setOption(fds[0],
                                   btlso::SocketOptUtil::BTESO_TCPLEVEL,
                                   btlso::SocketOptUtil::BTESO_TCPNODELAY,
                                   1);

    d_fds[0] = static_cast<int>(fds[0]);
    d_fds[1] = static_cast<int>(fds[1]);

    return 0;
}

int TcpTimerEventManager_ControlChannel::serverRead()
{
    int rc = d_numPendingRequests.swap(0);
    char byte;

    const int numBytes = btlso::SocketImpUtil::read(&byte, serverFd(), 1);
    if (numBytes <= 0) {
        return -1;                                                    // RETURN
    }

    ++d_numServerReads;
    d_numServerBytesRead += numBytes;

    return rc;
}

                         // --------------------------------
                         // class TcpTimerEventManager
                         // --------------------------------

// PRIVATE METHODS
void TcpTimerEventManager::initialize()
{
    BSLS_ASSERT(d_allocator_p);

    btlso::TimeMetrics *metrics = d_collectMetrics ? &d_metrics : 0;

    // Initialize the (managed) event manager.
#ifdef BSLS_PLATFORM_OS_LINUX
    if (btlso::DefaultEventManager<>::isSupported()) {
        d_manager_p = new (*d_allocator_p)
                           btlso::DefaultEventManager<>(metrics, d_allocator_p);
    }
    else {
        d_manager_p = new (*d_allocator_p)
                           btlso::DefaultEventManager<btlso::Platform::POLL>(
                                                       metrics, d_allocator_p);
    }
#else
    d_manager_p = new (*d_allocator_p)
                           btlso::DefaultEventManager<>(metrics, d_allocator_p);
#endif

    d_isManagedFlag = 1;

    // Initialize the functor containing the dispatch thread's entry point
    // method.
    d_dispatchThreadEntryPoint
        = bdlf::Function<void (*)()>(
                bdlf::MemFnUtil::memFn(
                      &TcpTimerEventManager::dispatchThreadEntryPoint
                    , this)
              , d_allocator_p);

    // Create the queue of executed timers.
    d_executeQueue_p = new (*d_allocator_p)
                        bsl::vector<bdlf::Function<void (*)()> >(d_allocator_p);
    d_executeQueue_p->reserve(4);
}

int TcpTimerEventManager::initiateControlChannelRead()
{
    // Wait for the dispatcher thread to start and process
    // the request.

    bdlmtt::Mutex     mutex;
    bdlmtt::Condition condition;

    TcpTimerEventManager_Request *req =
        new (d_requestPool) TcpTimerEventManager_Request(
                                     TcpTimerEventManager_Request::NO_OP,
                                     &condition,
                                     &mutex,
                                     d_allocator_p);
    BSLS_ASSERT(-1 == req->result());
    bdlmtt::LockGuard<bdlmtt::Mutex> lock(&mutex);

    d_requestQueue.pushBack(req);
    int ret = d_controlChannel_p->clientWrite(true);

    if (0 > ret) {
        d_requestQueue.popBack();
        d_requestPool.deleteObjectRaw(req);
        return ret;                                               // RETURN
    }
    req->waitForResult();
    d_requestPool.deleteObjectRaw(req);
    return 0;
}

void TcpTimerEventManager::controlCb()
    // At least one request is pending on the queue.  Process as many
    // as there are.
{
    int numRequests = d_controlChannel_p->serverRead();

    if (numRequests < 0) {
        const int rc = reinitializeControlChannel();
        BSLS_ASSERT_OPT(0 == rc);

        return;                                                       // RETURN
    }

    BSLS_ASSERT(0 <= numRequests);
    BSLS_ASSERT(numRequests <= d_requestQueue.length());

    for (int i = 0; i < numRequests; ++i) {
        BSLS_ASSERT_SAFE(d_requestQueue.length() > 0);

        TcpTimerEventManager_Request *req = d_requestQueue.popFront();

        switch (req->opCode()) {
            case TcpTimerEventManager_Request::TERMINATE: {
                BSLS_ASSERT(-1 == req->result());
                req->setResult(0);
                d_terminateThread = 1;
                req->signal();
            } break;
            case TcpTimerEventManager_Request::NO_OP: {
                BSLS_ASSERT(-1 == req->result());
                req->setResult(0);
                req->signal();
            } break;
            case TcpTimerEventManager_Request::REGISTER_SOCKET_EVENT: {
                d_manager_p->registerSocketEvent(req->socketHandle(),
                                                 req->event(),
                                                 req->callback());
                d_numTotalSocketEvents = d_manager_p->numEvents()-1;
                d_requestPool.deleteObjectRaw(req);
            } break;
            case TcpTimerEventManager_Request::EXECUTE:   // FALL THROUGH
            case TcpTimerEventManager_Request::REGISTER_TIMER: {
                BSLS_ASSERT(0 == req->timerId());
                d_requestPool.deleteObjectRaw(req);
            } break;
            case TcpTimerEventManager_Request::RESCHEDULE_TIMER: {
                BSLS_ASSERT(0 != req->timerId());
                d_requestPool.deleteObjectRaw(req);
            } break;
            case TcpTimerEventManager_Request::DEREGISTER_TIMER: {
                BSLS_ASSERT(0 != req->timerId());
                d_timerQueue.remove((int)(bsls::Types::IntPtr)req->timerId());
            } break;
            case TcpTimerEventManager_Request::DEREGISTER_ALL_TIMERS: {
                d_timerQueue.removeAll();
            } break;
            case TcpTimerEventManager_Request::
                                                DEREGISTER_ALL_SOCKET_EVENTS: {
                d_manager_p->deregisterAll();
            } break;
            case TcpTimerEventManager_Request::DEREGISTER_SOCKET_EVENT: {
                d_manager_p->deregisterSocketEvent(req->socketHandle(),
                                                   req->event());
                d_numTotalSocketEvents = d_manager_p->numEvents()-1;
                d_requestPool.deleteObjectRaw(req);
            } break;
            case TcpTimerEventManager_Request::DEREGISTER_SOCKET: {
                d_manager_p->deregisterSocket(req->socketHandle());
                d_numTotalSocketEvents = d_manager_p->numEvents()-1;
                d_requestPool.deleteObjectRaw(req);
            } break;
            case TcpTimerEventManager_Request::NUM_SOCKET_EVENTS: {
                int result = d_manager_p->numSocketEvents(req->socketHandle());
                req->setResult(result);
                req->signal();
            } break;
            case TcpTimerEventManager_Request::IS_REGISTERED: {
                req->setResult(
                    d_manager_p->isRegistered(req->socketHandle(),
                                              req->event()));
                req->signal();
            } break;
            default: {
                BSLS_ASSERT("MUST BE UNREACHABLE BY DESIGN" && 0);
            }
        }
    }
}

void TcpTimerEventManager::dispatchThreadEntryPoint()
{
    if (d_collectMetrics) {
        d_metrics.switchTo(btlso::TimeMetrics::BTESO_CPU_BOUND);
    }

    bsl::vector<bdlf::Function<void (*)()> > *requestsPtr
        = new (*d_allocator_p)
                        bsl::vector<bdlf::Function<void (*)()> >(d_allocator_p);
    requestsPtr->reserve(4);
    bslma::AutoRawDeleter< bsl::vector<bdlf::Function<void (*)()> >
                        , bslma::Allocator>
                                    autoDelete(&requestsPtr, d_allocator_p, 1);

    // Set the state to BTEMT_ENABLED before dispatching any events
    // (DRQS 15212134).  Note that the thread calling 'enable' should be
    // blocked (awaiting a response on the control channel) and holding a write
    // lock to 'd_stateLock'.

    d_state = BTEMT_ENABLED;

    while (1) {
        // Dispatch socket events, from shorter to longer timeout.
        int rc = 0;
        if (d_executeQueue_p->size()) {
            bsls::TimeInterval zeroTimeout;
            rc = d_manager_p->dispatch(zeroTimeout, 0);// non-blocking
        } else if (d_timerQueue.length()) {
            bsls::TimeInterval timeout;
            d_timerQueue.minTime(&timeout);
            rc = d_manager_p->dispatch(timeout, 0);    // blocking w/ timeout
        }
        else {
            rc = d_manager_p->dispatch(0);             // blocking indefinitely
        }

        if (rc < 0) {
            // Check if the control channel is still connected or not

            btlso::IPv4Address address;
            rc = btlso::SocketImpUtil::getPeerAddress(
                                               &address,
                                               d_controlChannel_p->serverFd());
            if (0 != rc) {
                rc = reinitializeControlChannel();
                BSLS_ASSERT_OPT(0 == rc);
            }
        }

        // Process executed callbacks (without timeouts), in respective order.
        {
            // A lock is necessary here, and not just an atomic pointer, since
            // we must guarantee that the pop_back in execute() must hold the
            // same functor that was enqueued, which would no longer be true if
            // swap below could occur between the push_back(functor) and
            // pop_back(functor).

            bdlmtt::LockGuard<bdlmtt::Mutex> lockGuard(&d_executeQueueLock);

            BSLS_ASSERT(0 == requestsPtr->size());
            bsl::swap(d_executeQueue_p, requestsPtr);
        }

        int numCallbacks = requestsPtr->size();
        for (int i = 0; i < numCallbacks; ++i) {
            (*requestsPtr)[i]();
        }
        requestsPtr->clear();

        // Process expired timers in increasing time order.
        if (d_timerQueue.length()) {
            const int NUM_TIMERS = 32;
            const int SIZE = NUM_TIMERS *
                sizeof(bdlcc::TimeQueueItem<bdlf::Function<void (*)()> >);

            char BUFFER[SIZE];
            bdlma::BufferedSequentialAllocator bufferAllocator(BUFFER, SIZE);

            bsl::vector<bdlcc::TimeQueueItem<bdlf::Function<void (*)()> > >
                                                    requests(&bufferAllocator);
            d_timerQueue.popLE(bdlt::CurrentTime::now(), &requests);
            int numTimers = requests.size();
            for (int i = 0; i < numTimers; ++i) {
                requests[i].data()();
            }
        }

        // If a signal to quit has been issued leave immediately,
        // but only after processing expired callbacks (above).
        // This guarantees that memory associated with open channels
        // is deallocated.
        {
            if (d_terminateThread.relaxedLoad()) {  // it is volatile
                if (d_collectMetrics) {
                    d_metrics.switchTo(btlso::TimeMetrics::BTESO_IO_BOUND);
                }
                BSLS_ASSERT(0 == d_requestQueue.queue().length());
                BSLS_ASSERT(BTEMT_ENABLED == d_state);
                return;
            }
        }
    }
    BSLS_ASSERT("MUST BE UNREACHABLE BY DESIGN." && 0);
}

int TcpTimerEventManager::reinitializeControlChannel()
{
    ++d_numControlChannelReinitializations;
    BSLS_ASSERT_OPT(d_numControlChannelReinitializations <= MAX_NUM_RETRIES);

    d_manager_p->deregisterSocket(d_controlChannel_p->serverFd());

    int rc = d_controlChannel_p->close();
    BSLS_ASSERT_OPT(0 == rc);

    rc = d_controlChannel_p->open();
    BSLS_ASSERT_OPT(0 == rc);

    // Register the server fd of 'd_controlChannel_p' for READs.
    btlso::EventManager::Callback cb(
                  bdlf::MemFnUtil::memFn(&TcpTimerEventManager::controlCb,
                                        this),
                  d_allocator_p);

    rc = d_manager_p->registerSocketEvent(d_controlChannel_p->serverFd(),
                                          btlso::EventType::BTESO_READ,
                                          cb);
    if (rc) {
        printf("%s(%d): Failed to register controlChannel for READ events"
               " in TcpTimerEventManager constructor\n",
               __FILE__, __LINE__);
        BSLS_ASSERT("Failed to register controlChannel for READ events" &&
                    0);
        return rc;
    }

    bdlmtt::ThreadUtil::Handle handle;
    bdlmtt::ThreadAttributes   attributes;
    attributes.setDetachedState(bdlmtt::ThreadAttributes::BCEMT_CREATE_DETACHED);

    bdlf::Function<void (*)()> initiateReadFunctor = bdlf::Function<void (*)()>(
      bdlf::BindUtil::bindA(
                       d_allocator_p,
                       &TcpTimerEventManager::initiateControlChannelRead,
                       this));

    rc = bdlmtt::ThreadUtil::create(&handle, attributes, initiateReadFunctor);
    BSLS_ASSERT_OPT(0 == rc);
    return 0;
}

// CREATORS
TcpTimerEventManager::TcpTimerEventManager(
                                         bslma::Allocator *threadSafeAllocator)
: d_requestPool(sizeof(TcpTimerEventManager_Request),
                threadSafeAllocator)
, d_requestQueue(threadSafeAllocator)
, d_dispatcher(bdlmtt::ThreadUtil::invalidHandle())
, d_state(BTEMT_DISABLED)
, d_terminateThread(0)
, d_timerQueue(threadSafeAllocator)
, d_metrics(btlso::TimeMetrics::BTESO_MIN_NUM_CATEGORIES,
            btlso::TimeMetrics::BTESO_IO_BOUND,
            threadSafeAllocator)
, d_collectMetrics(true)
, d_numTotalSocketEvents(0)
, d_numControlChannelReinitializations(0)
, d_allocator_p(bslma::Default::allocator(threadSafeAllocator))
{
    initialize();
}

TcpTimerEventManager::TcpTimerEventManager(
                                        bool               collectTimeMetrics,
                                        bslma::Allocator  *threadSafeAllocator)
: d_requestPool(sizeof(TcpTimerEventManager_Request),
                threadSafeAllocator)
, d_requestQueue(threadSafeAllocator)
, d_dispatcher(bdlmtt::ThreadUtil::invalidHandle())
, d_state(BTEMT_DISABLED)
, d_terminateThread(0)
, d_timerQueue(threadSafeAllocator)
, d_metrics(btlso::TimeMetrics::BTESO_MIN_NUM_CATEGORIES,
            btlso::TimeMetrics::BTESO_IO_BOUND,
            threadSafeAllocator)
, d_collectMetrics(collectTimeMetrics)
, d_numTotalSocketEvents(0)
, d_numControlChannelReinitializations(0)
, d_allocator_p(bslma::Default::allocator(threadSafeAllocator))
{
    initialize();
}

TcpTimerEventManager::TcpTimerEventManager(
                                        bool               collectTimeMetrics,
                                        bool               poolTimerMemory,
                                        bslma::Allocator  *threadSafeAllocator)
: d_requestPool(sizeof(TcpTimerEventManager_Request),
                threadSafeAllocator)
, d_requestQueue(threadSafeAllocator)
, d_dispatcher(bdlmtt::ThreadUtil::invalidHandle())
, d_state(BTEMT_DISABLED)
, d_terminateThread(0)
, d_timerQueue(poolTimerMemory, threadSafeAllocator)
, d_metrics(btlso::TimeMetrics::BTESO_MIN_NUM_CATEGORIES,
            btlso::TimeMetrics::BTESO_IO_BOUND,
            threadSafeAllocator)
, d_collectMetrics(collectTimeMetrics)
, d_numTotalSocketEvents(0)
, d_numControlChannelReinitializations(0)
, d_allocator_p(bslma::Default::allocator(threadSafeAllocator))
{
    initialize();
}

TcpTimerEventManager::TcpTimerEventManager(
                                        Hint,
                                        bslma::Allocator  *threadSafeAllocator)
: d_requestPool(sizeof(TcpTimerEventManager_Request),
                threadSafeAllocator)
, d_requestQueue(threadSafeAllocator)
, d_dispatcher(bdlmtt::ThreadUtil::invalidHandle())
, d_state(BTEMT_DISABLED)
, d_terminateThread(0)
, d_timerQueue(threadSafeAllocator)
, d_metrics(btlso::TimeMetrics::BTESO_MIN_NUM_CATEGORIES,
            btlso::TimeMetrics::BTESO_IO_BOUND,
            threadSafeAllocator)
, d_collectMetrics(true)
, d_numTotalSocketEvents(0)
, d_numControlChannelReinitializations(0)
, d_allocator_p(bslma::Default::allocator(threadSafeAllocator))
{
    initialize();
}

TcpTimerEventManager::TcpTimerEventManager(
                                        Hint,
                                        bool               collectTimeMetrics,
                                        bslma::Allocator  *threadSafeAllocator)
: d_requestPool(sizeof(TcpTimerEventManager_Request),
                threadSafeAllocator)
, d_requestQueue(threadSafeAllocator)
, d_dispatcher(bdlmtt::ThreadUtil::invalidHandle())
, d_state(BTEMT_DISABLED)
, d_terminateThread(0)
, d_timerQueue(threadSafeAllocator)
, d_metrics(btlso::TimeMetrics::BTESO_MIN_NUM_CATEGORIES,
            btlso::TimeMetrics::BTESO_IO_BOUND,
            threadSafeAllocator)
, d_collectMetrics(collectTimeMetrics)
, d_numTotalSocketEvents(0)
, d_numControlChannelReinitializations(0)
, d_allocator_p(bslma::Default::allocator(threadSafeAllocator))
{
    initialize();
}

TcpTimerEventManager::TcpTimerEventManager(
                                        Hint,
                                        bool               collectTimeMetrics,
                                        bool               poolTimerMemory,
                                        bslma::Allocator  *threadSafeAllocator)
: d_requestPool(sizeof(TcpTimerEventManager_Request),
                threadSafeAllocator)
, d_requestQueue(threadSafeAllocator)
, d_dispatcher(bdlmtt::ThreadUtil::invalidHandle())
, d_state(BTEMT_DISABLED)
, d_terminateThread(0)
, d_timerQueue(poolTimerMemory, threadSafeAllocator)
, d_metrics(btlso::TimeMetrics::BTESO_MIN_NUM_CATEGORIES,
            btlso::TimeMetrics::BTESO_IO_BOUND,
            threadSafeAllocator)
, d_collectMetrics(collectTimeMetrics)
, d_numTotalSocketEvents(0)
, d_numControlChannelReinitializations(0)
, d_allocator_p(bslma::Default::allocator(threadSafeAllocator))
{
    initialize();
}

TcpTimerEventManager::TcpTimerEventManager(
                                       btlso::EventManager *rawEventManager,
                                       bslma::Allocator   *threadSafeAllocator)
: d_requestPool(sizeof(TcpTimerEventManager_Request),
                                                           threadSafeAllocator)
, d_requestQueue(threadSafeAllocator)
, d_dispatcher(bdlmtt::ThreadUtil::invalidHandle())
, d_state(BTEMT_DISABLED)
, d_terminateThread(0)
, d_manager_p(rawEventManager)
, d_isManagedFlag(0)
, d_timerQueue(threadSafeAllocator)
, d_metrics(btlso::TimeMetrics::BTESO_MIN_NUM_CATEGORIES,
            btlso::TimeMetrics::BTESO_IO_BOUND,
            threadSafeAllocator)
, d_collectMetrics(false)
, d_numTotalSocketEvents(0)
, d_numControlChannelReinitializations(0)
, d_allocator_p(bslma::Default::allocator(threadSafeAllocator))
{
    BSLS_ASSERT(rawEventManager);

    // Initialize the functor containing the dispatch thread's entry point
    // method.
    d_dispatchThreadEntryPoint
        = bdlf::Function<void (*)()>(
                bdlf::MemFnUtil::memFn(&TcpTimerEventManager
                                                    ::dispatchThreadEntryPoint,
                                      this),
                d_allocator_p);

    // Create the queue of executed timers.
    d_executeQueue_p
        = new (*d_allocator_p)
                       bsl::vector<bdlf::Function<void (*)()> >(d_allocator_p);
}

TcpTimerEventManager::~TcpTimerEventManager()
{
    disable();
    bsl::vector<bdlf::Function<void (*)()> > *executeQueue = d_executeQueue_p;
    d_allocator_p->deleteObjectRaw(executeQueue);
    if (d_isManagedFlag) {
        d_allocator_p->deleteObjectRaw(d_manager_p);
    }
}

// MANIPULATORS
int TcpTimerEventManager::disable()
{
    if (d_state == BTEMT_DISABLED) {
        return 0;
    }

    if(bdlmtt::ThreadUtil::isEqual(bdlmtt::ThreadUtil::self(),
                                 d_dispatcher))
    {
        return 1;
    }

    bdlmtt::WriteLockGuard<bdlmtt::RWMutex> guard(&d_stateLock);
    {
        // Synchronized section.
        if (d_state == BTEMT_DISABLED) {
            return 0;
        }

        // Send dispatcher thread request to exit and wait until it
        // terminates, via 'join'.
        bdlmtt::Mutex mutex;
        bdlmtt::Condition condition;
        bdlmtt::ThreadUtil::Handle dispatcherHandle = d_dispatcher;

        TcpTimerEventManager_Request *req =
           new (d_requestPool) TcpTimerEventManager_Request(
                                 TcpTimerEventManager_Request::TERMINATE,
                                 &condition,
                                 &mutex,
                                 d_allocator_p);
        d_requestQueue.pushBack(req);
        if (0 > d_controlChannel_p->clientWrite()) {
            d_requestQueue.popBack();
            d_requestPool.deleteObjectRaw(req);
            return -1;
        }

        // Note that for this function, the wait for result is subsumed
        // by joining with the thread.
        int rc = bdlmtt::ThreadUtil::join(dispatcherHandle);

        BSLS_ASSERT(0 == rc);
        d_requestPool.deleteObjectRaw(req);
        d_state = BTEMT_DISABLED;

        // Release the control channel object.
        BSLS_ASSERT(0 != d_controlChannel_p);
        d_manager_p->deregisterSocket(d_controlChannel_p->serverFd());
        d_controlChannel_p.clear();
    }
    return 0;
}

int TcpTimerEventManager::enable(const bcemt_Attribute& attr)
{
    if(bdlmtt::ThreadUtil::isEqual(bdlmtt::ThreadUtil::self(), d_dispatcher)) {
        return 0;
    }

    if (BTEMT_ENABLED == d_state) {
        return 0;
    }

    bdlmtt::WriteLockGuard<bdlmtt::RWMutex> guard(&d_stateLock);
    {
        // Synchronized section.
        if (BTEMT_ENABLED == d_state) {
            return 0;
        }

        BSLS_ASSERT(0 == d_controlChannel_p);

        // Create control channel object.
        d_controlChannel_p.load(
              new (*d_allocator_p) TcpTimerEventManager_ControlChannel(),
              d_allocator_p);

        if (btlso::SocketHandle::INVALID_SOCKET_HANDLE ==
                                              d_controlChannel_p->serverFd()) {
            // Sockets were not successfully created.

            return -1;
        }


        // Register the server fd of 'd_controlChannel_p' for READs.
        btlso::EventManager::Callback cb(
                  bdlf::MemFnUtil::memFn(&TcpTimerEventManager::controlCb,
                                        this),
                  d_allocator_p);

        int rc = d_manager_p->registerSocketEvent(
                                                d_controlChannel_p->serverFd(),
                                                btlso::EventType::BTESO_READ,
                                                cb);
        if (rc) {
            printf("%s(%d): Failed to register controlChannel for READ events"
                    " in TcpTimerEventManager constructor\n",
                    __FILE__, __LINE__);
            BSLS_ASSERT("Failed to register controlChannel for READ events" &&
                        0);
            return rc;
        }

#if defined(BSLS_PLATFORM_OS_UNIX)
        sigset_t newset, oldset;
        sigfillset(&newset);
        static const int synchronousSignals[] = {
            SIGBUS,
            SIGFPE,
            SIGILL,
            SIGSEGV,
            SIGSYS,
            SIGABRT,
            SIGTRAP,
        #if !defined(BSLS_PLATFORM_OS_CYGWIN) || defined(SIGIOT)
            SIGIOT
        #endif
         };
        static const int SIZE = sizeof synchronousSignals
                                                  / sizeof *synchronousSignals;
        for (int i = 0; i < SIZE; ++i) {
            sigdelset(&newset, synchronousSignals[i]);
        }

        pthread_sigmask(SIG_BLOCK, &newset, &oldset);
#endif

        d_terminateThread = 0;
        rc = bdlmtt::ThreadUtil::create((bdlmtt::ThreadUtil::Handle*)&d_dispatcher,
                                      attr,
                                      d_dispatchThreadEntryPoint);

#if defined(BSLS_PLATFORM_OS_UNIX)
        // Restore the mask.
        pthread_sigmask(SIG_SETMASK, &oldset, &newset);
#endif
        if (rc) {
            return rc;
        }
        return initiateControlChannelRead();
    }
    return 0;
}

int TcpTimerEventManager::registerSocketEvent(
        const btlso::SocketHandle::Handle&           handle,
        btlso::EventType::Type                       event,
        const btlso::TimerEventManager::Callback&    callback)
{
    if (bdlmtt::ThreadUtil::isEqual(
                    bdlmtt::ThreadUtil::self(), d_dispatcher))
    {
        int rc = d_manager_p->registerSocketEvent(handle, event, callback);
        d_numTotalSocketEvents = d_manager_p->numEvents()-1;
        return rc;
    }

    bdlmtt::ReadLockGuard<bdlmtt::RWMutex> guard(&d_stateLock);

    if (BTEMT_DISABLED == d_state) {
        d_stateLock.unlock();
        d_stateLock.lockWrite();
    }

    switch (d_state) {
      case BTEMT_ENABLED: {
        // Processing thread is enabled -- enqueue the request.

        TcpTimerEventManager_Request *req =
            new (d_requestPool) TcpTimerEventManager_Request(
                                                                handle,
                                                                event,
                                                                callback,
                                                                d_allocator_p);
        d_requestQueue.pushBack(req);
        if (0 > d_controlChannel_p->clientWrite()) {
            d_requestQueue.popBack();
            d_requestPool.deleteObjectRaw(req);
            return -1;
        }
      } break;
      case BTEMT_DISABLED: {
        // Processing thread is disabled -- upgrade to write lock
        // and process request in this thread.

        int rc = d_manager_p->registerSocketEvent(handle, event, callback);
        d_numTotalSocketEvents = d_manager_p->numEvents();
        return rc;
      }
    }

    return 0;
}

void *TcpTimerEventManager::registerTimer(
        const bsls::TimeInterval&                 timeout,
        const btlso::TimerEventManager::Callback& callback)
{
    if (bdlmtt::ThreadUtil::isEqual(
                    bdlmtt::ThreadUtil::self(), d_dispatcher)) {
        void *id = reinterpret_cast<void*>(d_timerQueue.add(timeout,
                                                            callback));
        return id;
    }

    void *result = (void *)0;
    bdlmtt::ReadLockGuard<bdlmtt::RWMutex> guard(&d_stateLock);
    {
        switch (d_state) {
          case BTEMT_ENABLED: {
            // As performance optimization, we may do the following:
            // Time queue is thread-safe.  Therefore, we don't have to
            // synchronize the following operation.

            int isNewTop = 0;
            int newLength;
            int handle =
                d_timerQueue.add(timeout, callback, &isNewTop, &newLength);

            if (!isNewTop) {
                result = reinterpret_cast<void*>(handle);
                BSLS_ASSERT(result);
            }
            else {
                // Signal dispatcher for the new minimum, if needed.

                TcpTimerEventManager_Request *req =
                    new (d_requestPool) TcpTimerEventManager_Request(
                                                                timeout,
                                                                callback,
                                                                d_allocator_p);
                d_requestQueue.pushBack(req);
                if (0 > d_controlChannel_p->clientWrite()) {
                    d_requestQueue.popBack();
                    d_requestPool.deleteObjectRaw(req);
                    d_timerQueue.remove(handle);
                    result = (void*)0;
                    BSLS_ASSERT("Failed to register timer" && result);
                }
                else {
                    result = reinterpret_cast<void*>(handle);
                    BSLS_ASSERT(result);
                }
            }
          } break;
          case BTEMT_DISABLED: {
            // Processing thread is disabled -- register directly
            // since the timer queue is thread-safe.

            int newTop, newLength = 0;
            result = reinterpret_cast<void*>(
                d_timerQueue.add(timeout, callback, &newTop, &newLength));
            BSLS_ASSERT(result);
          } break;
        }
    }
    return result;
}

int TcpTimerEventManager::rescheduleTimer(
                                             const void               *id,
                                             const bsls::TimeInterval&  timeout)
{
    if (bdlmtt::ThreadUtil::isEqual(
                    bdlmtt::ThreadUtil::self(), d_dispatcher)) {
        return d_timerQueue.update((int)(bsls::Types::IntPtr)id, timeout);
                                                                      // RETURN
    }

    int rc;
    bdlmtt::ReadLockGuard<bdlmtt::RWMutex> guard(&d_stateLock);
    {
        switch (d_state) {
          case BTEMT_ENABLED: {
            // As performance optimization, we may do the following:
            // Time queue is thread-safe.  Therefore, we don't have to
            // synchronize the following operation.

            int isNewTop = 0;
            rc           = d_timerQueue.update((int)(bsls::Types::IntPtr)id,
                                               timeout,
                                               &isNewTop);
            if (!rc && isNewTop) {
                // Signal dispatcher for the new minimum, if needed.

                TcpTimerEventManager_Request *req =
                    new (d_requestPool) TcpTimerEventManager_Request(
                                                                id,
                                                                timeout,
                                                                d_allocator_p);

                d_requestQueue.pushBack(req);
                if (0 > d_controlChannel_p->clientWrite()) {
                    d_requestQueue.popBack();
                    d_requestPool.deleteObjectRaw(req);
                    d_timerQueue.remove((int)(bsls::Types::IntPtr)id);
                    BSLS_ASSERT("Failed to reschedule timer" && 0);
                }
            }
          } break;
          case BTEMT_DISABLED: {
            // Processing thread is disabled -- register directly
            // since the timer queue is thread-safe.

            rc = d_timerQueue.update((int)(bsls::Types::IntPtr)id, timeout);
          } break;
        }
    }
    return rc;
}

void TcpTimerEventManager::deregisterSocketEvent(
        const btlso::SocketHandle::Handle& handle,
        btlso::EventType::Type             event)
{
    if (bdlmtt::ThreadUtil::isEqual(
                    bdlmtt::ThreadUtil::self(), d_dispatcher)) {
        d_manager_p->deregisterSocketEvent(handle, event);
        d_numTotalSocketEvents = d_manager_p->numEvents()-1;
        return;
    }

    bdlmtt::ReadLockGuard<bdlmtt::RWMutex> guard(&d_stateLock);

    if (BTEMT_DISABLED == d_state) {
        d_stateLock.unlock();
        d_stateLock.lockWrite();
    }

    switch (d_state) {
      case BTEMT_ENABLED: {
        // Processing thread is enabled -- enqueue the request.

        TcpTimerEventManager_Request *req =
            new (d_requestPool) TcpTimerEventManager_Request(
                                                                handle,
                                                                event,
                                                                d_allocator_p);
        d_requestQueue.pushBack(req);
        if (0 > d_controlChannel_p->clientWrite()) {
            d_requestQueue.popBack();
            d_requestPool.deleteObjectRaw(req);
        }
      } break;
      case BTEMT_DISABLED: {
        // Processing thread is disabled -- upgrade to write lock
        // and process request in this thread.

        d_manager_p->deregisterSocketEvent(handle, event);

        // When disabled, the control channel object is destroyed, no need
        // to minus one from 'numEvents()'.

        d_numTotalSocketEvents = d_manager_p->numEvents();
        return;
      }
    }
}

void TcpTimerEventManager::execute(const bdlf::Function<void (*)()>&
                                                                       functor)
{
    if (bdlmtt::ThreadUtil::isEqual(
                    bdlmtt::ThreadUtil::self(), d_dispatcher)) {
        bdlmtt::LockGuard<bdlmtt::Mutex> guard(&d_executeQueueLock);
        d_executeQueue_p->push_back(functor);
        return;
    }

    bdlmtt::ReadLockGuard<bdlmtt::RWMutex> stateLockGuard(&d_stateLock);
    bdlmtt::LockGuard<bdlmtt::Mutex>       executeQueueGuard(&d_executeQueueLock);

    switch (d_state) {
      case BTEMT_ENABLED: {

        // Processing thread is enabled -- enqueue the request.
        d_executeQueue_p->push_back(functor);

        // Signal dispatcher for the new executed events, only if it has not
        // been signaled already.

        if (1 == d_executeQueue_p->size()) {

            TcpTimerEventManager_Request *req =
                new (d_requestPool) TcpTimerEventManager_Request(
                                                                functor,
                                                                d_allocator_p);

            d_requestQueue.pushBack(req);
            if (0 > d_controlChannel_p->clientWrite()) {
                d_requestQueue.popBack();
                d_requestPool.deleteObjectRaw(req);
                d_executeQueue_p->pop_back();

                // guaranteed to be 'functor'

                BSLS_ASSERT("Failed to execute functor" && 0);
            }
        }
      } break;
      case BTEMT_DISABLED: {
        // Processing thread is disabled -- Simply enqueue the request.

        d_executeQueue_p->push_back(functor);
      }
    }
}

void TcpTimerEventManager::clearExecuteQueue()
{
    bdlmtt::LockGuard<bdlmtt::Mutex> guard(&d_executeQueueLock);
    d_executeQueue_p->clear();
}

void TcpTimerEventManager::deregisterSocket(
        const btlso::SocketHandle::Handle& handle)
{
    if (bdlmtt::ThreadUtil::isEqual(bdlmtt::ThreadUtil::self(), d_dispatcher)) {
        d_manager_p->deregisterSocket(handle);
        d_numTotalSocketEvents = d_manager_p->numEvents()-1;
        return;
    }

    bdlmtt::ReadLockGuard<bdlmtt::RWMutex> guard(&d_stateLock);

    if (BTEMT_DISABLED == d_state) {
        d_stateLock.unlock();
        d_stateLock.lockWrite();
    }

    switch (d_state) {
      case BTEMT_ENABLED: {
        // Processing thread is enabled -- enqueue the request.

        TcpTimerEventManager_Request *req =
            new (d_requestPool) TcpTimerEventManager_Request(
                                                                handle,
                                                                d_allocator_p);
        d_requestQueue.pushBack(req);
        if (0 > d_controlChannel_p->clientWrite()) {
            d_requestQueue.popBack();
            d_requestPool.deleteObjectRaw(req);
        }
      } break;
      case BTEMT_DISABLED: {
        // Processing thread is disabled -- upgrade to write lock
        // and process request in this thread.

        d_manager_p->deregisterSocket(handle);

        // When disabled, the control channel object is destroyed, no need
        // to minus one from 'numEvents()'.

        d_numTotalSocketEvents = d_manager_p->numEvents();
      }
    }
}

void TcpTimerEventManager::deregisterAllSocketEvents()
{
    if (bdlmtt::ThreadUtil::isEqual(bdlmtt::ThreadUtil::self(), d_dispatcher)) {
        d_manager_p->deregisterAll();
        d_numTotalSocketEvents = 0;
        return;
    }

    bdlmtt::ReadLockGuard<bdlmtt::RWMutex> guard(&d_stateLock);

    if (BTEMT_DISABLED == d_state) {
        d_stateLock.unlock();
        d_stateLock.lockWrite();
    }

    switch (d_state) {
      case BTEMT_ENABLED: {
        // Processing thread is enabled -- enqueue the request.

        TcpTimerEventManager_Request *req =
           new (d_requestPool) TcpTimerEventManager_Request(
             TcpTimerEventManager_Request::DEREGISTER_ALL_SOCKET_EVENTS,
             d_allocator_p);
        d_requestQueue.pushBack(req);
        if (0 > d_controlChannel_p->clientWrite()) {
            d_requestQueue.popBack();
            d_requestPool.deleteObjectRaw(req);
        }
      } break;
      case BTEMT_DISABLED: {
        // Processing thread is disabled -- upgrade to write lock
        // and process request in this thread.

        d_manager_p->deregisterAll();
        d_numTotalSocketEvents = 0;
      }
    }
}

void TcpTimerEventManager::deregisterTimer(const void *id)
{
    // We can just remove it.  If its at the top, dispatcher will
    // pick a new top on the next iteration.

    d_timerQueue.remove((int)(bsls::Types::IntPtr)id);
}

void TcpTimerEventManager::deregisterAllTimers()
{
    d_timerQueue.removeAll();
}

void TcpTimerEventManager::deregisterAll()
{
    deregisterAllTimers();
    deregisterAllSocketEvents();
}

// ACCESSORS
int TcpTimerEventManager::isRegistered(
        const btlso::SocketHandle::Handle& handle,
        btlso::EventType::Type             event) const
{
    if (bdlmtt::ThreadUtil::isEqual(
                    bdlmtt::ThreadUtil::self(), d_dispatcher)) {
        return d_manager_p->isRegistered(handle, event);
    }

    int result;

    bdlmtt::ReadLockGuard<bdlmtt::RWMutex> guard(&d_stateLock);

    if (BTEMT_DISABLED == d_state) {
        d_stateLock.unlock();
        d_stateLock.lockWrite();
    }

    switch (d_state) {
      case BTEMT_ENABLED: {
        // Processing thread is enabled -- enqueue the request.

        bdlmtt::Mutex     mutex;
        bdlmtt::Condition condition;
        TcpTimerEventManager_Request *req =
            new (d_requestPool) TcpTimerEventManager_Request(
                                                                handle,
                                                                event,
                                                                &condition,
                                                                &mutex,
                                                                d_allocator_p);
        d_requestQueue.pushBack(req);
        bdlmtt::LockGuard<bdlmtt::Mutex> lock(&mutex);
        if (0 > d_controlChannel_p->clientWrite()) {
            d_requestQueue.popBack();
            d_requestPool.deleteObjectRaw(req);
            result = -1;
        }
        else {
            req->waitForResult();
            result = req->result();
            d_requestPool.deleteObjectRaw(req);
        }
      } break;
      case BTEMT_DISABLED: {
        // Processing thread is disabled -- upgrade to write lock
        // and process request in this thread.

        result = d_manager_p->isRegistered(handle, event);
      }
    }

    return result;
}

int TcpTimerEventManager::numEvents() const
{
    return numTimers() + numTotalSocketEvents();
}

int TcpTimerEventManager::numTimers() const
{
    return d_timerQueue.length();
}

int TcpTimerEventManager::numSocketEvents(
        const btlso::SocketHandle::Handle& handle) const
{
    if (bdlmtt::ThreadUtil::isEqual(
                    bdlmtt::ThreadUtil::self(), d_dispatcher)) {
        return d_manager_p->numSocketEvents(handle);
    }
    int result;

    bdlmtt::ReadLockGuard<bdlmtt::RWMutex> guard(&d_stateLock);

    if (BTEMT_DISABLED == d_state) {
        d_stateLock.unlock();
        d_stateLock.lockWrite();
    }

    switch (d_state) {
      case BTEMT_ENABLED: {
        // Processing thread is enabled -- enqueue the request.

        bdlmtt::Mutex mutex;
        bdlmtt::Condition condition;
        TcpTimerEventManager_Request *req =
            new (d_requestPool) TcpTimerEventManager_Request(
                                                                handle,
                                                                &condition,
                                                                &mutex,
                                                                d_allocator_p);
        d_requestQueue.pushBack(req);
        bdlmtt::LockGuard<bdlmtt::Mutex> lock(&mutex);
        if (0 > d_controlChannel_p->clientWrite()) {
            d_requestQueue.popBack();
            d_requestPool.deleteObjectRaw(req);
            result = -1;
        }
        else {
            req->waitForResult();
            result = req->result();
            d_requestPool.deleteObjectRaw(req);
        }
      } break;
      case BTEMT_DISABLED: {
        // Processing thread is disabled -- upgrade to write lock
        // and process request in this thread.

        result = d_manager_p->numSocketEvents(handle);
      }
    }

    return result;
}

int TcpTimerEventManager::numTotalSocketEvents() const
{
    return d_numTotalSocketEvents;
}

int TcpTimerEventManager::isEnabled() const
{
    return d_state == BTEMT_ENABLED; // d_state is volatile

/*
    bdlmtt::LockGuard<bdlmtt::Mutex> lock(&d_cs);
    return d_dispatcher != bdlmtt::ThreadUtil::invalidHandle();
*/

}
}  // close package namespace

}  // close namespace BloombergLP

// ---------------------------------------------------------------------------
// NOTICE:
//      Copyright (C) Bloomberg L.P., 2007
//      All Rights Reserved.
//      Property of Bloomberg L.P. (BLP)
//      This software is made available solely pursuant to the
//      terms of a BLP license agreement which governs its use.
// ----------------------------- END-OF-FILE ---------------------------------
