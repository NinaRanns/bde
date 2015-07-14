// btlsc_timedcbchannelallocator.h       -*-C++-*-
#ifndef INCLUDED_BTLSC_TIMEDCBCHANNELALLOCATOR
#define INCLUDED_BTLSC_TIMEDCBCHANNELALLOCATOR

#ifndef INCLUDED_BSLS_IDENT
#include <bsls_ident.h>
#endif
BSLS_IDENT("$Id: $")

//@PURPOSE: Provide protocol for stream-based channel allocators with timeout.
//
//@CLASSES:
// btlsc::TimedCbChannelAllocator: non-blocking channel allocator with timeout
//
//@SEE_ALSO: btlsc_timedchannelallocator btemc_timedcbchannelallocator
//
//@AUTHOR: Tom Marshall (tmarshal)
//
//@DESCRIPTION: This component defines an abstract interface for a non-blocking
// mechanism that allocates and deallocates non-blocking channels with and
// without timeout capability; the allocation itself also has timeout
// capability.  Each channel allocated through this interface is an end point
// of a bi-directional stream-based communication connection to a peer;
// connection details, such as who the peer is, whether there is indeed one
// peer or multiple similar peers, and how the connection came to be, are not
// relevant to this channel-allocator protocol, and are therefore abstracted.
//
///Protocol Hierarchy
///------------------
// The interface hierarchy (defined by direct inheritance) of the
// 'btlsc::TimedCbChannelAllocator' protocol is as follows:
//..
//                  ,-----------------------------.
//                 ( btlsc::TimedCbChannelAllocator )
//                  `-----------------------------'
//                                 |
//                                 V
//                     ,------------------------.
//                    ( btlsc::CbChannelAllocator )
//                     `------------------------'
//..
// This protocol adds the ability to allocate timed channels as well as a
// "timeout" capability for the allocation methods themselves.  Note that
// 'deallocate' cannot block, fail, or throw, and thus does not need a timeout
// facility.
//
///Non-Blocking Channel Allocation
///-------------------------------
// This protocol establishes methods for allocating non-blocking stream-based
// channels in a non-blocking manner; each method registers a user-supplied
// callback function object (functor) and returns immediately.  A successful
// return status implies that the registered callback will be invoked (and,
// conversely, an unsuccessful status implies otherwise.)  The callback, in
// turn, will communicate the results of the registered allocation attempt.
//
// Enqueued allocation operations proceed asynchronously to one of three
// possible results: (1) "success" -- the requested channel is allocated and
// its address is passes to the callback function; in this case the callback
// "status" argument has no meaning (2) "interrupted" -- the operation was
// interrupted (e.g., via a timeout): the channel address is null and the
// (non-negative) status conveys the nature of the interruption, (3) "canceled"
// -- the operation was aborted (synchronously) by an explicit call to
// 'cancelAll': the channel-allocator address in null and the "status" is -1,
// or (4) "error" -- an implementation-dependent error occurred: the channel
// address is null and status is less than -1.  The user may retry interrupted
// and cancelled operations with a reasonable expectation of success.  An
// "error" status implies that the allocation is unlikely to succeed if
// retried, but does not necessarily invalidate the allocator.  The 'isInvalid'
// method may be used to confirm the occurrence of a permanent error.  If the
// allocator is valid, an allocation request will be enqueued and may succeed.
// Otherwise, the allocation request itself will not succeed.
//
// The meanings of the callback function status value for an unsuccessful
// allocation (i.e., a null channel address) are summarized as follows:
//..
//  "status"    meaning (only when returned channel address is null)
//  --------    -------------------------------------------------------------
//  positive    Interruption by an "asynchronous event"
//
//  zero        Interruption by a user-requested timeout
//
//  -1          Operation explicitly canceled (synchronously) by the user
//
//  < -1        Allocation operation unable to succeed at this time
//..
// Note that unless asynchronous events are explicitly enabled (see below),
// they are ignored, and "status" will never be positive.  Note also that
// whether the callback is invoked before or after the registering method
// returns is not specified; in either case, if the registration was
// successful, then the return value will reflect success.
//
///Callback Functor Registration
///- - - - - - - - - - - - - - -
// Once an operation is successfully initiated, a (reference-counted) copy of
// the ('bcef') callback functor is retained by the concrete allocator until
// the callback is executed.  Therefore, the user need not be concerned with
// preserving any resources used by the callback.
//
///Asynchronous Events
///-------------------
// Allocation methods in this protocol anticipate the possible occurrence of an
// "asynchronous event" (AE) during execution.  A common example of an AE is a
// Unix signal, but note that a specific Unix signal *may* not result in an AE,
// and an AE is certainly not limited to signals, even on Unix platforms.
//
// This interface cannot fully specify either the nature of or the behavior
// resulting from an AE, but certain restrictions can be (and are) imposed.  By
// default, AEs are either ignored or, if that is not possible, cause an error.
// At the user's option, however, a concrete implementation can be authorized
// to return, if possible, with an "interrupted" status (leaving the factory
// unaffected) upon the occurrence of an AE.  Such authorizations are made
// explicitly by incorporating into the optional (trailing) integer 'flags'
// argument to a method call the 'btesc_Flag::BTESC_ASYNC_INTERRUPT' value.
//
///Timeouts
///--------
// A timeout is registered by the caller, when a method is invoked, as a
// 'bsls::TimeInterval' value indicating the absolute *system* time after which
// the operation should be interrupted.  Information regarding the nature of an
// interruption is provided via the callback as the 'status' value (see above).
//
// Concrete allocators will make a "best effort" to honor the timeout request
// promptly, but no guarantee can be made as to the maximum duration of any
// particular allocation attempt.  Any implementation of this interface will
// support a timeout granularity of ten milliseconds (0.01s) or less.  The
// timeout is guaranteed *not* to occur before the specified time has passed.
// If a timeout is specified with a time that has already passed, the
// allocation will be attempted, but will not block.  Note that a long-running
// operation may affect subsequent allocation requests.
//
///Usage
///-----
// The purpose of the 'btlsc::TimedCbChannelAllocator' protocol is to isolate
// the act of requesting a connection from details such as who it will be
// connected to and which side initiated the connection.  In this example we
// will consider both the Server and Client sides of a 'my_Tick' reporting
// service.  Since each side of this service could potentially be a library
// component, we do not want to embed into either side the details of how
// connections will be established.  It is sufficient that, when a tick needs
// to be sent or received, a channel is obtained, the tick is transmitted, and
// the channel is returned to its allocator.  Note that this example serves to
// illustrate the use of the 'btlsc::TimedChannelAllocator' and does not
// represent production-quality software.  Note 'my_Tick' will be used by both
// the server and client.
//..
//  class my_Tick {
//      char   d_name[5];
//      double d_bestBid;
//      double d_bestOffer;
//    public:
//      my_Tick() { }
//      my_Tick(const char *ticker);
//      my_Tick(const char *ticker, double bestBid, double bestOffer);
//      ~my_Tick() { assert (d_bestBid > 0); };
//
//      static int maxSupportedBdexVersion() { return 1; }
//      static int maxSupportedVersion() { return maxSupportedBdexVersion(); }
//
//      template <class STREAM>
//      STREAM& bdexStreamOut(STREAM& stream, int version) const;
//          // Write this 'my_Tick' to the specified output 'stream' formatted
//          // in the specified 'version' and return a reference to the
//          // modifiable 'stream'.  If 'version' is not supported, 'stream'
//          // is unmodified.  Note that the 'version' is not written to the
//          // stream.  See the package group level documentation for more
//          // information on 'bdex' streaming of container types.
//
//      template <class STREAM>
//      STREAM& bdexStreamIn(STREAM& stream, int version);
//          // Assign to this object the value read from the specified input
//          // 'stream' using the specified 'version' and return a reference
//          // to the modifiable 'stream'.  If 'stream' is initially invalid,
//          // the operation has no effect.  If 'stream' becomes invalid
//          // during this operation, this object is valid, but its value is
//          // undefined.  If the specified 'version' isn't supported,
//          // 'stream' is marked invalid, but this object is unaltered.
//          // Note that no version is read from 'stream'.  (See the
//          // package-group-level documentation for more information on
//          // 'bdex' streaming of container types.)
//
//      void print(bsl::ostream& stream) const;
//  };
//
//  my_Tick::my_Tick(const char *ticker)
//  : d_bestBid(0)
//  , d_bestOffer(0)
//  {
//     #ifdef BSLS_PLATFORM_OS_UNIX
//     snprintf(d_name, sizeof d_name, "%s",ticker);
//     #else
//     if (_snprintf(d_name, sizeof d_name, "%s",ticker) < 0) {
//         d_name[sizeof d_name - 1] = '\0';
//     }
//     #endif
//  }
//
//  my_Tick::my_Tick(const char *ticker, double bestBid, double bestOffer)
//  : d_bestBid(bestBid)
//  , d_bestOffer(bestOffer)
//  {
//      #ifdef BSLS_PLATFORM_OS_UNIX
//      snprintf(d_name, sizeof d_name, "%s",ticker);
//      #else
//      if (_snprintf(d_name, sizeof d_name, "%s",ticker) < 0) {
//          d_name[sizeof d_name - 1] = '\0';
//      }
//      #endif
//  }
//
//  void my_Tick::print(bsl::ostream& stream) const {
//      stream << "(" << d_name << ", " << d_bestBid << ", " << d_bestOffer
//             << ")" << endl;
//  }
//
//  inline
//  bsl::ostream& operator<<(bsl::ostream& stream, const my_Tick& tick) {
//      tick.print(stream);
//      return stream;
//  }
//
//  template <class STREAM>
//  inline
//  STREAM& my_Tick::bdexStreamOut(STREAM& stream, int version) const
//  {
//      switch (version) {
//        case 1: {
//          stream.putString(d_name);
//          stream.putFloat64(d_bestBid);
//          stream.putFloat64(d_bestOffer);
//        } break;
//      }
//      return stream;
//  }
//
//  template <class STREAM>
//  STREAM& my_Tick::bdexStreamIn(STREAM& stream, int version)
//  {
//      switch (version) {
//        case 1: {
//            bsl::string temp1;
//            stream.getString(temp1);
//            int maxLen = sizeof (d_name) - 1, // the valid name length
//                len = temp1.length();
//            if (len < maxLen) {
//                strcpy(d_name, temp1.c_str());
//            }
//            else {
//                strncpy(d_name, temp1.c_str(), len);
//                d_name[len] = 0;
//            }
//            stream.getFloat64(d_bestBid);
//            stream.getFloat64(d_bestOffer);
//        } break;
//      }
//      return stream;
//  }
//
//..
// Let's also assume that we have a function that knows how to print platform
// neutral encodings of type 'my_Tick':
//..
//  static void myPrintTick(bsl::ostream& stream, const char *buffer, int len)
//      // Print the value of the specified 'buffer' interpreted as a
//      // 'bdex' byte-stream representation of a 'my_Tick' value, to the
//      // specified 'stream' or report an error to 'stream' if 'buffer' is
//      // determined *not* to hold an encoding of a valid 'my_Tick' value.
//  {
//      my_Tick tick;
//      bdlxxxx::ByteInStream input(buffer, len);
//      input >> tick;
//
//      stream << tick;
//  }
//..
///Server Side
///-----------
// The following class illustrates how we might implement a tick-reporter
// server using just the 'btlsc::TimedCbChannelAllocator' and
// 'btlsc::TimedCbChannel' protocols.  In this implementation the "allocate"
// functor (but not the "read" functor) is created in the constructor and
// cached for repeated use.  Note that buffered reads avoid having to supply a
// buffer, and *may* improve throughput if connections are preserved (pooled)
// in the particular *concrete* channel allocator (supplied at construction).
//..
//  class my_TickReporter {
//      // This class implements a server that accepts connections, extracts
//      // from each connection a single 'my_Tick' value, and reports that
//      // value to a console stream; both the acceptor and console stream
//      // are supplied at construction.
//
//      btlsc::TimedCbChannelAllocator *d_acceptor_p; // incoming connections
//      btlso::TcpTimerEventManager    *d_eventManager_p; // Only needed for
//                                                       // timeCb()'s use,
//                                                       // could be erased
//                                                       // otherwise.
//      bsl::ostream&                  d_console;    // where to put tick info
//
//      btlsc::TimedCbChannelAllocator::TimedCallback
//                                     d_allocFunctor;  // reused
//
//    private:
//      void acceptCb(btlsc::TimedCbChannel     *clientChannel,
//                    int                       status,
//                    const bsls::TimeInterval&  timeout);
//          // Called when a new client channel has been accepted.
//          // ...
//
//      void readCb(const char           *buffer,
//                  int                   status,
//                  int                   asyncStatus,
//                  btlsc::TimedCbChannel *clientChannel);
//          // Called when a 'my_Tick' value has been read from the channel.
//          // ...
//
//      void timeCb(int                  lastNumTicks,
//                  int                 *curNumTicks,
//                  bsls::TimeInterval    lastTime);
//          // To calculate the tick send/receive rate (Ticks/second).
//
//    private:
//      my_TickReporter(const my_TickReporter&);             // not impl.
//      my_TickReporter& operator=(const my_TickReporter&);  // not impl.
//
//    public:
//      my_TickReporter(bsl::ostream&                  console,
//                      btlsc::TimedCbChannelAllocator *acceptor,
//                      btlso::TcpTimerEventManager    *d_eventManager_p);
//          // Create a non-blocking tick-reporter using the specified
//          // 'acceptor' to establish incoming client connections, each
//          // transmitting a single 'my_Tick' value; write these values
//          // to the specified 'console' stream.  If the acceptor is idle
//          // for more than five minutes, print a message to the 'console'
//          // stream supplied at construction and continue.  To guard
//          // against malicious clients, a connection that does not produce
//          // a tick value within one minute will be summarily dropped.
//
//      ~my_TickReporter();
//          // Destroy this server object.
//  };
//
//
//  const double ACCEPT_TIME_LIMIT = 300;               // 5 minutes
//  const double   READ_TIME_LIMIT =  60;               // 1 minutes
//
//  static int calculateMyTickMessageSize()
//      // Calculate and return the number of bytes encoding of a
//      // 'my_Tick' value (called just once, see below).
//  {
//      double bid = 0, offer = 0;
//      my_Tick dummy("dummy", bid, offer);
//
//      //typedef bdlxxxx::ByteOutSTREAM bdlxxxx::ByteOutStream;
//      bdlxxxx::ByteOutStream bos;
//      bos << dummy;
//
//      return bos.length();
//  }
//
//  static int myTickMessageSize()
//      // Return the number of bytes in a 'bdex' byte-stream encoding
//      // of a 'my_Tick' value without creating a runtime-initialized
//      // file-scope static variable (which is link-order dependent).
//  {
//      static const int MESSAGE_SIZE = calculateMyTickMessageSize();
//      return MESSAGE_SIZE;
//  }
//
//  void my_TickReporter::acceptCb(btlsc::TimedCbChannel     *clientChannel,
//                                 int                       status,
//                                 const bsls::TimeInterval&  timeout)
//  {
//      bsls::TimeInterval nextTimeout(timeout);
//
//      if (clientChannel) {     // Successfully created a connection.
//
//          const int               numBytes = ::myTickMessageSize();
//
//          const bsls::TimeInterval now      = bdlt::CurrentTime::now();
//
//          // Create one-time (buffered) read functor holding 'clientChannel'.
//          btlsc::TimedCbChannel::BufferedReadCallback readFunctor(
//                  bdlf::BindUtil::bind(&my_TickReporter::readCb,
//                                      this,
//                                      _1, _2, _3,
//                                      clientChannel));
//
//          // Install read callback (timeout, but no raw or async interrupt).
//
//          if (clientChannel->timedBufferedRead(numBytes,
//                                               now + READ_TIME_LIMIT,
//                                               readFunctor)) {
//              d_console << "Error: Unable even to register a read operation"
//                           " on this channel." << bsl::endl;
//              d_acceptor_p->deallocate(clientChannel);
//          }
//         nextTimeout += ACCEPT_TIME_LIMIT;
//      }
//      else if (0 == status) {  // Interrupted due to timeout event.
//          d_console << "Acceptor timed out, continuing..." << bsl::endl;
//          nextTimeout += ACCEPT_TIME_LIMIT;
//      }
//      else if (status > 0) {   // Interrupted by unspecified event.
//          assert(0); // Impossible, "async interrupts" were not authorized.
//      }
//      else {                   // Allocation operation is unable to succeed.
//          assert(status < 0);
//          d_console << "Error: The channel allocator is not working now."
//                    << bsl::endl;
//
//          // Note that attempting to re-register an allocate operation below
//          // will fail only if the channel allocator is permanently disabled.
//      }
//
//      // In all cases, attempt to reinstall the (reusable) accept callback.
//
//      if (d_acceptor_p->timedAllocateTimed(d_allocFunctor,
//                                  nextTimeout + bdlt::CurrentTime::now())) {
//          d_console << "Error: unable to register accept operation."
//                    << bsl::endl;
//          // This server is hosed.
//      }
//  }
//
//  void my_TickReporter::readCb(const char           *buffer,
//                               int                   status,
//                               int                   asyncStatus,
//                               btlsc::TimedCbChannel *clientChannel)
//  {
//      static int curNumTicks = 0;
//
//      const int msgSize = ::myTickMessageSize();
//
//      enum { TIME_LEN = 15 };  // 15 seconds
//      bsls::TimeInterval now = bdlt::CurrentTime::now();
//      static bsls::TimeInterval lastTime(now);
//      const bsls::TimeInterval PERIOD(TIME_LEN, 0);
//
//      if (msgSize == status) {  // Encoded-tick value read successfully.
//          assert(buffer);
//          ++curNumTicks;
//          if (0 == (curNumTicks % 200000)) {
//              cout << curNumTicks << " ticks has been received. " << endl;
//              cout << "The current time value: " << now << endl;
//          }
//          // Just to print one tick to verify the format.
//          if (1 == curNumTicks) {
//              cout << "The first tick's value: ";
//              ::myPrintTick(bsl::cout, buffer, msgSize);
//          }
//          if (1 == curNumTicks) {
//              bdlf::Function<void (*)()> timerFunctor(
//                      bdlf::BindUtil::bind(&my_TickReporter::timeCb,
//                                          this,
//                                          0,
//                                          &curNumTicks,
//                                          now));
//              d_eventManager_p->registerTimer(now + TIME_LEN, timerFunctor);
//          }
//
//          btlsc::TimedCbChannel::BufferedReadCallback readFunctor(
//                      bdlf::BindUtil::bind(&my_TickReporter::readCb,
//                                          this,
//                                          _1, _2, _3,
//                                          clientChannel));
//          if (clientChannel->timedBufferedRead(msgSize,
//                                             now + READ_TIME_LIMIT,
//                                             readFunctor)) {
//              d_console << "Error: Unable even to register a read operation"
//                         " on this channel." << bsl::endl;
//              d_acceptor_p->deallocate(clientChannel);
//          }
//      }
//      else if (0 <= status) {   // Tick message was interrupted.
//
//          assert(buffer); // Data in buffer is available for inspection
//                          // (but remains in the channel's buffer).
//
//          // Must be a TIMEOUT event since neither raw (partial) reads
//          // nor (external) asynchronous interrupts were authorized.
//
//          assert(0 == asyncStatus);   // must be timeout event!
//
//          d_console << "Error: Unable to read tick value before timing out; "
//                       "read aborted."
//                    << " status = " << status << "; asyncStatus = "
//                    << asyncStatus << bsl::endl;
//      }
//      else { // Tick-message read failed.
//          if (-1 == status) {
//              d_console << "Error: Unable to read tick value from channel: "
//                           " The connection is closed by the client."
//                        << bsl::endl;
//          }
//          else {
//              d_console << "Unknown Error: Unable to read tick from channel."
//                        << bsl::endl;
//          }
//      }
//  }
//
//  void my_TickReporter::timeCb(int                lastNumTicks,
//                               int               *curNumTicks,
//                               bsls::TimeInterval  lastTime)
//  {
//      int numTicks = *curNumTicks - lastNumTicks;
//      enum { TIME_LEN = 15 };  // 15 seconds
//      bsls::TimeInterval now = bdlt::CurrentTime::now();
//
//      bsls::TimeInterval timePeriod = now - lastTime;
//      double numSeconds = timePeriod.seconds() +
//                            (double) timePeriod.nanoseconds() / 1000000000;
//      cout << numTicks <<" ticks were sent in "
//           << numSeconds << " seconds." << endl;
//
//      cout << "The read rate is " << (int) (numTicks / numSeconds)
//           << " Ticks/second." << endl << endl;
//
//      bdlf::Function<void (*)()> timerFunctor(
//                      bdlf::BindUtil::bind(&my_TickReporter::timeCb,
//                                          this,
//                                          *curNumTicks,
//                                          curNumTicks,
//                                          now));
//      d_eventManager_p->registerTimer(now + TIME_LEN, timerFunctor);
//  }
//
//  my_TickReporter::my_TickReporter(bsl::ostream&                  console,
//                                 btlsc::TimedCbChannelAllocator *acceptor,
//                                 btlso::TcpTimerEventManager    *eventManager)
//  : d_console(console)
//  , d_acceptor_p(acceptor)
//  , d_eventManager_p(eventManager)
//  {
//      assert(&d_console);
//      assert(d_acceptor_p);
//
//     // Attempt to install the first accept callback.
//
//      bsls::TimeInterval timeout = bdlt::CurrentTime::now()
//          + ACCEPT_TIME_LIMIT;
//
//      // load reusable allocate functor
//      d_allocFunctor = bdlf::BindUtil::bind(&my_TickReporter::acceptCb,
//                                           this,
//                                           timeout));
//
//      if (d_acceptor_p->timedAllocateTimed(d_allocFunctor, timeout)) {
//          d_console << "Error: Unable to install accept operation."
//                    << bsl::endl;
//          // This server is hosed.
//      }
//  }
//
//  my_TickReporter::~my_TickReporter()
//  {
//      assert(&d_console);
//      assert(d_acceptor_p);
//  }
//
//
//  int main (int argc, const char *argv[])
//  {
//      enum { DEFAULT_PORT = 5000 };
//
//      const int portNumber = argc > 1 ? atoi(argv[1]) : DEFAULT_PORT;
//
//      btlso::IPv4Address address;
//      address.setPortNumber(portNumber);
//
//      btlso::TcpTimerEventManager sem;    // concrete manager
//      btlso::InetStreamSocketFactory<btlso::IPv4Address> sf;
//      btlsos::TcpTimedCbAcceptor acceptor(&sf, &sem);
//
//      assert(0 == acceptor.open(address, DEFAULT_QUEUE_SIZE));
//
//      if (acceptor.isInvalid()) {
//          bsl::cout << "Error: Unable to create acceptor" << bsl::endl;
//                 return -1;
//      }
//
//      my_TickReporter reporter(bsl::cout, &acceptor, &sem);
//
//      cout << "server: " << address
//           << "; begins to dispatch()....." << endl;
//
//      while (0 != sem.dispatch(0)) {
//          // Do nothing.
//      }
//  }
//
//..
// Note that when the server can go out of scope before all events are
// processed, it is important to implement it using the envelope/letter pattern
// where a *counted* *handle* to the internal letter representation is present
// in each active callback functor to preserve the server's internal state
// until all functors operating on it have been invoked.
//
///Client Side
///-----------
// In order to use this 'my_Tick' reporting service, clients will need to know
// where such a service resides and how to establish such connections on
// demand.  We will use the 'btlsc::TimedCbChannelAllocator' protocol to
// abstract those details out of the stable software that generates (or
// forwards) ticks.  For the purposes of this example, let's assume that ticks
// are generated in some ASCII format and arrive in fixed size chunks (e.g., 80
// bytes) from a separate process.  Note that of the three callback methods
// 'readCb', 'connectCb', and 'writeCb', only 'readCb' requires no additional,
// call-specific user data; hence we can easily create it once at construction,
// and productively cache it for repeated used.  We will choose to reload the
// others each time (which is admittedly somewhat less efficient).
//..
//  class my_TickerplantSimulator {
//      // Accept raw tick values in ASCII sent as fixed-sized packets via a
//      // single 'btlsc::TimedCbChannel' and send them asynchronously one by
//      // one to a peer (or similar peers) connected via channels provided via
//      // a 'btlsc::TimedCbChannelAllocator'.  Both the output channel
//      // allocator and the input channel are supplied at construction.
//
//      btlsc::TimedCbChannelAllocator *d_connector_p;  // outgoing connections
//      btlso::TcpTimerEventManager    *d_eventManager_p;
//      bsl::ostream&                  d_console;      // where to write errors
//      const int                      d_inputSize;    // input packet size
//
//      bdlf::Function<void (*)(btlsc::TimedCbChannel*, int)>
//                                                 d_allocateFunctor;
//      btlsc::TimedCbChannel::BufferedReadCallback d_readFunctor;  // reused
//
//    private:
//      void connectCb(btlsc::TimedCbChannel *serverChannel,
//                     int                   status);
//          // Called when a new server channel has been established.
//          // ...
//
//      void writeCb(int                   status,
//                   int                   asyncStatus,
//                   btlsc::TimedCbChannel *serverChannel,
//                   int                   messageSize,
//                   int                   maxTicks);
//          // Called when a write operation to the server channel ends.
//          // ...
//
//      void timeCb(int                  lastNumTicks,
//                  int                 *curNumTicks,
//                  int                  maxTicks,
//                  bsls::TimeInterval    lastTime);
//          // To calculate the tick send/receive rate (Ticks/second).
//
//    private:  // Not implemented.
//      my_TickerplantSimulator(const my_TickerplantSimulator&);
//      my_TickerplantSimulator& operator=(const my_TickerplantSimulator&);
//
//    public:
//      my_TickerplantSimulator(
//                           bsl::ostream&                  console,
//                           btlsc::TimedCbChannelAllocator *connector,
//                           btlso::TcpTimerEventManager    *d_eventManager_p,
//                           int                            inputSize);
//          // Create a non-blocking ticker-plant simulator using the
//          // specified 'input' channel to read ASCII tick records of
//          // the specified 'inputSize' and convert each record to a
//          // 'my_Tick' structure; each tick value is sent asynchronously
//          // to a peer via a distinct channel obtained from the specified
//          // 'connector', reporting any errors to the specified 'console'.
//          // If 'connector' fails or is unable to succeed after 30 seconds,
//          // or if transmission itself exceeds 10 seconds, display a message
//          // on 'console' and abort the transmission.  If three successive
//          // reads of the input channel fail to produce a valid ticks,
//          // invalidate the channel and shut down this simulator.  The
//          // behavior is undefined unless 0 < inputSize.
//
//      ~my_TickerplantSimulator();
//          // Destroy this simulator object.
//      int connect();
//  };
//
//  static const bsls::TimeInterval CONNECT_TIME_LIMIT(30, 0);  // 30 seconds
//  static const bsls::TimeInterval WRITE_TIME_LIMIT(3600, 0);  // 1 hour
//
//  void my_TickerplantSimulator::connectCb(
//                                         btlsc::TimedCbChannel *serverChannel,
//                                         int                   status)
//  {
//      if (serverChannel) {            // Successfully created a connection.
//          struct Tick {               // for usage example
//              char   d_name[5];
//              double d_bestBid;
//              double d_bestOffer;
//          } ticks[] = {
//            {"MSF1", 20.5, 10.5},
//            {"MSF2", 21.5, 11.5},
//            {"MSF3", 22.5, 12.5},
//            {"MSF4", 23.5, 13.5},
//            {"MSF5", 24.5, 14.5},
//            {"MSF6", 25.5, 15.5},
//            {"MSF7", 26.5, 16.5},
//            {"MSF8", 27.5, 17.5},
//          };
//          const int NUM_TICKS = sizeof ticks / sizeof *ticks;
//          enum { NUM_ITERATIONS = 1000000 };
//
//          for (int i = 0; i < NUM_ITERATIONS; ++i) {
//               my_Tick tick(ticks[i % NUM_TICKS].d_name,
//                            ticks[i % NUM_TICKS].d_bestBid,
//                            ticks[i % NUM_TICKS].d_bestOffer);
//
//               bdlxxxx::ByteOutStream bos;
//               bos << tick;
//               int msgSize = bos.length();
//
//               btlsc::TimedCbChannel::WriteCallback functor(
//                      bdlf::BindUtil::bind(&my_TickerplantSimulator::writeCb,
//                                          this,
//                                          _1, _2,
//                                          serverChannel,
//                                          msgSize,
//                                          (int) NUM_ITERATIONS));
//
//               bsls::TimeInterval now = bdlt::CurrentTime::now();
//
//               // Initiate a timed non-blocking write operation.
//               if (serverChannel->timedBufferedWrite(
//                                             bos.data(),
//                                             msgSize,
//                                             now + WRITE_TIME_LIMIT,
//                                             functor)) {
//
//                   d_console << "Error: Unable even to register a write"
//                                " operation on this channel." << bsl::endl;
//
//                   d_connector_p->deallocate(serverChannel);
//               }
//          }
//      }
//      else if (status > 0 ) {  // Interrupted due to external event.
//          assert (0);  // Impossible, not authorized.
//      }
//      else if (0 == status) {  // Interrupted due to timeout event.
//          d_console << "Error: Connector timed out, transition aborted."
//                    << bsl::endl;
//      }
//      else {  // Connector failed.
//          assert (0 < status);
//          bsl::cout << "Error: Unable to connect to server." << bsl::endl;
//
//          // The server is down; invalidate the input channel, allowing
//          // existing write operations to complete before the simulator
//          // shuts down.
//      }
//  }
//
//
//  void my_TickerplantSimulator::writeCb(int                   status,
//                                        int                   asyncStatus,
//                                        btlsc::TimedCbChannel *serverChannel,
//                                        int                   msgSize,
//                                        int                   maxTicks)
//  {
//      static int curNumTicks = 0;
//      static int lastNumTicks = 0;
//      assert(serverChannel);
//      assert(0 < msgSize);
//      assert(status <= msgSize);
//
//      enum { TIME_LEN = 15 };  // 15 seconds
//      bsls::TimeInterval now = bdlt::CurrentTime::now();
//      static bsls::TimeInterval lastTime(now);
//      const bsls::TimeInterval PERIOD(TIME_LEN, 0);
//
//      // Tracing message: Print out any abnormal tick sending information.
//      if (msgSize != status) {
//          d_console << "msgSize = " << msgSize << "; status = "
//                    << status << "; asyncStatus = " << asyncStatus
//                    << bsl::endl;
//      }
//
//      if (msgSize == status) {
//          // Encoded tick value written successfully.
//          ++curNumTicks;
//          if (0 == (curNumTicks % 200000)) {
//              d_console << curNumTicks << " ticks has been sent. "
//                        << bsl::endl;
//              d_console << "The current time value: "
//                        << now << bsl::endl;
//          }
//
//          if (1 == curNumTicks) {
//              bdlf::Function<void (*)()> timerFunctor(
//                      bdlf::BindUtil::bind(&my_TickerplantSimulator::timeCb,
//                                          this,
//                                          0,
//                                          &curNumTicks,
//                                          maxTicks,
//                                          now));
//              d_eventManager_p->registerTimer(now + TIME_LEN, timerFunctor);
//              d_console << "registered a timer.  time" << now + TIME_LEN
//                        << bsl::endl;
//          }
//      }
//      else if (0 <= status) {   // Tick message timed out.
//
//          assert(0 == asyncStatus // only form of partial-write authorized
//                 || 0 >  asyncStatus // This operations was dequeued due to
//                 && 0 == status); // a previous partial write operation.
//          if (0 < asyncStatus) {
//              d_console << "The request is interrupted." << bsl::endl;
//          }
//          else if (0 == asyncStatus) {
//              d_console << "Write of tick data timed out." << bsl::endl;
//
//              if (status > 0) {
//                  d_console << "Partial tick data written;"
//                            << "  status = " << status
//                            << "asyncStatus = " << asyncStatus << bsl::endl;
//              }
//              else {
//                  d_console << "No data was written; channel is still valid."
//                            << bsl::endl;
//              }
//          }
//          else {
//              assert(0 > asyncStatus && 0 == status);
//              if (0 == status) {
//                  d_console << "This operation was dequeued. "
//                            << "status = " << status << "; asyncStatus = "
//                            << asyncStatus << bsl::endl;
//              }
//              else {
//                  d_console << "Write (efficiently) transmitted " << status
//                            << " of bytes." << bsl::endl;
//              }
//              assert(0);
//          }
//      }
//      else {  // Tick message write failed.
//          assert(0 > status);
//          d_console << "Error: Unable to write tick value to server."
//                    << bsl::endl;
//      }
//  }
//
//  void my_TickerplantSimulator::timeCb(int                lastNumTicks,
//                                       int               *curNumTicks,
//                                       int                maxTicks,
//                                       bsls::TimeInterval  lastTime)
//  {
//      int numTicks = *curNumTicks - lastNumTicks;
//      enum { TIME_LEN = 15 };  // 15 seconds
//      bsls::TimeInterval now = bdlt::CurrentTime::now();
//
//      bsls::TimeInterval timePeriod = now - lastTime;
//      double numSeconds = timePeriod.seconds() +
//                            (double) timePeriod.nanoseconds() / 1000000000;
//      cout << numTicks <<" ticks were sent in "
//           << numSeconds << " seconds." << endl;
//
//      cout << "The send rate is " << (int) (numTicks / numSeconds)
//           << " Ticks/second." << endl << endl;
//      if (*curNumTicks < maxTicks) {
//          bdlf::Function<void (*)()> timerFunctor
//                  bdlf::BindUtil::bind(
//                            &my_TickerplantSimulator::timeCb,
//                            this,
//                            *curNumTicks, curNumTicks, maxTicks,
//                            now));
//          d_eventManager_p->registerTimer(now + TIME_LEN, timerFunctor);
//      }
//  }
//
//  my_TickerplantSimulator::
//        my_TickerplantSimulator(bsl::ostream&                  console,
//                                btlsc::TimedCbChannelAllocator *connector,
//                                btlso::TcpTimerEventManager    *eventManager,
//                                int                            inputSize)
//  : d_connector_p(connector)
//  , d_eventManager_p(eventManager)
//  , d_console(console)
//  , d_inputSize(inputSize)
//  {
//      assert(&console);
//      assert(connector);
//      assert(eventManager);
//      assert(0 < inputSize);
//
//      d_allocateFunctor = bdlf::BindUtil::bind(
//                                  &my_TickerplantSimulator::connectCb,
//                                  this);
//  }
//
//
//  int my_TickerplantSimulator::connect() {
//      return d_connector_p->timedAllocateTimed(d_allocateFunctor,
//                            bdlt::CurrentTime::now() + CONNECT_TIME_LIMIT);
//
//  }
//
//  my_TickerplantSimulator::~my_TickerplantSimulator()
//  {
//      assert(&d_console);
//      assert(d_connector_p);
//      assert(0 < d_inputSize);
//  }
//
//  int main (int argc, const char *argv[])
//  {
//      const char *const DEFAULT_HOST = "sundev1";
//      enum { DEFAULT_PORT = 5000 };
//      enum { DEFAULT_SIZE = 10 };
//
//      const char *hostName = argc > 1 ? argv[1]       : DEFAULT_HOST;
//      const int portNumber = argc > 2 ? atoi(argv[2]) : DEFAULT_PORT;
//      const int inputSize  = argc > 3 ? atoi(argv[3]) : DEFAULT_SIZE;
//
//      // INBOUND:
//      // This simulator accepts connections on port 'DEFAULT_PORT'
//      // only.
//
//      btlso::IPv4Address serverAddress;
//
//      int ret = btlso::ResolveUtil::getAddress(&serverAddress,
//                                              hostName);
//      assert(0 == ret);
//      serverAddress.setPortNumber(portNumber);
//      cout << "serverAddress = " << serverAddress << endl;
//
//      btlso::InetStreamSocketFactory<btlso::IPv4Address> sf;
//      btlso::TcpTimerEventManager::Hint
//      infrequentRegistrationHint =
//                   btlso::TcpTimerEventManager::BTESO_INFREQUENT_REGISTRATION;
//      btlso::TcpTimerEventManager sem(infrequentRegistrationHint);
//
//      btlsos::TcpTimedCbConnector connector(&sf, &sem);
//      connector.setPeer(serverAddress);
//
//      my_TickerplantSimulator
//          simulator(bsl::cout, &connector, &sem, inputSize);
//      assert(0 == simulator.connect());
//
//      cout << "client begins to dispatch()....." << endl;
//
//      while (0 != sem.dispatch(0)) {
//          // Do nothing.
//      }
//  }
//
//..
// Please remember that these example code snippets are intended to illustrate
// the use of 'btlsc::TimedCbChannelAllocator' and do not represent
// production-quality software.

#ifndef INCLUDED_BTLSCM_VERSION
#include <btlscm_version.h>
#endif

#ifndef INCLUDED_BDLF_FUNCTION
#include <bdlf_function.h>
#endif

namespace BloombergLP {



// Updated by 'bde-replace-bdet-forward-declares.py -m bdlt': 2015-02-03
// Updated declarations tagged with '// bdet -> bdlt'.

namespace bsls { class TimeInterval; }                          // bdet -> bdlt

namespace bdet {typedef ::BloombergLP::bsls::TimeInterval TimeInterval;    // bdet -> bdlt
}  // close package namespace


namespace btlsc {class CbChannel;
class TimedCbChannel;

                        // ===================================
                        // class TimedCbChannelAllocator
                        // ===================================

class TimedCbChannelAllocator
// : public CbChannelAllocator                // TBD: later
{
    // This class is a protocol (pure abstract interface) for a non-blocking
    // mechanism that allocates end points of communications channels
    // supporting timed and untimed non-blocking (buffered and non-buffered)
    // read and write operations on a byte stream.  A 'bcef' callback functor
    // communicates the results of the asynchronous allocation.  A successful
    // allocation passes to the callback the address of a channel, in which
    // case the callback's "status" argument has no significance.  Otherwise, a
    // non-negative callback status indicates an asynchronous interruption (0
    // for a caller-requested timeout), a status of -1 implies that the
    // operation was canceled (synchronously) by the caller (see 'cancelAll'),
    // and a negative status value other than -1 implies an error (i.e., an
    // inability of this object to succeed at the present time).  A failure to
    // register an allocation operation (or an explicit call to 'invalidate')
    // implies that the channel allocator is permanently invalid (see
    // 'isInvalid').  An invalid allocator cannot successfully register new
    // allocation operations, but pending allocation operations will not
    // necessarily fail.  Note that an invalid allocator is still capable of
    // deallocation.

  private:
    TimedCbChannelAllocator&
        operator=(const TimedCbChannelAllocator&); // not implemented

  public:
    // TYPES
    typedef bdlf::Function<void (*)(CbChannel *, int)> Callback;
        // Invoked as a result of an 'allocate' or 'timedAllocate' request,
        // 'Callback' is an alias for a callback function object (functor) that
        // returns 'void' and takes as arguments the (possibly null) address of
        // a callback "channel" and an integer "status" indicating either an
        // interruption (non-negative, 0 if and only if timeout) or an error
        // (negative).  Note that "status" is meaningful only if "channel" is
        // 0.

    typedef bdlf::Function<void (*)(TimedCbChannel *, int)> TimedCallback;
        // Invoked as a result of an 'allocateTimed' or 'timedAllocateTimed'
        // request, 'TimedCallback' is an alias for a callback function object
        // (functor) that returns 'void' and takes as arguments the (possibly
        // null) address of a timed callback "channel" and an integer "status"
        // indicating either an interruption (non-negative, 0 if and only if
        // timeout) or an error (negative).  Note that "status" is meaningful
        // only if "channel" is 0.

    // CREATORS
    virtual ~TimedCbChannelAllocator();
        // Destroy this channel (required for syntactic consistency only).

    // MANIPULATORS
    virtual int allocate(const Callback& callback,
                         int             flags = 0)                        = 0;
        // Initiate a non-blocking operation to allocate a callback channel;
        // execute the specified 'callback' functor after the allocation
        // operation terminates.  If the optionally specified 'flags'
        // incorporates 'btesc_Flag::BTESC_ASYNC_INTERRUPT', "asynchronous
        // events" are permitted to interrupt the allocation; by default, such
        // events are ignored.  Return 0 on successful initiation, and a
        // non-zero value otherwise (in which case 'callback' will not be
        // invoked).
        //
        // When invoked, 'callback' is passed the (possibly null) address of a
        // callback channel and an integer "status".  If that address is not 0,
        // the allocation succeeded and status has no meaning; a non-null
        // channel address will remain valid until deallocated explicitly (see
        // 'deallocate').  If the address is 0, a positive status indicates an
        // interruption due to an asynchronous event; subsequent allocation
        // attempts may succeed.  A status of -1 implies that the allocation
        // operation was "canceled" (synchronously) by the caller (see
        // 'cancelAll') and, often, may be retried successfully.  A status less
        // than -1 indicates a more persistent error, but not necessarily a
        // permanent one; the allocator itself may still be valid (see
        // 'isInvalid').  The behavior is undefined unless 'callback' is valid.

    virtual int timedAllocate(const Callback&          callback,
                              const bsls::TimeInterval& timeout,
                              int                      flags = 0)          = 0;
        // Initiate a non-blocking operation to allocate a callback channel or
        // interrupt after the specified absolute 'timeout' time is reached;
        // execute the specified 'callback' functor after the allocation
        // operation terminates.  If the optionally specified 'flags'
        // incorporates 'btesc_Flag::BTESC_ASYNC_INTERRUPT', "asynchronous
        // events" are permitted to interrupt the allocation; by default, such
        // events are ignored.  Return 0 on successful initiation, and a
        // non-zero value otherwise (in which case 'callback' will not be
        // invoked).
        //
        // When invoked, 'callback' is passed the (possibly null) address of a
        // callback channel and an integer "status".  If that address is not 0,
        // the allocation succeeded and status has no meaning; a non-null
        // channel-allocator address will remain valid until deallocated
        // explicitly (see 'deallocate').  If the address is 0, a 0 status
        // indicates that the operation has timed out, while a positive status
        // indicates an interruption due to an asynchronous event.  In either
        // case, subsequent allocation attempts may succeed.  A status of -1
        // implies that the allocation operation was "canceled" (synchronously)
        // by the caller (see 'cancelAll') and, often, may be retried
        // successfully.  A status less than -1 indicates a more persistent
        // error, but not necessarily a permanent one; the allocator itself may
        // still be valid (see 'isInvalid').  Note that if the specified
        // 'timeout' value has already passed, the allocation will still be
        // attempted, but the attempt will not block.

    virtual int allocateTimed(const TimedCallback& timedCallback,
                              int                  flags = 0)              = 0;
        // Initiate a non-blocking operation to allocate a timed callback
        // channel; execute the specified 'timedCallback' functor after the
        // allocation operation terminates.  If the optionally specified
        // 'flags' incorporates 'btesc_Flag::BTESC_ASYNC_INTERRUPT',
        // "asynchronous events" are permitted to interrupt the allocation; by
        // default, such events are ignored.  Return 0 on successful
        // initiation, and a non-zero value otherwise (in which case
        // 'timedCallback' will not be invoked).
        //
        // When invoked, 'timedCallback' is passed the (possibly null) address
        // of a timed callback channel and an integer "status".  If that
        // address is not 0, the allocation succeeded and status has no
        // meaning; a non-null channel address will remain valid until
        // deallocated explicitly (see 'deallocate').  If the address is 0, a
        // positive status indicates an interruption due to an asynchronous
        // event; subsequent allocation attempts may succeed.  A status of -1
        // implies that the allocation operation was "canceled" (synchronously)
        // by the caller (see 'cancelAll') and, often, may be retried
        // successfully.  A status less than -1 indicates a more persistent
        // error, but not necessarily a permanent one; the allocator itself may
        // still be valid (see 'isInvalid').  The behavior is undefined unless
        // 'callback' is valid.

    virtual int timedAllocateTimed(const TimedCallback&     timedCallback,
                                   const bsls::TimeInterval& timeout,
                                   int                      flags = 0)     = 0;
        // Initiate a non-blocking operation to allocate a timed callback
        // channel or interrupt after the specified absolute 'timeout' time is
        // reached; execute the specified 'callback' functor after the
        // allocation operation terminates.  If the optionally specified
        // 'flags' incorporates 'btesc_Flag::BTESC_ASYNC_INTERRUPT',
        // "asynchronous events" are permitted to interrupt the allocation; by
        // default, such events are ignored.  Return 0 on successful
        // initiation, and a non-zero value otherwise (in which case
        // 'timedCallback' will not be invoked).
        //
        // When invoked, 'timedCallback' is passed the (possibly null) address
        // of a timed callback channel and an integer "status".  If that
        // address is not 0, the allocation succeeded and status has no
        // meaning; a non-null channel-allocator address will remain valid
        // until deallocated explicitly (see 'deallocate').  If the address is
        // 0, a 0 status indicates that the operation has timed out, while a
        // positive "status" indicates an interruption due to an asynchronous
        // event.  In either case, subsequent allocation attempts may succeed.
        // A status of -1 implies that the allocation operation was "canceled"
        // (synchronously) by the caller (see 'cancelAll') and, often, may be
        // retried successfully.  A status less than -1 indicates a more
        // persistent error, but not necessarily a permanent one; the allocator
        // itself may still be valid (see 'isInvalid').  Note that if the
        // specified 'timeout' value has already passed, the allocation will
        // still be attempted, but the attempt will not block.

    virtual void cancelAll() = 0;
        // Immediately cancel all pending operations on this allocator,
        // invoking each registered allocation callback in turn.  Each callback
        // will be invoked with a null channel and a status of -1.  This method
        // may be invoked successfully on an invalid allocator; however,
        // calling the method does not invalidate the allocator.  Note that
        // calling 'cancelAll' from a callback that has itself been canceled
        // simply extends the set of canceled operations to include any new
        // ones initiated since the previous 'cancelAll' was invoked.

    virtual void deallocate(CbChannel *channel) = 0;
        // Terminate all operations on the specified 'channel', invoke each
        // pending callback with the appropriate status, and reclaim all
        // afforded channel services.  The behavior is undefined unless
        // 'channel' is currently allocated from this allocator, (i.e., was
        // previously obtained from this instance and has not subsequently
        // deallocated deallocated).  Note that this method can never block.

    virtual void invalidate() = 0;
        // Place this allocator in a permanently invalid state.  No new
        // allocation operations can be initiated; deallocation and previously
        // initiated allocation requests are not affected.

    // ACCESSORS
    virtual int isInvalid() const = 0;
        // Return 1 if this allocator is permanently invalid and 0 otherwise.
        // An invalid allocator can never again register a request to allocate
        // a channel, but *may* succeed in completing existing enqueued
        // requests; deallocation operations are unaffected.  Note that the
        // significance of a 0 return cannot be relied upon beyond the return
        // of this method.
};
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
