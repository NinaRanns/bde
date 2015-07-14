// btlso_streamsocketfactory.t.cpp        -*-C++-*-

#include <btlso_streamsocketfactory.h>

#include <btlso_streamsocket.h>

#include <bsl_memory.h>
#include <bdlf_bind.h>
#include <bdlf_function.h>
#include <bdlf_placeholder.h>
#include <bslma_managedptr.h>

#include <bsl_cstdlib.h>     // atoi()
#include <bsl_cstring.h>     // memcpy()
#include <bsl_iostream.h>

using namespace BloombergLP;
using namespace bsl;  // automatically added by script

//=============================================================================
//                              TEST PLAN
//-----------------------------------------------------------------------------
//                              OVERVIEW
// We are testing a pure protocol class as well as a set of overloaded
// operators.  We need to verify that a concrete derived class compiles
// and links.  Verify that the allocate and deallocate functions for the
// concrete derived class are called and return a pointer to a concrete
// stream socket.
//-----------------------------------------------------------------------------
// [ 1] virtual ~btlso::StreamSocketFactory();
// [ 1] virtual btlso::StreamSocket<ADDRESS> *allocate() = 0;
// [ 1] virtual void deallocate(btlso::StreamSocket<ADDRESS> * socket) = 0;
//-----------------------------------------------------------------------------
// [ 2] USAGE TEST - Make sure main usage compiles and works properly.
//=============================================================================

//=============================================================================
//                    STANDARD BDE ASSERT TEST MACRO
//-----------------------------------------------------------------------------
static int testStatus = 0;
static void aSsErT(int c, const char *s, int i)
{
    if (c) {
        cout << "Error " << __FILE__ << "(" << i << "): " << s
             << "    (failed)" << endl;
        if (testStatus >= 0 && testStatus <= 100) ++testStatus;
    }
}
#define ASSERT(X) { aSsErT(!(X), #X, __LINE__); }

//=============================================================================
//                    GLOBAL TESTING CLASSES AND FUNCTIONS
//-----------------------------------------------------------------------------

                            // ===================
                            // class TestIPAddress
                            // ===================

struct TestIPAddress {
    int d_address;
    int d_port;
};

                           // ======================
                           // class TestStreamSocket
                           // ======================

class TestStreamSocket : public btlso::StreamSocket<TestIPAddress>
{

    // CLASS DATA
    static int  d_allocatedCount;

    // INSTANCE DATA
    int        *d_fcode_p;

    // FRIENDS
    friend class TestStreamSocketFactory;

  public:
    // CLASS METHODS
    static int getCount();

    // CREATORS
    TestStreamSocket(int *opCode);

    ~TestStreamSocket();

    // MANIPULATORS
    int read(char *buffer, int length);
    int readv(const btls::Iovec * iovec, int size);
    int write(const char *buffer, int length);
    int writev(const btls::Ovec *ovec, int size);
    int writev(const btls::Iovec *iovec, int size);
    int shutdown(bteso_Flag::ShutdownType);
    int connect(const TestIPAddress& address);
    int bind(const TestIPAddress& address);
    int listen(int backlog);
    int accept(btlso::StreamSocket<TestIPAddress>** socket, TestIPAddress *);
    int accept(btlso::StreamSocket<TestIPAddress>** socket);
    btlso::StreamSocket<TestIPAddress> * accept(TestIPAddress *);
    int setBlockingMode(bteso_Flag::BlockingMode flag);
    int setLingerOption(const btlso::SocketOptUtil::LingerData& options);
    int setOption(int level, int option, int value);
    int waitForConnect(const bsls::TimeInterval&);
    int waitForAccept(const bsls::TimeInterval&);
    int waitForIO(bteso_Flag::IOWaitType type, const bsls::TimeInterval& t);
    int waitForIO(bteso_Flag::IOWaitType type);

    // ACCESSORS
    int blockingMode(bteso_Flag::BlockingMode *flag) const;
    int connectionStatus() const;
    btlso::SocketHandle::Handle handle() const;
    int lingerOption(btlso::SocketOptUtil::LingerData *result) const;
    int localAddress(TestIPAddress *result) const;
    int peerAddress(TestIPAddress *result) const;
    int socketOption(int *result, int level, int option) const;
};

// CLASS DATA
int TestStreamSocket::d_allocatedCount = 0;

// CLASS METHODS
int TestStreamSocket::getCount()
{
    return d_allocatedCount;
}

// CREATORS
TestStreamSocket::TestStreamSocket(int *opCode)
: d_fcode_p(opCode)
{
    ++d_allocatedCount;
}

TestStreamSocket::~TestStreamSocket()
{
    --d_allocatedCount;
}

// MANIPULATORS
int TestStreamSocket::read(char *buffer, int length)
    { *d_fcode_p = 1; return -1; }
int TestStreamSocket::readv(const btls::Iovec * iovec, int size)
    { *d_fcode_p = 2; return -1; }
int TestStreamSocket::write(const char *buffer, int length)
    { *d_fcode_p = 3; return -1; }
int TestStreamSocket::writev(const btls::Ovec *ovec, int size)
    { *d_fcode_p = 4; return -1; }
int TestStreamSocket::writev(const btls::Iovec *iovec, int size)
    { *d_fcode_p = 5; return -1; }
int TestStreamSocket::shutdown(bteso_Flag::ShutdownType)
    { *d_fcode_p = 6; return -1; }
int TestStreamSocket::connect(const TestIPAddress& address)
    { *d_fcode_p = 7; return -1; }
int TestStreamSocket::bind(const TestIPAddress& address)
    { *d_fcode_p = 8; return -1; }
int TestStreamSocket::listen(int backlog)
    { *d_fcode_p = 9; return -1; }
int TestStreamSocket::accept(btlso::StreamSocket<TestIPAddress>** socket,
                             TestIPAddress *)
    { *d_fcode_p = 10; return -1; }
int TestStreamSocket::accept(btlso::StreamSocket<TestIPAddress>** socket)
    { *d_fcode_p = 11; return -1; }
btlso::StreamSocket<TestIPAddress> * TestStreamSocket::accept(TestIPAddress *)
    { *d_fcode_p = 12; return 0; }
int TestStreamSocket::setBlockingMode(bteso_Flag::BlockingMode flag)
    { *d_fcode_p = 13; return 0;  }
int TestStreamSocket::setLingerOption(
                                const btlso::SocketOptUtil::LingerData& options)
    { *d_fcode_p = 14; return 0;  }
int TestStreamSocket::setOption(int level, int option, int value)
    { *d_fcode_p = 15; return 0;  }
int TestStreamSocket::waitForConnect(const bsls::TimeInterval&)
    { *d_fcode_p = 16; return -1; }
int TestStreamSocket::waitForAccept(const bsls::TimeInterval&)
    { *d_fcode_p = 17; return -1; }
int TestStreamSocket::waitForIO(bteso_Flag::IOWaitType   type,
                                const bsls::TimeInterval& t)
    { *d_fcode_p = 18; return -1; }
int TestStreamSocket::waitForIO(bteso_Flag::IOWaitType type)
    { *d_fcode_p = 19; return -1; }

// ACCESSORS
int TestStreamSocket::blockingMode(bteso_Flag::BlockingMode *flag) const
    { *d_fcode_p = 20; return -1; }
int TestStreamSocket::connectionStatus() const
    { *d_fcode_p = 21; return 0;  }
btlso::SocketHandle::Handle TestStreamSocket::handle() const
    { *d_fcode_p = 22; return 0;  }
int TestStreamSocket::lingerOption(
                                 btlso::SocketOptUtil::LingerData *result) const
    { *d_fcode_p = 23; return 0;  }
int TestStreamSocket::localAddress(TestIPAddress *result) const
    { *d_fcode_p = 24; return 0;  }
int TestStreamSocket::peerAddress(TestIPAddress *result) const
    { *d_fcode_p = 25; return 0;  }
int TestStreamSocket::socketOption(int *result, int level, int option) const
    { *d_fcode_p = 26; return 0;  }

                       // =============================
                       // class TestStreamSocketFactory
                       // =============================

class TestStreamSocketFactory : public btlso::StreamSocketFactory<TestIPAddress>
{
    // CLASS DATA
    static int d_allocatedCount;
    static int d_opCode;

  public:
    // CLASS METHODS
    static int getCount();

    static int getOpCode();

    // CREATORS
    TestStreamSocketFactory();

    ~TestStreamSocketFactory();

    // MANIPULATORS
    btlso::StreamSocket<TestIPAddress> *allocate();

    void deallocate(btlso::StreamSocket<TestIPAddress> *socket);
};

// CLASS DATA
int TestStreamSocketFactory::d_allocatedCount = 0;

int TestStreamSocketFactory::d_opCode = 0;

// CLASS METHODS
int TestStreamSocketFactory::getCount()
{
    return d_allocatedCount;
}

int TestStreamSocketFactory::getOpCode()
{
    return d_opCode;
}

// CREATORS
TestStreamSocketFactory::TestStreamSocketFactory()
{
}

TestStreamSocketFactory::~TestStreamSocketFactory()
{
}

// MANIPULATORS
btlso::StreamSocket<TestIPAddress> * TestStreamSocketFactory::allocate()
{
    ++d_allocatedCount;

    return new TestStreamSocket(&d_opCode);
}

void TestStreamSocketFactory::deallocate(
                                     btlso::StreamSocket<TestIPAddress> *socket)
{
    --d_allocatedCount;
    ASSERT(d_allocatedCount >= 0);

    delete (TestStreamSocket *) socket;
}

                              // ================
                              // local function f
                              // ================

void f(btlso::StreamSocketFactory<TestIPAddress> *factory)
{
    btlso::StreamSocket<TestIPAddress> *socket = factory->allocate();
    if (!socket) {
        // cout << "Socket could not be allocated" << endl;
        ASSERT(1);
    }
    else {

        if (socket->setBlockingMode(bteso_Flag::BTESO_NONBLOCKING_MODE) != 0) {
            //cout << "Blocking Mode could not be set" << endl;
            ASSERT(1);
        }
        else {
            //cout << "further socket operations" << endl;
        }
    }

    factory->deallocate(socket);
}

//=============================================================================
//                      MAIN PROGRAM
//-----------------------------------------------------------------------------

int main(int argc, char *argv[]) {

    int test = argc > 1 ? atoi(argv[1]) : 0;
    int verbose = argc > 2;
    // int veryVerbose = argc > 3;

    cout << "TEST " << __FILE__ << " CASE " << test << endl;

    switch (test) { case 0:  // Zero is always the leading case.
      case 2: {
        // --------------------------------------------------------------------
        // USAGE TEST:
        //   This test is really just to make sure the syntax is correct.
        // Testing:
        //   USAGE TEST - Make sure main usage compiles and works properly.
        // --------------------------------------------------------------------

        if (verbose) cout << "Usage example." << endl
                          << "==============" << endl;

         TestStreamSocketFactory testFactory;
         f(&testFactory);

// Example 2: Binding a stream socket to a smart pointer
// - - - - - - - - - - - - - - - - - - - - - - - - - - -
         TestStreamSocketFactory tf;
         {
             btlso::StreamSocketFactory<TestIPAddress> *factory = &tf;

// It is occasionally necessary to create smart pointers to manage a
// 'btlso::StreamSocket' object.  A simple example of this use-case is when a
// user allocates a stream socket and wants to pass it to a higher level pool
// for management.  This component provides,
// 'btlso::StreamSocketFactoryDeleter', that contains a 'deleter' method,
// 'deleteObject', that can safely deallocate the stream socket on its 
// destruction.
//
// The example below shows the syntax for constructing managed and shared
// ptr objects to a stream socket using 'btlso::StreamSocketFactoryDeleter'.
// This example assumes that a concrete 'btlso::StreamSocketFactory' named
// 'factory' is available and can be used to allocate stream socket objects.
//
// First, we allocate a stream socket:
//..
    btlso::StreamSocket<TestIPAddress> *sa  = factory->allocate();
//..
// Then, we construct a managed stream socket, 'saManagedPtr', using
// 'bslma::ManagedPtr' below:
//..
    typedef btlso::StreamSocketFactoryDeleter Deleter;
//
    bslma::ManagedPtr<btlso::StreamSocket<TestIPAddress> >
                       saManagedPtr(sa,
                                    factory,
                                    &Deleter::deleteObject<TestIPAddress>);
    ASSERT(1 == tf.getCount());
         }

         ASSERT(0 == tf.getCount());
      } break;
      case 1: {
        // ----------------------------------------------------------------
        // TESTING ALLOCATE AND DEALLOCATE FUNCTIONS
        //
        // Plan:
        //
        // Testing
        //   virtual ~btlso::StreamSocketFactory();
        //   virtual btlso::StreamSocket<ADDRESS> *allocate() = 0;
        //   virtual void deallocate(btlso::StreamSocket<ADDRESS> * socket) = 0;
        // ----------------------------------------------------------------

        if (verbose) cout << "PROTOCOL TEST" << endl
                          << "=============" << endl;

        TestStreamSocketFactory factory;

        btlso::StreamSocketFactory<TestIPAddress> *factoryP = &factory;

        ASSERT(factory.getCount() == 0);
        ASSERT(factory.getOpCode() == 0);
        ASSERT(TestStreamSocket::getCount() == 0);

        btlso::StreamSocket<TestIPAddress> *socketP = factoryP->allocate();

        // Verify that correct factory performs the allocate.
        ASSERT(factory.getCount() == 1);
        ASSERT(TestStreamSocket::getCount() == 1);

        // Invoke a function on the socket returned and verify that
        // the function on the test concrete class was called.
        char buf[100];
        ASSERT(socketP->shutdown(bteso_Flag::BTESO_SHUTDOWN_BOTH) == -1);
        ASSERT(factory.getOpCode() == 6);

        factoryP->deallocate(socketP);

        // Verify that correct factory performs the deallocate.
        ASSERT(factory.getCount() == 0);
        ASSERT(TestStreamSocket::getCount() == 0);

        socketP = factoryP->allocate();
        ASSERT(factory.getCount() == 1);
        ASSERT(TestStreamSocket::getCount() == 1);

        {
            btlso::StreamSocketFactoryAutoDeallocateGuard<TestIPAddress>
                                                      guard(socketP, factoryP);

            guard.release();
        }
        ASSERT(socketP != 0);
        ASSERT(factory.getCount() == 1);
        ASSERT(TestStreamSocket::getCount() == 1);

        {
            btlso::StreamSocketFactoryAutoDeallocateGuard<TestIPAddress>
                                                     guard(&socketP, factoryP);

            guard.release();
        }
        ASSERT(socketP != 0);
        ASSERT(factory.getCount() == 1);
        ASSERT(TestStreamSocket::getCount() == 1);

        {
            btlso::StreamSocketFactoryAutoDeallocateGuard<TestIPAddress>
                                                      guard(socketP, factoryP);
        }
        ASSERT(socketP != 0);
        ASSERT(factory.getCount() == 0);
        ASSERT(TestStreamSocket::getCount() == 0);

        socketP = factoryP->allocate();
        ASSERT(socketP != 0);
        ASSERT(factory.getCount() == 1);
        ASSERT(TestStreamSocket::getCount() == 1);

        {
            btlso::StreamSocketFactoryAutoDeallocateGuard<TestIPAddress>
                                                     guard(&socketP, factoryP);
        }
        ASSERT(socketP == 0);
        ASSERT(factory.getCount() == 0);
        ASSERT(TestStreamSocket::getCount() == 0);

      } break;
      default: {
        cerr << "WARNING: CASE `" << test << "' NOT FOUND." << endl;
        testStatus = -1;
      }
    }

    if (testStatus > 0) {
        cerr << "Error, non-zero test status = " << testStatus << "." << endl;
    }
    return testStatus;
}

// ---------------------------------------------------------------------------
// NOTICE:
//      Copyright (C) Bloomberg L.P., 2002
//      All Rights Reserved.
//      Property of Bloomberg L.P. (BLP)
//      This software is made available solely pursuant to the
//      terms of a BLP license agreement which governs its use.
// ----------------------------- END-OF-FILE ---------------------------------
