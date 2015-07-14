// btlmt_asyncchannel.cpp          -*-C++-*-
#include <btlmt_asyncchannel.h>

#include <bsls_ident.h>
BSLS_IDENT_RCSID(btlmt_asyncchannel_cpp,"$Id$ $CSID$")

#ifdef BTE_FOR_TESTING_ONLY
// These dependencies need to be here for the bde_build.pl script to
// generate the proper makefiles, but do not need to be compiled into the
// component's .o file.  The symbol BTE_FOR_TESTING_ONLY should remain
// undefined, and is here only because '#if 0' is optimized away by the
// bde_build.pl script.

#include <btlmt_message.h>                  // for testing only
#endif

namespace BloombergLP {

namespace btlmt {
AsyncChannel::~AsyncChannel()
{
}
}  // close package namespace

} // close namespace BloombergLP

// ---------------------------------------------------------------------------
// NOTICE:
//      Copyright (C) Bloomberg L.P., 2007
//      All Rights Reserved.
//      Property of Bloomberg L.P. (BLP)
//      This software is made available solely pursuant to the
//      terms of a BLP license agreement which governs its use.
// ----------------------------- END-OF-FILE ---------------------------------
