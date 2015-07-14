// btls5_networkdescriptionutil.cpp                                   -*-C++-*-
#include <btls5_networkdescriptionutil.h>

#include <bsls_ident.h>
BSLS_IDENT_RCSID(btls5_networkdescriptionutil_cpp, "$Id$ $CSID$")

namespace BloombergLP {

namespace btls5 {
                     // -----------------------------------
                     // struct NetworkDescriptionUtil
                     // -----------------------------------

// CLASS METHODS
bool NetworkDescriptionUtil::isWellFormed(
                                 const NetworkDescription& socks5Servers)
{
    const bsl::size_t levels = socks5Servers.numLevels();
    bool hasEmptyLevel = false;
    for (bsl::size_t i = 0; i < levels; ++i) {
        if (!socks5Servers.numProxies(i)) {
            hasEmptyLevel = true;
            break;
        }
    }
    return 0 < levels && !hasEmptyLevel;
}

void NetworkDescriptionUtil::setLevelCredentials(
                                        NetworkDescription *proxyNetwork,
                                        bsl::size_t               level,
                                        const Credentials&  credentials)
{
    BSLS_ASSERT(proxyNetwork);
    BSLS_ASSERT(level < proxyNetwork->numLevels());

    bsl::size_t order = 0;
    for (NetworkDescription::ProxyIterator
                proxy = proxyNetwork->beginLevel(level),
                end = proxyNetwork->endLevel(level);
             proxy != end;
             ++proxy) {
        proxyNetwork->setCredentials(level, order, credentials);
        ++order;
    }
}

void NetworkDescriptionUtil::setAllCredentials(
                                        NetworkDescription *proxyNetwork,
                                        const Credentials&  credentials)
{
    BSLS_ASSERT(proxyNetwork);

    for (bsl::size_t level = 0, end = proxyNetwork->numLevels();
            level != end;
            ++level) {
        setLevelCredentials(proxyNetwork, level, credentials);
    }
}
}  // close package namespace


}  // close enterprise namespace

// ---------------------------------------------------------------------------
// NOTICE:
//      Copyright (C) Bloomberg L.P., 2013
//      All Rights Reserved.
//      Property of Bloomberg L.P. (BLP)
//      This software is made available solely pursuant to the
//      terms of a BLP license agreement which governs its use.
// ----------------------------- END-OF-FILE ---------------------------------
