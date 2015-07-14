// balm_integercollector.cpp                                          -*-C++-*-
#include <balm_integercollector.h>

#include <bsls_ident.h>
BSLS_IDENT_RCSID(balm_integercollector_cpp,"$Id$ $CSID$")

#include <bsls_types.h>

#include <bsl_climits.h>

namespace BloombergLP {

                          // ---------------------------
                          // class balm::IntegerCollector
                          // ---------------------------

// PUBLIC CONSTANTS
const int balm::IntegerCollector::DEFAULT_MIN = INT_MAX;
const int balm::IntegerCollector::DEFAULT_MAX = INT_MIN;

namespace balm {
// MANIPULATORS
void IntegerCollector::loadAndReset(MetricRecord *record)
{
    int                count;
    bsls::Types::Int64 total;
    int                min;
    int                max;
    {
        bdlmtt::LockGuard<bdlmtt::Mutex> guard(&d_mutex);
        count = d_count;
        total = d_total;
        min   = d_min;
        max   = d_max;

        d_count = 0;
        d_total = 0;
        d_min   = DEFAULT_MIN;
        d_max   = DEFAULT_MAX;
    }
    // Perform the conversion to double values outside of the lock.
    record->metricId() = d_metricId;
    record->count()    = count;
    record->total()    = static_cast<double>(total);
    record->min()      = (DEFAULT_MIN == min)
                       ? MetricRecord::DEFAULT_MIN
                       : min;
    record->max()      = (DEFAULT_MAX == max)
                       ? MetricRecord::DEFAULT_MAX
                       : max;
}

// ACCESSORS
void IntegerCollector::load(MetricRecord *record) const
{
    int                count;
    bsls::Types::Int64 total;
    int                min;
    int                max;

    {
        bdlmtt::LockGuard<bdlmtt::Mutex> guard(&d_mutex);
        count = d_count;
        total = d_total;
        min   = d_min;
        max   = d_max;
    }

    // Perform the conversion to double values outside of the lock.
    record->metricId() = d_metricId;
    record->count()    = count;
    record->total()    = static_cast<double>(total);
    record->min()      = (DEFAULT_MIN == min)
                       ? MetricRecord::DEFAULT_MIN
                       : min;
    record->max()      = (DEFAULT_MAX == max)
                       ? MetricRecord::DEFAULT_MAX
                       : max;
}
}  // close package namespace

}  // close namespace BloombergLP

// ---------------------------------------------------------------------------
// NOTICE:
//      Copyright (C) Bloomberg L.P., 2009
//      All Rights Reserved.
//      Property of Bloomberg L.P. (BLP)
//      This software is made available solely pursuant to the
//      terms of a BLP license agreement which governs its use.
// ----------------------------- END-OF-FILE ---------------------------------
