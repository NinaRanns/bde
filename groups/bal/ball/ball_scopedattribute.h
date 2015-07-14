// ball_scopedattribute.h   -*-C++-*-
#ifndef INCLUDED_BALL_SCOPEDATTRIBUTE
#define INCLUDED_BALL_SCOPEDATTRIBUTE

//@PURPOSE: Provide a scoped guard for a single BAEL attribute attributes
//
//@CLASSES:
//   ball::ScopedAttribute: single attribute scoped guard
//
//@SEE_ALSO: ball_attribute
//
//@AUTHOR: Henry Verschell (hverschell)
//
//@DESCRIPTION: This component defines a scoped BAEL attribute guard.  It
// defines a single attribute for the current thread while it is in scope.
//
///Usage
///-----
// Suppose that service requests for a fictional service with id '999' are
// handled asynchronously by the function below.  Creating an instance of this
// class will set BAEL attributes for any logging performed while the
// request is being processed:
//..
//  void handleServiceRequest(const Request& request)
//  {
//     BALL_LOG_SET_CATEGORY("MY.SERVICE");
//
//     ball::ScopedAttribute attribute("request", request.selectionName());
//
//     BALL_LOG_TRACE << "Handling request: " << request << BALL_LOG_END;
//
//     // handle request here
//  }
//..
// Attribute "request" will be set in the calling thread and will affect
// publication of any BAEL messages for the lifetime of 'attribute'.

#ifndef INCLUDED_BALSCM_VERSION
#include <balscm_version.h>
#endif

#ifndef INCLUDED_BALL_ATTRIBUTECONTAINER
#include <ball_attributecontainer.h>
#endif

#ifndef INCLUDED_BALL_ATTRIBUTECONTEXT
#include <ball_attributecontext.h>
#endif

#ifndef INCLUDED_BSLMA_ALLOCATOR
#include <bslma_allocator.h>
#endif

#ifndef INCLUDED_BSLS_TYPES
#include <bsls_types.h>
#endif

#ifndef INCLUDED_BSL_IOSFWD
#include <bsl_iosfwd.h>
#endif

#ifndef INCLUDED_BSL_STRING
#include <bsl_string.h>
#endif

namespace BloombergLP {

namespace ball {
                    // ====================================
                    // class ScopedAttribute_Container
                    // ====================================

class ScopedAttribute_Container : public AttributeContainer {
    // This component-private class is a concrete implementation of the
    // 'AttributeContainer' protocol for a single attribute.

    // DATA
    Attribute d_attribute;

    // NOT IMPLEMENTED
    ScopedAttribute_Container(const ScopedAttribute_Container&);
    ScopedAttribute_Container& operator=(
                                   const ScopedAttribute_Container&);

  public:
    // CREATORS
    ScopedAttribute_Container(const char         *name,
                                   const bsl::string&  value,
                                   bslma::Allocator   *basicAllocator);
    ScopedAttribute_Container(const char         *name,
                                   int                 value);
    ScopedAttribute_Container(const char         *name,
                                   bsls::Types::Int64  value);
       // Create a BAEL attribute container holding a single rule, associating
       // the specified 'name' with the specified 'value'.  Optionally specify
       // a 'basicAllocator used to supply memory.  If 'basicAllocator' is 0 or
       // unspecified, the currently installed default allocator is used.

    virtual ~ScopedAttribute_Container();
       // Destroy this object;

    // ACCESSORS
    virtual bool hasValue(const Attribute& value) const;
       // Return 'true' if the specified 'value' is the same as the value held
       // in this container, and 'false' otherwise.

    virtual bsl::ostream& print(bsl::ostream& stream,
                                int           level = 0,
                                int           spacesPerLevel = 4) const;
        // Format this object to the specified output 'stream' at the (absolute
        // value of) the optionally specified indentation 'level' and return a
        // reference to 'stream'.  If 'level' is specified, optionally specify
        // 'spacesPerLevel', the number of spaces per indentation level for
        // this and all of its nested objects.  If 'level' is negative,
        // suppress indentation of the first line.  If 'spacesPerLevel' is
        // negative, format the entire output on one line, suppressing all but
        // the initial indentation (as governed by 'level').  If 'stream' is
        // not valid on entry, this operation has no effect.
};

                         // ==========================
                         // class ScopedAttribute
                         // ==========================

class ScopedAttribute {
    // Provides a scoped guard that sets BAEL attributes for "serviceId" and
    // "uuid" in the current thread.

    // DATA
    ScopedAttribute_Container d_container;  // contains the attribute

    const AttributeContext::iterator        // reference to attribute
                                   d_it;         // container

    // NOT IMPLEMENTED
    ScopedAttribute(const ScopedAttribute&);
    ScopedAttribute& operator=(const ScopedAttribute&);

  public:
    // CREATORS
    ScopedAttribute(const char         *name,
                         const bsl::string&  value,
                         bslma::Allocator   *basicAllocator = 0);
    ScopedAttribute(const char         *name,
                         int                 value);
    ScopedAttribute(const char         *name,
                         bsls::Types::Int64  value);
      // Set BAEL logging attributes for the current thread for the scope of
      // this object, associating the specified 'name' with the specified
      // 'value'.  Optionally specify a 'basicAllocator' used to supply memory.
      // If 'basicAllocator' is 0 or unspecified, the currently installed
      // default allocator is used.

    ~ScopedAttribute();
      // Remove the attributes managed by this object from the BAEL system, and
      // destroy this object.
};

// ============================================================================
//                          INLINE FUNCTION DEFINITIONS
// ============================================================================

                    // ------------------------------------
                    // class ScopedAttribute_Container
                    // ------------------------------------

// CREATORS
inline
ScopedAttribute_Container::ScopedAttribute_Container(
        const char         *name,
        const bsl::string&  value,
        bslma::Allocator   *basicAllocator)
: d_attribute(name, value.c_str(), basicAllocator)
{
}

inline
ScopedAttribute_Container::ScopedAttribute_Container(
        const char *name,
        int         value)
: d_attribute(name, value)
{
}

inline
ScopedAttribute_Container::ScopedAttribute_Container(
        const char         *name,
        bsls::Types::Int64  value)
: d_attribute(name, value)
{
}

// ACCESSORS
inline
bool ScopedAttribute_Container::hasValue(const Attribute& value)
                                                                          const
{
    return d_attribute == value;
}

                         // --------------------------
                         // class ScopedAttribute
                         // --------------------------

// CREATORS
inline
ScopedAttribute::ScopedAttribute(
        const char         *name,
        const bsl::string&  value,
        bslma::Allocator   *basicAllocator)
: d_container(name, value, basicAllocator)
, d_it(AttributeContext::getContext()->addAttributes(&d_container))
{
}

inline
ScopedAttribute::ScopedAttribute(
        const char *name,
        int         value)
: d_container(name, value)
, d_it(AttributeContext::getContext()->addAttributes(&d_container))
{
}

inline
ScopedAttribute::ScopedAttribute(
        const char         *name,
        bsls::Types::Int64  value)
: d_container(name, value)
, d_it(AttributeContext::getContext()->addAttributes(&d_container))
{
}

inline
ScopedAttribute::~ScopedAttribute()
{
    AttributeContext::getContext()->removeAttributes(d_it);
}
}  // close package namespace

}  // close namespace BloombergLP

#endif

// ----------------------------------------------------------------------------
// NOTICE:
//      Copyright (C) Bloomberg L.P., 2009
//      All Rights Reserved.
//      Property of Bloomberg L.P. (BLP)
//      This software is made available solely pursuant to the
//      terms of a BLP license agreement which governs its use.
// ------------------------------ END-OF-FILE ---------------------------------
