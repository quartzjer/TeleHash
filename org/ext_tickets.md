# Tickets

(very rough, being defined!)

One of the principles of telehash is to operate as distributed and decentralized as possible, and a core part of enabling that is having default methods for allowing apps to perform actions independently without requiring an online centralized resource to verify their permissions to do so.  For example, an app should be able to assert a user's identity to another app such that the receiving app can verify the user from the request alone and not be required to ask a central authority.

A ticket is the name for a special portable packet that any hashname can use and share within a network.  It is identified by the SHA1(signature) of the full packet, and that hash value is used as the id for the ticket that is referenced in many other places.

Tickets act as a temporary binding (they all are required to have an `x` expiration and can be short or long-lived) from one hashname to another of some level of trust around a shared resource or identity.  All tickets also require a `type` field with a value of one of the following (custom app-defined ones use the "_name" pattern):

* network - the `to` is trusted to be part of this network by the `from`
* app - the `to` is running software written by the `from`
* user - the `to` is acting on behalf of a user of this network as identified within this link
* friend - the `to` has verified they are acting on behalf of a friend that is trusted by a user

(TODO: broken-out examples of how to create and use these tickets, and how they are chained together)

