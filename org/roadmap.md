Experimental!
=============

## Topics / Search

By default the `seek` and `see` allow searching for and discovery of another hashname and facilitate connecting to them, but often an app will need to find instances based on some other form of identity (username) or temporary address (group chat).  While an operator can completely facilitate this process in a centralized way, the goal of telehash is to enable as much functionality as possible via decentralized methods.

To search for another instance based on a non-hashname id, the `seek` is used identically as when searching for a hashname, the target id is hashed and sent as the value.  When responding any hashname returns a `see` like normal of closer hashnames, but may also return a `refs` array (identical format to the `see`) of hashnames that have registered with it as a reference for the incoming `seek` value.  The seeker should continue processing any `see` values and collecting all refs so that the app can determine what it wants to do with the search results.

To contact any hashname in a ref that is behind a NAT it may need to be sought as well as there's no guarantee that the hashname returning it in a ref may have a line open still.

To register a reference with any hashname it must be sent in a voucher (from a trusted hashname, either an operator or some other network-defined trust pattern). A packet with a `ref` and the value is the hash to match against for an incoming `seek`.  The voucher has a built-in expiration, and the hashname storing the reference should store the newest 10 at most per hash and a reasonable max limit of 100 unique refs per requesting hashname to prevent abuse.  There is no default response to a `ref` unless the packet also contained the identical `seek` in which a normal response would then include the new reference in a `refs` result.

To cancel a reference at any point, the requestor simply sends the identical `ref` value along with a `end` of true (no voucher needed), also no response unless a normal included `seek` generates one.

All `ref` requests must also be signed or inline.

## Profiles / User Identity

(rough notes)

There must be a 3rd party authority for a user identity scheme (by it's very definition), and that authority can generate vouchers for any hashname during authentication where it then associates the hashname with the user's profile/id (and expiration).

The vouchers can then be sent to any other hashname and act as a temporary association of that user to that hashname, enabling communication to them.

The auth vouchers must contain the unique id and the human-readable name represented by the id, and other optional but common fields of an image url and general url.

The network may also support searching that unique id to request an extended profile object from running instances or from an operator.

## App / Instance Identity

(rough notes)

There are many combinations of identity patterns and parties involved when considering the operators/owners of networks, the creators of apps joined to one or more networks, and the users of the apps and networks.

One common pattern is the example of "facebook.com" being the network, and "causes.org" being the app that is joined to the facebook network for a user. In telehash, the causes app needs to identify itself to facebook as causes as well as once it is authorized by a user, to carry the identity of that user as well. This chaining of multiple parties involves two vouchers, the first one is between the instance of the causes app at the start of an authorization and causes's own network, vouching that the hashname of that instance is indeed from causes.  That causes voucher is then presented along with the user authentication flow, and the successful result will create a new hashname of the public key of the instance joined to the facebook network along with a voucher for that facebook user's id.

## Devices / Sensors

Devices, sensors, or other low-power, low-resource, or intermittently connected instances often have special requirements, may not be able to maintain lines and need help being part of a network with smart gateways.  Additional patterns need to be explored here how to make these patterns easier to solve with telehash.