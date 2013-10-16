Telehash v2 draft
=================

# Introduction

> Note: This is the second major version of the Telehash protocol, the
> first one is deprecated and was a minimal experimental spec 
> to create a distributed hash table, it is a work in progress yet
> and unstable!

Telehash is a new encrypted mesh protocol enabling applications to
find, identify, and communicate directly with each other.  It is built
on public-key security (PKI) and fundamentally creates peer-to-peer
(P2P) connections using a distributed hash-table (DHT) to form the
mesh.

As a protocol, it does not provide direct end-user functionality or
application specifications. Instead, Telehash is primarily a tool for
developers of specifications and of libraries/SDKs to use in creating
modern applications which require rich experiences and private
interconnectivity at scale.

The principle idea that drove the creation and development of Telehash
is the belief that any application instance should be able to easily and
securely talk to any other application instance, whether they are two
instances of the same application, or completely different
applications. They should be able to do so directly, and in any
environment, from servers to mobile devices down to embedded systems
and sensors.

By enabling this freedom for developers as a foundation for their
applications, Telehash enables the same freedom for the people using
them - that the user can connect, share, and communicate more easily
and with control of their privacy.

The challenges and complexity today in connecting applications via
existing technologies such as APIs, OAuth, and REST is only increasing,
often forcing fundamentally insecure, centralized, and closed/gated
communication platforms.  By adopting Telehash in any application, that
application immediately has a powerful set of open tools for not only
its own needs, but can then also enable connectivity to and from
applications created by others easily. These tools include the ability
to have friends, sharing, feeds, tagging, search, notifications,
discovery, and other social patterns.

Telehash has a fundamental requirement to remain simple and
light-weight in order to support applications running on networked
devices and sensors. The design goals also include not forcing any
particular architectural design such as client-server,
centralized/federated/distributed, polling/push, REST, streaming,
publish-subscribe, or message passing... any can be used, as Telehash
simply facilitates secure connectivity between any two or more
applications.

# Protocol Details

## Glossary

  * **DHT**: Distributed Hash Table (based on [Kademlia][])
  * **NAT**: A device/router that acts as a bridge to internal IPPs
    (Network Address Translation)
  * **Hashname**: The unique ID of an individual application/instance
    using Telehash
  * **Packet**: A UDP packet less than 1400 bytes sent between any
    hashnames
  * **Switch**: The name of the software layer or service parsing
    packets for one or more hashnames
  * **Line**: When any two hashnames connect and exchange their identity
    to form a temporary encrypted session (like a VPN tunnel between
    them)
  * **Channels**: Dynamic bi-directional transport that can transfer
    reliable/ordered or lossy binary/json mixed content within any line

## Telehash Switches

In order to use Telehash in an application, the application will need
to include a software layer that talks to the Internet and processes
Telehash packets, known as a "switch".

It is highly recommended to use an existing switch library or service
for your environment, rather than creating one from scratch. This will
help insure that the security, identity, and networking aspects are
verified properly. If there isn't one which meets your needs, then we
would love your help - pull requests to list them here are welcome!

* Node.js - [node-telehash](https://github.com/quartzjer/node-telehash)
* D - [telehash.d](https://github.com/temas/telehash.d)
* Python - [plinth](https://github.com/stpeter/plinth)
* Javascript (browser) [thjs](http://github.com/quartzjer/thjs)
* C [telehash-c](http://github.com/quartzjer/telehash-c)
* Ruby - [ruby-telehash](https://github.com/dwaite/ruby-telehash)
* Go - [gogotelehash](https://github.com/dizzyd/gogotelehash)
* Java - [telehash-java](https://github.com/kubes/telehash-java)
* ObjectiveC [Objective-Telehash](https://github.com/jsmecham/Objective-Telehash)

## Creating Applications

In addition to a switch, each instance of an application must generate
its own unique, private RSA keypair. This keypair is used to used to
identify and each application instance when communicating with other
applications.

An application must also bundle and optionally provide a mechanism to
retrieve a list of "seeds" - well-known and accessible DHT members.
This will be used to bootstrap and connect into the DHT. The entries in
this list will consist minimally of the public key, IP address and port
of each seed.

## Hashnames
Every instance of an application has a unique public id that is called
it's "hashname". Any application instance can use the DHT to find
others by knowing their hashname. By default there is a single global
DHT, but Telehash also supports applications creating their own private
DHTs if needed.

The hashname, which identifies an endpoint within Telehash, is a
64-character hex string, formed by the [SHA-256][] digest of the DER binary
RSA public key. This key is required to have at least 2048 bits.

Since the DHT is based on Kademlia, this makes a [Sybil][] attack more
difficult, but not impossible. Additional techniques which are described 
later are used to further combat this attack when deciding which hashnames to
maintain in the bucket list.

Here's an example of how to create a hashname using Node.js:

``` js
var key = new
    Buffer("MIIBIjANBgkqhkiG9w0BAQEFAAOCAQ8AMIIBCgKCAQEArq38dvKzL5W2Qqp
    gQN7Ao5OWFhX04aIrYZH5sLjOzyI0gWZ6ZzpQifRk+L1yNU3nkotKfeQF5zzZvo4F7Y
    C4fZgkCN2TnvBihKj25CHVDKLOtV01LvPvEEX+oHQyUzT90FT5UUIdOqTXHY4yT+nox
    bQOAMSOsJHulpIMeDR+hPWYuZ5eZWfRimu0vEE1ujAeKGUk5avKtNtJIRDXBRRem/CB
    PG5QRe+54w94Xwp1l3VQdJaD+qRKBEG/hhSVqUHfRqVccNR4AV+q37XGDAupGc7YUJ6
    qnj7TnapQGrSno13IG+2PIhL3gB1lMWEGE/hwN1dxuUAXXsIgPU3KwIDAQAB",
    "base64");
var hashname =
    require("crypto").createHash("sha256").update(key).digest('hex');
```

## Packets

Packets are a foundational piece of the protocol. A packet uses JSON as
a core extensible and widely supported data format, but also must
support binary data transfer for efficiency.

Every packet must begin with two bytes which form a network byte-order
short unsigned integer. This integer represents the length in bytes of
the UTF-8 encoded JSON object which follows it. The JSON portion of the
packet is required, so the length must be at least 2 (the JSON document
`{}`), and less than the length of the raw UDP message minus the two 
bytes for length value.

Any remaining bytes on the packet are considered raw binary and
referenced as the 'BODY' when used in definitions below.

The format is thus:

    <length><JSON>[BODY]

An example of how to decode a packet, written in Node.js:

``` js
dgram.createSocket("udp4", function(msg){
    var length = msg.readUInt16BE(0);
    var js = JSON.parse(msg.toString("utf8", 2, length + 2));
    var body = msg.slice(length + 2);
});
```

### JSON

The JSON section of a packet often acts as a header of a binary payload.
The fields used vary depending on the context and are specified below,
but all top-level packets contain a `type` field with a string value.

### BODY

The optional BODY is always a raw binary of the remainder bytes between
the packet's total length and that of the JSON as indicated by LENGTH
above. Often a BODY is another full raw packet and will be decoded
identically to being read from the UDP socket, this pattern of one
packet enclosing/attaching another is how the RSA signatures and
encryption are implemented.

The BODY is also used as the raw (optionally binary) content transport
for channels and for any app-specific usage.

### Packet Processing

The Telehash specification as well as other applications may use
packets recursively, by embedding the (possibly encrypted) byte
representation of an inner packet into the BODY of an outer one. As a
result, switch implementations must be prepared to decode packets not
just from UDP messagess, but from a BODY or after decryption.

## Cryptography

Telehash uses several different cryptographic algorithms to secure traffic 
between applications. This section attempts to summarize the elements and how 
they are leveraged.

### Identification

Each application instance is identified by a hashname, which is derived from 
the instance's [RSA public key](rsa) via [SHA-256][] digest. This RSA key is 
represented in canonical [DER][] format, and there is no certificate or
trust chain information associated with the key.

Use of a node hashname derived from a public key (vs. one chosen randomly) 
makes a [Sybil][] attack more difficult, as you cannot target creation of
specific node identifiers. It does not make such attacks impossible, however.

For this reason, implementations must send DER-format RSA keys as the canonical 
form of their public key, and recipients (when processing [`open`](#open) and 
[`connect`](#connect) messages) must insure that the received key is in 
[DER][] format and that the received key indeed digests to the correct 
hashname.

### RSA Key pair

The application instance [RSA public key](rsa) is used to sign the body of 
[`open`](#open) packets to authenticate the sender. Incoming [`open`](#open) 
packets also contain the sender's [Elliptic Curve](ecc) public key, which is 
encrypted with the recipient's RSA public key. This EC public key is also used 
to derive an [AES][] key which partially encrypts the packet (see below). As 
a result, the RSA private key of the recipient is required to decrypt the 
packet.
  
A minimum 2048 bit key size is required for RSA keys. Implementations should
refuse to communicate with a party using shorter keys.
  
The algorithms used are RSA with [OAEP][] (SHA-1, MFG1, aka PKCS1 v2) for encrypting the 
sender's Elliptic Curve public key, and RSA with a SHA-256 digest and PKCS1 v1.5
padding for signing the encrypted inner packet.
  
The RSA public key of the recipient of an [`open`](#open) packet must be known.
There are normally three ways that you learn about the public key of a 
hashname:
  
* Public keys of any seed members must be known.
* You receive and decrypt the sender's public key as part of an [`open`](#open)
  packet
* You receive and decrypt the public key of an introduced peer as part of a
  [`connect`](#connect) request

Top level packets do not have any addressing, instead relying on the ip/port 
corresponding to a single hashname. You may be able to determine the sender of 
an [`open`](#open) packet by attempting to verify the embedded RSA signature 
against known public keys.

### Elliptic Curve Diffie-Hellman
[Elliptic Curves](ecc) (EC) are used ephemerally to generate [`line`](#line) 
packet [AES][] keys. This is used to provide forward secrecy of line 
communication.

A public EC key pair is created when you send an [`open`](#open) packet. The 
[NIST P-256][] curve group (sometimes referred to as "prime256v1" or 
"secp256r1") is used for these keys. Due to fewer implementations, EC keys 
are exchanged in "[uncompressed][]" encoding.

The public key is exchanged encrypted with the recipient's 
[RSA public key](rsa) in an [`open`](#open) packet. It is also used as part of 
[`open`](#open) packet processing, as the [SHA-256][] digest of the public 
key is used to AES encrypt the inner packet.

These keys live until the line has been established or has failed. Generally 
retries of an [`open`](#open) packet will be identical (contain the same EC 
public key).

Once you have both sent and received [`open`](#open) packets, you use 
[Elliptic Curve Diffie-Hellman](ecdh) to derive a value that becomes that basis 
of the two line keys. The forming of the two line keys is described in the next
section.

<a name='aes' />
### AES
[AES][] is used to protect the inner data of an [`open`](#open) packet, as 
well as all line communication.

AES ciphers are created with a Counter algorithm mode ([CTR][]) and 
[PKCS1.5 padding](pkcs15). The counter is always zero, and not incremented as 
part of [channel](#channel) sequencing.

An Elliptic Curve public key is sent as part of an [`open`](#open) packet, 
encrypted with the recipient's [public RSA key](rsa). The [SHA-256][] digest of
this key is used to create an AES key which is used to encrypt the inner 
packet data, and another key is created to encrypt the attached signature.

Once [`open`](#open) packets have been exchanged, the two elliptic curves are 
used with [ECDH][] to derive a value used for the two line keys. A line key 
is formed by SHA-256 digest of the concatenation of the derived ECDH value, 
binary source line identifier, and binary destination line identifier.
This results in one key used to encrypt outgoing  packets, and a separate key 
used to decrypt incoming packets.

## Packet types
When a packet is being processed from a UDP message initially, it's
JSON must contain a `type` field with a string value of `open` or
`line`, each of which determine how to process the BODY of the packet:

 * [`open`](#open) - this is a request to establish a new encrypted
   session (a `line`) and contains the data to do so
 * [`line`](#line) - this is an encrypted packet for an already
   established session

Once a line is established, all packets sent thereafter within a line
must be part of a [channel](#channel) as the content-transport method
between any two hashnames.

<a name="open" />
### `open` - Establishing a Line

A packet of `"type":"open"` is used to establish a temporary encrypted
session between any two hashnames, this session is called a "line". You
must know the RSA public key of the recipient in order to send an
`open` message to them.

Every open packet requires the additional JSON keys of `open`, `iv`,
and `sig`, here's an example of the JSON part of the packet:

```js
{
    "type":"open",
    "open":"ZkQklgyD91XQaih07VBUGeQmAM9tnR5qMGMavZ9TNqQMCVfTW8TxDr9y37cgB8g6r9dngWLjXuRKe+nYNAG/1ZU4XK+GiR2vUBS8VTcAMzBcUls+GIfZU6WO/zEIu4ra1I1vI8qnYY5MqS/FQ/kMXk9RyzERaD38IWZLk3DYhn8VYPnVnQX62mE5u20usMWQt99F8ITLy972nOhdx5y9RUnnSrtc1SD9xr8O0rco22NtOEWC3uAISwC9xuihT+U7OEcvSKolScFI4oSpRu+DQWl19EAuG9ACqhs5+X3qNeBRMSH8w5+ThOVHaAWKGfFs/FNMdAte3ki8rFesMtfhXQ==",
    "iv":"60aa6514ef28178f816d701b9d81a29d",
    "sig":"o6buYor8o3YXkPIDJjufn9udfWDJt5hrgnVtKtvZI2ObOPlPSqlb2AdH6QsC7CuwtboGlt6eMbE7Ep6Js2CXeksXTCSZOJ99US7TH0kZ1H1aDqxYpQlM6BADOjG6YOcW+EhniotNUBiw3r02Xt4ohSm0wXxQ97JM95ntFBRnWr1vG25d+5pJQE4LyN2TwB4uApu9zeUoTPhF7daJQOcIMn9en+XxyuBsG61oR/x29bpaoZJGnKrk2DGH1jDnI5GpxIKUbT/Pa7QOlrICUCjGDgxy2TMQ+fiip5sIflxtFUPM/BV9mh4K7/ZaekJXTFfG2FKvJFytQkWbisDVy5EbEA=="
}
```

The required values of the open packet are defined as:

   * `open` - a base64 string value that is is created by generating a
     new elliptic (ECC) public key and using RSA to encrypt it *to* the
	 recipient's RSA public key. The ECC keypair should be generated
	 using the P-256 ([nistp256/secp256r/X9.62 prime256v1] 
	 (http://tools.ietf.org/html/rfc6239#page-4)) curve. The key
     should be in the uncompressed form, 65 bytes (ANSI X9.63 format). The RSA encryption
     should use PKCS1 OAEP (v2) padding.
   * `iv` - 16 random bytes hex encoded to be used as an initialization
     vector
   * `sig` - a string created by first using the sender's RSA public key
     to sign (SHA256 hash and PKCS1 v1.5 padding) the attached
     (encrypted) binary body, then encrypt that signature (detailed below), then base64 encoded.

The BODY of the open packet will be a binary encrypted blob containing
another packet. The encryption is done using AES-256-CTR using the IV
value above. The AES key is formed by doing a SHA-256 hash of the ECC
public key in uncompressed form. 

Here's an example of the JSON content of this inner packet before
encryption:

```js
{
    "to":"851042800434dd49c45299c6c3fc69ab427ec49862739b6449e1fcd77b27d3a6",
    "at":1375983687346,
    "line":"8b945f90f08940c573c29352d767fee4"
}
```

The inner packet's required values are defined as:

   * `to` - which hashname this line is being created to
   * `line` - the unique id the recipient must use for sending any line
     packets, 16 random bytes hex encoded
   * `at` - an integer timestamp (milliseconds since epoch) of when it
     was sent, used to another open request is newer
   * `family` - an [optional field](#family) that can be included/set
     by the app to add a grouping or other identity

This inner packet holds the sender's RSA public key as the BODY
attachment, in binary DER format.

#### Packet Generation

A rough order of the steps needed create a new open packet are:

  1. Verify you have the recipient public key. If you do not have the
     recipient's public key, you may need to trigger the recipient to
     connect to you first. This is done via a [`connect`](#connect),
     described later.
  2. Generate an IV and a line identifier from a secure random source,
     both 16 bytes
  3. Generate a new elliptic curve keypair, based on the "nistp256"
     curve
  4. SHA-256 hash the public elliptic key to form the encryption key
     for the inner packet
  5. Form the inner packet containing a current timestamp `at`, `line`
     identifier, recipient `hashname`, and `family` (if you have such a
     value). Your own RSA public key is the packet BODY in the binary DER format.
  6. Encrypt the inner packet using the hashed public elliptic key from
     #4 and the IV you generated at #2 using AES-256-CTR.
  7. Create a signature from the encrypted inner packet using your own
     RSA keypair, a SHA 256 digest, and PKCSv1.5 padding
	8. Encrypt the signature using a new AES-256-CTR cipher with the same IV and a new SHA-256 key hashed from the public elliptic key + the line value (16 bytes from #5), then base64 encode the result as the value for the `sig` param.
  9. Create an `open` param, by encrypting the public elliptic curve key
     you generated (in uncompressed form, aka ANSI X9.63) with the recipient's RSA
     public key and OAEP padding.
  10. Form the outer packet containing the `open` type, `open` param, the
     generated `iv`, and the `sig` value.
  11. If you have also received an open packet from the recipient
      hostname, you may use it now to generate a `line` shared secret,
	  using the ECC public key they sent in their `open` packet and
	  Elliptic Curve Diffie-Hellman (ECDH).

An example of the generation of an open packet in Node.js is:

 ```js
 var eccSession = new eccKey("nistp256");

 var attached = {js:{}, body:myRSAPublicKey};
 attached.js.to = recipientHashname;
 attached.js.at = Date.now();
 var line = crypto.randomBytes(16);
 attached.js.line = line.toString("hex");
 var attachedRaw = packetEncode(attached);

 var open = {js:{type:"open"}};
 open.js.open = rsa(recipientRSAPublicKey).encrypt(eccSession.PublicKey, "PKCS1_OAEP_PADDING").toString("base64");

 var aesKey = crypto.createHash("sha256").update(eccSession.PublicKey).digest();
 var aesIV = crypto.randomBytes(16);
 var aesCipher = crypto.createCipheriv("AES-256-CTR", aesKey, aesIV);
 open.body = Buffer.concat([aesCipher.update(attachedRaw),aesCipher.final()]);
 open.js.iv = aesIV.toString("hex");
 aesKey = crypto.createHash("sha256").update(eccSession.PublicKey).update(line).digest();
 var aesCipher = crypto.createCipheriv("AES-256-CTR", aesKey, aesIV); 
 open.js.sig = Buffer.concat([aesCipher.update(rsa(myRSAPrivateKey).hashAndSign("sha256", open.body, "PKCS1_PADDING")),aesCipher.final()]).toString("base64");

 var openRaw = packetEncode(open);
 ```

#### Packet Processing
To process an `open` packet, the recipient will follow the following
rough order of steps:

  1. Using your private key and OAEP padding, decrypt the `open` param,
     extracting the ECC public key (in uncompressed form) of the sender
  2. Hash the ECC public key with SHA-256 to generate an AES key
  3. Decrypt the inner packet using the generated key and IV value with
     the AES-256-CTR algorithm.
  4. Verify the `to` value of the inner packet matches your hashname
  5. Extract the RSA public key of the sender from the inner packet BODY (binary DER format)
  6. SHA-256 hash the RSA public key to derive the sender's hashname
  7. Verify the `at` timestamp is both within a reasonable amount of
     time to account for network delays and clock skew, and is newer
     than any other 'open' requests received from the sender.
  8. SHA-256 hash the ECC public key with the 16 bytes derived from the inner `line` hex value to generate an new AES key
  9. Decrypt the outer packet `sig` value using AES-256-CTR with the key from #8 and the same IV value as #3.
  10. Using the RSA public key of the sender, verify the signature (decrypted in #9) of
     the original (encrypted) form of the inner packet
  11. If an open packet has not already been sent to this hashname, do
     so by creating one following the steps above
  12. After sending your own open packet in response, you may now generate a `line` shared secret using the received and sent ECC public keys and Elliptic Curve Diffie-Hellman (ECDH).

Example open packet validation logic in node (simplified):

```js
// generate these to identify the line being created for each sender
var myEccSession = new eccKey("nistp256");
var myLineId = crypto.randomBytes(16);

var packet = packetDecode(openRaw);

var open = rsa(myRSAPrivateKey).decrypt(packet.js.open, "base64", "PKCS1_OAEP_PADDING");
var senderEccPublicKey = new eccKey("nistp256", open);

var aesKey = crypto.createHash('sha256').update(open).digest();
var aesIV = new Buffer(packet.js.iv, "hex");
var aesDecipher = crypto.createDecipheriv("AES-256-CTR", aesKey, aesIV);
var attached = packetDecode(Buffer.concat([aesDecipher.update(packet.body),aesDecipher.final()]);

var line = new Buffer(attached.js.line, "hex");
if(attached.js.to !== self.hashname || !line) return; // must match recipients hashname and have a line

var senderRSAPublicKey = deciphered.body;
var aesKey = crypto.createHash('sha256').update(open).update(line).digest();
var aesDecipher = crypto.createDecipheriv("AES-256-CTR", aesKey, aesIV);
var sig = Buffer.concat([aesDecipher.update(new Buffer(packet.js.sig, "base64")),aesDecipher.final()]);
var valid = rsa(senderRSAPublicKey).hashAndVerify("sha256", packet.body, sig, "PKCS1_PADDING");
if(!valid) return;

// generate the aes session key used for all of the line encryption
var ecdheSecret = myEccSession.deriveSharedSecret(senderEccPublicKey);
var lineEncryptKey = crypto.createHash("sha256")
  .update(ecdheSecret)
  .update(myLineId)
  .update(line)
  .digest();
var lineDecryptKey = crypto.createHash("sha256")
  .update(ecdheSecret)
  .update(line)
  .update(myLineId)
  .digest();
```

<a name="line" />
### `line` - Packet Encryption

A packet with a `"type":"line"` is only sent/valid after an open has
been exchanged, and is required to have a `line` parameter (such as
`"line":"be22ad779a631f63336fe051d5aa2ab2"`) with the value being the
same as the one the recipient sent in its `open` packet, and a random
IV value (such as `"iv":"8b945f90f08940c573c29352d767fee4"`) used for
the AES encryption.  This ensures that no line packets can be received
without being invited in an open. Any unknown ones are just ignored.

The BODY is a binary encoded encrypted packet using AES-256-CTR with
the key that was generated for the line and the 16-byte initialization
vector decoded from the included "iv" hex value.  Once decrypted, the
recipient then processes it as a normal packet (LENGTH/JSON/BODY) from
the sending hashname.  All decrypted packets must contain a "c"
value as defined below to identify the channel they belong to.

<a name="channel" />
### `channel` - Content Transport

All data sent between any two hashnames (inside a line packet) must
contain a `c` parameter with a unique value (16 random bytes hex
encoded) determined by the sender for each channel.

A channel always begins with `type` parameter in the first packet, to distinguish to the
recipient what kind of channel it is. A channel may have only one response,
or it may be long-lived with many packets exchanged using the same
"c" identifier (depending on the type of channel). At any point a
channel can be closed by sending an `"end":true`, and if a channel is
being closed due to an error, a string message can be included as the
value of the `"err":"message"`.

The following values for `type` are used to locate and communicate with
application instances on the DHT. They are part of the core spec, and
must be implemented by all switches:

  * [`seek`](#seek) - return any pointers to other closer hashnames for
    the given `hash` (DHT), answer contains `see`
  * [`peer`](#peer) - ask the recipient to make an introduction to one
    of it's peers
  * [`connect`](#connect) - a request asking to try to open a
    connection to a given hashname (result of a `peer`)

Additional `type` values can be defined by any application as long as they begin with an underscore like `"type":"_custom"` and all custom values in the JSON are in an underscore key and object value like:

```json
{
	"c":"ab945f90f08940c573c29352d767fee4",
	"type":"_hello",
	"_":{"custom":"values"}
}
```

Common usage patterns are also being defined as extensions, such that switches can start to
support them internally easily, these are still experimental:

  * `[sockets][]` - raw socket proxy/transport
  * `[tickets][]` - a way to create portable data containers that are signed by 
    a hashname

<a name="reliable" />
### Channel Reliability

All UDP packets are by their very nature lossy, so channels must
be able to replicate TCP features such as reliability, retransmission,
and buffering/backpressure mechanisms. This is done by requiring a
lightweight `"seq":0` field on every packet. All seq values start at 0
and increment per packet sent. A buffer of these packets must be kept
keyed by the seq value until the receiving hashname has responded
confirming them in a `ack` and not in the `miss`. The `ack` is the
highest known `seq` value received. The `miss` is an array of integers
and must be sent along with any `ack` if in the process of receiving
packets there were any missing sequences, containing in any order the
missing sequence values up to the `ack`.  Upon receipt those missed
packets should be resent verbatim.

When a channel is started, only one packet may be sent (seq:0) until the recipient has responded and ack'd that initial type packet, only then can additional packets be sent with that same channel id.

By default a channel should be invalidated if a sequence has been missed
three or more times, or there's more than 100 missed packets by default
(senders cannot send more than that without a confirming range). When
there's consistently missing packets, senders should limit the number
of packets beyond the confirmed range. (needs more examples/definition)

When reliability isn't required for a channel, either side can send an
`ack` of the last received `seq` value without tracking any misses,
while still following the 100-max-outstanding rule to provide for any
congestion/loss detection.

<a name="seek" />
### `"type":"seek"` - Finding Hashnames (DHT)

The core of telehash is a basic Kademlia-based DHT. The bulk of the complexity is in the rules around maintaining a mesh of lines and calculating distance explained [below](#kademlia). The `"seek":"851042800434dd49c45299c6c3fc69ab427ec49862739b6449e1fcd77b27d3a6"` value is always another hashname that the app is trying to connect to.

When one hashname wants to connect to another hashname, it finds the closest lines it knows and sends a `seek` containing the hash value to them.  They return a compact `"see":[...]` array of addresses that are closest to the hash value.  The addresses are a compound comma-delimited string containing the "hash,ip,port" (these are intentionally not JSON as the verbosity is not helpful here), for example "1700b2d3081151021b4338294c9cec4bf84a2c8bdf651ebaa976df8cff18075c,123.45.67.89,10111". 

<a name="peer" />
### `"type":"peer"` - Introductions to new hashnames

For any hashname to send an open to another it must first have it's hashname, so there is a two step process starting with a peer request. Since new hashnames are discovered only from another (in the `see` values), they are tracked as a "via" so that they can be sent a peer request when a connection is being made to a hashname they sent.

This also serves as a workaround if any NAT exists, so that the two hashnames can send a packet to each other to make sure the path between them is open, this is called "hole punching." A peer request requires a `"peer":[...]` where the value is an array of hashnames the sender is trying to reach. The recipient of the peer request must then send a connect (below) to each of the target hashnames (that it already must have an open line to).

<a name="connect" />
### `"type":"connect"` - Connect to a hashname

The connect request is an immediate result of a peer request and must also contain an `"ip":"1.2.3.4"` and `"port":5678` with the values being of the peer requestor and a BODY of their public key.

The recipient can use the given IP, port, and public key to send an open request to the target.  If a NAT is suspected to exist, the target should have already sent a packet to ensure their side has a path mapped through the NAT and the open should then make it through.

# Switch Behaviors

Besides parsing the protocol, decoding the packets and processing the different channel types, in order to fully implement telehash a switch must also internally track and respond with the correct behaviors to support both the DHT and manage the reliability of channels that require it.

<a name="kademlia" />
## [Kademlia][]

(this area in progress...)

Every switch must have both a startup/seeding routine, and a background line maintenance process in order to properly support the Kademlia-based DHT.

The core data structure to support this within a switch is a list of "buckets", one for each bit of distance between it's own hashname and every other hashname encountered, 160 of them.  These buckets can contain a variable number of hashnames each, but it's recommended for any switch to limit the size of each bucket to a reasonable number more than 1 and less than 10 so as to not consume too much memory or network resources maintaining lines.

The seeding process involves recursively performing a [seek](#seek) for it's own hashname against the list of seeds (provided by the app).

The maintenance process involves tracking all the hashnames seen (result of any seek) and trying to ensure a line is open to the minimum number of hashnames in each bucket.

A new seek should be sent to every idle/inactive line at least once every 60 seconds if the switch believes it is behind a NAT (to maintain the NAT mapping), or once every hour if it knows it is not.  By seeking it's own hashname, any new hashnames near it will be returned to help maintain the DHT.

## Channel Congestion Control

(this area in progress...)

Reliability is optional for channels based on their type, each type must define if it requires it or not, and custom ones from the application must signal if they don't require it.

<a name="family" />
## Family Usage

The family key included in the open packet can be used to create a private/closed DHT by only responding to packets that have a valid value.  The app should always set the family value and provide a function to validate it.

It is also used to optimize the DHT for both resistance to general flooding and for search speed by creating an affinity for other hashnames with a trusted family value.  If an app predominantly only searches for and connects to hashnames created by other instances of itself, it should always send a family value that its other instances can validate.  Every switch must include the ability to maintain a preference in it's bucket list for the DHT, and if the app has a family then up to 50% of each bucket should be dedicated to any peers that have been validated.


[rsa]:     https://en.wikipedia.org/wiki/RSA_(algorithm)
[sha-256]: https://en.wikipedia.org/wiki/SHA-2
[sybil]:   https://en.wikipedia.org/wiki/Sybil_attack
[ecc]:     https://en.wikipedia.org/wiki/Elliptic_curve_cryptography
[der]:     https://en.wikipedia.org/wiki/Distinguished_Encoding_Rules
[aes]:     https://en.wikipedia.org/wiki/Advanced_Encryption_Standard
[oaep]:    https://en.wikipedia.org/wiki/Optimal_asymmetric_encryption_padding
[ecdh]:    https://en.wikipedia.org/wiki/Elliptic_curve_Diffie–Hellman
[ctr]:     https://en.wikipedia.org/wiki/CTR_mode#Counter_.28CTR.29
[pkcs15]:  https://en.wikipedia.org/wiki/PKCS1

[nist p-256]: http://csrc.nist.gov/groups/ST/toolkit/documents/dss/NISTReCur.pdf
[uncompressed]: https://www.secg.org/collateral/sec1_final.pdf

[sockets]: ext_sockets.md
[tickets]: ext_tickets.md
[kademlia]: https://en.wikipedia.org/wiki/Kademlia
