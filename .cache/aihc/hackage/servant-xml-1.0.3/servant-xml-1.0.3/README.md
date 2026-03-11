# servant-xml

Servant support for XML.

Types with a `ToXml` instance will be automatically marshalled into XML
and successfully returned by Servant endpoints.
Types with a `FromXml` instance can be decoded from request bodies.

In implementing these typeclass instances, you can use the primatives
found in the `xmlbf` library.
