CQL Cassandra driver
====================

This library uses the [cql](http://hackage.haskell.org/package/cql) library
which implements Cassandra's CQL protocol and complements it with the
neccessary I/O operations. The feature-set includes:

*Node discovery*

The driver discovers nodes automatically from a small set of bootstrap nodes.

*Customisable load-balancing policies*

In addition to pre-built LB policies such as round-robin, users of this
library can provide their own policies if desired.

*Support for connection streams*

Requests can be multiplexed over a few connections.

*Customisable retry settings*

Support for default retry settings as well as local overrides per query.

*Prepared queries*

Prepared queries are an optimisation which parse and prepare a query only
once on Cassandra nodes but execute it many times with different concrete
values.

*TLS support*

Client to node communication can optionally use transport layer security
(using HsOpenSSL).

License
=======

See [LICENSE](./LICENSE).

Cassandra Logo License
======================

Copyright © 2018 Apache Software Foundation

Licensed under the Apache License, Version 2.0 (the "License"); you may not use
this file except in compliance with the License. You may obtain a copy of the
License at https://www.apache.org/licenses/LICENSE-2.0. Unless required by
applicable law or agreed to in writing, software distributed under the License
is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
KIND, either express or implied. See the License for the specific language
governing permissions and limitations under the License.

